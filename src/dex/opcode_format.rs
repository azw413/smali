// Tracing options

//macro_rules! smali_trace { ($($arg:tt)*) => { eprintln!($($arg)*); } }
macro_rules! smali_trace {
    ($($arg:tt)*) => {};
}

//macro_rules! smali_warn { ($($arg:tt)*) => { eprintln!($($arg)*); } }
macro_rules! smali_warn {
    ($($arg:tt)*) => {};
}

use crate::dex::error::DexError;
use crate::dex::opcodes::OPCODES;
use crate::smali_parse::parse_op;
use crate::types::{
    ArrayDataDirective, ArrayDataElement, Label, PackedSwitchDirective, SmaliOp,
    SparseSwitchDirective, SparseSwitchEntry,
};
use bitflags::bitflags;
use once_cell::sync::Lazy;
use rangemap::RangeInclusiveMap;
use std::collections::HashMap;
use std::ops::RangeInclusive;

const ARRAY_DATA_SIGNATURE: u16 = 0x0300;
const PACKED_SWITCH_SIGNATURE: u16 = 0x0100;
const SPARSE_SWITCH_SIGNATURE: u16 = 0x0200;

/// Maps raw Dalvik register numbers to smali-style names (vN / pN)
/// based on a method's total register count and input (parameter) size.
pub struct RegMapper {
    pub registers_size: u16,
    pub ins_size: u16,
}

impl RegMapper {
    #[inline]
    pub fn map_name(&self, raw: u16) -> String {
        let params_base = self.registers_size.saturating_sub(self.ins_size);
        if raw >= params_base {
            format!("p{}", raw - params_base)
        } else {
            format!("v{}", raw)
        }
    }
}

#[inline]
fn fmt_reg(mapper: Option<&RegMapper>, raw: u16) -> String {
    if let Some(m) = mapper {
        m.map_name(raw)
    } else {
        format!("v{}", raw)
    }
}

///
/// Resolves DEX pool references (string/type/field/method/...) into printable Smali text.
/// The decoder works with any resolver; callers can pass a real Dex-backed resolver later.
pub trait RefResolver {
    fn string(&self, idx: u32) -> String;
    fn type_desc(&self, idx: u32) -> String;
    fn field_ref(&self, idx: u32) -> (String, String, String); // (class, name, desc)
    fn method_ref(&self, idx: u32) -> (String, String, String); // (class, name, proto)
    fn call_site(&self, idx: u32) -> String;
    fn method_handle(&self, idx: u32) -> String;
    fn proto(&self, idx: u32) -> String;
}

/// Minimal placeholder resolver that prints stable placeholders like string@42.
/// This keeps the decoder usable without a DexFile context.
pub struct PlaceholderResolver;

impl RefResolver for PlaceholderResolver {
    fn string(&self, idx: u32) -> String {
        format!("\"string@{}\"", idx)
    }
    fn type_desc(&self, idx: u32) -> String {
        format!("Ltype@{};", idx)
    }
    fn field_ref(&self, idx: u32) -> (String, String, String) {
        // Disambiguate placeholder pools: field refs live in a different index space
        // than type/class ids in real DEX. Use a distinct class prefix so it’s obvious
        // in logs/traces that this was derived from a field-id, not a type-id.
        (
            format!("Lfield_class@{};", idx),
            format!("field@{}", idx),
            String::from("Ljava/lang/Object;"),
        )
    }
    fn method_ref(&self, idx: u32) -> (String, String, String) {
        // Same idea: method refs are a separate pool. Keep a valid descriptor so the
        // smali parser accepts it, but mark the class with a method-specific prefix.
        (
            format!("Lmethod_class@{};", idx),
            format!("method@{}", idx),
            String::from("()V"),
        )
    }
    fn call_site(&self, idx: u32) -> String {
        format!("\"callsite@{}\"", idx)
    }
    fn method_handle(&self, idx: u32) -> String {
        format!("\"handle@{}\"", idx)
    }
    fn proto(&self, _idx: u32) -> String {
        String::from("()V")
    }
}

/// Public helper types for assembling `SmaliOp` sequences into encoded Dalvik bytecode.
///
/// These scaffolding APIs mirror the decoder surfaces in this file so future assembler
/// phases can reuse opcode metadata/format logic when lowering high-level instructions
/// into their on-disk encodings.
pub mod assemble {
    use super::canonical_label_name;
    use super::{
        ARRAY_DATA_SIGNATURE, Opcode, PACKED_SWITCH_SIGNATURE, ReferenceType, RegMapper,
        SPARSE_SWITCH_SIGNATURE, opcode_by_name,
    };
    use crate::dex::error::DexError;
    use crate::smali_ops::{FieldRef, MethodRef, RegisterRange, SmaliRegister};
    use crate::types::{
        ArrayDataDirective, ArrayDataElement, DexOp, Label, MethodSignature, Modifier,
        PackedSwitchDirective, SmaliMethod, SmaliOp, SparseSwitchDirective, TypeSignature,
        VerificationErrorRef, parse_methodsignature,
    };
    use std::collections::HashMap;

    /// Resolves high-level references (strings, fields, methods, etc.) into the
    /// concrete indices expected by encoded Dalvik instructions.
    #[allow(dead_code)]
    pub trait AssemblerIndexResolver {
        fn string_index(&self, value: &str) -> Result<u32, DexError>;
        fn type_index(&self, descriptor: &str) -> Result<u32, DexError>;
        fn field_index(
            &self,
            class_desc: &str,
            name: &str,
            type_desc: &str,
        ) -> Result<u32, DexError>;
        fn method_index(
            &self,
            class_desc: &str,
            name: &str,
            proto: &MethodSignature,
        ) -> Result<u32, DexError>;
        fn proto_index(&self, proto: &MethodSignature) -> Result<u32, DexError>;
        fn call_site_index(&self, name: &str) -> Result<u32, DexError>;
        fn method_handle_index(&self, literal: &str) -> Result<u32, DexError>;
    }

    /// Result of assembling a single method body into Dalvik bytecode.
    #[allow(dead_code)]
    #[derive(Debug, Default, Clone)]
    pub struct LineEvent {
        pub offset_cu: u32,
        pub line: u32,
    }

    #[derive(Debug, Clone)]
    pub enum DebugDirective {
        PrologueEnd,
        EpilogueBegin,
        StartLocal {
            register: u16,
            name: Option<String>,
            descriptor: Option<String>,
            signature: Option<String>,
        },
        EndLocal {
            register: u16,
        },
        RestartLocal {
            register: u16,
        },
    }

    #[derive(Debug, Clone)]
    pub struct DebugDirectiveEvent {
        pub offset_cu: u32,
        pub directive: DebugDirective,
    }

    #[allow(dead_code)]
    #[derive(Debug)]
    pub struct MethodAssemblyResult {
        pub registers_size: u16,
        pub ins_size: u16,
        pub outs_size: u16,
        pub tries_size: u16,
        pub encoded_code_units: Vec<u16>,
        pub label_offsets: HashMap<String, u32>,
        pub line_events: Vec<LineEvent>,
        pub debug_events: Vec<DebugDirectiveEvent>,
    }

    /// Entry point for lowering `SmaliOp` instructions into encoded bytecode.
    #[allow(dead_code)]
    pub struct MethodAssembler<'a, R: AssemblerIndexResolver> {
        resolver: &'a R,
        api_level: i32,
        art_version: i32,
        mapper: Option<RegMapper>,
    }

    impl<'a, R: AssemblerIndexResolver> MethodAssembler<'a, R> {
        pub fn new(resolver: &'a R, api_level: i32, art_version: i32) -> Self {
            MethodAssembler {
                resolver,
                api_level,
                art_version,
                mapper: None,
            }
        }

        pub fn with_reg_mapper(mut self, mapper: RegMapper) -> Self {
            self.mapper = Some(mapper);
            self
        }

        pub fn assemble_method(
            &self,
            class_desc: &str,
            method: &SmaliMethod,
        ) -> Result<MethodAssemblyResult, DexError> {
            let layout = MethodRegisterLayout::new(method)?;
            let mut state = MethodAssemblyState::new(layout);

            for op in &method.ops {
                match op {
                    SmaliOp::Label(label) => state.define_label(label)?,
                    SmaliOp::Line(line) => state.record_line(*line),
                    SmaliOp::Prologue => {
                        state.record_debug(DebugDirective::PrologueEnd);
                    }
                    SmaliOp::Epilogue => {
                        state.record_debug(DebugDirective::EpilogueBegin);
                    }
                    SmaliOp::Local {
                        register,
                        name,
                        descriptor,
                        signature,
                    } => {
                        let reg = resolve_register(&state.layout, register)?;
                        state.record_debug(DebugDirective::StartLocal {
                            register: reg,
                            name: name.clone(),
                            descriptor: descriptor.clone(),
                            signature: signature.clone(),
                        });
                    }
                    SmaliOp::EndLocal { register } => {
                        let reg = resolve_register(&state.layout, register)?;
                        state.record_debug(DebugDirective::EndLocal { register: reg });
                    }
                    SmaliOp::RestartLocal { register } => {
                        let reg = resolve_register(&state.layout, register)?;
                        state.record_debug(DebugDirective::RestartLocal { register: reg });
                    }
                    SmaliOp::Catch(_) => {}
                    SmaliOp::Op(dex_op) => {
                        let inst = self.lower_dex_op(dex_op, class_desc, method, &state.layout)?;
                        state.push_instruction(inst, true);
                    }
                    SmaliOp::ArrayData(array) => state.push_array_data(array)?,
                    SmaliOp::PackedSwitch(dir) => state.push_packed_switch(dir)?,
                    SmaliOp::SparseSwitch(dir) => state.push_sparse_switch(dir)?,
                }
            }

            state.finish(self.api_level, self.art_version)
        }

        pub fn api_level(&self) -> i32 {
            self.api_level
        }

        pub fn art_version(&self) -> i32 {
            self.art_version
        }

        pub fn resolver(&self) -> &R {
            self.resolver
        }

        pub fn mapper(&self) -> Option<&RegMapper> {
            self.mapper.as_ref()
        }
    }

    /// Helper describing how a yet-to-be-implemented opcode encoder should look up
    /// referenced indices based on the opcode's metadata.
    #[derive(Debug)]
    #[allow(dead_code)]
    pub struct ReferenceRequest<'a> {
        pub reference_type: ReferenceType,
        pub class_desc: &'a str,
        pub name: &'a str,
        pub proto: &'a MethodSignature,
    }

    #[derive(Debug, Clone, Copy)]
    struct MethodRegisterLayout {
        registers_size: u16,
        ins_size: u16,
    }

    #[derive(Debug, Clone, Copy)]
    struct PackedInvokeRegisters {
        regs: [u16; 5],
        count: u8,
    }

    impl MethodRegisterLayout {
        fn new(method: &SmaliMethod) -> Result<Self, DexError> {
            let ins_size = compute_ins_size(method);
            let locals = method.locals as u32;
            if locals > u16::MAX as u32 {
                return Err(DexError::new("method locals exceed register limit"));
            }
            let registers_size = ins_size as u32 + locals;
            if registers_size > u16::MAX as u32 {
                return Err(DexError::new("method register count exceeds u16 range"));
            }
            Ok(MethodRegisterLayout {
                registers_size: registers_size as u16,
                ins_size,
            })
        }
    }

    struct MethodAssemblyState {
        layout: MethodRegisterLayout,
        instructions: Vec<LoweredInstruction>,
        labels: HashMap<String, usize>,
        current_offset_cu: usize,
        pending_labels: Vec<String>,
        line_events: Vec<LineEvent>,
        debug_events: Vec<DebugDirectiveEvent>,
    }

    impl MethodAssemblyState {
        fn new(layout: MethodRegisterLayout) -> Self {
            MethodAssemblyState {
                layout,
                instructions: Vec::new(),
                labels: HashMap::new(),
                current_offset_cu: 0,
                pending_labels: Vec::new(),
                line_events: Vec::new(),
                debug_events: Vec::new(),
            }
        }

        fn record_line(&mut self, line: u32) {
            self.line_events.push(LineEvent {
                offset_cu: self.current_offset_cu as u32,
                line,
            });
        }

        fn record_debug(&mut self, directive: DebugDirective) {
            self.debug_events.push(DebugDirectiveEvent {
                offset_cu: self.current_offset_cu as u32,
                directive,
            });
        }

        fn define_label(&mut self, label: &Label) -> Result<(), DexError> {
            let name = canonical_label_name(&label.0);
            if let Some(existing) = self.labels.get(&name) {
                if *existing == self.current_offset_cu {
                    return Ok(());
                }
                return Err(DexError::new(&format!(
                    "duplicate label :{} encountered during assembly",
                    name
                )));
            }
            self.labels.insert(name.clone(), self.current_offset_cu);
            self.pending_labels.push(name);
            Ok(())
        }

        fn push_instruction(&mut self, mut inst: LoweredInstruction, consume_labels: bool) {
            inst.offset_cu = self.current_offset_cu;
            self.current_offset_cu += inst.size_cu();
            self.instructions.push(inst);
            if consume_labels {
                self.pending_labels.clear();
            } else {
                for name in &self.pending_labels {
                    if let Some(entry) = self.labels.get_mut(name) {
                        *entry = self.current_offset_cu;
                    }
                }
            }
        }

        fn push_array_data(&mut self, directive: &ArrayDataDirective) -> Result<(), DexError> {
            self.ensure_payload_alignment(".array-data")?;
            let units = encode_array_data_units(directive)?;
            let inst = LoweredInstruction::new_payload(LoweredKind::ArrayPayload { units });
            self.push_instruction(inst, true);
            Ok(())
        }

        fn push_packed_switch(
            &mut self,
            directive: &PackedSwitchDirective,
        ) -> Result<(), DexError> {
            self.ensure_payload_alignment(".packed-switch")?;
            let inst = LoweredInstruction::new_payload(LoweredKind::PackedSwitchPayload {
                directive: directive.clone(),
            });
            self.push_instruction(inst, true);
            Ok(())
        }

        fn push_sparse_switch(
            &mut self,
            directive: &SparseSwitchDirective,
        ) -> Result<(), DexError> {
            self.ensure_payload_alignment(".sparse-switch")?;
            let inst = LoweredInstruction::new_payload(LoweredKind::SparseSwitchPayload {
                directive: directive.clone(),
            });
            self.push_instruction(inst, true);
            Ok(())
        }

        fn ensure_payload_alignment(&mut self, directive_name: &str) -> Result<(), DexError> {
            if self.current_offset_cu % 2 != 0 {
                let opcode = opcode_by_name("nop").ok_or_else(|| {
                    DexError::new(&format!("opcode nop not present for {}", directive_name))
                })?;
                let inst = LoweredInstruction::new(opcode, LoweredKind::Format10x);
                self.push_instruction(inst, false);
            }
            Ok(())
        }

        fn finish(
            mut self,
            api_level: i32,
            art_version: i32,
        ) -> Result<MethodAssemblyResult, DexError> {
            loop {
                let payload_bases = self.compute_payload_bases()?;
                let mut code = Vec::with_capacity(self.current_offset_cu);
                let mut branch_overflow: Option<usize> = None;
                for (idx, inst) in self.instructions.iter().enumerate() {
                    match inst.encode(
                        api_level,
                        art_version,
                        &self.labels,
                        &payload_bases,
                        &mut code,
                    ) {
                        Ok(()) => {}
                        Err(InstructionEncodeError::BranchOutOfRange) => {
                            branch_overflow = Some(idx);
                            break;
                        }
                        Err(InstructionEncodeError::Dex(err)) => return Err(err),
                    }
                }

                if let Some(idx) = branch_overflow {
                    self.promote_branch(idx)?;
                    continue;
                }

                let label_offsets = self
                    .labels
                    .into_iter()
                    .map(|(name, offset)| (name, offset as u32))
                    .collect();

                return Ok(MethodAssemblyResult {
                    registers_size: self.layout.registers_size,
                    ins_size: self.layout.ins_size,
                    outs_size: 0,
                    tries_size: 0,
                    encoded_code_units: code,
                    label_offsets,
                    line_events: self.line_events,
                    debug_events: self.debug_events,
                });
            }
        }

        fn compute_payload_bases(&self) -> Result<HashMap<usize, usize>, DexError> {
            let mut bases = HashMap::new();
            for inst in &self.instructions {
                if let LoweredKind::Format31t { label, .. } = &inst.kind {
                    let payload_offset = self.labels.get(label).ok_or_else(|| {
                        DexError::new(&format!(
                            "label :{} referenced by {} not defined",
                            label, inst.opcode.name
                        ))
                    })?;
                    bases.entry(*payload_offset).or_insert(inst.offset_cu);
                }
            }
            Ok(bases)
        }

        fn promote_branch(&mut self, index: usize) -> Result<(), DexError> {
            let inst = self.instructions.get_mut(index).ok_or_else(|| {
                DexError::new("branch overflow reported for out-of-range instruction index")
            })?;
            let (old_size, delta) = inst.widen_branch()?;
            if delta == 0 {
                return Err(DexError::new(
                    "branch offset does not fit even in widest encoding",
                ));
            }
            self.adjust_offsets(index, old_size, delta);
            Ok(())
        }

        fn adjust_offsets(&mut self, index: usize, old_size: usize, new_size: usize) {
            if new_size == old_size {
                return;
            }
            let delta = new_size - old_size;
            let changed_offset = self.instructions[index].offset_cu;
            for inst in self.instructions.iter_mut().skip(index + 1) {
                inst.offset_cu += delta;
            }
            for value in self.labels.values_mut() {
                if *value > changed_offset {
                    *value += delta;
                }
            }
            self.current_offset_cu += delta;
        }
    }

    struct LoweredInstruction {
        opcode: &'static Opcode,
        kind: LoweredKind,
        offset_cu: usize,
    }

    impl LoweredInstruction {
        fn new(opcode: &'static Opcode, kind: LoweredKind) -> Self {
            LoweredInstruction {
                opcode,
                kind,
                offset_cu: 0,
            }
        }

        fn new_payload(kind: LoweredKind) -> Self {
            let opcode = opcode_by_name("nop").expect("nop opcode must exist");
            LoweredInstruction {
                opcode,
                kind,
                offset_cu: 0,
            }
        }

        fn set_opcode(&mut self, opcode: &'static Opcode) {
            self.opcode = opcode;
        }

        fn size_cu(&self) -> usize {
            match &self.kind {
                LoweredKind::Format10x => 1,
                LoweredKind::Format11n { .. } => 1,
                LoweredKind::Format11x { .. } => 1,
                LoweredKind::Format12x { .. } => 1,
                LoweredKind::Format20bc { .. } => 2,
                LoweredKind::Format21s { .. } => 2,
                LoweredKind::Format21ih { .. } => 2,
                LoweredKind::Format21lh { .. } => 2,
                LoweredKind::Format21t { .. } => 2,
                LoweredKind::Format22c { .. } => 2,
                LoweredKind::Format22cs { .. } => 2,
                LoweredKind::Format22t { .. } => 2,
                LoweredKind::Format22b { .. } => 2,
                LoweredKind::Format22s { .. } => 2,
                LoweredKind::Format23x { .. } => 2,
                LoweredKind::Format35c { .. } => 3,
                LoweredKind::Format35mi { .. } => 3,
                LoweredKind::Format35ms { .. } => 3,
                LoweredKind::Format3rc { .. } => 3,
                LoweredKind::Format3rmi { .. } => 3,
                LoweredKind::Format3rms { .. } => 3,
                LoweredKind::Format45cc { .. } => 4,
                LoweredKind::Format4rcc { .. } => 4,
                LoweredKind::Format31i { .. } => 3,
                LoweredKind::Format31t { .. } => 3,
                LoweredKind::Format51l { .. } => 5,
                LoweredKind::Format21c { .. } => 2,
                LoweredKind::Format31c { .. } => 3,
                LoweredKind::Format22x { .. } => 2,
                LoweredKind::Format32x { .. } => 3,
                LoweredKind::ArrayPayload { units } => units.len(),
                LoweredKind::PackedSwitchPayload { directive } => {
                    // ident + size + first_key (2 code units) + targets (2 per entry)
                    4 + directive.targets.len() * 2
                }
                LoweredKind::SparseSwitchPayload { directive } => {
                    // ident + size + keys (2 CU per entry) + targets (2 CU per entry)
                    2 + directive.entries.len() * 4
                }
                LoweredKind::Branch { encoding, .. } => match encoding {
                    BranchEncoding::Format10t => 1,
                    BranchEncoding::Format20t => 2,
                    BranchEncoding::Format30t => 3,
                },
            }
        }

        fn widen_branch(&mut self) -> Result<(usize, usize), DexError> {
            let old_size = self.size_cu();
            if let LoweredKind::Branch {
                encoding, variant, ..
            } = &mut self.kind
            {
                match encoding {
                    BranchEncoding::Format10t => {
                        *encoding = BranchEncoding::Format20t;
                        *variant = BranchVariant::Goto16;
                        let opcode = variant.opcode()?;
                        self.set_opcode(opcode);
                    }
                    BranchEncoding::Format20t => {
                        *encoding = BranchEncoding::Format30t;
                        *variant = BranchVariant::Goto32;
                        let opcode = variant.opcode()?;
                        self.set_opcode(opcode);
                    }
                    BranchEncoding::Format30t => {
                        return Ok((old_size, old_size));
                    }
                }
                let new_size = self.size_cu();
                return Ok((old_size, new_size));
            }
            Err(DexError::new(
                "attempted to widen non-branch instruction during assembly",
            ))
        }

        fn encode(
            &self,
            api_level: i32,
            art_version: i32,
            labels: &HashMap<String, usize>,
            payload_bases: &HashMap<usize, usize>,
            out: &mut Vec<u16>,
        ) -> Result<(), InstructionEncodeError> {
            match &self.kind {
                LoweredKind::ArrayPayload { units } => {
                    out.extend_from_slice(units);
                    return Ok(());
                }
                LoweredKind::PackedSwitchPayload { directive } => {
                    encode_packed_switch_payload(
                        directive,
                        labels,
                        payload_bases,
                        self.offset_cu,
                        out,
                    )?;
                    return Ok(());
                }
                LoweredKind::SparseSwitchPayload { directive } => {
                    encode_sparse_switch_payload(
                        directive,
                        labels,
                        payload_bases,
                        self.offset_cu,
                        out,
                    )?;
                    return Ok(());
                }
                _ => {}
            }
            let opcode_value = self
                .opcode
                .get_opcode_value(api_level, art_version)
                .ok_or_else(|| {
                    DexError::new(&format!(
                        "opcode {} not available for api {} art {}",
                        self.opcode.name, api_level, art_version
                    ))
                })
                .map_err(InstructionEncodeError::Dex)?;

            match &self.kind {
                LoweredKind::Format10x => {
                    out.push(opcode_value as u16);
                }
                LoweredKind::Format11n { reg, literal } => {
                    if *reg > 0x0f {
                        return Err(DexError::new(
                            "const/4 destination register must be below v16",
                        )
                        .into());
                    }
                    if !(-8..=7).contains(literal) {
                        return Err(
                            DexError::new("const/4 literal must be in signed 4-bit range").into(),
                        );
                    }
                    let reg_bits = (reg & 0x0f) as u16;
                    let lit_bits = ((*literal as i16) & 0x0f) as u16;
                    let encoded =
                        ((lit_bits & 0x0f) << 12) | ((reg_bits & 0x0f) << 8) | opcode_value as u16;
                    out.push(encoded);
                }
                LoweredKind::Format11x { reg } => {
                    let encoded = ((*reg & 0x00ff) << 8) | opcode_value as u16;
                    out.push(encoded);
                }
                LoweredKind::Format12x { dest, src } => {
                    let encoded = (((*src & 0x0f) as u16) << 12)
                        | (((*dest & 0x0f) as u16) << 8)
                        | opcode_value as u16;
                    out.push(encoded);
                }
                LoweredKind::Format20bc { kind, index } => {
                    let encoded = (((*kind as u16) & 0x00ff) << 8) | opcode_value as u16;
                    out.push(encoded);
                    out.push(*index);
                }
                LoweredKind::Format21s { reg, literal } => {
                    let encoded = (((*reg & 0x00ff) as u16) << 8) | opcode_value as u16;
                    out.push(encoded);
                    out.push(*literal as u16);
                }
                LoweredKind::Format21ih { reg, literal } => {
                    let encoded = (((*reg & 0x00ff) as u16) << 8) | opcode_value as u16;
                    out.push(encoded);
                    out.push(*literal as u16);
                }
                LoweredKind::Format21lh { reg, literal } => {
                    let encoded = (((*reg & 0x00ff) as u16) << 8) | opcode_value as u16;
                    out.push(encoded);
                    out.push(*literal as u16);
                }
                LoweredKind::Format31i { reg, literal } => {
                    let encoded = (((*reg & 0x00ff) as u16) << 8) | opcode_value as u16;
                    out.push(encoded);
                    out.push((*literal & 0xffff) as u16);
                    out.push(((*literal >> 16) & 0xffff) as u16);
                }
                LoweredKind::Format31t { reg, label } => {
                    let delta = branch_delta(label, labels, self.offset_cu)?;
                    let first = (((*reg & 0x00ff) as u16) << 8) | opcode_value as u16;
                    out.push(first);
                    out.push((delta as u32 & 0xffff) as u16);
                    out.push(((delta as u32) >> 16) as u16);
                }
                LoweredKind::Format51l { reg, literal } => {
                    let encoded = (((*reg & 0x00ff) as u16) << 8) | opcode_value as u16;
                    out.push(encoded);
                    out.push((*literal & 0xffff) as u16);
                    out.push(((*literal >> 16) & 0xffff) as u16);
                    out.push(((*literal >> 32) & 0xffff) as u16);
                    out.push(((*literal >> 48) & 0xffff) as u16);
                }
                LoweredKind::Format21c { reg, index } => {
                    if *index > u16::MAX as u32 {
                        return Err(DexError::new("reference index exceeds 16-bit range").into());
                    }
                    let encoded = (((*reg & 0x00ff) as u16) << 8) | opcode_value as u16;
                    out.push(encoded);
                    out.push(*index as u16);
                }
                LoweredKind::Format31c { reg, index } => {
                    let encoded = (((*reg & 0x00ff) as u16) << 8) | opcode_value as u16;
                    out.push(encoded);
                    out.push((*index & 0xffff) as u16);
                    out.push(((*index >> 16) & 0xffff) as u16);
                }
                LoweredKind::Format21t { reg, label } => {
                    let delta = branch_delta(label, labels, self.offset_cu)?;
                    if !(i16::MIN as i32..=i16::MAX as i32).contains(&delta) {
                        return Err(
                            DexError::new("if-*z target offset exceeds 16-bit range").into()
                        );
                    }
                    let encoded = (((*reg & 0x00ff) as u16) << 8) | opcode_value as u16;
                    out.push(encoded);
                    out.push(delta as i16 as u16);
                }
                LoweredKind::Format22c {
                    reg_a,
                    reg_b,
                    index,
                } => {
                    if *index > u16::MAX as u32 {
                        return Err(
                            DexError::new("field reference index exceeds 16-bit range").into()
                        );
                    }
                    let encoded = (((*reg_b & 0x000f) as u16) << 12)
                        | (((*reg_a & 0x000f) as u16) << 8)
                        | opcode_value as u16;
                    out.push(encoded);
                    out.push(*index as u16);
                }
                LoweredKind::Format22cs {
                    reg_a,
                    reg_b,
                    index,
                } => {
                    let encoded = (((*reg_b & 0x000f) as u16) << 12)
                        | (((*reg_a & 0x000f) as u16) << 8)
                        | opcode_value as u16;
                    out.push(encoded);
                    out.push(*index);
                }
                LoweredKind::Format22t {
                    reg_a,
                    reg_b,
                    label,
                } => {
                    let delta = branch_delta(label, labels, self.offset_cu)?;
                    if !(i16::MIN as i32..=i16::MAX as i32).contains(&delta) {
                        return Err(DexError::new("if-* target offset exceeds 16-bit range").into());
                    }
                    let encoded = (((*reg_b & 0x000f) as u16) << 12)
                        | (((*reg_a & 0x000f) as u16) << 8)
                        | opcode_value as u16;
                    out.push(encoded);
                    out.push(delta as i16 as u16);
                }
                LoweredKind::Format22b { dest, src, literal } => {
                    if *dest > u8::MAX as u16 || *src > u8::MAX as u16 {
                        return Err(DexError::new("22b register index exceeds 8-bit range").into());
                    }
                    let first = (((*dest & 0x00ff) as u16) << 8) | opcode_value as u16;
                    let second =
                        (((*literal as i16) & 0x00ff) as u16) << 8 | ((*src & 0x00ff) as u16);
                    out.push(first);
                    out.push(second);
                }
                LoweredKind::Format22s { dest, src, literal } => {
                    let first = (((*src & 0x000f) as u16) << 12)
                        | (((*dest & 0x000f) as u16) << 8)
                        | opcode_value as u16;
                    out.push(first);
                    out.push(*literal as u16);
                }
                LoweredKind::Format22x { dest, src } => {
                    let encoded = (((*dest & 0x00ff) as u16) << 8) | opcode_value as u16;
                    out.push(encoded);
                    out.push(*src);
                }
                LoweredKind::Format32x { dest, src } => {
                    out.push(opcode_value as u16);
                    out.push(*dest);
                    out.push(*src);
                }
                LoweredKind::Format23x { dest, src1, src2 } => {
                    let first = (((*dest & 0x00ff) as u16) << 8) | opcode_value as u16;
                    let second = (((*src2 & 0x00ff) as u16) << 8) | ((*src1 & 0x00ff) as u16);
                    out.push(first);
                    out.push(second);
                }
                LoweredKind::Format35c { index, regs, count } => {
                    if *index > u16::MAX as u32 {
                        return Err(
                            DexError::new("method reference index exceeds 16-bit range").into()
                        );
                    }
                    let (c, d, e, f, g) = unpack_invoke_regs(*count, regs);
                    let word0 = ((u16::from(*count) & 0x000f) << 12)
                        | ((g & 0x000f) << 8)
                        | opcode_value as u16;
                    let word1 = *index as u16;
                    let word2 = ((f & 0x000f) << 12)
                        | ((e & 0x000f) << 8)
                        | ((d & 0x000f) << 4)
                        | (c & 0x000f);
                    out.push(word0);
                    out.push(word1);
                    out.push(word2);
                }
                LoweredKind::Format35mi { index, regs, count } => {
                    let (c, d, e, f, g) = unpack_invoke_regs(*count, regs);
                    let word0 = ((u16::from(*count) & 0x000f) << 12)
                        | ((g & 0x000f) << 8)
                        | opcode_value as u16;
                    let word1 = *index;
                    let word2 = ((f & 0x000f) << 12)
                        | ((e & 0x000f) << 8)
                        | ((d & 0x000f) << 4)
                        | (c & 0x000f);
                    out.push(word0);
                    out.push(word1);
                    out.push(word2);
                }
                LoweredKind::Format35ms { index, regs, count } => {
                    let (c, d, e, f, g) = unpack_invoke_regs(*count, regs);
                    let word0 = ((u16::from(*count) & 0x000f) << 12)
                        | ((g & 0x000f) << 8)
                        | opcode_value as u16;
                    let word1 = *index as u16;
                    let word2 = ((f & 0x000f) << 12)
                        | ((e & 0x000f) << 8)
                        | ((d & 0x000f) << 4)
                        | (c & 0x000f);
                    out.push(word0);
                    out.push(word1);
                    out.push(word2);
                }
                LoweredKind::Format3rc {
                    first_reg,
                    count,
                    index,
                } => {
                    if *index > u16::MAX as u32 {
                        return Err(
                            DexError::new("method reference index exceeds 16-bit range").into()
                        );
                    }
                    if *count > u8::MAX as u16 {
                        return Err(DexError::new("invoke/range count exceeds 8-bit range").into());
                    }
                    out.push(((*count & 0x00ff) << 8) | opcode_value as u16);
                    out.push(*index as u16);
                    out.push(*first_reg);
                }
                LoweredKind::Format3rmi {
                    first_reg,
                    count,
                    index,
                } => {
                    out.push(((*count & 0x00ff) << 8) | opcode_value as u16);
                    out.push(*index);
                    out.push(*first_reg);
                }
                LoweredKind::Format3rms {
                    first_reg,
                    count,
                    index,
                } => {
                    out.push(((*count & 0x00ff) << 8) | opcode_value as u16);
                    out.push(*index);
                    out.push(*first_reg);
                }
                LoweredKind::Format45cc {
                    index,
                    proto_index,
                    regs,
                    count,
                } => {
                    if *index > u16::MAX as u32 {
                        return Err(DexError::new(
                            "invoke-polymorphic reference index exceeds 16-bit range",
                        )
                        .into());
                    }
                    if *proto_index > u16::MAX as u32 {
                        return Err(DexError::new(
                            "invoke-polymorphic proto index exceeds 16-bit range",
                        )
                        .into());
                    }
                    let (c, d, e, f, g) = unpack_invoke_regs(*count, regs);
                    let word0 = ((u16::from(*count) & 0x000f) << 12)
                        | ((g & 0x000f) << 8)
                        | opcode_value as u16;
                    let word1 = *index as u16;
                    let word2 = ((f & 0x000f) << 12)
                        | ((e & 0x000f) << 8)
                        | ((d & 0x000f) << 4)
                        | (c & 0x000f);
                    out.push(word0);
                    out.push(word1);
                    out.push(word2);
                    out.push(*proto_index as u16);
                }
                LoweredKind::Format4rcc {
                    first_reg,
                    count,
                    index,
                    proto_index,
                } => {
                    if *index > u16::MAX as u32 {
                        return Err(DexError::new(
                            "invoke-polymorphic/range reference index exceeds 16-bit range",
                        )
                        .into());
                    }
                    if *proto_index > u16::MAX as u32 {
                        return Err(DexError::new(
                            "invoke-polymorphic/range proto index exceeds 16-bit range",
                        )
                        .into());
                    }
                    if *count > u8::MAX as u16 {
                        return Err(DexError::new(
                            "invoke-polymorphic/range count exceeds 8-bit range",
                        )
                        .into());
                    }
                    out.push(((*count & 0x00ff) << 8) | opcode_value as u16);
                    out.push(*index as u16);
                    out.push(*first_reg);
                    out.push(*proto_index as u16);
                }
                LoweredKind::Branch {
                    label, encoding, ..
                } => {
                    let delta = branch_delta(label, labels, self.offset_cu)?;
                    match encoding {
                        BranchEncoding::Format10t => {
                            if !(i8::MIN as i32..=i8::MAX as i32).contains(&delta) {
                                return Err(InstructionEncodeError::BranchOutOfRange);
                            }
                            let imm = (delta as i8 as u8) as u16;
                            let encoded = (imm << 8) | opcode_value as u16;
                            out.push(encoded);
                        }
                        BranchEncoding::Format20t => {
                            if !(i16::MIN as i32..=i16::MAX as i32).contains(&delta) {
                                return Err(InstructionEncodeError::BranchOutOfRange);
                            }
                            out.push(opcode_value as u16);
                            out.push(delta as i16 as u16);
                        }
                        BranchEncoding::Format30t => {
                            out.push(opcode_value as u16);
                            let lo = delta as i32 as u32 & 0xffff;
                            let hi = ((delta as i32 as u32) >> 16) & 0xffff;
                            out.push(lo as u16);
                            out.push(hi as u16);
                        }
                    }
                }
                LoweredKind::ArrayPayload { .. }
                | LoweredKind::PackedSwitchPayload { .. }
                | LoweredKind::SparseSwitchPayload { .. } => {
                    unreachable!("payload variants handled before opcode encoding")
                }
            }

            Ok(())
        }
    }

    fn branch_delta(
        label: &str,
        labels: &HashMap<String, usize>,
        current_offset_cu: usize,
    ) -> Result<i32, DexError> {
        let target = labels.get(label).ok_or_else(|| {
            DexError::new(&format!(
                "branch target label :{} not defined in method",
                label
            ))
        })?;
        Ok(*target as i32 - current_offset_cu as i32)
    }

    fn encode_array_data_units(array: &ArrayDataDirective) -> Result<Vec<u16>, DexError> {
        if array.element_width <= 0 {
            return Err(DexError::new("array-data element width must be positive"));
        }
        let width = array.element_width as usize;
        if !matches!(width, 1 | 2 | 4 | 8) {
            return Err(DexError::new(
                "array-data element width must be one of 1, 2, 4, or 8 bytes",
            ));
        }
        let mut units = Vec::new();
        units.push(ARRAY_DATA_SIGNATURE);
        units.push(width as u16);
        let size = array.elements.len() as u32;
        units.push((size & 0xffff) as u16);
        units.push(((size >> 16) & 0xffff) as u16);

        let mut data_bytes: Vec<u8> = Vec::new();
        for element in &array.elements {
            match (width, element) {
                (1, ArrayDataElement::Byte(v)) => data_bytes.push(*v as u8),
                (2, ArrayDataElement::Short(v)) => data_bytes.extend_from_slice(&v.to_le_bytes()),
                (4, ArrayDataElement::Int(v)) => data_bytes.extend_from_slice(&v.to_le_bytes()),
                (4, ArrayDataElement::Float(v)) => {
                    data_bytes.extend_from_slice(&v.to_bits().to_le_bytes())
                }
                (8, ArrayDataElement::Long(v)) => data_bytes.extend_from_slice(&v.to_le_bytes()),
                (8, ArrayDataElement::Double(v)) => {
                    data_bytes.extend_from_slice(&v.to_bits().to_le_bytes())
                }
                _ => {
                    return Err(DexError::new(
                        "array-data element width does not match element type",
                    ));
                }
            }
        }

        if data_bytes.len() % 2 != 0 {
            data_bytes.push(0);
        }
        for chunk in data_bytes.chunks(2) {
            units.push(u16::from_le_bytes([chunk[0], chunk[1]]));
        }
        Ok(units)
    }

    fn encode_packed_switch_payload(
        directive: &PackedSwitchDirective,
        labels: &HashMap<String, usize>,
        payload_bases: &HashMap<usize, usize>,
        payload_offset: usize,
        out: &mut Vec<u16>,
    ) -> Result<(), DexError> {
        let base = payload_bases.get(&payload_offset).ok_or_else(|| {
            DexError::new("packed-switch payload is not referenced by any switch instruction")
        })?;
        out.push(PACKED_SWITCH_SIGNATURE);
        if directive.targets.len() > u16::MAX as usize {
            return Err(DexError::new("packed-switch target count exceeds limit"));
        }
        out.push(directive.targets.len() as u16);
        let first_key = directive.first_key as u32;
        out.push((first_key & 0xffff) as u16);
        out.push(((first_key >> 16) & 0xffff) as u16);
        for target in &directive.targets {
            let target_offset = labels.get(&target.0).ok_or_else(|| {
                DexError::new(&format!(
                    "packed-switch references undefined label :{}",
                    target.0
                ))
            })?;
            let delta = checked_delta(*target_offset, *base)?;
            out.push((delta as u32 & 0xffff) as u16);
            out.push(((delta as u32) >> 16) as u16);
        }
        Ok(())
    }

    fn encode_sparse_switch_payload(
        directive: &SparseSwitchDirective,
        labels: &HashMap<String, usize>,
        payload_bases: &HashMap<usize, usize>,
        payload_offset: usize,
        out: &mut Vec<u16>,
    ) -> Result<(), DexError> {
        let base = payload_bases.get(&payload_offset).ok_or_else(|| {
            DexError::new("sparse-switch payload is not referenced by any switch instruction")
        })?;
        if directive.entries.len() > u16::MAX as usize {
            return Err(DexError::new("sparse-switch entry count exceeds limit"));
        }
        out.push(SPARSE_SWITCH_SIGNATURE);
        out.push(directive.entries.len() as u16);
        for entry in &directive.entries {
            let key = entry.key as u32;
            out.push((key & 0xffff) as u16);
            out.push(((key >> 16) & 0xffff) as u16);
        }
        for entry in &directive.entries {
            let target_offset = labels.get(&entry.target.0).ok_or_else(|| {
                DexError::new(&format!(
                    "sparse-switch references undefined label :{}",
                    entry.target.0
                ))
            })?;
            let delta = checked_delta(*target_offset, *base)?;
            out.push((delta as u32 & 0xffff) as u16);
            out.push(((delta as u32) >> 16) as u16);
        }
        Ok(())
    }

    fn checked_delta(target: usize, base: usize) -> Result<i32, DexError> {
        let delta = target as i64 - base as i64;
        if delta < i32::MIN as i64 || delta > i32::MAX as i64 {
            return Err(DexError::new(
                "switch payload target offset exceeds 32-bit range",
            ));
        }
        Ok(delta as i32)
    }

    fn unpack_invoke_regs(count: u8, regs: &[u16; 5]) -> (u16, u16, u16, u16, u16) {
        let take = |idx: usize| -> u16 { if (count as usize) > idx { regs[idx] } else { 0 } };
        (take(0), take(1), take(2), take(3), take(4))
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum QuickInvokeKind {
        Virtual,
        Super,
    }

    fn detect_quick_field(field: &FieldRef) -> Result<Option<u16>, DexError> {
        if field.class != "Lquick;" {
            return Ok(None);
        }
        let Some(rest) = field.name.strip_prefix("field@") else {
            return Err(DexError::new(
                "quick field placeholder must be written as Lquick;->field@N:Type",
            ));
        };
        let idx = rest
            .parse::<u16>()
            .map_err(|_| DexError::new("quick field index out of range"))?;
        Ok(Some(idx))
    }

    fn detect_quick_method(
        opcode_name: &str,
        method: &MethodRef,
    ) -> Result<Option<(QuickInvokeKind, u16)>, DexError> {
        if method.class != "Lquick;" {
            return Ok(None);
        }

        let (kind, suffix) = if let Some(rest) = method.name.strip_prefix("virtual@") {
            if !opcode_name.starts_with("invoke-virtual") {
                return Err(DexError::new(
                    "quick virtual targets must be paired with invoke-virtual opcodes",
                ));
            }
            (QuickInvokeKind::Virtual, rest)
        } else if let Some(rest) = method.name.strip_prefix("super@") {
            if !opcode_name.starts_with("invoke-super") {
                return Err(DexError::new(
                    "quick super targets must be paired with invoke-super opcodes",
                ));
            }
            (QuickInvokeKind::Super, rest)
        } else {
            return Err(DexError::new(
                "unsupported quick method placeholder (expected virtual@N or super@N)",
            ));
        };

        let idx = suffix
            .parse::<u16>()
            .map_err(|_| DexError::new("quick method index out of range"))?;
        Ok(Some((kind, idx)))
    }

    #[derive(Debug)]
    enum LoweredKind {
        Format10x,
        Format11n {
            reg: u16,
            literal: i8,
        },
        Format11x {
            reg: u16,
        },
        Format12x {
            dest: u16,
            src: u16,
        },
        Format20bc {
            kind: u8,
            index: u16,
        },
        Format21s {
            reg: u16,
            literal: i16,
        },
        Format21ih {
            reg: u16,
            literal: i16,
        },
        Format21lh {
            reg: u16,
            literal: i16,
        },
        Format21t {
            reg: u16,
            label: String,
        },
        Format31i {
            reg: u16,
            literal: i32,
        },
        Format31t {
            reg: u16,
            label: String,
        },
        Format51l {
            reg: u16,
            literal: i64,
        },
        Format21c {
            reg: u16,
            index: u32,
        },
        Format31c {
            reg: u16,
            index: u32,
        },
        Format22c {
            reg_a: u16,
            reg_b: u16,
            index: u32,
        },
        Format22cs {
            reg_a: u16,
            reg_b: u16,
            index: u16,
        },
        Format22t {
            reg_a: u16,
            reg_b: u16,
            label: String,
        },
        Format22b {
            dest: u16,
            src: u16,
            literal: i8,
        },
        Format22s {
            dest: u16,
            src: u16,
            literal: i16,
        },
        Format22x {
            dest: u16,
            src: u16,
        },
        Format32x {
            dest: u16,
            src: u16,
        },
        Format23x {
            dest: u16,
            src1: u16,
            src2: u16,
        },
        ArrayPayload {
            units: Vec<u16>,
        },
        PackedSwitchPayload {
            directive: PackedSwitchDirective,
        },
        SparseSwitchPayload {
            directive: SparseSwitchDirective,
        },
        Format35c {
            index: u32,
            regs: [u16; 5],
            count: u8,
        },
        Format35mi {
            index: u16,
            regs: [u16; 5],
            count: u8,
        },
        Format35ms {
            index: u16,
            regs: [u16; 5],
            count: u8,
        },
        Format3rc {
            first_reg: u16,
            count: u16,
            index: u32,
        },
        Format3rmi {
            first_reg: u16,
            count: u16,
            index: u16,
        },
        Format3rms {
            first_reg: u16,
            count: u16,
            index: u16,
        },
        Format45cc {
            index: u32,
            proto_index: u32,
            regs: [u16; 5],
            count: u8,
        },
        Format4rcc {
            first_reg: u16,
            count: u16,
            index: u32,
            proto_index: u32,
        },
        Branch {
            label: String,
            encoding: BranchEncoding,
            variant: BranchVariant,
        },
    }

    #[derive(Debug)]
    enum BranchEncoding {
        Format10t,
        Format20t,
        Format30t,
    }

    enum InstructionEncodeError {
        Dex(DexError),
        BranchOutOfRange,
    }

    impl From<DexError> for InstructionEncodeError {
        fn from(value: DexError) -> Self {
            InstructionEncodeError::Dex(value)
        }
    }

    #[derive(Clone, Copy, Debug)]
    enum BranchVariant {
        Goto,
        Goto16,
        Goto32,
    }

    impl BranchVariant {
        fn opcode(self) -> Result<&'static Opcode, DexError> {
            let name = match self {
                BranchVariant::Goto => "goto",
                BranchVariant::Goto16 => "goto/16",
                BranchVariant::Goto32 => "goto/32",
            };
            opcode_by_name(name)
                .ok_or_else(|| DexError::new(&format!("opcode {} not present in table", name)))
        }
    }

    fn compute_ins_size(method: &SmaliMethod) -> u16 {
        let mut count = 0u16;
        if !method
            .modifiers
            .iter()
            .any(|m| matches!(m, Modifier::Static))
        {
            count = count.saturating_add(1);
        }
        for arg in &method.signature.args {
            count = count.saturating_add(type_width(arg));
        }
        count
    }

    fn type_width(sig: &TypeSignature) -> u16 {
        match sig {
            TypeSignature::Long | TypeSignature::Double => 2,
            _ => 1,
        }
    }

    fn parse_method_descriptor(descriptor: &str) -> Result<MethodSignature, DexError> {
        match parse_methodsignature(descriptor) {
            Ok((rest, sig)) if rest.is_empty() => Ok(sig),
            _ => Err(DexError::new(&format!(
                "invalid method descriptor {}",
                descriptor
            ))),
        }
    }

    impl<'a, R: AssemblerIndexResolver> MethodAssembler<'a, R> {
        fn lower_dex_op(
            &self,
            op: &DexOp,
            _class_desc: &str,
            _method: &SmaliMethod,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            match op {
                DexOp::Const4 { dest, value } => {
                    let opcode = opcode_by_name("const/4")
                        .ok_or_else(|| DexError::new("opcode const/4 not present in table"))?;
                    let reg = ensure_reg8(resolve_register(layout, dest)?, "const/4 destination")?;
                    Ok(LoweredInstruction::new(
                        opcode,
                        LoweredKind::Format11n {
                            reg,
                            literal: *value,
                        },
                    ))
                }
                DexOp::Nop => {
                    let opcode = opcode_by_name("nop")
                        .ok_or_else(|| DexError::new("opcode nop not present in table"))?;
                    Ok(LoweredInstruction::new(opcode, LoweredKind::Format10x))
                }
                DexOp::Const16 { dest, value } => {
                    let opcode = opcode_by_name("const/16")
                        .ok_or_else(|| DexError::new("opcode const/16 not present in table"))?;
                    let reg = ensure_reg8(resolve_register(layout, dest)?, "const/16 dest")?;
                    Ok(LoweredInstruction::new(
                        opcode,
                        LoweredKind::Format21s {
                            reg,
                            literal: *value,
                        },
                    ))
                }
                DexOp::Const { dest, value } => {
                    let opcode = opcode_by_name("const")
                        .ok_or_else(|| DexError::new("opcode const not present in table"))?;
                    let reg = ensure_reg8(resolve_register(layout, dest)?, "const dest")?;
                    Ok(LoweredInstruction::new(
                        opcode,
                        LoweredKind::Format31i {
                            reg,
                            literal: *value,
                        },
                    ))
                }
                DexOp::ConstHigh16 { dest, value } => {
                    let opcode = opcode_by_name("const/high16")
                        .ok_or_else(|| DexError::new("opcode const/high16 not present in table"))?;
                    let reg = ensure_reg8(resolve_register(layout, dest)?, "const/high16 dest")?;
                    Ok(LoweredInstruction::new(
                        opcode,
                        LoweredKind::Format21ih {
                            reg,
                            literal: *value,
                        },
                    ))
                }
                DexOp::ConstWide16 { dest, value } => {
                    let opcode = opcode_by_name("const-wide/16").ok_or_else(|| {
                        DexError::new("opcode const-wide/16 not present in table")
                    })?;
                    let reg = ensure_reg8(resolve_register(layout, dest)?, "const-wide/16 dest")?;
                    Ok(LoweredInstruction::new(
                        opcode,
                        LoweredKind::Format21s {
                            reg,
                            literal: *value,
                        },
                    ))
                }
                DexOp::ConstWide32 { dest, value } => {
                    let opcode = opcode_by_name("const-wide/32").ok_or_else(|| {
                        DexError::new("opcode const-wide/32 not present in table")
                    })?;
                    let reg = ensure_reg8(resolve_register(layout, dest)?, "const-wide/32 dest")?;
                    Ok(LoweredInstruction::new(
                        opcode,
                        LoweredKind::Format31i {
                            reg,
                            literal: *value,
                        },
                    ))
                }
                DexOp::ConstWide { dest, value } => {
                    let opcode = opcode_by_name("const-wide")
                        .ok_or_else(|| DexError::new("opcode const-wide not present in table"))?;
                    let reg = ensure_reg8(resolve_register(layout, dest)?, "const-wide dest")?;
                    Ok(LoweredInstruction::new(
                        opcode,
                        LoweredKind::Format51l {
                            reg,
                            literal: *value,
                        },
                    ))
                }
                DexOp::ConstWideHigh16 { dest, value } => {
                    let opcode = opcode_by_name("const-wide/high16").ok_or_else(|| {
                        DexError::new("opcode const-wide/high16 not present in table")
                    })?;
                    let reg =
                        ensure_reg8(resolve_register(layout, dest)?, "const-wide/high16 dest")?;
                    Ok(LoweredInstruction::new(
                        opcode,
                        LoweredKind::Format21lh {
                            reg,
                            literal: *value,
                        },
                    ))
                }
                DexOp::ConstClass { dest, class } => {
                    self.lower_type_const("const-class", dest, class, layout)
                }
                DexOp::ConstMethodHandle {
                    dest,
                    method_handle,
                } => self.lower_method_handle_const(
                    "const-method-handle",
                    dest,
                    method_handle,
                    layout,
                ),
                DexOp::ConstMethodType { dest, proto } => {
                    self.lower_method_type_const("const-method-type", dest, proto, layout)
                }
                DexOp::ReturnVoid => {
                    let opcode = opcode_by_name("return-void")
                        .ok_or_else(|| DexError::new("opcode return-void not present in table"))?;
                    Ok(LoweredInstruction::new(opcode, LoweredKind::Format10x))
                }
                DexOp::Return { src } => self.lower_return("return", src, layout),
                DexOp::ReturnObject { src } => self.lower_return("return-object", src, layout),
                DexOp::ReturnWide { src } => self.lower_return("return-wide", src, layout),
                DexOp::Move { dest, src } => {
                    self.lower_move("move", dest, src, layout, MoveEncoding::Format12x)
                }
                DexOp::MoveFrom16 { dest, src } => {
                    self.lower_move("move/from16", dest, src, layout, MoveEncoding::Format22x)
                }
                DexOp::Move16 { dest, src } => {
                    self.lower_move("move/16", dest, src, layout, MoveEncoding::Format32x)
                }
                DexOp::MoveObject { dest, src } => {
                    self.lower_move("move-object", dest, src, layout, MoveEncoding::Format12x)
                }
                DexOp::MoveObjectFrom16 { dest, src } => self.lower_move(
                    "move-object/from16",
                    dest,
                    src,
                    layout,
                    MoveEncoding::Format22x,
                ),
                DexOp::MoveObject16 { dest, src } => {
                    self.lower_move("move-object/16", dest, src, layout, MoveEncoding::Format32x)
                }
                DexOp::MoveWide { dest, src } => {
                    self.lower_move("move-wide", dest, src, layout, MoveEncoding::Format12x)
                }
                DexOp::MoveWideFrom16 { dest, src } => self.lower_move(
                    "move-wide/from16",
                    dest,
                    src,
                    layout,
                    MoveEncoding::Format22x,
                ),
                DexOp::MoveWide16 { dest, src } => {
                    self.lower_move("move-wide/16", dest, src, layout, MoveEncoding::Format32x)
                }
                DexOp::MoveResult { dest } => self.lower_move_result("move-result", dest, layout),
                DexOp::MoveResultWide { dest } => {
                    self.lower_move_result("move-result-wide", dest, layout)
                }
                DexOp::MoveResultObject { dest } => {
                    self.lower_move_result("move-result-object", dest, layout)
                }
                DexOp::ConstString { dest, value } => {
                    let opcode = opcode_by_name("const-string")
                        .ok_or_else(|| DexError::new("opcode const-string not present in table"))?;
                    let reg = ensure_reg8(resolve_register(layout, dest)?, "const-string dest")?;
                    let index = self.resolver.string_index(value)?;
                    if index > u16::MAX as u32 {
                        let jumbo = opcode_by_name("const-string/jumbo").ok_or_else(|| {
                            DexError::new("opcode const-string/jumbo not present in table")
                        })?;
                        return Ok(LoweredInstruction::new(
                            jumbo,
                            LoweredKind::Format31c { reg, index },
                        ));
                    }
                    Ok(LoweredInstruction::new(
                        opcode,
                        LoweredKind::Format21c { reg, index },
                    ))
                }
                DexOp::ConstStringJumbo { dest, value } => {
                    let opcode = opcode_by_name("const-string/jumbo").ok_or_else(|| {
                        DexError::new("opcode const-string/jumbo not present in table")
                    })?;
                    let reg =
                        ensure_reg8(resolve_register(layout, dest)?, "const-string/jumbo dest")?;
                    let index = self.resolver.string_index(value)?;
                    Ok(LoweredInstruction::new(
                        opcode,
                        LoweredKind::Format31c { reg, index },
                    ))
                }
                DexOp::Goto { offset } => {
                    self.lower_branch(BranchVariant::Goto, offset, BranchEncoding::Format10t)
                }
                DexOp::Goto16 { offset } => {
                    self.lower_branch(BranchVariant::Goto16, offset, BranchEncoding::Format20t)
                }
                DexOp::Goto32 { offset } => {
                    self.lower_branch(BranchVariant::Goto32, offset, BranchEncoding::Format30t)
                }
                DexOp::MonitorEnter { src } => {
                    self.lower_single_reg_op("monitor-enter", src, layout)
                }
                DexOp::MonitorExit { src } => self.lower_single_reg_op("monitor-exit", src, layout),
                DexOp::Throw { src } => self.lower_single_reg_op("throw", src, layout),
                DexOp::ThrowVerificationError { kind, reference } => {
                    self.lower_throw_verification_error(*kind, reference)
                }
                DexOp::MoveException { dest } => {
                    self.lower_single_reg_op("move-exception", dest, layout)
                }
                DexOp::AGet { dest, array, index } => {
                    self.lower_array_get("aget", dest, array, index, layout)
                }
                DexOp::AGetWide { dest, array, index } => {
                    self.lower_array_get("aget-wide", dest, array, index, layout)
                }
                DexOp::AGetObject { dest, array, index } => {
                    self.lower_array_get("aget-object", dest, array, index, layout)
                }
                DexOp::AGetBoolean { dest, array, index } => {
                    self.lower_array_get("aget-boolean", dest, array, index, layout)
                }
                DexOp::AGetByte { dest, array, index } => {
                    self.lower_array_get("aget-byte", dest, array, index, layout)
                }
                DexOp::AGetChar { dest, array, index } => {
                    self.lower_array_get("aget-char", dest, array, index, layout)
                }
                DexOp::AGetShort { dest, array, index } => {
                    self.lower_array_get("aget-short", dest, array, index, layout)
                }
                DexOp::APut { src, array, index } => {
                    self.lower_array_put("aput", src, array, index, layout)
                }
                DexOp::APutWide { src, array, index } => {
                    self.lower_array_put("aput-wide", src, array, index, layout)
                }
                DexOp::APutObject { src, array, index } => {
                    self.lower_array_put("aput-object", src, array, index, layout)
                }
                DexOp::APutBoolean { src, array, index } => {
                    self.lower_array_put("aput-boolean", src, array, index, layout)
                }
                DexOp::APutByte { src, array, index } => {
                    self.lower_array_put("aput-byte", src, array, index, layout)
                }
                DexOp::APutChar { src, array, index } => {
                    self.lower_array_put("aput-char", src, array, index, layout)
                }
                DexOp::APutShort { src, array, index } => {
                    self.lower_array_put("aput-short", src, array, index, layout)
                }
                DexOp::ArrayLength { dest, array } => {
                    self.lower_two_reg_format12x("array-length", dest, array, layout)
                }
                DexOp::FillArrayData { reg, offset } => {
                    self.lower_format31t("fill-array-data", reg, offset, layout)
                }
                DexOp::PackedSwitch { reg, offset } => {
                    self.lower_format31t("packed-switch", reg, offset, layout)
                }
                DexOp::SparseSwitch { reg, offset } => {
                    self.lower_format31t("sparse-switch", reg, offset, layout)
                }
                DexOp::CheckCast { dest, class } => {
                    self.lower_type_check("check-cast", dest, class, layout)
                }
                DexOp::InstanceOf { dest, src, class } => {
                    self.lower_two_reg_type_op("instance-of", dest, src, class, layout)
                }
                DexOp::NewInstance { dest, class } => {
                    self.lower_type_const("new-instance", dest, class, layout)
                }
                DexOp::NewArray {
                    dest,
                    size_reg,
                    class,
                } => self.lower_two_reg_type_op("new-array", dest, size_reg, class, layout),
                DexOp::FilledNewArray { registers, class } => {
                    self.lower_filled_new_array("filled-new-array", registers, class, layout)
                }
                DexOp::FilledNewArrayRange { registers, class } => self
                    .lower_filled_new_array_range(
                        "filled-new-array/range",
                        registers,
                        class,
                        layout,
                    ),
                DexOp::IGet {
                    dest,
                    object,
                    field,
                } => self.lower_instance_field_read("iget", dest, object, field, layout),
                DexOp::IGetWide {
                    dest,
                    object,
                    field,
                } => self.lower_instance_field_read("iget-wide", dest, object, field, layout),
                DexOp::IGetObject {
                    dest,
                    object,
                    field,
                } => self.lower_instance_field_read("iget-object", dest, object, field, layout),
                DexOp::IGetBoolean {
                    dest,
                    object,
                    field,
                } => self.lower_instance_field_read("iget-boolean", dest, object, field, layout),
                DexOp::IGetByte {
                    dest,
                    object,
                    field,
                } => self.lower_instance_field_read("iget-byte", dest, object, field, layout),
                DexOp::IGetChar {
                    dest,
                    object,
                    field,
                } => self.lower_instance_field_read("iget-char", dest, object, field, layout),
                DexOp::IGetShort {
                    dest,
                    object,
                    field,
                } => self.lower_instance_field_read("iget-short", dest, object, field, layout),
                DexOp::IPut { src, object, field } => {
                    self.lower_instance_field_write("iput", src, object, field, layout)
                }
                DexOp::IPutWide { src, object, field } => {
                    self.lower_instance_field_write("iput-wide", src, object, field, layout)
                }
                DexOp::IPutObject { src, object, field } => {
                    self.lower_instance_field_write("iput-object", src, object, field, layout)
                }
                DexOp::IPutBoolean { src, object, field } => {
                    self.lower_instance_field_write("iput-boolean", src, object, field, layout)
                }
                DexOp::IPutByte { src, object, field } => {
                    self.lower_instance_field_write("iput-byte", src, object, field, layout)
                }
                DexOp::IPutChar { src, object, field } => {
                    self.lower_instance_field_write("iput-char", src, object, field, layout)
                }
                DexOp::IPutShort { src, object, field } => {
                    self.lower_instance_field_write("iput-short", src, object, field, layout)
                }
                DexOp::SGet { dest, field } => {
                    self.lower_static_field_get("sget", dest, field, layout)
                }
                DexOp::SGetWide { dest, field } => {
                    self.lower_static_field_get("sget-wide", dest, field, layout)
                }
                DexOp::SGetObject { dest, field } => {
                    self.lower_static_field_get("sget-object", dest, field, layout)
                }
                DexOp::SGetBoolean { dest, field } => {
                    self.lower_static_field_get("sget-boolean", dest, field, layout)
                }
                DexOp::SGetByte { dest, field } => {
                    self.lower_static_field_get("sget-byte", dest, field, layout)
                }
                DexOp::SGetChar { dest, field } => {
                    self.lower_static_field_get("sget-char", dest, field, layout)
                }
                DexOp::SGetShort { dest, field } => {
                    self.lower_static_field_get("sget-short", dest, field, layout)
                }
                DexOp::SPut { src, field } => {
                    self.lower_static_field_put("sput", src, field, layout)
                }
                DexOp::SPutWide { src, field } => {
                    self.lower_static_field_put("sput-wide", src, field, layout)
                }
                DexOp::SPutObject { src, field } => {
                    self.lower_static_field_put("sput-object", src, field, layout)
                }
                DexOp::SPutBoolean { src, field } => {
                    self.lower_static_field_put("sput-boolean", src, field, layout)
                }
                DexOp::SPutByte { src, field } => {
                    self.lower_static_field_put("sput-byte", src, field, layout)
                }
                DexOp::SPutChar { src, field } => {
                    self.lower_static_field_put("sput-char", src, field, layout)
                }
                DexOp::SPutShort { src, field } => {
                    self.lower_static_field_put("sput-short", src, field, layout)
                }
                DexOp::AddInt { dest, src1, src2 } => {
                    self.lower_three_reg("add-int", dest, src1, src2, layout)
                }
                DexOp::SubInt { dest, src1, src2 } => {
                    self.lower_three_reg("sub-int", dest, src1, src2, layout)
                }
                DexOp::MulInt { dest, src1, src2 } => {
                    self.lower_three_reg("mul-int", dest, src1, src2, layout)
                }
                DexOp::DivInt { dest, src1, src2 } => {
                    self.lower_three_reg("div-int", dest, src1, src2, layout)
                }
                DexOp::RemInt { dest, src1, src2 } => {
                    self.lower_three_reg("rem-int", dest, src1, src2, layout)
                }
                DexOp::AndInt { dest, src1, src2 } => {
                    self.lower_three_reg("and-int", dest, src1, src2, layout)
                }
                DexOp::OrInt { dest, src1, src2 } => {
                    self.lower_three_reg("or-int", dest, src1, src2, layout)
                }
                DexOp::XorInt { dest, src1, src2 } => {
                    self.lower_three_reg("xor-int", dest, src1, src2, layout)
                }
                DexOp::ShlInt { dest, src1, src2 } => {
                    self.lower_three_reg("shl-int", dest, src1, src2, layout)
                }
                DexOp::ShrInt { dest, src1, src2 } => {
                    self.lower_three_reg("shr-int", dest, src1, src2, layout)
                }
                DexOp::UshrInt { dest, src1, src2 } => {
                    self.lower_three_reg("ushr-int", dest, src1, src2, layout)
                }
                DexOp::AddLong { dest, src1, src2 } => {
                    self.lower_three_reg("add-long", dest, src1, src2, layout)
                }
                DexOp::SubLong { dest, src1, src2 } => {
                    self.lower_three_reg("sub-long", dest, src1, src2, layout)
                }
                DexOp::MulLong { dest, src1, src2 } => {
                    self.lower_three_reg("mul-long", dest, src1, src2, layout)
                }
                DexOp::DivLong { dest, src1, src2 } => {
                    self.lower_three_reg("div-long", dest, src1, src2, layout)
                }
                DexOp::RemLong { dest, src1, src2 } => {
                    self.lower_three_reg("rem-long", dest, src1, src2, layout)
                }
                DexOp::AndLong { dest, src1, src2 } => {
                    self.lower_three_reg("and-long", dest, src1, src2, layout)
                }
                DexOp::OrLong { dest, src1, src2 } => {
                    self.lower_three_reg("or-long", dest, src1, src2, layout)
                }
                DexOp::XorLong { dest, src1, src2 } => {
                    self.lower_three_reg("xor-long", dest, src1, src2, layout)
                }
                DexOp::ShlLong { dest, src1, src2 } => {
                    self.lower_three_reg("shl-long", dest, src1, src2, layout)
                }
                DexOp::ShrLong { dest, src1, src2 } => {
                    self.lower_three_reg("shr-long", dest, src1, src2, layout)
                }
                DexOp::UshrLong { dest, src1, src2 } => {
                    self.lower_three_reg("ushr-long", dest, src1, src2, layout)
                }
                DexOp::AddFloat { dest, src1, src2 } => {
                    self.lower_three_reg("add-float", dest, src1, src2, layout)
                }
                DexOp::SubFloat { dest, src1, src2 } => {
                    self.lower_three_reg("sub-float", dest, src1, src2, layout)
                }
                DexOp::MulFloat { dest, src1, src2 } => {
                    self.lower_three_reg("mul-float", dest, src1, src2, layout)
                }
                DexOp::DivFloat { dest, src1, src2 } => {
                    self.lower_three_reg("div-float", dest, src1, src2, layout)
                }
                DexOp::RemFloat { dest, src1, src2 } => {
                    self.lower_three_reg("rem-float", dest, src1, src2, layout)
                }
                DexOp::AddDouble { dest, src1, src2 } => {
                    self.lower_three_reg("add-double", dest, src1, src2, layout)
                }
                DexOp::SubDouble { dest, src1, src2 } => {
                    self.lower_three_reg("sub-double", dest, src1, src2, layout)
                }
                DexOp::MulDouble { dest, src1, src2 } => {
                    self.lower_three_reg("mul-double", dest, src1, src2, layout)
                }
                DexOp::DivDouble { dest, src1, src2 } => {
                    self.lower_three_reg("div-double", dest, src1, src2, layout)
                }
                DexOp::RemDouble { dest, src1, src2 } => {
                    self.lower_three_reg("rem-double", dest, src1, src2, layout)
                }
                DexOp::AddInt2Addr { reg, src } => {
                    self.lower_two_reg_format12x("add-int/2addr", reg, src, layout)
                }
                DexOp::SubInt2Addr { reg, src } => {
                    self.lower_two_reg_format12x("sub-int/2addr", reg, src, layout)
                }
                DexOp::MulInt2Addr { reg, src } => {
                    self.lower_two_reg_format12x("mul-int/2addr", reg, src, layout)
                }
                DexOp::DivInt2Addr { reg, src } => {
                    self.lower_two_reg_format12x("div-int/2addr", reg, src, layout)
                }
                DexOp::RemInt2Addr { reg, src } => {
                    self.lower_two_reg_format12x("rem-int/2addr", reg, src, layout)
                }
                DexOp::AndInt2Addr { reg, src } => {
                    self.lower_two_reg_format12x("and-int/2addr", reg, src, layout)
                }
                DexOp::OrInt2Addr { reg, src } => {
                    self.lower_two_reg_format12x("or-int/2addr", reg, src, layout)
                }
                DexOp::XorInt2Addr { reg, src } => {
                    self.lower_two_reg_format12x("xor-int/2addr", reg, src, layout)
                }
                DexOp::ShlInt2Addr { reg, src } => {
                    self.lower_two_reg_format12x("shl-int/2addr", reg, src, layout)
                }
                DexOp::ShrInt2Addr { reg, src } => {
                    self.lower_two_reg_format12x("shr-int/2addr", reg, src, layout)
                }
                DexOp::UshrInt2Addr { reg, src } => {
                    self.lower_two_reg_format12x("ushr-int/2addr", reg, src, layout)
                }
                DexOp::AddLong2Addr { reg, src } => {
                    self.lower_two_reg_format12x("add-long/2addr", reg, src, layout)
                }
                DexOp::SubLong2Addr { reg, src } => {
                    self.lower_two_reg_format12x("sub-long/2addr", reg, src, layout)
                }
                DexOp::MulLong2Addr { reg, src } => {
                    self.lower_two_reg_format12x("mul-long/2addr", reg, src, layout)
                }
                DexOp::DivLong2Addr { reg, src } => {
                    self.lower_two_reg_format12x("div-long/2addr", reg, src, layout)
                }
                DexOp::RemLong2Addr { reg, src } => {
                    self.lower_two_reg_format12x("rem-long/2addr", reg, src, layout)
                }
                DexOp::AndLong2Addr { reg, src } => {
                    self.lower_two_reg_format12x("and-long/2addr", reg, src, layout)
                }
                DexOp::OrLong2Addr { reg, src } => {
                    self.lower_two_reg_format12x("or-long/2addr", reg, src, layout)
                }
                DexOp::XorLong2Addr { reg, src } => {
                    self.lower_two_reg_format12x("xor-long/2addr", reg, src, layout)
                }
                DexOp::ShlLong2Addr { reg, src } => {
                    self.lower_two_reg_format12x("shl-long/2addr", reg, src, layout)
                }
                DexOp::ShrLong2Addr { reg, src } => {
                    self.lower_two_reg_format12x("shr-long/2addr", reg, src, layout)
                }
                DexOp::UshrLong2Addr { reg, src } => {
                    self.lower_two_reg_format12x("ushr-long/2addr", reg, src, layout)
                }
                DexOp::AddFloat2Addr { reg, src } => {
                    self.lower_two_reg_format12x("add-float/2addr", reg, src, layout)
                }
                DexOp::SubFloat2Addr { reg, src } => {
                    self.lower_two_reg_format12x("sub-float/2addr", reg, src, layout)
                }
                DexOp::MulFloat2Addr { reg, src } => {
                    self.lower_two_reg_format12x("mul-float/2addr", reg, src, layout)
                }
                DexOp::DivFloat2Addr { reg, src } => {
                    self.lower_two_reg_format12x("div-float/2addr", reg, src, layout)
                }
                DexOp::RemFloat2Addr { reg, src } => {
                    self.lower_two_reg_format12x("rem-float/2addr", reg, src, layout)
                }
                DexOp::AddDouble2Addr { reg, src } => {
                    self.lower_two_reg_format12x("add-double/2addr", reg, src, layout)
                }
                DexOp::SubDouble2Addr { reg, src } => {
                    self.lower_two_reg_format12x("sub-double/2addr", reg, src, layout)
                }
                DexOp::MulDouble2Addr { reg, src } => {
                    self.lower_two_reg_format12x("mul-double/2addr", reg, src, layout)
                }
                DexOp::DivDouble2Addr { reg, src } => {
                    self.lower_two_reg_format12x("div-double/2addr", reg, src, layout)
                }
                DexOp::RemDouble2Addr { reg, src } => {
                    self.lower_two_reg_format12x("rem-double/2addr", reg, src, layout)
                }
                DexOp::NegInt { dest, src } => {
                    self.lower_two_reg_format12x("neg-int", dest, src, layout)
                }
                DexOp::NotInt { dest, src } => {
                    self.lower_two_reg_format12x("not-int", dest, src, layout)
                }
                DexOp::NegLong { dest, src } => {
                    self.lower_two_reg_format12x("neg-long", dest, src, layout)
                }
                DexOp::NotLong { dest, src } => {
                    self.lower_two_reg_format12x("not-long", dest, src, layout)
                }
                DexOp::NegFloat { dest, src } => {
                    self.lower_two_reg_format12x("neg-float", dest, src, layout)
                }
                DexOp::NegDouble { dest, src } => {
                    self.lower_two_reg_format12x("neg-double", dest, src, layout)
                }
                DexOp::IntToLong { dest, src } => {
                    self.lower_two_reg_format12x("int-to-long", dest, src, layout)
                }
                DexOp::IntToFloat { dest, src } => {
                    self.lower_two_reg_format12x("int-to-float", dest, src, layout)
                }
                DexOp::IntToDouble { dest, src } => {
                    self.lower_two_reg_format12x("int-to-double", dest, src, layout)
                }
                DexOp::LongToInt { dest, src } => {
                    self.lower_two_reg_format12x("long-to-int", dest, src, layout)
                }
                DexOp::LongToFloat { dest, src } => {
                    self.lower_two_reg_format12x("long-to-float", dest, src, layout)
                }
                DexOp::LongToDouble { dest, src } => {
                    self.lower_two_reg_format12x("long-to-double", dest, src, layout)
                }
                DexOp::FloatToInt { dest, src } => {
                    self.lower_two_reg_format12x("float-to-int", dest, src, layout)
                }
                DexOp::FloatToLong { dest, src } => {
                    self.lower_two_reg_format12x("float-to-long", dest, src, layout)
                }
                DexOp::FloatToDouble { dest, src } => {
                    self.lower_two_reg_format12x("float-to-double", dest, src, layout)
                }
                DexOp::DoubleToInt { dest, src } => {
                    self.lower_two_reg_format12x("double-to-int", dest, src, layout)
                }
                DexOp::DoubleToLong { dest, src } => {
                    self.lower_two_reg_format12x("double-to-long", dest, src, layout)
                }
                DexOp::DoubleToFloat { dest, src } => {
                    self.lower_two_reg_format12x("double-to-float", dest, src, layout)
                }
                DexOp::IntToByte { dest, src } => {
                    self.lower_two_reg_format12x("int-to-byte", dest, src, layout)
                }
                DexOp::IntToChar { dest, src } => {
                    self.lower_two_reg_format12x("int-to-char", dest, src, layout)
                }
                DexOp::IntToShort { dest, src } => {
                    self.lower_two_reg_format12x("int-to-short", dest, src, layout)
                }
                DexOp::AddIntLit8 { dest, src, literal } => {
                    self.lower_lit8("add-int/lit8", dest, src, *literal, layout)
                }
                DexOp::RSubIntLit8 { dest, src, literal } => {
                    self.lower_lit8("rsub-int/lit8", dest, src, *literal, layout)
                }
                DexOp::MulIntLit8 { dest, src, literal } => {
                    self.lower_lit8("mul-int/lit8", dest, src, *literal, layout)
                }
                DexOp::DivIntLit8 { dest, src, literal } => {
                    self.lower_lit8("div-int/lit8", dest, src, *literal, layout)
                }
                DexOp::RemIntLit8 { dest, src, literal } => {
                    self.lower_lit8("rem-int/lit8", dest, src, *literal, layout)
                }
                DexOp::AndIntLit8 { dest, src, literal } => {
                    self.lower_lit8("and-int/lit8", dest, src, *literal, layout)
                }
                DexOp::OrIntLit8 { dest, src, literal } => {
                    self.lower_lit8("or-int/lit8", dest, src, *literal, layout)
                }
                DexOp::XorIntLit8 { dest, src, literal } => {
                    self.lower_lit8("xor-int/lit8", dest, src, *literal, layout)
                }
                DexOp::ShlIntLit8 { dest, src, literal } => {
                    self.lower_lit8("shl-int/lit8", dest, src, *literal, layout)
                }
                DexOp::ShrIntLit8 { dest, src, literal } => {
                    self.lower_lit8("shr-int/lit8", dest, src, *literal, layout)
                }
                DexOp::UshrIntLit8 { dest, src, literal } => {
                    self.lower_lit8("ushr-int/lit8", dest, src, *literal, layout)
                }
                DexOp::AddIntLit16 { dest, src, literal } => {
                    self.lower_lit16("add-int/lit16", dest, src, *literal, layout)
                }
                DexOp::RSubIntLit16 { dest, src, literal } => {
                    self.lower_lit16("rsub-int", dest, src, *literal, layout)
                }
                DexOp::MulIntLit16 { dest, src, literal } => {
                    self.lower_lit16("mul-int/lit16", dest, src, *literal, layout)
                }
                DexOp::DivIntLit16 { dest, src, literal } => {
                    self.lower_lit16("div-int/lit16", dest, src, *literal, layout)
                }
                DexOp::RemIntLit16 { dest, src, literal } => {
                    self.lower_lit16("rem-int/lit16", dest, src, *literal, layout)
                }
                DexOp::AndIntLit16 { dest, src, literal } => {
                    self.lower_lit16("and-int/lit16", dest, src, *literal, layout)
                }
                DexOp::OrIntLit16 { dest, src, literal } => {
                    self.lower_lit16("or-int/lit16", dest, src, *literal, layout)
                }
                DexOp::XorIntLit16 { dest, src, literal } => {
                    self.lower_lit16("xor-int/lit16", dest, src, *literal, layout)
                }
                DexOp::InvokeVirtual { registers, method } => {
                    self.lower_invoke("invoke-virtual", registers, method, layout)
                }
                DexOp::InvokeSuper { registers, method } => {
                    self.lower_invoke("invoke-super", registers, method, layout)
                }
                DexOp::InvokeInterface { registers, method } => {
                    self.lower_invoke("invoke-interface", registers, method, layout)
                }
                DexOp::InvokeDirect { registers, method } => {
                    self.lower_invoke("invoke-direct", registers, method, layout)
                }
                DexOp::InvokeStatic { registers, method } => {
                    self.lower_invoke("invoke-static", registers, method, layout)
                }
                DexOp::ExecuteInline {
                    registers,
                    inline_index,
                } => self.lower_execute_inline(registers, *inline_index, layout),
                DexOp::InvokeVirtualRange { range, method } => {
                    self.lower_invoke_range("invoke-virtual/range", range, method, layout)
                }
                DexOp::InvokeSuperRange { range, method } => {
                    self.lower_invoke_range("invoke-super/range", range, method, layout)
                }
                DexOp::InvokeDirectRange { range, method } => {
                    self.lower_invoke_range("invoke-direct/range", range, method, layout)
                }
                DexOp::InvokeStaticRange { range, method } => {
                    self.lower_invoke_range("invoke-static/range", range, method, layout)
                }
                DexOp::InvokeInterfaceRange { range, method } => {
                    self.lower_invoke_range("invoke-interface/range", range, method, layout)
                }
                DexOp::ExecuteInlineRange {
                    range,
                    inline_index,
                } => self.lower_execute_inline_range(range, *inline_index, layout),
                DexOp::InvokeCustom {
                    registers,
                    call_site,
                } => self.lower_invoke_custom("invoke-custom", registers, call_site, layout),
                DexOp::InvokeCustomRange { range, call_site } => {
                    self.lower_invoke_custom_range("invoke-custom/range", range, call_site, layout)
                }
                DexOp::InvokePolymorphic {
                    registers,
                    method,
                    proto,
                } => self.lower_invoke_polymorphic(
                    "invoke-polymorphic",
                    registers,
                    method,
                    proto,
                    layout,
                ),
                DexOp::InvokePolymorphicRange {
                    range,
                    method,
                    proto,
                } => self.lower_invoke_polymorphic_range(
                    "invoke-polymorphic/range",
                    range,
                    method,
                    proto,
                    layout,
                ),
                DexOp::IfEq { reg1, reg2, offset } => {
                    self.lower_if_two_reg("if-eq", reg1, reg2, offset, layout)
                }
                DexOp::IfNe { reg1, reg2, offset } => {
                    self.lower_if_two_reg("if-ne", reg1, reg2, offset, layout)
                }
                DexOp::IfLt { reg1, reg2, offset } => {
                    self.lower_if_two_reg("if-lt", reg1, reg2, offset, layout)
                }
                DexOp::IfGe { reg1, reg2, offset } => {
                    self.lower_if_two_reg("if-ge", reg1, reg2, offset, layout)
                }
                DexOp::IfGt { reg1, reg2, offset } => {
                    self.lower_if_two_reg("if-gt", reg1, reg2, offset, layout)
                }
                DexOp::IfLe { reg1, reg2, offset } => {
                    self.lower_if_two_reg("if-le", reg1, reg2, offset, layout)
                }
                DexOp::IfEqz { reg, offset } => {
                    self.lower_if_single_reg("if-eqz", reg, offset, layout)
                }
                DexOp::IfNez { reg, offset } => {
                    self.lower_if_single_reg("if-nez", reg, offset, layout)
                }
                DexOp::IfLtz { reg, offset } => {
                    self.lower_if_single_reg("if-ltz", reg, offset, layout)
                }
                DexOp::IfGez { reg, offset } => {
                    self.lower_if_single_reg("if-gez", reg, offset, layout)
                }
                DexOp::IfGtz { reg, offset } => {
                    self.lower_if_single_reg("if-gtz", reg, offset, layout)
                }
                DexOp::IfLez { reg, offset } => {
                    self.lower_if_single_reg("if-lez", reg, offset, layout)
                }
                DexOp::CmplFloat { dest, src1, src2 } => {
                    self.lower_three_reg("cmpl-float", dest, src1, src2, layout)
                }
                DexOp::CmpgFloat { dest, src1, src2 } => {
                    self.lower_three_reg("cmpg-float", dest, src1, src2, layout)
                }
                DexOp::CmplDouble { dest, src1, src2 } => {
                    self.lower_three_reg("cmpl-double", dest, src1, src2, layout)
                }
                DexOp::CmpgDouble { dest, src1, src2 } => {
                    self.lower_three_reg("cmpg-double", dest, src1, src2, layout)
                }
                DexOp::CmpLong { dest, src1, src2 } => {
                    self.lower_three_reg("cmp-long", dest, src1, src2, layout)
                }
                _ => Err(DexError::new(&format!(
                    "assembler does not support opcode {:?} yet",
                    op
                ))),
            }
        }

        fn lower_branch(
            &self,
            variant: BranchVariant,
            label: &Label,
            encoding: BranchEncoding,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = variant.opcode()?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Branch {
                    label: label.0.clone(),
                    encoding,
                    variant,
                },
            ))
        }

        fn lower_return(
            &self,
            opcode_name: &str,
            reg: &SmaliRegister,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let reg = ensure_reg8(resolve_register(layout, reg)?, opcode_name)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format11x { reg },
            ))
        }

        fn lower_move(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            src: &SmaliRegister,
            layout: &MethodRegisterLayout,
            encoding: MoveEncoding,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = resolve_register(layout, dest)?;
            let src_reg = resolve_register(layout, src)?;
            let kind = match encoding {
                MoveEncoding::Format12x => LoweredKind::Format12x {
                    dest: ensure_reg4(dest_reg, &format!("{} dest", opcode_name))?,
                    src: ensure_reg4(src_reg, &format!("{} src", opcode_name))?,
                },
                MoveEncoding::Format22x => LoweredKind::Format22x {
                    dest: ensure_reg8(dest_reg, &format!("{} dest", opcode_name))?,
                    src: src_reg,
                },
                MoveEncoding::Format32x => LoweredKind::Format32x {
                    dest: dest_reg,
                    src: src_reg,
                },
            };
            Ok(LoweredInstruction::new(opcode, kind))
        }

        fn lower_move_result(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg8(resolve_register(layout, dest)?, opcode_name)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format11x { reg: dest_reg },
            ))
        }

        fn lower_format31t(
            &self,
            opcode_name: &str,
            reg: &SmaliRegister,
            label: &Label,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let reg_idx = ensure_reg8(
                resolve_register(layout, reg)?,
                &format!("{} register", opcode_name),
            )?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format31t {
                    reg: reg_idx,
                    label: label.0.clone(),
                },
            ))
        }

        fn lower_lit8(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            src: &SmaliRegister,
            literal: i8,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg8(
                resolve_register(layout, dest)?,
                &format!("{} dest", opcode_name),
            )?;
            let src_reg = ensure_reg8(
                resolve_register(layout, src)?,
                &format!("{} src", opcode_name),
            )?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format22b {
                    dest: dest_reg,
                    src: src_reg,
                    literal,
                },
            ))
        }

        fn lower_lit16(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            src: &SmaliRegister,
            literal: i16,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg4(
                resolve_register(layout, dest)?,
                &format!("{} dest", opcode_name),
            )?;
            let src_reg = ensure_reg4(
                resolve_register(layout, src)?,
                &format!("{} src", opcode_name),
            )?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format22s {
                    dest: dest_reg,
                    src: src_reg,
                    literal,
                },
            ))
        }

        fn lower_invoke(
            &self,
            opcode_name: &str,
            registers: &[SmaliRegister],
            method: &MethodRef,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let packed = self.pack_invoke_registers(opcode_name, registers, layout)?;
            if let Some((kind, idx)) = detect_quick_method(opcode_name, method)? {
                return self.lower_quick_invoke(kind, packed, idx);
            }
            let index = self.resolve_method_index(method)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format35c {
                    index,
                    regs: packed.regs,
                    count: packed.count,
                },
            ))
        }

        fn lower_invoke_range(
            &self,
            opcode_name: &str,
            range: &RegisterRange,
            method: &MethodRef,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let (first_reg, count) = self.pack_invoke_range(opcode_name, range, layout)?;
            if let Some((kind, idx)) = detect_quick_method(opcode_name, method)? {
                return self.lower_quick_invoke_range(kind, first_reg, count, idx);
            }
            let index = self.resolve_method_index(method)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format3rc {
                    first_reg,
                    count,
                    index,
                },
            ))
        }

        fn lower_quick_invoke(
            &self,
            kind: QuickInvokeKind,
            regs: PackedInvokeRegisters,
            index: u16,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode_name = match kind {
                QuickInvokeKind::Virtual => "invoke-virtual-quick",
                QuickInvokeKind::Super => "invoke-super-quick",
            };
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format35ms {
                    index,
                    regs: regs.regs,
                    count: regs.count,
                },
            ))
        }

        fn lower_quick_invoke_range(
            &self,
            kind: QuickInvokeKind,
            first_reg: u16,
            count: u16,
            index: u16,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode_name = match kind {
                QuickInvokeKind::Virtual => "invoke-virtual-quick/range",
                QuickInvokeKind::Super => "invoke-super-quick/range",
            };
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format3rms {
                    first_reg,
                    count,
                    index,
                },
            ))
        }

        fn lower_invoke_custom(
            &self,
            opcode_name: &str,
            registers: &[SmaliRegister],
            call_site: &str,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let packed = self.pack_invoke_registers(opcode_name, registers, layout)?;
            let index = self.resolver.call_site_index(call_site)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format35c {
                    index,
                    regs: packed.regs,
                    count: packed.count,
                },
            ))
        }

        fn lower_invoke_custom_range(
            &self,
            opcode_name: &str,
            range: &RegisterRange,
            call_site: &str,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let (first_reg, count) = self.pack_invoke_range(opcode_name, range, layout)?;
            let index = self.resolver.call_site_index(call_site)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format3rc {
                    first_reg,
                    count,
                    index,
                },
            ))
        }

        fn lower_invoke_polymorphic(
            &self,
            opcode_name: &str,
            registers: &[SmaliRegister],
            method: &MethodRef,
            proto: &str,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let packed = self.pack_invoke_registers(opcode_name, registers, layout)?;
            let method_index = self.resolve_method_index(method)?;
            let proto_index = self.resolve_proto_index(proto)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format45cc {
                    index: method_index,
                    proto_index,
                    regs: packed.regs,
                    count: packed.count,
                },
            ))
        }

        fn lower_invoke_polymorphic_range(
            &self,
            opcode_name: &str,
            range: &RegisterRange,
            method: &MethodRef,
            proto: &str,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let (first_reg, count) = self.pack_invoke_range(opcode_name, range, layout)?;
            let method_index = self.resolve_method_index(method)?;
            let proto_index = self.resolve_proto_index(proto)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format4rcc {
                    first_reg,
                    count,
                    index: method_index,
                    proto_index,
                },
            ))
        }

        fn lower_array_get(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            array: &SmaliRegister,
            index: &SmaliRegister,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg8(
                resolve_register(layout, dest)?,
                &format!("{} dest", opcode_name),
            )?;
            let array_reg = ensure_reg8(
                resolve_register(layout, array)?,
                &format!("{} array", opcode_name),
            )?;
            let index_reg = ensure_reg8(
                resolve_register(layout, index)?,
                &format!("{} index", opcode_name),
            )?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format23x {
                    dest: dest_reg,
                    src1: array_reg,
                    src2: index_reg,
                },
            ))
        }

        fn lower_array_put(
            &self,
            opcode_name: &str,
            src: &SmaliRegister,
            array: &SmaliRegister,
            index: &SmaliRegister,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let value_reg = ensure_reg8(
                resolve_register(layout, src)?,
                &format!("{} value", opcode_name),
            )?;
            let array_reg = ensure_reg8(
                resolve_register(layout, array)?,
                &format!("{} array", opcode_name),
            )?;
            let index_reg = ensure_reg8(
                resolve_register(layout, index)?,
                &format!("{} index", opcode_name),
            )?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format23x {
                    dest: value_reg,
                    src1: array_reg,
                    src2: index_reg,
                },
            ))
        }

        fn lower_two_reg_format12x(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            src: &SmaliRegister,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg4(
                resolve_register(layout, dest)?,
                &format!("{} dest", opcode_name),
            )?;
            let src_reg = ensure_reg4(
                resolve_register(layout, src)?,
                &format!("{} source", opcode_name),
            )?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format12x {
                    dest: dest_reg,
                    src: src_reg,
                },
            ))
        }

        fn lower_instance_field_read(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            object: &SmaliRegister,
            field: &FieldRef,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg4(
                resolve_register(layout, dest)?,
                &format!("{} dest", opcode_name),
            )?;
            let obj_reg = ensure_reg4(
                resolve_register(layout, object)?,
                &format!("{} object", opcode_name),
            )?;
            if let Some(idx) = detect_quick_field(field)? {
                return self.lower_quick_field_accessor(opcode_name, dest_reg, obj_reg, idx);
            }
            let index = self.resolve_field_index(field)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format22c {
                    reg_a: dest_reg,
                    reg_b: obj_reg,
                    index,
                },
            ))
        }

        fn lower_instance_field_write(
            &self,
            opcode_name: &str,
            src: &SmaliRegister,
            object: &SmaliRegister,
            field: &FieldRef,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let src_reg = ensure_reg4(
                resolve_register(layout, src)?,
                &format!("{} value", opcode_name),
            )?;
            let obj_reg = ensure_reg4(
                resolve_register(layout, object)?,
                &format!("{} object", opcode_name),
            )?;
            if let Some(idx) = detect_quick_field(field)? {
                return self.lower_quick_field_accessor(opcode_name, src_reg, obj_reg, idx);
            }
            let index = self.resolve_field_index(field)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format22c {
                    reg_a: src_reg,
                    reg_b: obj_reg,
                    index,
                },
            ))
        }

        fn lower_quick_field_accessor(
            &self,
            opcode_name: &str,
            reg_a: u16,
            reg_b: u16,
            index: u16,
        ) -> Result<LoweredInstruction, DexError> {
            let quick_name = format!("{}-quick", opcode_name);
            let opcode = opcode_by_name(&quick_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", quick_name))
            })?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format22cs {
                    reg_a,
                    reg_b,
                    index,
                },
            ))
        }

        fn lower_static_field_get(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            field: &FieldRef,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg8(
                resolve_register(layout, dest)?,
                &format!("{} dest", opcode_name),
            )?;
            let index = self.resolve_field_index(field)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format21c {
                    reg: dest_reg,
                    index,
                },
            ))
        }

        fn lower_static_field_put(
            &self,
            opcode_name: &str,
            src: &SmaliRegister,
            field: &FieldRef,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let src_reg = ensure_reg8(
                resolve_register(layout, src)?,
                &format!("{} value", opcode_name),
            )?;
            let index = self.resolve_field_index(field)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format21c {
                    reg: src_reg,
                    index,
                },
            ))
        }

        fn lower_type_const(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            class_desc: &str,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg8(
                resolve_register(layout, dest)?,
                &format!("{} dest", opcode_name),
            )?;
            let index = self.resolve_type_index(class_desc)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format21c {
                    reg: dest_reg,
                    index,
                },
            ))
        }

        fn lower_type_check(
            &self,
            opcode_name: &str,
            reg: &SmaliRegister,
            class_desc: &str,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let reg_idx = ensure_reg8(
                resolve_register(layout, reg)?,
                &format!("{} register", opcode_name),
            )?;
            let index = self.resolve_type_index(class_desc)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format21c {
                    reg: reg_idx,
                    index,
                },
            ))
        }

        fn lower_two_reg_type_op(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            src: &SmaliRegister,
            class_desc: &str,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg4(
                resolve_register(layout, dest)?,
                &format!("{} dest", opcode_name),
            )?;
            let src_reg = ensure_reg4(
                resolve_register(layout, src)?,
                &format!("{} source", opcode_name),
            )?;
            let index = self.resolve_type_index(class_desc)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format22c {
                    reg_a: dest_reg,
                    reg_b: src_reg,
                    index,
                },
            ))
        }

        fn lower_filled_new_array(
            &self,
            opcode_name: &str,
            regs: &[SmaliRegister],
            class_desc: &str,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let packed = self.pack_invoke_registers(opcode_name, regs, layout)?;
            let index = self.resolve_type_index(class_desc)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format35c {
                    index,
                    regs: packed.regs,
                    count: packed.count,
                },
            ))
        }

        fn lower_filled_new_array_range(
            &self,
            opcode_name: &str,
            range: &RegisterRange,
            class_desc: &str,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let (first_reg, count) = self.pack_invoke_range(opcode_name, range, layout)?;
            let index = self.resolve_type_index(class_desc)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format3rc {
                    first_reg,
                    count,
                    index,
                },
            ))
        }

        fn lower_method_handle_const(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            literal: &str,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg8(
                resolve_register(layout, dest)?,
                &format!("{} dest", opcode_name),
            )?;
            let index = self.resolver.method_handle_index(literal)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format21c {
                    reg: dest_reg,
                    index,
                },
            ))
        }

        fn lower_method_type_const(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            proto: &str,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg8(
                resolve_register(layout, dest)?,
                &format!("{} dest", opcode_name),
            )?;
            let signature = parse_method_descriptor(proto)?;
            let index = self.resolver.proto_index(&signature)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format21c {
                    reg: dest_reg,
                    index,
                },
            ))
        }

        fn lower_throw_verification_error(
            &self,
            kind: u8,
            reference: &VerificationErrorRef,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name("throw-verification-error").ok_or_else(|| {
                DexError::new("opcode throw-verification-error not present in table")
            })?;
            let index = self.resolve_verification_reference(reference)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format20bc { kind, index },
            ))
        }

        fn resolve_verification_reference(
            &self,
            reference: &VerificationErrorRef,
        ) -> Result<u16, DexError> {
            match reference {
                VerificationErrorRef::None => Ok(0),
                VerificationErrorRef::Type(desc) => {
                    let idx = self.resolve_type_index(desc)?;
                    ensure_u16_index(idx, "type")
                }
                VerificationErrorRef::Field(field) => {
                    let idx = self.resolve_field_index(field)?;
                    ensure_u16_index(idx, "field")
                }
                VerificationErrorRef::Method(method) => {
                    let idx = self.resolve_method_index(method)?;
                    ensure_u16_index(idx, "method")
                }
            }
        }

        fn lower_execute_inline(
            &self,
            registers: &[SmaliRegister],
            inline_index: u16,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name("execute-inline")
                .ok_or_else(|| DexError::new("opcode execute-inline not present in table"))?;
            let packed = self.pack_invoke_registers("execute-inline", registers, layout)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format35mi {
                    index: inline_index,
                    regs: packed.regs,
                    count: packed.count,
                },
            ))
        }

        fn lower_execute_inline_range(
            &self,
            range: &RegisterRange,
            inline_index: u16,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name("execute-inline/range")
                .ok_or_else(|| DexError::new("opcode execute-inline/range not present in table"))?;
            let (first_reg, count) =
                self.pack_invoke_range("execute-inline/range", range, layout)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format3rmi {
                    first_reg,
                    count,
                    index: inline_index,
                },
            ))
        }

        fn lower_single_reg_op(
            &self,
            opcode_name: &str,
            reg: &SmaliRegister,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let reg_idx = ensure_reg8(
                resolve_register(layout, reg)?,
                &format!("{} reg", opcode_name),
            )?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format11x { reg: reg_idx },
            ))
        }

        fn resolve_field_index(&self, field: &FieldRef) -> Result<u32, DexError> {
            self.resolver
                .field_index(&field.class, &field.name, &field.descriptor)
        }

        fn lower_if_single_reg(
            &self,
            opcode_name: &str,
            reg: &SmaliRegister,
            label: &Label,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let reg_idx = ensure_reg8(resolve_register(layout, reg)?, opcode_name)?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format21t {
                    reg: reg_idx,
                    label: label.0.clone(),
                },
            ))
        }

        fn lower_if_two_reg(
            &self,
            opcode_name: &str,
            reg1: &SmaliRegister,
            reg2: &SmaliRegister,
            label: &Label,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let lhs = ensure_reg4(
                resolve_register(layout, reg1)?,
                &format!("{} first operand", opcode_name),
            )?;
            let rhs = ensure_reg4(
                resolve_register(layout, reg2)?,
                &format!("{} second operand", opcode_name),
            )?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format22t {
                    reg_a: lhs,
                    reg_b: rhs,
                    label: label.0.clone(),
                },
            ))
        }

        fn lower_three_reg(
            &self,
            opcode_name: &str,
            dest: &SmaliRegister,
            src1: &SmaliRegister,
            src2: &SmaliRegister,
            layout: &MethodRegisterLayout,
        ) -> Result<LoweredInstruction, DexError> {
            let opcode = opcode_by_name(opcode_name).ok_or_else(|| {
                DexError::new(&format!("opcode {} not present in table", opcode_name))
            })?;
            let dest_reg = ensure_reg8(
                resolve_register(layout, dest)?,
                &format!("{} dest", opcode_name),
            )?;
            let src1_reg = ensure_reg8(
                resolve_register(layout, src1)?,
                &format!("{} first operand", opcode_name),
            )?;
            let src2_reg = ensure_reg8(
                resolve_register(layout, src2)?,
                &format!("{} second operand", opcode_name),
            )?;
            Ok(LoweredInstruction::new(
                opcode,
                LoweredKind::Format23x {
                    dest: dest_reg,
                    src1: src1_reg,
                    src2: src2_reg,
                },
            ))
        }

        fn pack_invoke_registers(
            &self,
            opcode_name: &str,
            registers: &[SmaliRegister],
            layout: &MethodRegisterLayout,
        ) -> Result<PackedInvokeRegisters, DexError> {
            if registers.len() > 5 {
                return Err(DexError::new(&format!(
                    "{} specifies {} registers; maximum for this form is 5",
                    opcode_name,
                    registers.len()
                )));
            }
            let mut packed = [0u16; 5];
            for (idx, reg) in registers.iter().enumerate() {
                let resolved = resolve_register(layout, reg)?;
                packed[idx] = ensure_reg4(resolved, &format!("{} argument {}", opcode_name, idx))?;
            }
            Ok(PackedInvokeRegisters {
                regs: packed,
                count: registers.len() as u8,
            })
        }

        fn pack_invoke_range(
            &self,
            opcode_name: &str,
            range: &RegisterRange,
            layout: &MethodRegisterLayout,
        ) -> Result<(u16, u16), DexError> {
            let start = resolve_register(layout, &range.start)?;
            let end = resolve_register(layout, &range.end)?;
            if end < start {
                return Err(DexError::new(&format!(
                    "{} range end precedes start",
                    opcode_name
                )));
            }
            let count = end - start + 1;
            if count > u8::MAX as u16 {
                return Err(DexError::new(&format!(
                    "{} range covers {} registers; maximum is 255",
                    opcode_name, count
                )));
            }
            Ok((start, count))
        }

        fn resolve_method_index(&self, method: &MethodRef) -> Result<u32, DexError> {
            let signature = parse_method_descriptor(&method.descriptor)?;
            self.resolver
                .method_index(&method.class, &method.name, &signature)
        }

        fn resolve_proto_index(&self, descriptor: &str) -> Result<u32, DexError> {
            let signature = parse_method_descriptor(descriptor)?;
            self.resolver.proto_index(&signature)
        }

        fn resolve_type_index(&self, descriptor: &str) -> Result<u32, DexError> {
            self.resolver.type_index(descriptor)
        }
    }

    fn resolve_register(
        layout: &MethodRegisterLayout,
        register: &SmaliRegister,
    ) -> Result<u16, DexError> {
        match register {
            SmaliRegister::Local(idx) => {
                if *idx >= layout.registers_size {
                    return Err(DexError::new(&format!(
                        "register v{} exceeds allocated register count {}",
                        idx, layout.registers_size
                    )));
                }
                Ok(*idx)
            }
            SmaliRegister::Parameter(idx) => {
                let base = layout.registers_size.saturating_sub(layout.ins_size);
                let actual = base + *idx;
                if actual >= layout.registers_size {
                    return Err(DexError::new(&format!(
                        "parameter register p{} out of range (ins={})",
                        idx, layout.ins_size
                    )));
                }
                Ok(actual)
            }
        }
    }

    fn ensure_reg8(reg: u16, context: &str) -> Result<u16, DexError> {
        if reg > u8::MAX as u16 {
            return Err(DexError::new(&format!(
                "{} register index {} exceeds 8-bit encoding",
                context, reg
            )));
        }
        Ok(reg)
    }

    fn ensure_reg4(reg: u16, context: &str) -> Result<u16, DexError> {
        if reg > 0x0f {
            return Err(DexError::new(&format!(
                "{} register index {} exceeds 4-bit encoding; consider /from16 form",
                context, reg
            )));
        }
        Ok(reg)
    }

    fn ensure_u16_index(value: u32, context: &str) -> Result<u16, DexError> {
        if value > u16::MAX as u32 {
            return Err(DexError::new(&format!(
                "{} index {} exceeds 16-bit range",
                context, value
            )));
        }
        Ok(value as u16)
    }

    #[derive(Debug, Clone, Copy)]
    enum MoveEncoding {
        Format12x,
        Format22x,
        Format32x,
    }
}

/// Represents different types of references used by opcodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceType {
    None,
    String,
    Type,
    Field,
    Method,
    CallSite,
    MethodProto,
    MethodHandle,
}

// Defines various flags that can be associated with an opcode.
bitflags! {
    pub struct OpcodeFlags: u32 {
        const CAN_THROW = 0x1;
        const ODEX_ONLY = 0x2;
        const CAN_CONTINUE = 0x4;
        const SETS_RESULT = 0x8;
        const SETS_REGISTER = 0x10;
        const SETS_WIDE_REGISTER = 0x20;
        const QUICK_FIELD_ACCESSOR = 0x40;
        const VOLATILE_FIELD_ACCESSOR = 0x80;
        const STATIC_FIELD_ACCESSOR = 0x100;
        const JUMBO_OPCODE = 0x200;
        const CAN_INITIALIZE_REFERENCE = 0x400;
    }
}

/// Represents an opcode with its associated properties and mappings.
pub struct Opcode {
    pub name: &'static str,
    pub reference_type: ReferenceType,
    pub reference_type2: Option<ReferenceType>,
    pub format: Format,
    pub flags: OpcodeFlags,
    pub api_to_value_map: RangeInclusiveMap<i32, u16>,
    pub art_version_to_value_map: RangeInclusiveMap<i32, u16>,
}

/// Encapsulates API and ART version ranges along with opcode values.
pub struct VersionConstraint {
    pub api_range: Option<RangeInclusive<i32>>,
    pub art_version_range: Option<RangeInclusive<i32>>,
    pub opcode_value: u16,
}

impl Opcode {
    /// Creates a new Opcode instance.
    pub(crate) fn new(
        version_constraints: Vec<VersionConstraint>,
        name: &'static str,
        reference_type: ReferenceType,
        reference_type2: Option<ReferenceType>,
        format: Format,
        flags: OpcodeFlags,
    ) -> Self {
        let mut api_to_value_map = RangeInclusiveMap::new();
        let mut art_version_to_value_map = RangeInclusiveMap::new();

        for vc in version_constraints.iter() {
            if let Some(api_range) = &vc.api_range {
                if !api_range.is_empty() {
                    api_to_value_map.insert(api_range.clone(), vc.opcode_value);
                }
            }
            if let Some(art_range) = &vc.art_version_range {
                if !art_range.is_empty() {
                    art_version_to_value_map.insert(art_range.clone(), vc.opcode_value);
                }
            }
        }

        Opcode {
            name,
            reference_type,
            reference_type2,
            format,
            flags,
            api_to_value_map,
            art_version_to_value_map,
        }
    }

    pub(crate) fn new_no_flags(
        version_constraints: Vec<VersionConstraint>,
        name: &'static str,
        reference_type: ReferenceType,
        format: Format,
    ) -> Self {
        Opcode::new(
            version_constraints,
            name,
            reference_type,
            None,
            format,
            OpcodeFlags::empty(),
        )
    }

    /// Helper function similar to Java's `firstApi`.
    pub(crate) fn first_api(opcode_value: u16, api: i32) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: Some(api..=i32::MAX),
            art_version_range: None,
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `lastApi`.
    pub(crate) fn last_api(opcode_value: u16, api: i32) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: Some(i32::MIN..=api),
            art_version_range: None,
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `betweenApi`.
    pub(crate) fn between_api(
        opcode_value: u16,
        min_api: i32,
        max_api: i32,
    ) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: Some(min_api..=max_api),
            art_version_range: None,
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `firstArtVersion`.
    pub(crate) fn first_art_version(opcode_value: u16, art_version: i32) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: None,
            art_version_range: Some(art_version..=i32::MAX),
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `lastArtVersion`.
    pub(crate) fn last_art_version(opcode_value: u16, art_version: i32) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: None,
            art_version_range: Some(i32::MIN..=art_version),
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `allVersions`.
    pub(crate) fn all_versions(opcode_value: u16) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: Some(i32::MIN..=i32::MAX),
            art_version_range: Some(i32::MIN..=i32::MAX),
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `allApis`.
    pub(crate) fn all_apis(opcode_value: u16) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: Some(i32::MIN..=i32::MAX),
            art_version_range: None,
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `allArtVersions`.
    pub(crate) fn all_art_versions(opcode_value: u16) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: None,
            art_version_range: Some(i32::MIN..=i32::MAX),
            opcode_value,
        }]
    }

    /// Combines multiple vectors of `VersionConstraint` into one.
    pub(crate) fn combine(
        constraints: Vec<VersionConstraint>,
        other: Vec<VersionConstraint>,
    ) -> Vec<VersionConstraint> {
        let mut combined = constraints;
        combined.extend(other);
        combined
    }

    /// Determines if the opcode can throw an exception.
    pub fn can_throw(&self) -> bool {
        self.flags.contains(OpcodeFlags::CAN_THROW)
    }

    /// Determines if the opcode is ODEX only.
    pub fn odex_only(&self) -> bool {
        self.flags.contains(OpcodeFlags::ODEX_ONLY)
    }

    /// Determines if execution can continue to the next instruction.
    pub fn can_continue(&self) -> bool {
        self.flags.contains(OpcodeFlags::CAN_CONTINUE)
    }

    /// Determines if the opcode sets the "hidden" result register.
    pub fn sets_result(&self) -> bool {
        self.flags.contains(OpcodeFlags::SETS_RESULT)
    }

    /// Determines if the opcode sets the value of its first register.
    pub fn sets_register(&self) -> bool {
        self.flags.contains(OpcodeFlags::SETS_REGISTER)
    }

    /// Determines if the opcode sets the value of its first register to a wide type.
    pub fn sets_wide_register(&self) -> bool {
        self.flags.contains(OpcodeFlags::SETS_WIDE_REGISTER)
    }

    /// Determines if the opcode is a quick field accessor.
    pub fn is_quick_field_accessor(&self) -> bool {
        self.flags.contains(OpcodeFlags::QUICK_FIELD_ACCESSOR)
    }

    /// Determines if the opcode is a volatile field accessor.
    pub fn is_volatile_field_accessor(&self) -> bool {
        self.flags.contains(OpcodeFlags::VOLATILE_FIELD_ACCESSOR)
    }

    /// Determines if the opcode is a static field accessor.
    pub fn is_static_field_accessor(&self) -> bool {
        self.flags.contains(OpcodeFlags::STATIC_FIELD_ACCESSOR)
    }

    /// Determines if the opcode is a jumbo opcode.
    pub fn is_jumbo_opcode(&self) -> bool {
        self.flags.contains(OpcodeFlags::JUMBO_OPCODE)
    }

    /// Determines if the opcode can initialize an uninitialized object reference.
    pub fn can_initialize_reference(&self) -> bool {
        self.flags.contains(OpcodeFlags::CAN_INITIALIZE_REFERENCE)
    }

    /// Retrieves the opcode value based on API and ART version.
    pub fn get_opcode_value(&self, api: i32, art_version: i32) -> Option<u16> {
        // Priority: API mapping first, then ART version mapping
        if let Some(&value) = self.api_to_value_map.get(&api) {
            return Some(value);
        }
        if let Some(&value) = self.art_version_to_value_map.get(&art_version) {
            return Some(value);
        }
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    Format10t,
    Format10x,
    Format11n,
    Format11x,
    Format12x,
    Format20bc,
    Format20t,
    Format21c,
    Format21ih,
    Format21lh,
    Format21s,
    Format21t,
    Format22b,
    Format22c,
    Format22cs,
    Format22s,
    Format22t,
    Format22x,
    Format23x,
    Format30t,
    Format31c,
    Format31i,
    Format31t,
    Format32x,
    Format35c,
    Format35mi,
    Format35ms,
    Format3rc,
    Format3rmi,
    Format3rms,
    Format45cc,
    Format4rcc,
    Format51l,
    ArrayPayload,
    PackedSwitchPayload,
    SparseSwitchPayload,
    UnresolvedOdexInstruction,
}

impl Format {
    /// Returns the size associated with the format.
    pub const fn size(&self) -> i32 {
        match self {
            // Size 2
            Format::Format10t
            | Format::Format10x
            | Format::Format11n
            | Format::Format11x
            | Format::Format12x => 2,

            // Size 4
            Format::Format20bc
            | Format::Format20t
            | Format::Format21c
            | Format::Format21ih
            | Format::Format21lh
            | Format::Format21s
            | Format::Format21t
            | Format::Format22b
            | Format::Format22c
            | Format::Format22cs
            | Format::Format22s
            | Format::Format22t
            | Format::Format22x
            | Format::Format23x => 4,

            // Size 6
            Format::Format30t
            | Format::Format31c
            | Format::Format31i
            | Format::Format31t
            | Format::Format32x
            | Format::Format35c
            | Format::Format35mi
            | Format::Format35ms
            | Format::Format3rc
            | Format::Format3rmi
            | Format::Format3rms => 6,

            // Size 8
            Format::Format45cc | Format::Format4rcc => 8,

            // Size 10
            Format::Format51l => 10,

            // Size -1 for payload formats and unresolved instructions
            Format::ArrayPayload
            | Format::PackedSwitchPayload
            | Format::SparseSwitchPayload
            | Format::UnresolvedOdexInstruction => -1,
        }
    }

    /// Indicates whether the format is a payload format.
    pub const fn is_payload_format(&self) -> bool {
        matches!(
            self,
            Format::ArrayPayload | Format::PackedSwitchPayload | Format::SparseSwitchPayload
        )
    }
}

// Helpers for pulling format encoding
#[inline]
fn u16_at(code: &[u16], pc: usize) -> u16 {
    code[pc]
}
#[inline]
fn u16_at_opt(code: &[u16], pc: usize) -> Option<u16> {
    code.get(pc).copied()
}

#[inline]
fn require_cu(code: &[u16], pc: usize, need: usize, opname: &str) -> Result<(), DexError> {
    if pc + need > code.len() {
        return Err(DexError::new(&format!(
            "Truncated {} at pc {}: need {} code units, have {}",
            opname,
            pc,
            need,
            code.len().saturating_sub(pc)
        )));
    }
    Ok(())
}
#[inline]
fn op(inst: u16) -> u8 {
    (inst & 0x00ff) as u8
}
#[inline]
fn a8(inst: u16) -> u8 {
    (inst >> 8) as u8
} // 11x AA, 21x AA, …
#[inline]
fn a4(inst: u16) -> u8 {
    ((inst >> 8) & 0x0f) as u8
} // 12x A (low nibble of high byte)
#[inline]
fn b4(inst: u16) -> u8 {
    ((inst >> 12) & 0x0f) as u8
} // 12x B (high nibble of high byte)
#[inline]
fn s16(x: u16) -> i16 {
    x as i16
}
#[inline]
fn s8(x: u8) -> i8 {
    x as i8
}
#[inline]
fn s4(x: u8) -> i8 {
    ((x as i8) << 4) >> 4
}

#[inline]
fn format_size_cu(fmt: Format) -> usize {
    match fmt.size() {
        n if n > 0 => (n as usize) / 2,
        _ => 1, // payloads handled specially later; avoid stall
    }
}

// Helper for computing branch targets robustly: (pc as i32 + off), but only if in bounds
#[inline]
fn add_off_i32(pc: usize, off: i32, len: usize) -> Option<usize> {
    // Compute (pc as i32 + off) safely, returning None if result is <0 or >= len
    let base = pc as i32;
    let sum = base.checked_add(off)?;
    if sum < 0 || (sum as usize) >= len {
        None
    } else {
        Some(sum as usize)
    }
}

#[inline]
fn add_off_from(base_pc: usize, off: i32, len: usize) -> Option<usize> {
    let base = base_pc as i32;
    let sum = base.checked_add(off)?;
    if sum < 0 || (sum as usize) >= len {
        None
    } else {
        Some(sum as usize)
    }
}

#[inline]
fn reg_valid(mapper: Option<&RegMapper>, r: u16) -> bool {
    if let Some(m) = mapper {
        r < m.registers_size
    } else {
        true
    }
}

use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PayloadKind {
    Array,
    PackedSwitch,
    SparseSwitch,
}

#[inline]
fn payload_len_cu(kind: PayloadKind, code: &[u16], pc: usize) -> Option<usize> {
    match kind {
        PayloadKind::Array => {
            // header: ident(0x0300), element_width(u16), size(lo u16, hi u16)
            if pc + 4 > code.len() {
                return None;
            }
            let elem_width = u16_at(code, pc + 1) as usize;
            let size_lo = u16_at(code, pc + 2) as usize;
            let size_hi = u16_at(code, pc + 3) as usize;
            let count = (size_hi << 16) | size_lo;
            let bytes_len = elem_width.checked_mul(count)?;
            let data_cu = (bytes_len + 1) / 2; // ceil(bytes/2)
            Some(4 + data_cu)
        }
        PayloadKind::PackedSwitch => {
            // header: ident(0x0100), size(u16), first_key(i32), then size * target(i32)
            if pc + 4 > code.len() {
                return None;
            }
            let sz = u16_at(code, pc + 1) as usize;
            Some(4 + sz * 2)
        }
        PayloadKind::SparseSwitch => {
            // header: ident(0x0200), size(u16), then size * key(i32) and size * target(i32)
            if pc + 2 > code.len() {
                return None;
            }
            let sz = u16_at(code, pc + 1) as usize;
            Some(2 + sz * 4)
        }
    }
}

fn collect_labels_and_payloads(
    code: &[u16],
    opcode_cache: &HashMap<u16, &Opcode>,
) -> (
    HashMap<usize, String>,
    HashMap<usize, PayloadKind>,
    HashMap<usize, usize>,
) {
    let mut labels: HashMap<usize, String> = HashMap::new();
    let mut payloads: HashMap<usize, PayloadKind> = HashMap::new();
    let mut payload_base: HashMap<usize, usize> = HashMap::new();
    let mut cond_count = 0usize;
    let mut goto_count = 0usize;
    let mut array_count = 0usize;
    let mut psw_count = 0usize;
    let mut ssw_count = 0usize;

    let mut pc: usize = 0;
    while pc < code.len() {
        // If we are sitting on a known payload target, skip its body so we don't
        // misinterpret payload words as opcodes in this pass.
        if let Some(kind) = payloads.get(&pc).copied() {
            if let Some(consumed) = payload_len_cu(kind, code, pc) {
                smali_trace!(
                    "[smali][collect] skip payload {:?} at pc {} ({} CU)",
                    kind,
                    pc,
                    consumed
                );
                pc += consumed;
                continue;
            } else {
                smali_warn!(
                    "[smali][collect] WARN: truncated payload {:?} at pc {} — stopping",
                    kind,
                    pc
                );
                break;
            }
        }
        let inst = u16_at(code, pc);
        let opc = op(inst) as u16;
        let Some(opdef) = opcode_cache.get(&opc) else {
            pc += 1;
            continue;
        };
        let fmt = opdef.format;
        // Ensure we have enough code units for this instruction before reading operands
        let need_cu = format_size_cu(fmt);
        if pc + need_cu > code.len() {
            smali_warn!(
                "[smali][collect] WARN: truncated {} at pc {} in first pass: need {} CU, have {} — stopping",
                opdef.name,
                pc,
                need_cu,
                code.len().saturating_sub(pc)
            );
            break;
        }
        match fmt {
            // conditional branches (16-bit offset)
            Format::Format21t | Format::Format22t => {
                let off = s16(u16_at(code, pc + 1)) as i32;
                let size_cu = format_size_cu(fmt);
                let tgt_cur = add_off_i32(pc, off, code.len());
                let tgt_nxt = add_off_from(pc + size_cu, off, code.len());
                smali_trace!(
                    "[smali][collect][t] {} @pc {} off {} -> cur={:?} nxt={:?}",
                    opdef.name,
                    pc,
                    off,
                    tgt_cur,
                    tgt_nxt
                );
                let chosen = match (tgt_cur, tgt_nxt) {
                    (Some(t), None) | (Some(t), Some(_)) => Some(t),
                    (None, Some(t)) => {
                        smali_trace!(
                            "[smali][collect][t] NOTE: using next-pc base for {} at pc {}",
                            opdef.name,
                            pc
                        );
                        Some(t)
                    }
                    (None, None) => None,
                };
                if let Some(tgt) = chosen {
                    labels.entry(tgt).or_insert_with(|| {
                        let s = format!(":cond_{}", cond_count);
                        cond_count += 1;
                        s
                    });
                } else {
                    smali_warn!(
                        "[smali][collect] WARN: 2xt branch target OOB at pc {} (off {}), skipping",
                        pc,
                        off
                    );
                }
                pc += size_cu;
            }
            // gotos
            Format::Format10t => {
                let off = s8(a8(inst)) as i32;
                let size_cu = format_size_cu(fmt);
                let tgt_cur = add_off_i32(pc, off, code.len());
                let tgt_nxt = add_off_from(pc + size_cu, off, code.len());
                smali_trace!(
                    "[smali][collect][t] {} @pc {} off {} -> cur={:?} nxt={:?}",
                    opdef.name,
                    pc,
                    off,
                    tgt_cur,
                    tgt_nxt
                );
                let chosen = match (tgt_cur, tgt_nxt) {
                    (Some(t), None) | (Some(t), Some(_)) => Some(t),
                    (None, Some(t)) => {
                        smali_trace!(
                            "[smali][collect][t] NOTE: using next-pc base for {} at pc {}",
                            opdef.name,
                            pc
                        );
                        Some(t)
                    }
                    (None, None) => None,
                };
                if let Some(tgt) = chosen {
                    labels.entry(tgt).or_insert_with(|| {
                        let s = format!(":goto_{}", goto_count);
                        goto_count += 1;
                        s
                    });
                } else {
                    smali_warn!(
                        "[smali][collect] WARN: 10t target OOB at pc {} (off {}), skipping",
                        pc,
                        off
                    );
                }
                pc += size_cu;
            }
            Format::Format20t => {
                let off = s16(u16_at(code, pc + 1)) as i32;
                let size_cu = format_size_cu(fmt);
                let tgt_cur = add_off_i32(pc, off, code.len());
                let tgt_nxt = add_off_from(pc + size_cu, off, code.len());
                smali_trace!(
                    "[smali][collect][t] {} @pc {} off {} -> cur={:?} nxt={:?}",
                    opdef.name,
                    pc,
                    off,
                    tgt_cur,
                    tgt_nxt
                );
                let chosen = match (tgt_cur, tgt_nxt) {
                    (Some(t), None) | (Some(t), Some(_)) => Some(t),
                    (None, Some(t)) => {
                        smali_trace!(
                            "[smali][collect][t] NOTE: using next-pc base for {} at pc {}",
                            opdef.name,
                            pc
                        );
                        Some(t)
                    }
                    (None, None) => None,
                };
                if let Some(tgt) = chosen {
                    labels.entry(tgt).or_insert_with(|| {
                        let s = format!(":goto_{}", goto_count);
                        goto_count += 1;
                        s
                    });
                } else {
                    smali_warn!(
                        "[smali][collect] WARN: 20t target OOB at pc {} (off {}), skipping",
                        pc,
                        off
                    );
                }
                pc += size_cu;
            }
            Format::Format30t => {
                let lo = u16_at(code, pc + 1) as u32;
                let hi = u16_at(code, pc + 2) as u32;
                let off32 = ((hi << 16) | lo) as i32;
                let size_cu = format_size_cu(fmt);
                let tgt_cur = add_off_i32(pc, off32, code.len());
                let tgt_nxt = add_off_from(pc + size_cu, off32, code.len());
                smali_trace!(
                    "[smali][collect][t] {} @pc {} off {} -> cur={:?} nxt={:?}",
                    opdef.name,
                    pc,
                    off32,
                    tgt_cur,
                    tgt_nxt
                );
                let chosen = match (tgt_cur, tgt_nxt) {
                    (Some(t), None) | (Some(t), Some(_)) => Some(t),
                    (None, Some(t)) => {
                        smali_trace!(
                            "[smali][collect][t] NOTE: using next-pc base for {} at pc {}",
                            opdef.name,
                            pc
                        );
                        Some(t)
                    }
                    (None, None) => None,
                };
                if let Some(tgt) = chosen {
                    labels.entry(tgt).or_insert_with(|| {
                        let s = format!(":goto_{}", goto_count);
                        goto_count += 1;
                        s
                    });
                } else {
                    smali_warn!(
                        "[smali][collect] WARN: 30t target OOB at pc {} (off {}), skipping",
                        pc,
                        off32
                    );
                }
                pc += size_cu;
            }
            // payload targets via 31t
            Format::Format31t => {
                let lo = u16_at(code, pc + 1) as u32;
                let hi = u16_at(code, pc + 2) as u32;
                let off32 = ((hi << 16) | lo) as i32;
                let size_cu = format_size_cu(fmt);
                let tgt_cur = add_off_i32(pc, off32, code.len());
                let tgt_nxt = add_off_from(pc + size_cu, off32, code.len());
                smali_trace!(
                    "[smali][collect][t] {} @pc {} off {} -> cur={:?} nxt={:?}",
                    opdef.name,
                    pc,
                    off32,
                    tgt_cur,
                    tgt_nxt
                );
                let Some(tgt) = (match (tgt_cur, tgt_nxt) {
                    (Some(t), None) | (Some(t), Some(_)) => Some(t),
                    (None, Some(t)) => {
                        smali_trace!(
                            "[smali][collect][t] NOTE: using next-pc base for {} at pc {}",
                            opdef.name,
                            pc
                        );
                        Some(t)
                    }
                    (None, None) => None,
                }) else {
                    smali_warn!(
                        "[smali][collect] WARN: 31t target {} for {} at pc {} is out of bounds (code len {}), skipping payload collection",
                        off32,
                        opdef.name,
                        pc,
                        code.len()
                    );
                    pc += size_cu;
                    continue;
                };
                if tgt % 2 != 0 {
                    smali_warn!(
                        "[smali][collect][t] WARN: payload target {} for {} not 32-bit aligned",
                        tgt,
                        opdef.name
                    );
                }
                if let Some(id) = code.get(tgt) {
                    smali_trace!(
                        "[smali][collect][t] 31t payload ident at {} = 0x{:04x}",
                        tgt,
                        id
                    );
                }

                // record the base pc of the switch instruction for this payload
                payload_base.insert(tgt, pc);

                // classify by opcode name
                let (prefix, kind) = if opdef.name == "fill-array-data" {
                    (":array_", PayloadKind::Array)
                } else if opdef.name == "packed-switch" {
                    (":pswitch_data_", PayloadKind::PackedSwitch)
                } else {
                    (":sswitch_data_", PayloadKind::SparseSwitch)
                };
                let label = labels.entry(tgt).or_insert_with(|| {
                    let idx = match kind {
                        PayloadKind::Array => {
                            let x = array_count;
                            array_count += 1;
                            x
                        }
                        PayloadKind::PackedSwitch => {
                            let x = psw_count;
                            psw_count += 1;
                            x
                        }
                        PayloadKind::SparseSwitch => {
                            let x = ssw_count;
                            ssw_count += 1;
                            x
                        }
                    };
                    format!("{}{}", prefix, idx)
                });
                payloads.entry(tgt).or_insert(kind);

                // Additionally, collect labels for switch case targets so we can emit them before the target instructions
                match kind {
                    PayloadKind::PackedSwitch => {
                        // header: ident(0x0100), size(u16), first_key(i32), then size * target(i32)
                        if tgt + 4 > code.len() {
                            smali_warn!(
                                "[smali][collect] WARN: truncated packed-switch payload at {} — header not complete",
                                tgt
                            );
                        } else {
                            let size = u16_at(code, tgt + 1) as usize;
                            let targets_start = tgt + 4;
                            let base_pc = pc; // targets are relative to the switch instruction, not the payload
                            for i in 0..size {
                                if targets_start + i * 2 + 1 >= code.len() {
                                    break;
                                }
                                let lo = u16_at(code, targets_start + i * 2) as u32;
                                let hi = u16_at(code, targets_start + i * 2 + 1) as u32;
                                let off = ((hi << 16) | lo) as i32;
                                if let Some(case_pc) = add_off_i32(base_pc, off, code.len()) {
                                    labels
                                        .entry(case_pc)
                                        .or_insert_with(|| format!(":pswitch_{:x}_{i}", case_pc));
                                } else {
                                    smali_warn!(
                                        "[smali][collect] WARN: packed-switch case OOB at payload {} (off {}), skipping case {}",
                                        tgt,
                                        off,
                                        i
                                    );
                                }
                            }
                        }
                    }
                    PayloadKind::SparseSwitch => {
                        // header: ident(0x0200), size(u16), then size * key(i32) and size * target(i32)
                        if tgt + 2 > code.len() {
                            smali_warn!(
                                "[smali][collect] WARN: truncated sparse-switch payload at {} — header not complete",
                                tgt
                            );
                        } else {
                            let size = u16_at(code, tgt + 1) as usize;
                            let keys_start = tgt + 2;
                            let targets_start = keys_start + 2 * size;
                            let base_pc = pc; // targets are relative to the switch instruction, not the payload
                            for i in 0..size {
                                if targets_start + i * 2 + 1 >= code.len() {
                                    break;
                                }
                                let lo = u16_at(code, targets_start + i * 2) as u32;
                                let hi = u16_at(code, targets_start + i * 2 + 1) as u32;
                                let off = ((hi << 16) | lo) as i32;
                                if let Some(case_pc) = add_off_i32(base_pc, off, code.len()) {
                                    labels
                                        .entry(case_pc)
                                        .or_insert_with(|| format!(":sswitch_{:x}_{i}", case_pc));
                                } else {
                                    smali_warn!(
                                        "[smali][collect] WARN: sparse-switch case OOB at payload {} (off {}), skipping case {}",
                                        tgt,
                                        off,
                                        i
                                    );
                                }
                            }
                        }
                    }
                    _ => {}
                }

                pc += format_size_cu(fmt);
            }
            _ => {
                pc += format_size_cu(fmt);
            }
        }
    }

    (labels, payloads, payload_base)
}

fn parse_array_payload(code: &[u16], pc: usize) -> Result<(ArrayDataDirective, usize), DexError> {
    if pc + 4 > code.len() {
        return Err(DexError::new("Truncated array-data header"));
    }
    let ident = u16_at(code, pc);
    // Expect 0x0300 (low byte 0x00, high byte 0x03)
    if ident != 0x0300 {
        // Be lenient; proceed anyway
    }
    let elem_width = u16_at(code, pc + 1) as usize;
    let size_lo = u16_at(code, pc + 2) as u32;
    let size_hi = u16_at(code, pc + 3) as u32;
    let count = ((size_hi << 16) | size_lo) as usize;

    let bytes_len = elem_width
        .checked_mul(count)
        .ok_or_else(|| DexError::new("array-data overflow"))?;
    let data_start_cu = pc + 4;
    let data_cu = bytes_len.div_ceil(2); // ceil to code units
    if data_start_cu + data_cu > code.len() {
        return Err(DexError::new("Truncated array-data elements"));
    }

    // Reconstruct bytes from code units (little endian)
    let mut bytes: Vec<u8> = Vec::with_capacity(bytes_len);
    for i in 0..data_cu {
        let cu = u16_at(code, data_start_cu + i);
        let lo = (cu & 0x00ff) as u8;
        let hi = (cu >> 8) as u8;
        bytes.push(lo);
        if bytes.len() < bytes_len {
            bytes.push(hi);
        }
    }

    let mut elements: Vec<ArrayDataElement> = Vec::with_capacity(count);
    match elem_width {
        1 => {
            for i in 0..count {
                elements.push(ArrayDataElement::Byte(bytes[i] as i8));
            }
        }
        2 => {
            for i in 0..count {
                let o = i * 2;
                let v = i16::from_le_bytes([bytes[o], bytes[o + 1]]);
                elements.push(ArrayDataElement::Short(v));
            }
        }
        4 => {
            for i in 0..count {
                let o = i * 4;
                let v = i32::from_le_bytes([bytes[o], bytes[o + 1], bytes[o + 2], bytes[o + 3]]);
                elements.push(ArrayDataElement::Int(v));
            }
        }
        8 => {
            for i in 0..count {
                let o = i * 8;
                let v = i64::from_le_bytes([
                    bytes[o],
                    bytes[o + 1],
                    bytes[o + 2],
                    bytes[o + 3],
                    bytes[o + 4],
                    bytes[o + 5],
                    bytes[o + 6],
                    bytes[o + 7],
                ]);
                elements.push(ArrayDataElement::Long(v));
            }
        }
        _ => {
            return Err(DexError::new("Unsupported array-data element width"));
        }
    }

    let consumed_cu = 4 + data_cu;
    Ok((
        ArrayDataDirective {
            element_width: elem_width as i32,
            elements,
        },
        consumed_cu,
    ))
}

fn parse_packed_switch_payload(
    code: &[u16],
    pc: usize,
    base_pc: usize,
    labels: &HashMap<usize, String>,
) -> Result<(PackedSwitchDirective, usize), DexError> {
    smali_trace!(
        "[smali][parse][switch] payload pc {} base pc {}",
        pc,
        base_pc
    );
    // Expect ident 0x0100, size (u16), first_key (i32), then size * target (i32 rel to payload)
    if pc + 4 > code.len() {
        return Err(DexError::new("Truncated packed-switch header"));
    }
    let ident = u16_at(code, pc);
    if ident != 0x0100 { /* be lenient */ }
    let size = u16_at(code, pc + 1) as usize;
    if pc + 4 + size * 2 > code.len() {
        return Err(DexError::new("Truncated packed-switch targets"));
    }
    let fk_lo = u16_at(code, pc + 2) as u32;
    let fk_hi = u16_at(code, pc + 3) as u32;
    let first_key = ((fk_hi << 16) | fk_lo) as i32;

    let mut targets: Vec<Label> = Vec::with_capacity(size);
    let targets_start = pc + 4;
    for i in 0..size {
        let lo = u16_at(code, targets_start + i * 2) as u32;
        let hi = u16_at(code, targets_start + i * 2 + 1) as u32;
        let off = ((hi << 16) | lo) as i32;
        let case_pc = (base_pc as i32 + off) as usize;
        let name = labels
            .get(&case_pc)
            .cloned()
            .unwrap_or_else(|| format!(":pswitch_{:x}_{i}", case_pc));
        targets.push(Label(canonical_label_name(&name)));
    }

    let consumed_cu = 4 + size * 2; // header + targets (each target is i32 = 2 code units)
    Ok((PackedSwitchDirective { first_key, targets }, consumed_cu))
}

fn parse_sparse_switch_payload(
    code: &[u16],
    pc: usize,
    base_pc: usize,
    labels: &HashMap<usize, String>,
) -> Result<(SparseSwitchDirective, usize), DexError> {
    smali_trace!(
        "[smali][parse][switch] payload pc {} base pc {}",
        pc,
        base_pc
    );
    // Expect ident 0x0200, size (u16), then size * key (i32) then size * target (i32)
    if pc + 2 > code.len() {
        return Err(DexError::new("Truncated sparse-switch header"));
    }
    let ident = u16_at(code, pc);
    if ident != 0x0200 { /* be lenient */ }
    let size = u16_at(code, pc + 1) as usize;
    let keys_start = pc + 2;
    let targets_start = keys_start + size * 2;
    if targets_start + size * 2 > code.len() {
        return Err(DexError::new("Truncated sparse-switch entries"));
    }

    let mut entries: Vec<SparseSwitchEntry> = Vec::with_capacity(size);
    // Read keys
    let mut keys: Vec<i32> = Vec::with_capacity(size);
    for i in 0..size {
        let lo = u16_at(code, keys_start + i * 2) as u32;
        let hi = u16_at(code, keys_start + i * 2 + 1) as u32;
        keys.push(((hi << 16) | lo) as i32);
    }
    // Read targets and pair with keys
    for i in 0..size {
        let lo = u16_at(code, targets_start + i * 2) as u32;
        let hi = u16_at(code, targets_start + i * 2 + 1) as u32;
        let off = ((hi << 16) | lo) as i32;
        let case_pc = (base_pc as i32 + off) as usize;
        let name = labels
            .get(&case_pc)
            .cloned()
            .unwrap_or_else(|| format!(":sswitch_{:x}_{i}", case_pc));
        entries.push(SparseSwitchEntry {
            key: keys[i],
            target: Label(canonical_label_name(&name)),
        });
    }

    let consumed_cu = 2 + size * 4; // header + keys (2*size) + targets (2*size)
    Ok((SparseSwitchDirective { entries }, consumed_cu))
}

fn canonical_label_name(raw: &str) -> String {
    raw.trim_start_matches(':').to_string()
}

// Helpers for quick invoke normalization and placeholder method refs
#[inline]
fn normalize_quick_invoke_name(name: &str) -> &str {
    match name {
        "invoke-virtual-quick" => "invoke-virtual",
        "invoke-virtual-quick/range" => "invoke-virtual/range",
        "invoke-super-quick" => "invoke-super",
        "invoke-super-quick/range" => "invoke-super/range",
        _ => name,
    }
}

#[inline]
fn quick_method_placeholder(opname: &str, idx: u32) -> String {
    let kind = if opname.contains("virtual-quick") {
        "vtable"
    } else if opname.contains("super-quick") {
        "super"
    } else {
        "quick"
    };
    // Return a syntactically valid smali method ref
    format!("Lquick;->{}@{}()V", kind, idx)
}

#[inline]
fn normalize_quick_field_name(name: &str) -> &str {
    if let Some(stripped) = name.strip_suffix("-quick") {
        stripped
    } else {
        name
    }
}

// Choose a placeholder field type descriptor for quick field ops
#[inline]
fn quick_field_desc_from_name(name: &str) -> &'static str {
    if name.contains("object") {
        "Ljava/lang/Object;"
    } else if name.contains("wide") {
        "J"
    } else if name.contains("boolean") {
        "Z"
    } else if name.contains("byte") {
        "B"
    } else if name.contains("char") {
        "C"
    } else if name.contains("short") {
        "S"
    } else {
        "I"
    }
}

// --- Smali literal formatting helpers ---
#[inline]
fn fmt_wide_lit64(lit: i64) -> String {
    // Emit signed hex with an explicit L suffix; handle i64::MIN specially.
    if lit == i64::MIN {
        return String::from("-0x8000000000000000L");
    }
    if lit < 0 {
        let mag = (-lit) as u64;
        format!("-0x{:x}L", mag)
    } else {
        format!("0x{:x}L", lit as u64)
    }
}

#[inline]
fn fmt_lit32_for(opname: &str, lit: i32) -> String {
    // For const-wide/32 the 32-bit literal is sign-extended to 64; print with L
    if opname.starts_with("const-wide/") {
        let sext = lit as i64;
        return fmt_wide_lit64(sext);
    }
    // Otherwise, keep standard decimal for readability
    format!("{}", lit)
}

// Global lazy cache for opcode maps keyed by (api, art_version)
static OPCODE_MAP_CACHE: Lazy<Mutex<HashMap<(i32, i32), Arc<HashMap<u16, &'static Opcode>>>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

static OPCODE_NAME_MAP: Lazy<HashMap<&'static str, &'static Opcode>> = Lazy::new(|| {
    let mut map = HashMap::new();
    for opcode in OPCODES.iter() {
        map.insert(opcode.name, opcode);
    }
    map
});

fn get_opcode_map(api: i32, art_version: i32) -> Arc<HashMap<u16, &'static Opcode>> {
    let key = (api, art_version);
    let mut guard = OPCODE_MAP_CACHE
        .lock()
        .expect("opcode map cache lock poisoned");
    if let Some(m) = guard.get(&key) {
        return Arc::clone(m);
    }
    let mut map: HashMap<u16, &'static Opcode> = HashMap::new();
    for o in OPCODES.iter() {
        if let Some(opcode) = o.get_opcode_value(api, art_version) {
            map.insert(opcode, o);
        }
    }
    let arc = Arc::new(map);
    guard.insert(key, Arc::clone(&arc));
    arc
}

fn opcode_by_name(name: &str) -> Option<&'static Opcode> {
    OPCODE_NAME_MAP.get(name).copied()
}

// Format the opcode args based on the defined opcode
fn format_instruction_line(
    op: &Opcode,
    code: &[u16],
    pc: usize,
    res: &impl RefResolver,
    regmap: Option<&RegMapper>,
    labels: Option<&HashMap<usize, String>>,
) -> (String, usize) {
    match op.format {
        // Size in code units = 1
        Format::Format10x => {
            // e.g. nop, return-void
            (op.name.to_string(), 1)
        }
        Format::Format11x => {
            // AA | op
            let inst = u16_at(code, pc);
            let va = a8(inst);
            (format!("{} {}", op.name, fmt_reg(regmap, va as u16)), 1)
        }
        Format::Format11n => {
            // B:A | op  (A = dest register in low nibble, B = signed 4-bit literal in high nibble)
            let inst = u16_at(code, pc);
            let va = a4(inst); // low nibble of high byte
            let lit = s4(b4(inst)); // high nibble of high byte (signed)
            (
                format!("{} {}, {}", op.name, fmt_reg(regmap, va as u16), lit),
                1,
            )
        }
        Format::Format12x => {
            // B:A | op  (B = high nibble, A = low nibble of high byte)
            let inst = u16_at(code, pc);
            let va = a4(inst);
            let vb = b4(inst);
            (
                format!(
                    "{} {}, {}",
                    op.name,
                    fmt_reg(regmap, va as u16),
                    fmt_reg(regmap, vb as u16)
                ),
                1,
            )
        }
        // Size = 2
        Format::Format21s => {
            // AA | op, lit:BBBB (signed)
            let inst = u16_at(code, pc);
            let lit = s16(u16_at(code, pc + 1));
            (
                format!("{} {}, {}", op.name, fmt_reg(regmap, a8(inst) as u16), lit),
                2,
            )
        }
        Format::Format21c => {
            // AA | op, kind:BBBB (index → string/type/field/method/proto/handle/callsite)
            let inst = u16_at(code, pc);
            let idx = u16_at(code, pc + 1) as u32;
            let aa = a8(inst);
            let arg = match op.reference_type {
                ReferenceType::String => res.string(idx),
                ReferenceType::Type => res.type_desc(idx),
                ReferenceType::Field => {
                    let (c, n, d) = res.field_ref(idx);
                    format!("{}->{}:{}", c, n, d)
                }
                ReferenceType::Method => {
                    let (c, n, p) = res.method_ref(idx);
                    format!("{}->{}{}", c, n, p)
                }
                ReferenceType::MethodProto => res.proto(idx),
                ReferenceType::MethodHandle => res.method_handle(idx),
                ReferenceType::CallSite => res.call_site(idx),
                ReferenceType::None => format!("ref@{}", idx),
            };
            (
                format!("{} {}, {}", op.name, fmt_reg(regmap, aa as u16), arg),
                2,
            )
        }
        Format::Format21t => {
            // AA | op, +BBBB (signed 16-bit code-unit offset)
            let inst = u16_at(code, pc);
            let off = s16(u16_at(code, pc + 1)) as i32;
            let size_cu = format_size_cu(Format::Format21t);
            let tgt_cur = add_off_i32(pc, off, code.len());
            let tgt_nxt = add_off_from(pc + size_cu, off, code.len());
            let label = if let Some(t) = tgt_cur {
                labels
                    .and_then(|m| m.get(&t))
                    .cloned()
                    .unwrap_or_else(|| off.to_string())
            } else if let Some(t) = tgt_nxt {
                smali_trace!(
                    "[smali][fmt][t] NOTE: using next-pc base for {} at pc {}",
                    op.name,
                    pc
                );
                labels
                    .and_then(|m| m.get(&t))
                    .cloned()
                    .unwrap_or_else(|| off.to_string())
            } else {
                off.to_string()
            };
            (
                format!(
                    "{} {}, {}",
                    op.name,
                    fmt_reg(regmap, a8(inst) as u16),
                    label
                ),
                2,
            )
        }
        Format::Format21ih => {
            // AA | op, BBBB  (signed high-16 for 32-bit literal)
            let inst = u16_at(code, pc);
            let hi = s16(u16_at(code, pc + 1)) as i32;
            let lit = hi << 16;
            (
                format!("{} {}, {}", op.name, fmt_reg(regmap, a8(inst) as u16), lit),
                2,
            )
        }
        Format::Format21lh => {
            // AA | op, BBBB  (signed high-16 for 64-bit literal)
            let inst = u16_at(code, pc);
            let hi = s16(u16_at(code, pc + 1)) as i64;
            let lit = hi << 48;
            (
                format!(
                    "{} {}, {}",
                    op.name,
                    fmt_reg(regmap, a8(inst) as u16),
                    fmt_wide_lit64(lit)
                ),
                2,
            )
        }
        Format::Format22t => {
            // B:A | op, +CCCC  (A,B are regs; CCCC is signed 16-bit offset)
            let inst = u16_at(code, pc);
            let va = a4(inst);
            let vb = b4(inst);
            let off = s16(u16_at(code, pc + 1)) as i32;
            let label = if let Some(tgt) = add_off_i32(pc, off, code.len()) {
                labels
                    .and_then(|m| m.get(&tgt))
                    .cloned()
                    .unwrap_or_else(|| off.to_string())
            } else {
                off.to_string()
            };
            (
                format!(
                    "{} {}, {}, {}",
                    op.name,
                    fmt_reg(regmap, va as u16),
                    fmt_reg(regmap, vb as u16),
                    label
                ),
                2,
            )
        }
        Format::Format22x => {
            // AA | op, BBBB  (two registers: vAA, vBBBB)
            let inst0 = u16_at(code, pc);
            let a = a8(inst0) as u16;
            let b = u16_at(code, pc + 1);
            (
                format!("{} {}, {}", op.name, fmt_reg(regmap, a), fmt_reg(regmap, b)),
                2,
            )
        }
        Format::Format22b => {
            // AA | op, BB|CC  (dest=vAA, src=vBB, literal=CC signed 8-bit)
            let inst0 = u16_at(code, pc);
            let inst1 = u16_at(code, pc + 1);
            let a = a8(inst0) as u16;
            let b = inst1 & 0x00ff;
            let lit = s8((inst1 >> 8) as u8);
            (
                format!(
                    "{} {}, {}, {}",
                    op.name,
                    fmt_reg(regmap, a),
                    fmt_reg(regmap, b),
                    lit
                ),
                2,
            )
        }
        Format::Format22s => {
            // B:A | op, +CCCC  (A=dest, B=src, CCCC=signed 16-bit literal)
            let inst = u16_at(code, pc);
            let va = a4(inst);
            let vb = b4(inst);
            let lit = s16(u16_at(code, pc + 1));
            (
                format!(
                    "{} {}, {}, {}",
                    op.name,
                    fmt_reg(regmap, va as u16),
                    fmt_reg(regmap, vb as u16),
                    lit
                ),
                2,
            )
        }
        Format::Format23x => {
            // AA | op, C:B  (3 regs in 2 code units; BB=low byte of 2nd CU, CC=high byte)
            let inst0 = u16_at(code, pc);
            let inst1 = u16_at(code, pc + 1);
            let a = a8(inst0) as u16;
            let b = inst1 & 0x00ff; // low byte
            let c = inst1 >> 8; // high byte
            (
                format!(
                    "{} {}, {}, {}",
                    op.name,
                    fmt_reg(regmap, a),
                    fmt_reg(regmap, b),
                    fmt_reg(regmap, c)
                ),
                2,
            )
        }
        Format::Format10t => {
            let inst = u16_at(code, pc);
            let off = s8(a8(inst)) as i32;
            let label = if let Some(tgt) = add_off_i32(pc, off, code.len()) {
                labels
                    .and_then(|m| m.get(&tgt))
                    .cloned()
                    .unwrap_or_else(|| off.to_string())
            } else {
                off.to_string()
            };
            (format!("{} {}", op.name, label), 1)
        }
        Format::Format20t => {
            let off = s16(u16_at(code, pc + 1)) as i32;
            let label = if let Some(tgt) = add_off_i32(pc, off, code.len()) {
                labels
                    .and_then(|m| m.get(&tgt))
                    .cloned()
                    .unwrap_or_else(|| off.to_string())
            } else {
                off.to_string()
            };
            (format!("{} {}", op.name, label), 2)
        }
        Format::Format30t => {
            let lo = u16_at(code, pc + 1) as u32;
            let hi = u16_at(code, pc + 2) as u32;
            let off32 = ((hi << 16) | lo) as i32;
            let label = if let Some(tgt) = add_off_i32(pc, off32, code.len()) {
                labels
                    .and_then(|m| m.get(&tgt))
                    .cloned()
                    .unwrap_or_else(|| off32.to_string())
            } else {
                off32.to_string()
            };
            (format!("{} {}", op.name, label), 3)
        }
        Format::Format22c => {
            // B:A | op, CCCC  (A and B are 4-bit regs; CCCC is a field/type index)
            // For iput/iget family: iput-object vA, vB, Lcls;->name:Type
            let inst0 = u16_at(code, pc);
            let a = a4(inst0);
            let b = b4(inst0);
            let idx = u16_at(code, pc + 1) as u32;
            let target = match op.reference_type {
                ReferenceType::Field => {
                    let (cl, nm, ds) = res.field_ref(idx);
                    format!("{}->{}:{}", cl, nm, ds)
                }
                ReferenceType::Type => res.type_desc(idx),
                ReferenceType::Method => {
                    let (cl, nm, pr) = res.method_ref(idx);
                    format!("{}->{}{}", cl, nm, pr)
                }
                _ => format!("ref@{}", idx),
            };
            (
                format!(
                    "{} {}, {}, {}",
                    op.name,
                    fmt_reg(regmap, a as u16),
                    fmt_reg(regmap, b as u16),
                    target
                ),
                2,
            )
        }
        Format::Format22cs => {
            // B:A | op, CCCC (quick field accessor: instance get/put)
            let inst0 = u16_at(code, pc);
            let a = a4(inst0) as u16; // dest (iget*) or src (iput*)
            let b = b4(inst0) as u16; // object register
            let idx = u16_at(code, pc + 1) as u32; // quick field index/offset
            let desc = quick_field_desc_from_name(op.name);
            let field = format!("Lquick;->field@{}:{}", idx, desc);
            let opname = normalize_quick_field_name(op.name);
            (
                format!(
                    "{} {}, {}, {}",
                    opname,
                    fmt_reg(regmap, a),
                    fmt_reg(regmap, b),
                    field
                ),
                2,
            )
        }
        // Size = 3
        Format::Format31i => {
            // AA | op, lit32:BBBBBBBB (signed 32-bit literal across the next 2 code units)
            let inst0 = u16_at(code, pc);
            let lo = u16_at(code, pc + 1) as u32;
            let hi = u16_at(code, pc + 2) as u32;
            let lit = ((hi << 16) | lo) as i32;
            let lit_txt = fmt_lit32_for(op.name, lit);
            (
                format!(
                    "{} {}, {}",
                    op.name,
                    fmt_reg(regmap, a8(inst0) as u16),
                    lit_txt
                ),
                3,
            )
        }
        Format::Format31c => {
            // AA | op, kind:BBBBBBBB (32-bit index)
            let inst0 = u16_at(code, pc);
            let lo = u16_at(code, pc + 1) as u32;
            let hi = u16_at(code, pc + 2) as u32;
            let idx = (hi << 16) | lo;
            let aa = a8(inst0) as u16;
            let arg = match op.reference_type {
                ReferenceType::String => res.string(idx),
                ReferenceType::Type => res.type_desc(idx),
                ReferenceType::Field => {
                    let (c, n, d) = res.field_ref(idx);
                    format!("{}->{}:{}", c, n, d)
                }
                ReferenceType::Method => {
                    let (c, n, p) = res.method_ref(idx);
                    format!("{}->{}{}", c, n, p)
                }
                ReferenceType::MethodProto => res.proto(idx),
                ReferenceType::MethodHandle => res.method_handle(idx),
                ReferenceType::CallSite => res.call_site(idx),
                ReferenceType::None => format!("ref@{}", idx),
            };
            (format!("{} {}, {}", op.name, fmt_reg(regmap, aa), arg), 3)
        }
        Format::Format45cc => {
            // G|A | op, BBBB, F|E|D|C, HHHH  (A args, C..G regs, BBBB=method/callsite, HHHH=proto)
            let inst0 = u16_at(code, pc);
            let inst1 = u16_at(code, pc + 1);
            let inst2 = u16_at(code, pc + 2);
            let inst3 = u16_at(code, pc + 3);
            let a = ((inst0 >> 12) & 0x0f) as u8;
            let g = ((inst0 >> 8) & 0x0f) as u8;
            let c = (inst2 & 0x000f) as u8;
            let d = ((inst2 >> 4) & 0x0f) as u8;
            let e = ((inst2 >> 8) & 0x0f) as u8;
            let f = ((inst2 >> 12) & 0x0f) as u8;
            let idx = inst1 as u32; // method or call site
            let proto_idx = inst3 as u32; // proto or method handle depending on op
            let mut regs = Vec::new();
            for r in [c, d, e, f, g].into_iter().take(a as usize) {
                regs.push(fmt_reg(regmap, r as u16));
            }
            let (target, extra) = match op.reference_type {
                ReferenceType::Method => {
                    let (c, n, p) = res.method_ref(idx);
                    let proto = res.proto(proto_idx);
                    (format!("{}->{}{}", c, n, p), proto)
                }
                ReferenceType::CallSite => {
                    let cs = res.call_site(idx);
                    let proto = res.proto(proto_idx);
                    (cs, proto)
                }
                _ => (format!("ref@{}", idx), format!("proto@{}", proto_idx)),
            };
            (
                format!("{} {{{}}}, {}, {}", op.name, regs.join(", "), target, extra),
                4,
            )
        }
        Format::Format4rcc => {
            // AA | op, BBBB, CCCC, HHHH  (range invoke with 2 refs)
            let inst0 = u16_at(code, pc);
            let idx = u16_at(code, pc + 1) as u32; // method/callsite
            let first = u16_at(code, pc + 2);
            let proto_idx = u16_at(code, pc + 3) as u32; // proto/methodhandle
            let count = a8(inst0) as u16;
            let range = if count == 0 {
                String::from("{}")
            } else {
                format!(
                    "{{{} .. {}}}",
                    fmt_reg(regmap, first),
                    fmt_reg(regmap, first + count - 1)
                )
            };
            let (target, extra) = match op.reference_type {
                ReferenceType::Method => {
                    let (c, n, p) = res.method_ref(idx);
                    let proto = res.proto(proto_idx);
                    (format!("{}->{}{}", c, n, p), proto)
                }
                ReferenceType::CallSite => {
                    let cs = res.call_site(idx);
                    let proto = res.proto(proto_idx);
                    (cs, proto)
                }
                _ => (format!("ref@{}", idx), format!("proto@{}", proto_idx)),
            };
            (format!("{} {}, {}, {}", op.name, range, target, extra), 4)
        }
        Format::Format31t => {
            // AA | op, +BBBBBBBB (signed 32-bit code-unit offset)
            let inst0 = u16_at(code, pc);
            let lo = u16_at(code, pc + 1) as u32;
            let hi = u16_at(code, pc + 2) as u32;
            let off32 = ((hi << 16) | lo) as i32;
            let label = if let Some(tgt) = add_off_i32(pc, off32, code.len()) {
                labels
                    .and_then(|m| m.get(&tgt))
                    .cloned()
                    .unwrap_or_else(|| off32.to_string())
            } else {
                off32.to_string()
            };
            (
                format!(
                    "{} {}, {}",
                    op.name,
                    fmt_reg(regmap, a8(inst0) as u16),
                    label
                ),
                3,
            )
        }
        Format::Format32x => {
            // op | 00, AAAA, BBBB  (two 16-bit registers: dest=AAAA, src=BBBB)
            let a = u16_at(code, pc + 1);
            let b = u16_at(code, pc + 2);
            (
                format!("{} {}, {}", op.name, fmt_reg(regmap, a), fmt_reg(regmap, b)),
                3,
            )
        }
        Format::Format51l => {
            // AA | op, lit64:BBBBBBBBBBBBBBBB (signed 64-bit across next 4 code units)
            let inst0 = u16_at(code, pc);
            let b1 = u16_at(code, pc + 1) as u64;
            let b2 = u16_at(code, pc + 2) as u64;
            let b3 = u16_at(code, pc + 3) as u64;
            let b4 = u16_at(code, pc + 4) as u64;
            let lit_u = (b4 << 48) | (b3 << 32) | (b2 << 16) | b1;
            let lit = lit_u as i64; // interpret as signed
            (
                format!(
                    "{} {}, {}",
                    op.name,
                    fmt_reg(regmap, a8(inst0) as u16),
                    fmt_wide_lit64(lit)
                ),
                5,
            )
        }
        Format::Format35c => {
            // G|A | op, BBBB, F|E|D|C  (A=arg count, C..G=regs, kind BBBB)
            let inst0 = u16_at(code, pc);
            let inst1 = u16_at(code, pc + 1);
            let inst2 = u16_at(code, pc + 2);
            let a = ((inst0 >> 12) & 0x0f) as u8; // arg count
            let g = ((inst0 >> 8) & 0x0f) as u8;
            let c = (inst2 & 0x000f) as u8;
            let d = ((inst2 >> 4) & 0x0f) as u8;
            let e = ((inst2 >> 8) & 0x0f) as u8;
            let f = ((inst2 >> 12) & 0x0f) as u8;
            let idx = inst1 as u32;
            let target = match op.reference_type {
                ReferenceType::Method => {
                    let (cl, nm, pr) = res.method_ref(idx);
                    format!("{}->{}{}", cl, nm, pr)
                }
                ReferenceType::Type => res.type_desc(idx),
                ReferenceType::Field => {
                    let (cl, nm, ds) = res.field_ref(idx);
                    format!("{}->{}:{}", cl, nm, ds)
                }
                _ => format!("ref@{}", idx),
            };
            let mut regs = Vec::new();
            for (_, r) in [c, d, e, f, g].into_iter().take(a as usize).enumerate() {
                regs.push(fmt_reg(regmap, r as u16));
            }
            (
                format!("{} {{{}}}, {}", op.name, regs.join(", "), target),
                3,
            )
        }
        Format::Format35mi => {
            // G|A | op, BBBB, F|E|D|C  (A=arg count, regs, BBBB inline index)
            let inst0 = u16_at(code, pc);
            let inst1 = u16_at(code, pc + 1);
            let inst2 = u16_at(code, pc + 2);
            let a = ((inst0 >> 12) & 0x0f) as u8; // arg count
            let g = ((inst0 >> 8) & 0x0f) as u8;
            let c = (inst2 & 0x000f) as u8;
            let d = ((inst2 >> 4) & 0x0f) as u8;
            let e = ((inst2 >> 8) & 0x0f) as u8;
            let f = ((inst2 >> 12) & 0x0f) as u8;
            let idx = inst1 as u16;
            let mut regs = Vec::new();
            for r in [c, d, e, f, g].into_iter().take(a as usize) {
                regs.push(fmt_reg(regmap, r as u16));
            }
            (
                format!("{} {{{}}}, inline@{}", op.name, regs.join(", "), idx),
                3,
            )
        }
        Format::Format35ms => {
            // G|A | op, BBBB, F|E|D|C  (A=arg count, C..G=regs, BBBB is vtable index / quick index)
            let inst0 = u16_at(code, pc);
            let inst1 = u16_at(code, pc + 1);
            let inst2 = u16_at(code, pc + 2);
            let a = ((inst0 >> 12) & 0x0f) as u8; // arg count
            let g = ((inst0 >> 8) & 0x0f) as u8;
            let c = (inst2 & 0x000f) as u8;
            let d = ((inst2 >> 4) & 0x0f) as u8;
            let e = ((inst2 >> 8) & 0x0f) as u8;
            let f = ((inst2 >> 12) & 0x0f) as u8;
            let idx = inst1 as u32; // quick/vtable index
            let mut regs = Vec::new();
            for r in [c, d, e, f, g].into_iter().take(a as usize) {
                regs.push(fmt_reg(regmap, r as u16));
            }
            let target = quick_method_placeholder(op.name, idx);
            let opname = normalize_quick_invoke_name(op.name);
            (format!("{} {{{}}}, {}", opname, regs.join(", "), target), 3)
        }
        Format::Format3rc => {
            // AA | op, BBBB, CCCC (range: first=C, count=AA), kind BBBB
            let inst0 = u16_at(code, pc);
            let idx = u16_at(code, pc + 1) as u32;
            let first = u16_at(code, pc + 2);
            let count = a8(inst0) as u16;
            let range = if count == 0 {
                String::from("{}")
            } else {
                format!(
                    "{{{} .. {}}}",
                    fmt_reg(regmap, first),
                    fmt_reg(regmap, first + count - 1)
                )
            };
            let target = match op.reference_type {
                ReferenceType::Method => {
                    let (cl, nm, pr) = res.method_ref(idx);
                    format!("{}->{}{}", cl, nm, pr)
                }
                ReferenceType::Type => res.type_desc(idx),
                ReferenceType::Field => {
                    let (cl, nm, ds) = res.field_ref(idx);
                    format!("{}->{}:{}", cl, nm, ds)
                }
                _ => format!("ref@{}", idx),
            };
            (format!("{} {}, {}", op.name, range, target), 3)
        }
        Format::Format3rmi => {
            // AA | op, BBBB, CCCC  (range: first=C, count=AA, BBBB = inline index)
            let inst0 = u16_at(code, pc);
            let idx = u16_at(code, pc + 1);
            let first = u16_at(code, pc + 2);
            let count = a8(inst0) as u16;
            let range = if count == 0 {
                String::from("{}")
            } else {
                format!(
                    "{{{} .. {}}}",
                    fmt_reg(regmap, first),
                    fmt_reg(regmap, first + count - 1)
                )
            };
            (format!("{} {}, inline@{}", op.name, range, idx), 3)
        }
        Format::Format3rms => {
            // AA | op, BBBB, CCCC  (range: first=C, count=AA, BBBB = vtable/quick index)
            let inst0 = u16_at(code, pc);
            let idx = u16_at(code, pc + 1) as u32;
            let first = u16_at(code, pc + 2);
            let count = a8(inst0) as u16;
            let range = if count == 0 {
                String::from("{}")
            } else {
                format!(
                    "{{{} .. {}}}",
                    fmt_reg(regmap, first),
                    fmt_reg(regmap, first + count - 1)
                )
            };
            let target = quick_method_placeholder(op.name, idx);
            let opname = normalize_quick_invoke_name(op.name);
            (format!("{} {}, {}", opname, range, target), 3)
        }
        // Size = 2/3/… more formats as you implement:
        // Format::Format22x, Format::Format22t, Format::Format31i, Format::Format31t, Format::Format51l, …
        fmt => {
            {
                // Unknown/unimplemented format — return empty so caller can abort safely
                eprintln!(
                    "[smali][decode] ERROR: unimplemented format {:?} for {} at pc {}",
                    fmt, op.name, pc
                );
                (String::new(), 0)
            }
        }
    }
}

pub fn decode_with_ctx(
    bytecode: &[u8],
    api: i32,
    art_version: i32,
    res: &impl RefResolver,
    regmap: Option<&RegMapper>,
) -> Result<Vec<SmaliOp>, DexError> {
    // Fetch (or build once) the opcode map for this API/ART
    let opcode_cache = get_opcode_map(api, art_version);

    if (bytecode.len() & 1) != 0 {
        return Err(DexError::new("Odd-length method bytecode"));
    }
    let code: Vec<u16> = bytecode
        .chunks_exact(2)
        .map(|b| u16::from_le_bytes([b[0], b[1]]))
        .collect();

    // First pass: collect labels and payload kinds
    let (mut labels, payloads, payload_base) = collect_labels_and_payloads(&code, &opcode_cache);

    let mut output: Vec<SmaliOp> = Vec::new();
    let mut pc: usize = 0;
    while pc < code.len() {
        if let Some(kind) = payloads.get(&pc).copied() {
            // Emit label first (if present)
            if let Some(lbl) = labels.get(&pc) {
                let mut labelline = String::new();
                labelline.push_str(lbl);
                labelline.push('\n');
                let parsed_label = parse_op(&labelline);
                match parsed_label {
                    Ok((_, op)) => output.push(op),
                    Err(e) => fail!("Error parsing label: {}, {}", labelline, e),
                }
            }
            // Switch-case targets are relative to the switch instruction, not the payload.
            let base_pc = payload_base.get(&pc).copied().unwrap_or(pc);
            match kind {
                PayloadKind::Array => {
                    let (dir, consumed) = parse_array_payload(&code, pc)?;
                    output.push(SmaliOp::ArrayData(dir));
                    pc += consumed;
                    continue;
                }
                PayloadKind::PackedSwitch => {
                    let (dir, consumed) = parse_packed_switch_payload(&code, pc, base_pc, &labels)?;
                    output.push(SmaliOp::PackedSwitch(dir));
                    pc += consumed;
                    continue;
                }
                PayloadKind::SparseSwitch => {
                    let (dir, consumed) = parse_sparse_switch_payload(&code, pc, base_pc, &labels)?;
                    output.push(SmaliOp::SparseSwitch(dir));
                    pc += consumed;
                    continue;
                }
            }
        }
        let inst = u16_at(&code, pc);
        let opc = op(inst) as u16;
        let Some(opdef) = opcode_cache.get(&opc) else {
            // Dump a small window of code units to help diagnose misalignment
            let start = pc.saturating_sub(4);
            let end = (pc + 8).min(code.len());
            let mut hex = String::new();
            for i in start..end {
                hex.push_str(&format!("{:04x} ", code[i]));
            }
            // Heuristic guesses: maybe we started one/two code units late
            let mut guesses = String::new();
            for back in 1..=2 {
                if pc >= back {
                    let gpc = pc - back;
                    let ginst = u16_at(&code, gpc);
                    let gopc = op(ginst) as u16;
                    if let Some(gop) = opcode_cache.get(&gopc) {
                        let sz = format_size_cu(gop.format);
                        guesses.push_str(&format!(
                            "guess pc-{}: {} (0x{:02x}) fmt {:?} sizeCU {} words:",
                            back, gop.name, gopc, gop.format, sz
                        ));
                        let gend = (gpc + sz).min(code.len());
                        for j in gpc..gend {
                            guesses.push_str(&format!(" {:04x}", code[j]));
                        }
                        guesses.push_str(" | ");
                    } else {
                        guesses.push_str(&format!(
                            "guess pc-{}: opcode 0x{:02x} unknown | ",
                            back, gopc
                        ));
                    }
                }
            }
            let mut err = DexError::new(&format!("Unknown opcode 0x{:02x} at pc {}", opc, pc));
            err = DexError::with_context(err, format!("bytes[{}..{}] = {}", start, end, hex));
            if !guesses.is_empty() {
                err = DexError::with_context(err, guesses);
            }
            return Err(err);
        };

        // Unsupported odex-only format: abort decoding rather than risking misalignment
        if matches!(opdef.format, Format::Format20bc) {
            eprintln!(
                "[smali][decode] INFO: encountered unsupported {} (Format20bc) at pc {} — aborting decode for this method",
                opdef.name, pc
            );
            break;
        }

        // Ensure we have enough code units for this instruction before any further reads
        let mut need_cu = format_size_cu(opdef.format);
        if need_cu <= 0 {
            need_cu = 1;
        }
        // Quick-invoke fallback may read 3 code units even if format metadata disagrees
        let opname_for_need = opdef.name;
        if need_cu < 3
            && (opname_for_need.ends_with("-quick") || opname_for_need.ends_with("-quick/range"))
        {
            need_cu = 3;
        }
        if let Err(_e) = require_cu(&code, pc, need_cu, opname_for_need) {
            // Soft-terminate method decoding on a truncated trailing instruction.
            // Log so we don't lose track of this; likely an upstream sizing issue.
            let have = code.len().saturating_sub(pc);
            eprintln!(
                "[smali][decode] WARN: truncated {} at pc {}: need {} code units, have {} — soft-terminating",
                opname_for_need, pc, need_cu, have
            );
            break;
        }

        // Pre-decode register sanity check to detect mid-instruction starts (log and abort)
        if let Some(m) = regmap {
            match opdef.format {
                Format::Format22x => {
                    // AA | op, BBBB
                    let a = a8(inst) as u16;
                    let b = u16_at(&code, pc + 1);
                    if !reg_valid(Some(m), a) || !reg_valid(Some(m), b) {
                        eprintln!(
                            "[smali][decode] ERROR: unlikely 22x regs at pc {} (A={}, B={}) vs registers_size={} — aborting decode",
                            pc, a, b, m.registers_size
                        );
                        break;
                    }
                }
                Format::Format23x => {
                    // AA | op, C:B
                    let a = a8(inst) as u16;
                    let w1 = if pc + 1 < code.len() {
                        u16_at(&code, pc + 1)
                    } else {
                        0
                    };
                    let b = w1 & 0x00ff;
                    let c = w1 >> 8;
                    if !reg_valid(Some(m), a) || !reg_valid(Some(m), b) || !reg_valid(Some(m), c) {
                        let w0 = u16_at(&code, pc);
                        eprintln!(
                            "[smali][decode] ERROR: unlikely 23x regs at pc {} for {} (fmt {:?}) \
             (A={}, B={}, C={}) vs registers_size={} — words: {:04x} {:04x} — aborting decode",
                            pc, opdef.name, opdef.format, a, b, c, m.registers_size, w0, w1
                        );
                        break;
                    }
                }
                Format::Format32x => {
                    // op, AAAA, BBBB
                    let a = u16_at(&code, pc + 1);
                    let b = u16_at(&code, pc + 2);
                    if !reg_valid(Some(m), a) || !reg_valid(Some(m), b) {
                        eprintln!(
                            "[smali][decode] ERROR: unlikely 32x regs at pc {} (A={}, B={}) vs registers_size={} — aborting decode",
                            pc, a, b, m.registers_size
                        );
                        break;
                    }
                }
                _ => {}
            }
        }

        // Backfill a missing label on-the-fly if this is a branch and the first pass somehow missed it.
        match opdef.format {
            Format::Format21t => {
                let off = s16(u16_at(&code, pc + 1)) as i32;
                if let Some(tgt) = add_off_i32(pc, off, code.len()) {
                    labels
                        .entry(tgt)
                        .or_insert_with(|| format!(":addr_{:x}", tgt));
                }
            }
            Format::Format22t => {
                let off = s16(u16_at(&code, pc + 1)) as i32;
                if let Some(tgt) = add_off_i32(pc, off, code.len()) {
                    labels
                        .entry(tgt)
                        .or_insert_with(|| format!(":addr_{:x}", tgt));
                }
            }
            Format::Format10t => {
                let off = s8(a8(inst)) as i32;
                if let Some(tgt) = add_off_i32(pc, off, code.len()) {
                    labels
                        .entry(tgt)
                        .or_insert_with(|| format!(":addr_{:x}", tgt));
                }
            }
            Format::Format20t => {
                let off = s16(u16_at(&code, pc + 1)) as i32;
                if let Some(tgt) = add_off_i32(pc, off, code.len()) {
                    labels
                        .entry(tgt)
                        .or_insert_with(|| format!(":addr_{:x}", tgt));
                }
            }
            Format::Format30t => {
                let lo = u16_at(&code, pc + 1) as u32;
                let hi = u16_at(&code, pc + 2) as u32;
                let off32 = ((hi << 16) | lo) as i32;
                if let Some(tgt) = add_off_i32(pc, off32, code.len()) {
                    labels
                        .entry(tgt)
                        .or_insert_with(|| format!(":addr_{:x}", tgt));
                }
            }
            _ => {}
        }

        if let Some(lbl) = labels.get(&pc) {
            let mut labelline = String::new();
            labelline.push_str(lbl);
            labelline.push('\n');
            let parsed_label = parse_op(&labelline);
            match parsed_label {
                Ok((_, op)) => output.push(op),
                Err(e) => fail!("Error parsing label: {}, {}", labelline, e),
            }
        }

        let (mut line, size_cu) =
            format_instruction_line(opdef, &code, pc, res, regmap, Some(&labels));
        if size_cu == 0 || line.is_empty() {
            eprintln!(
                "[smali][decode] ERROR: aborting at pc {} due to unimplemented/missing formatter for {}",
                pc, opdef.name
            );
            break;
        }
        line.push('\n');
        let parsed = parse_op(&line);
        match parsed {
            Ok((_, op)) => output.push(op),
            Err(e) => fail!("Error parsing opcode: {}, {}", line, e),
        }
        pc += size_cu.max(1);
    }

    Ok(output)
}

pub fn decode_with_resolver(
    bytecode: &[u8],
    api: i32,
    art_version: i32,
    res: &impl RefResolver,
) -> Result<Vec<SmaliOp>, DexError> {
    decode_with_ctx(bytecode, api, art_version, res, None)
}

pub fn decode(bytecode: &[u8], api: i32, art_version: i32) -> Result<Vec<SmaliOp>, DexError> {
    let resolver = PlaceholderResolver;
    decode_with_resolver(bytecode, api, art_version, &resolver)
}
