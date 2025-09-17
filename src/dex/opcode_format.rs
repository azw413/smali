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
        if raw >= params_base { format!("p{}", raw - params_base) } else { format!("v{}", raw) }
    }
}

#[inline]
fn fmt_reg(mapper: Option<&RegMapper>, raw: u16) -> String {
    if let Some(m) = mapper { m.map_name(raw) } else { format!("v{}", raw) }
}
use std::collections::HashMap;
use bitflags::bitflags;
use rangemap::{RangeInclusiveMap};
use std::ops::{ RangeInclusive };
use once_cell::sync::Lazy;
use crate::dex::error::DexError;
use crate::dex::opcodes::OPCODES;
///
/// Resolves DEX pool references (string/type/field/method/...) into printable Smali text.
/// The decoder works with any resolver; callers can pass a real Dex-backed resolver later.
pub trait RefResolver {
    fn string(&self, idx: u32) -> String;
    fn type_desc(&self, idx: u32) -> String;
    fn field_ref(&self, idx: u32) -> (String, String, String);  // (class, name, desc)
    fn method_ref(&self, idx: u32) -> (String, String, String); // (class, name, proto)
    fn call_site(&self, idx: u32) -> String;
    fn method_handle(&self, idx: u32) -> String;
    fn proto(&self, idx: u32) -> String;
}

/// Minimal placeholder resolver that prints stable placeholders like string@42.
/// This keeps the decoder usable without a DexFile context.
pub struct PlaceholderResolver;

impl RefResolver for PlaceholderResolver {
    fn string(&self, idx: u32) -> String { format!("\"string@{}\"", idx) }
    fn type_desc(&self, idx: u32) -> String { format!("Ltype@{};", idx) }
    fn field_ref(&self, idx: u32) -> (String, String, String) {
        (format!("Lclass@{};", idx), format!("field@{}", idx), String::from("Ljava/lang/Object;"))
    }
    fn method_ref(&self, idx: u32) -> (String, String, String) {
        (format!("Lclass@{};", idx), format!("method@{}", idx), String::from("()V"))
    }
    fn call_site(&self, idx: u32) -> String { format!("callsite@{}", idx) }
    fn method_handle(&self, idx: u32) -> String { format!("handle@{}", idx) }
    fn proto(&self, _idx: u32) -> String { String::from("()V") }
}
use crate::smali_parse::parse_op;
use crate::types::{SmaliOp, ArrayDataDirective, ArrayDataElement, PackedSwitchDirective, SparseSwitchDirective, SparseSwitchEntry, Label};

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
        format: Format
    ) -> Self
    {
        Opcode::new(
            version_constraints,
            name,
            reference_type,
            None,
            format,
            OpcodeFlags::empty()
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
    pub(crate) fn between_api(opcode_value: u16, min_api: i32, max_api: i32) -> Vec<VersionConstraint> {
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
    pub(crate) fn combine(constraints: Vec<VersionConstraint>, other: Vec<VersionConstraint>) -> Vec<VersionConstraint> {
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
#[inline] fn u16_at(code: &[u16], pc: usize) -> u16 { code[pc] }

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
#[inline] fn op(inst: u16) -> u8 { (inst & 0x00ff) as u8 }
#[inline] fn a8(inst: u16) -> u8 { (inst >> 8) as u8 }          // 11x AA, 21x AA, …
#[inline] fn a4(inst: u16) -> u8 { ((inst >> 8) & 0x0f) as u8 } // 12x A (low nibble of high byte)
#[inline] fn b4(inst: u16) -> u8 { ((inst >> 12) & 0x0f) as u8 } // 12x B (high nibble of high byte)
#[inline] fn s16(x: u16) -> i16 { x as i16 }
#[inline] fn s8(x: u8) -> i8 { x as i8 }
#[inline] fn s4(x: u8) -> i8 { ((x as i8) << 4) >> 4 }

#[inline]
fn format_size_cu(fmt: Format) -> usize {
    match fmt.size() {
        n if n > 0 => (n as usize) / 2,
        _ => 1, // payloads handled specially later; avoid stall
    }
}

#[inline]
fn reg_valid(mapper: Option<&RegMapper>, r: u16) -> bool {
    if let Some(m) = mapper { r < m.registers_size } else { true }
}

use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PayloadKind { Array, PackedSwitch, SparseSwitch }

fn collect_labels_and_payloads(
    code: &[u16],
    opcode_cache: &HashMap<u16, &Opcode>,
) -> (HashMap<usize, String>, HashMap<usize, PayloadKind>) {
    let mut labels: HashMap<usize, String> = HashMap::new();
    let mut payloads: HashMap<usize, PayloadKind> = HashMap::new();
    let mut cond_count = 0usize;
    let mut goto_count = 0usize;
    let mut array_count = 0usize;
    let mut psw_count = 0usize;
    let mut ssw_count = 0usize;

    let mut pc: usize = 0;
    while pc < code.len() {
        let inst = u16_at(code, pc);
        let opc = op(inst) as u16;
        let Some(opdef) = opcode_cache.get(&opc) else {
            pc += 1; continue;
        };
        let fmt = opdef.format;
        match fmt {
            // conditional branches (16-bit offset)
            Format::Format21t | Format::Format22t => {
                let off = s16(u16_at(code, pc+1)) as i32;
                let tgt = (pc as i32 + off) as usize;
                labels.entry(tgt).or_insert_with(|| { let s = format!(":cond_{}", cond_count); cond_count+=1; s });
                pc += format_size_cu(fmt);
            }
            // gotos
            Format::Format10t => {
                let off = s8(a8(inst)) as i32;
                let tgt = (pc as i32 + off) as usize;
                labels.entry(tgt).or_insert_with(|| { let s = format!(":goto_{}", goto_count); goto_count+=1; s });
                pc += format_size_cu(fmt);
            }
            Format::Format20t => {
                let off = s16(u16_at(code, pc+1)) as i32;
                let tgt = (pc as i32 + off) as usize;
                labels.entry(tgt).or_insert_with(|| { let s = format!(":goto_{}", goto_count); goto_count+=1; s });
                pc += format_size_cu(fmt);
            }
            Format::Format30t => {
                let lo = u16_at(code, pc+1) as u32;
                let hi = u16_at(code, pc+2) as u32;
                let off32 = ((hi << 16) | lo) as i32;
                let tgt = (pc as i32 + off32) as usize;
                labels.entry(tgt).or_insert_with(|| { let s = format!(":goto_{}", goto_count); goto_count+=1; s });
                pc += format_size_cu(fmt);
            }
            // payload targets via 31t
            Format::Format31t => {
                let lo = u16_at(code, pc+1) as u32;
                let hi = u16_at(code, pc+2) as u32;
                let off32 = ((hi << 16) | lo) as i32;
                let tgt = (pc as i32 + off32) as usize;
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
                        PayloadKind::Array => { let x=array_count; array_count+=1; x },
                        PayloadKind::PackedSwitch => { let x=psw_count; psw_count+=1; x },
                        PayloadKind::SparseSwitch => { let x=ssw_count; ssw_count+=1; x },
                    };
                    format!("{}{}", prefix, idx)
                });
                payloads.entry(tgt).or_insert(kind);

                // Additionally, collect labels for switch case targets so we can emit them before the target instructions
                match kind {
                    PayloadKind::PackedSwitch => {
                        // header: ident(0x0100), size(u16), first_key(i32), then size * target(i32)
                        if tgt + 4 <= code.len() {
                            let size = u16_at(code, tgt+1) as usize;
                            let targets_start = tgt + 4;
                            for i in 0..size {
                                if targets_start + i*2 + 1 >= code.len() { break; }
                                let lo = u16_at(code, targets_start + i*2) as u32;
                                let hi = u16_at(code, targets_start + i*2 + 1) as u32;
                                let off = ((hi << 16) | lo) as i32;
                                let case_pc = (tgt as i32 + off) as usize;
                                labels.entry(case_pc).or_insert_with(|| format!(":pswitch_{}", i));
                            }
                        }
                    }
                    PayloadKind::SparseSwitch => {
                        // header: ident(0x0200), size(u16), then size * key(i32) and size * target(i32)
                        if tgt + 2 <= code.len() {
                            let size = u16_at(code, tgt+1) as usize;
                            let keys_start = tgt + 2;
                            let targets_start = keys_start + 2*size;
                            for i in 0..size {
                                if targets_start + i*2 + 1 >= code.len() { break; }
                                let lo = u16_at(code, targets_start + i*2) as u32;
                                let hi = u16_at(code, targets_start + i*2 + 1) as u32;
                                let off = ((hi << 16) | lo) as i32;
                                let case_pc = (tgt as i32 + off) as usize;
                                labels.entry(case_pc).or_insert_with(|| format!(":sswitch_{}", i));
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

    (labels, payloads)
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
    let elem_width = u16_at(code, pc+1) as usize;
    let size_lo = u16_at(code, pc+2) as u32;
    let size_hi = u16_at(code, pc+3) as u32;
    let count = ((size_hi << 16) | size_lo) as usize;

    let bytes_len = elem_width.checked_mul(count).ok_or_else(|| DexError::new("array-data overflow"))?;
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
        if bytes.len() < bytes_len { bytes.push(hi); }
    }

    let mut elements: Vec<ArrayDataElement> = Vec::with_capacity(count);
    match elem_width {
        1 => {
            for i in 0..count { elements.push(ArrayDataElement::Byte(bytes[i] as i8)); }
        }
        2 => {
            for i in 0..count {
                let o = i*2; let v = i16::from_le_bytes([bytes[o], bytes[o+1]]);
                elements.push(ArrayDataElement::Short(v));
            }
        }
        4 => {
            for i in 0..count {
                let o = i*4; let v = i32::from_le_bytes([bytes[o], bytes[o+1], bytes[o+2], bytes[o+3]]);
                elements.push(ArrayDataElement::Int(v));
            }
        }
        8 => {
            for i in 0..count {
                let o = i*8; let v = i64::from_le_bytes([
                    bytes[o], bytes[o+1], bytes[o+2], bytes[o+3],
                    bytes[o+4], bytes[o+5], bytes[o+6], bytes[o+7]
                ]);
                elements.push(ArrayDataElement::Long(v));
            }
        }
        _ => {
            return Err(DexError::new("Unsupported array-data element width"));
        }
    }

    let consumed_cu = 4 + data_cu;
    Ok((ArrayDataDirective { element_width: elem_width as i32, elements }, consumed_cu))
}

fn parse_packed_switch_payload(
    code: &[u16],
    pc: usize,
    labels: &HashMap<usize, String>,
) -> Result<(PackedSwitchDirective, usize), DexError> {
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
        let lo = u16_at(code, targets_start + i*2) as u32;
        let hi = u16_at(code, targets_start + i*2 + 1) as u32;
        let off = ((hi << 16) | lo) as i32;
        let case_pc = (pc as i32 + off) as usize;
        let name = labels
            .get(&case_pc)
            .cloned()
            .unwrap_or_else(|| format!(":pswitch_{}", i));
        targets.push(Label( name ));
    }

    let consumed_cu = 4 + size * 2; // header + targets (each target is i32 = 2 code units)
    Ok((PackedSwitchDirective { first_key, targets }, consumed_cu))
}

fn parse_sparse_switch_payload(
    code: &[u16],
    pc: usize,
    labels: &HashMap<usize, String>,
) -> Result<(SparseSwitchDirective, usize), DexError> {
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
        let lo = u16_at(code, keys_start + i*2) as u32;
        let hi = u16_at(code, keys_start + i*2 + 1) as u32;
        keys.push(((hi << 16) | lo) as i32);
    }
    // Read targets and pair with keys
    for i in 0..size {
        let lo = u16_at(code, targets_start + i*2) as u32;
        let hi = u16_at(code, targets_start + i*2 + 1) as u32;
        let off = ((hi << 16) | lo) as i32;
        let case_pc = (pc as i32 + off) as usize;
        let name = labels
            .get(&case_pc)
            .cloned()
            .unwrap_or_else(|| format!(":sswitch_{}", i));
        entries.push(SparseSwitchEntry { key: keys[i], target: Label ( name ) });
    }

    let consumed_cu = 2 + size * 4; // header + keys (2*size) + targets (2*size)
    Ok((SparseSwitchDirective { entries }, consumed_cu))
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
static OPCODE_MAP_CACHE: Lazy<Mutex<HashMap<(i32, i32), Arc<HashMap<u16, &'static Opcode>>>>> = Lazy::new(|| {
    Mutex::new(HashMap::new())
});

fn get_opcode_map(api: i32, art_version: i32) -> Arc<HashMap<u16, &'static Opcode>> {
    let key = (api, art_version);
    let mut guard = OPCODE_MAP_CACHE.lock().expect("opcode map cache lock poisoned");
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

// Format the opcode args based on the defined opcode
fn format_instruction_line(op: &Opcode, code: &[u16], pc: usize, res: &impl RefResolver,
                           regmap: Option<&RegMapper>, labels: Option<&HashMap<usize, String>>) -> (String, usize) {
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
            let va = a4(inst);          // low nibble of high byte
            let lit = s4(b4(inst));     // high nibble of high byte (signed)
            (format!("{} {}, {}", op.name, fmt_reg(regmap, va as u16), lit), 1)
        }
        Format::Format12x => {
            // B:A | op  (B = high nibble, A = low nibble of high byte)
            let inst = u16_at(code, pc);
            let va = a4(inst);
            let vb = b4(inst);
            (format!("{} {}, {}", op.name, fmt_reg(regmap, va as u16), fmt_reg(regmap, vb as u16)), 1)
        }
        // Size = 2
        Format::Format21s => {
            // AA | op, lit:BBBB (signed)
            let inst = u16_at(code, pc);
            let lit  = s16(u16_at(code, pc+1));
            (format!("{} {}, {}", op.name, fmt_reg(regmap, a8(inst) as u16), lit), 2)
        }
        Format::Format21c => {
            // AA | op, kind:BBBB (index → string/type/field/method/proto/handle/callsite)
            let inst = u16_at(code, pc);
            let idx  = u16_at(code, pc+1) as u32;
            let aa   = a8(inst);
            let arg = match op.reference_type {
                ReferenceType::String      => res.string(idx),
                ReferenceType::Type        => res.type_desc(idx),
                ReferenceType::Field       => { let (c,n,d)=res.field_ref(idx); format!("{}->{}:{}", c,n,d) }
                ReferenceType::Method      => { let (c,n,p)=res.method_ref(idx); format!("{}->{}{}", c,n,p) }
                ReferenceType::MethodProto => res.proto(idx),
                ReferenceType::MethodHandle=> res.method_handle(idx),
                ReferenceType::CallSite    => res.call_site(idx),
                ReferenceType::None        => format!("ref@{}", idx),
            };
            (format!("{} {}, {}", op.name, fmt_reg(regmap, aa as u16), arg), 2)
        }
        Format::Format21t => {
            // AA | op, +BBBB (signed 16-bit code-unit offset)
            let inst = u16_at(code, pc);
            let off  = s16(u16_at(code, pc+1)) as i32;
            let tgt = (pc as i32 + off) as usize;
            let label = labels
                .and_then(|m| m.get(&tgt))
                .cloned()
                .unwrap_or_else(|| off.to_string());
            (format!("{} {}, {}", op.name, fmt_reg(regmap, a8(inst) as u16), label), 2)
        }
        Format::Format21ih => {
            // AA | op, BBBB  (signed high-16 for 32-bit literal)
            let inst = u16_at(code, pc);
            let hi = s16(u16_at(code, pc+1)) as i32;
            let lit = hi << 16;
            (format!("{} {}, {}", op.name, fmt_reg(regmap, a8(inst) as u16), lit), 2)
        }
        Format::Format21lh => {
            // AA | op, BBBB  (signed high-16 for 64-bit literal)
            let inst = u16_at(code, pc);
            let hi = s16(u16_at(code, pc+1)) as i64;
            let lit = hi << 48;
            (format!("{} {}, {}", op.name, fmt_reg(regmap, a8(inst) as u16), fmt_wide_lit64(lit)), 2)
        }
        Format::Format22t => {
            // B:A | op, +CCCC  (A,B are regs; CCCC is signed 16-bit offset)
            let inst = u16_at(code, pc);
            let va = a4(inst);
            let vb = b4(inst);
            let off = s16(u16_at(code, pc+1)) as i32;
            let tgt = (pc as i32 + off) as usize;
            let label = labels
                .and_then(|m| m.get(&tgt))
                .cloned()
                .unwrap_or_else(|| off.to_string());
            (format!("{} {}, {}, {}", op.name, fmt_reg(regmap, va as u16), fmt_reg(regmap, vb as u16), label), 2)
        }
        Format::Format22x => {
            // AA | op, BBBB  (two registers: vAA, vBBBB)
            let inst0 = u16_at(code, pc);
            let a = a8(inst0) as u16;
            let b = u16_at(code, pc+1);
            (format!("{} {}, {}", op.name, fmt_reg(regmap, a), fmt_reg(regmap, b)), 2)
        }
        Format::Format22b => {
            // AA | op, BB|CC  (dest=vAA, src=vBB, literal=CC signed 8-bit)
            let inst0 = u16_at(code, pc);
            let inst1 = u16_at(code, pc+1);
            let a = a8(inst0) as u16;
            let b = inst1 & 0x00ff;
            let lit = s8((inst1 >> 8) as u8);
            (format!("{} {}, {}, {}", op.name, fmt_reg(regmap, a), fmt_reg(regmap, b), lit), 2)
        }
        Format::Format22s => {
            // B:A | op, +CCCC  (A=dest, B=src, CCCC=signed 16-bit literal)
            let inst = u16_at(code, pc);
            let va = a4(inst);
            let vb = b4(inst);
            let lit = s16(u16_at(code, pc+1));
            (format!("{} {}, {}, {}", op.name, fmt_reg(regmap, va as u16), fmt_reg(regmap, vb as u16), lit), 2)
        }
        Format::Format23x => {
            // AA | op, C:B  (3 regs in 2 code units; BB=low byte of 2nd CU, CC=high byte)
            let inst0 = u16_at(code, pc);
            let inst1 = u16_at(code, pc + 1);
            let a = a8(inst0) as u16;
            let b = inst1 & 0x00ff;  // low byte
            let c = inst1 >> 8;      // high byte
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
            let tgt = (pc as i32 + off) as usize;
            let label = labels
                .and_then(|m| m.get(&tgt))
                .cloned()
                .unwrap_or_else(|| off.to_string());
            (format!("{} {}", op.name, label), 1)
        }
        Format::Format20t => {
            let off  = s16(u16_at(code, pc+1)) as i32;
            let tgt = (pc as i32 + off) as usize;
            let label = labels
                .and_then(|m| m.get(&tgt))
                .cloned()
                .unwrap_or_else(|| off.to_string());
            (format!("{} {}", op.name, label), 2)
        }
        Format::Format30t => {
            let lo = u16_at(code, pc+1) as u32;
            let hi = u16_at(code, pc+2) as u32;
            let off32 = ((hi << 16) | lo) as i32;
            let tgt = (pc as i32 + off32) as usize;
            let label = labels
                .and_then(|m| m.get(&tgt))
                .cloned()
                .unwrap_or_else(|| off32.to_string());
            (format!("{} {}", op.name, label), 3)
        }
        Format::Format22c => {
            // B:A | op, CCCC  (A and B are 4-bit regs; CCCC is a field/type index)
            // For iput/iget family: iput-object vA, vB, Lcls;->name:Type
            let inst0 = u16_at(code, pc);
            let a = a4(inst0);
            let b = b4(inst0);
            let idx = u16_at(code, pc+1) as u32;
            let target = match op.reference_type {
                ReferenceType::Field  => { let (cl, nm, ds) = res.field_ref(idx); format!("{}->{}:{}", cl, nm, ds) }
                ReferenceType::Type   => res.type_desc(idx),
                ReferenceType::Method => { let (cl, nm, pr) = res.method_ref(idx); format!("{}->{}{}", cl, nm, pr) }
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
            let idx = u16_at(code, pc+1) as u32; // quick field index/offset
            let desc = quick_field_desc_from_name(op.name);
            let field = format!("Lquick;->field@{}:{}", idx, desc);
            let opname = normalize_quick_field_name(op.name);
            (format!("{} {}, {}, {}", opname, fmt_reg(regmap, a), fmt_reg(regmap, b), field), 2)
        }
        // Size = 3
        Format::Format31i => {
            // AA | op, lit32:BBBBBBBB (signed 32-bit literal across the next 2 code units)
            let inst0 = u16_at(code, pc);
            let lo = u16_at(code, pc+1) as u32;
            let hi = u16_at(code, pc+2) as u32;
            let lit = ((hi << 16) | lo) as i32;
            let lit_txt = fmt_lit32_for(op.name, lit);
            (format!("{} {}, {}", op.name, fmt_reg(regmap, a8(inst0) as u16), lit_txt), 3)
        }
        Format::Format31c => {
            // AA | op, kind:BBBBBBBB (32-bit index)
            let inst0 = u16_at(code, pc);
            let lo = u16_at(code, pc+1) as u32;
            let hi = u16_at(code, pc+2) as u32;
            let idx = (hi << 16) | lo;
            let aa  = a8(inst0) as u16;
            let arg = match op.reference_type {
                ReferenceType::String      => res.string(idx),
                ReferenceType::Type        => res.type_desc(idx),
                ReferenceType::Field       => { let (c,n,d)=res.field_ref(idx); format!("{}->{}:{}", c,n,d) }
                ReferenceType::Method      => { let (c,n,p)=res.method_ref(idx); format!("{}->{}{}", c,n,p) }
                ReferenceType::MethodProto => res.proto(idx),
                ReferenceType::MethodHandle=> res.method_handle(idx),
                ReferenceType::CallSite    => res.call_site(idx),
                ReferenceType::None        => format!("ref@{}", idx),
            };
            (format!("{} {}, {}", op.name, fmt_reg(regmap, aa), arg), 3)
        }
        Format::Format45cc => {
            // G|A | op, BBBB, F|E|D|C, HHHH  (A args, C..G regs, BBBB=method/callsite, HHHH=proto)
            let inst0 = u16_at(code, pc);
            let inst1 = u16_at(code, pc+1);
            let inst2 = u16_at(code, pc+2);
            let inst3 = u16_at(code, pc+3);
            let a = ((inst0 >> 12) & 0x0f) as u8;
            let g = ((inst0 >> 8)  & 0x0f) as u8;
            let c = ( inst2        & 0x000f) as u8;
            let d = ((inst2 >> 4)  & 0x0f) as u8;
            let e = ((inst2 >> 8)  & 0x0f) as u8;
            let f = ((inst2 >> 12) & 0x0f) as u8;
            let idx = inst1 as u32;      // method or call site
            let proto_idx = inst3 as u32; // proto or method handle depending on op
            let mut regs = Vec::new();
            for r in [c,d,e,f,g].into_iter().take(a as usize) { regs.push(fmt_reg(regmap, r as u16)); }
            let (target, extra) = match op.reference_type {
                ReferenceType::Method => {
                    let (c,n,p) = res.method_ref(idx);
                    let proto = res.proto(proto_idx);
                    (format!("{}->{}{}", c,n,p), proto)
                }
                ReferenceType::CallSite => {
                    let cs = res.call_site(idx);
                    let proto = res.proto(proto_idx);
                    (cs, proto)
                }
                _ => (format!("ref@{}", idx), format!("proto@{}", proto_idx)),
            };
            (format!("{} {{{}}}, {}, {}", op.name, regs.join(", "), target, extra), 4)
        }
        Format::Format4rcc => {
            // AA | op, BBBB, CCCC, HHHH  (range invoke with 2 refs)
            let inst0 = u16_at(code, pc);
            let idx   = u16_at(code, pc+1) as u32; // method/callsite
            let first = u16_at(code, pc+2);
            let proto_idx = u16_at(code, pc+3) as u32; // proto/methodhandle
            let count = a8(inst0) as u16;
            let range = if count==0 { String::from("{}") } else {
                format!("{{{} .. {}}}", fmt_reg(regmap, first), fmt_reg(regmap, first + count - 1))
            };
            let (target, extra) = match op.reference_type {
                ReferenceType::Method => {
                    let (c,n,p) = res.method_ref(idx);
                    let proto = res.proto(proto_idx);
                    (format!("{}->{}{}", c,n,p), proto)
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
            let lo = u16_at(code, pc+1) as u32;
            let hi = u16_at(code, pc+2) as u32;
            let off32 = ((hi << 16) | lo) as i32;
            let tgt = (pc as i32 + off32) as usize;
            let label = labels
                .and_then(|m| m.get(&tgt))
                .cloned()
                .unwrap_or_else(|| off32.to_string());
            (format!("{} {}, {}", op.name, fmt_reg(regmap, a8(inst0) as u16), label), 3)
        }
        Format::Format32x => {
            // op | 00, AAAA, BBBB  (two 16-bit registers: dest=AAAA, src=BBBB)
            let a = u16_at(code, pc+1);
            let b = u16_at(code, pc+2);
            (format!("{} {}, {}", op.name, fmt_reg(regmap, a), fmt_reg(regmap, b)), 3)
        }
        Format::Format51l => {
            // AA | op, lit64:BBBBBBBBBBBBBBBB (signed 64-bit across next 4 code units)
            let inst0 = u16_at(code, pc);
            let b1 = u16_at(code, pc+1) as u64;
            let b2 = u16_at(code, pc+2) as u64;
            let b3 = u16_at(code, pc+3) as u64;
            let b4 = u16_at(code, pc+4) as u64;
            let lit_u = (b4 << 48) | (b3 << 32) | (b2 << 16) | b1;
            let lit = lit_u as i64; // interpret as signed
            (format!("{} {}, {}", op.name, fmt_reg(regmap, a8(inst0) as u16), fmt_wide_lit64(lit)), 5)
        }
        Format::Format35c => {
            // G|A | op, BBBB, F|E|D|C  (A=arg count, C..G=regs, kind BBBB)
            let inst0 = u16_at(code, pc);
            let inst1 = u16_at(code, pc+1);
            let inst2 = u16_at(code, pc+2);
            let a = ((inst0 >> 12) & 0x0f) as u8; // arg count
            let g = ((inst0 >> 8)  & 0x0f) as u8;
            let c = ( inst2        & 0x000f) as u8;
            let d = ((inst2 >> 4)  & 0x0f) as u8;
            let e = ((inst2 >> 8)  & 0x0f) as u8;
            let f = ((inst2 >> 12) & 0x0f) as u8;
            let idx = inst1 as u32;
            let target = match op.reference_type {
                ReferenceType::Method => { let (cl, nm, pr) = res.method_ref(idx); format!("{}->{}{}", cl, nm, pr) }
                ReferenceType::Type   => res.type_desc(idx),
                ReferenceType::Field  => { let (cl, nm, ds) = res.field_ref(idx); format!("{}->{}:{}", cl, nm, ds) }
                _ => format!("ref@{}", idx),
            };
            let mut regs = Vec::new();
            for (_, r) in [c,d,e,f,g].into_iter().take(a as usize).enumerate() {
                regs.push(fmt_reg(regmap, r as u16));
            }
            (format!("{} {{{}}}, {}", op.name, regs.join(", "), target), 3)
        }
        Format::Format35mi => {
            // G|A | op, BBBB, F|E|D|C  (A=arg count, C..G=regs, BBBB is vtable index / quick index)
            let inst0 = u16_at(code, pc);
            let inst1 = u16_at(code, pc+1);
            let inst2 = u16_at(code, pc+2);
            let a = ((inst0 >> 12) & 0x0f) as u8; // arg count
            let g = ((inst0 >> 8)  & 0x0f) as u8;
            let c = ( inst2        & 0x000f) as u8;
            let d = ((inst2 >> 4)  & 0x0f) as u8;
            let e = ((inst2 >> 8)  & 0x0f) as u8;
            let f = ((inst2 >> 12) & 0x0f) as u8;
            let idx = inst1 as u32; // quick/vtable index
            let mut regs = Vec::new();
            for r in [c,d,e,f,g].into_iter().take(a as usize) {
                regs.push(fmt_reg(regmap, r as u16));
            }
            let target = quick_method_placeholder(op.name, idx);
            let opname = normalize_quick_invoke_name(op.name);
            (format!("{} {{{}}}, {}", opname, regs.join(", "), target), 3)
        }
        Format::Format3rc => {
            // AA | op, BBBB, CCCC (range: first=C, count=AA), kind BBBB
            let inst0 = u16_at(code, pc);
            let idx   = u16_at(code, pc+1) as u32;
            let first = u16_at(code, pc+2);
            let count = a8(inst0) as u16;
            let range = if count==0 { String::from("{}") } else {
                format!("{{{} .. {}}}", fmt_reg(regmap, first), fmt_reg(regmap, first + count - 1))
            };
            let target = match op.reference_type {
                ReferenceType::Method => { let (cl, nm, pr) = res.method_ref(idx); format!("{}->{}{}", cl, nm, pr) }
                ReferenceType::Type   => res.type_desc(idx),
                ReferenceType::Field  => { let (cl, nm, ds) = res.field_ref(idx); format!("{}->{}:{}", cl, nm, ds) }
                _ => format!("ref@{}", idx),
            };
            (format!("{} {}, {}", op.name, range, target), 3)
        }
        Format::Format3rmi => {
            // AA | op, BBBB, CCCC  (range: first=C, count=AA, BBBB = vtable/quick index)
            let inst0 = u16_at(code, pc);
            let idx   = u16_at(code, pc+1) as u32;
            let first = u16_at(code, pc+2);
            let count = a8(inst0) as u16;
            let range = if count==0 { String::from("{}") } else {
                format!("{{{} .. {}}}", fmt_reg(regmap, first), fmt_reg(regmap, first + count - 1))
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
                eprintln!("[smali][decode] ERROR: unimplemented format {:?} for {} at pc {}", fmt, op.name, pc);
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
    let (mut labels, payloads) = collect_labels_and_payloads(&code, &opcode_cache);

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
            match kind {
                PayloadKind::Array => {
                    let (dir, consumed) = parse_array_payload(&code, pc)?;
                    output.push(SmaliOp::ArrayData(dir));
                    pc += consumed;
                    continue;
                }
                PayloadKind::PackedSwitch => {
                    let (dir, consumed) = parse_packed_switch_payload(&code, pc, &labels)?;
                    output.push(SmaliOp::PackedSwitch(dir));
                    pc += consumed;
                    continue;
                }
                PayloadKind::SparseSwitch => {
                    let (dir, consumed) = parse_sparse_switch_payload(&code, pc, &labels)?;
                    output.push(SmaliOp::SparseSwitch(dir));
                    pc += consumed;
                    continue;
                }
            }
        }
        let inst = u16_at(&code, pc);
        let opc  = op(inst) as u16;
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
                    let gopc  = op(ginst) as u16;
                    if let Some(gop) = opcode_cache.get(&gopc) {
                        let sz = format_size_cu(gop.format);
                        guesses.push_str(&format!(
                            "guess pc-{}: {} (0x{:02x}) fmt {:?} sizeCU {} words:",
                            back, gop.name, gopc, gop.format, sz
                        ));
                        let gend = (gpc + sz).min(code.len());
                        for j in gpc..gend { guesses.push_str(&format!(" {:04x}", code[j])); }
                        guesses.push_str(" | ");
                    } else {
                        guesses.push_str(&format!("guess pc-{}: opcode 0x{:02x} unknown | ", back, gopc));
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
        if need_cu <= 0 { need_cu = 1; }
        // Quick-invoke fallback may read 3 code units even if format metadata disagrees
        let opname_for_need = opdef.name;
        if need_cu < 3 && (opname_for_need.ends_with("-quick") || opname_for_need.ends_with("-quick/range")) {
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
                    let b = u16_at(&code, pc+1);
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
                    let w1 = if pc + 1 < code.len() { u16_at(&code, pc + 1) } else { 0 };
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
                    let a = u16_at(&code, pc+1);
                    let b = u16_at(&code, pc+2);
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
                let off  = s16(u16_at(&code, pc+1)) as i32;
                let tgt = (pc as i32 + off) as usize;
                labels.entry(tgt).or_insert_with(|| format!(":addr_{:x}", tgt));
            }
            Format::Format22t => {
                let off  = s16(u16_at(&code, pc+1)) as i32;
                let tgt = (pc as i32 + off) as usize;
                labels.entry(tgt).or_insert_with(|| format!(":addr_{:x}", tgt));
            }
            Format::Format10t => {
                let off = s8(a8(inst)) as i32;
                let tgt = (pc as i32 + off) as usize;
                labels.entry(tgt).or_insert_with(|| format!(":addr_{:x}", tgt));
            }
            Format::Format20t => {
                let off  = s16(u16_at(&code, pc+1)) as i32;
                let tgt = (pc as i32 + off) as usize;
                labels.entry(tgt).or_insert_with(|| format!(":addr_{:x}", tgt));
            }
            Format::Format30t => {
                let lo = u16_at(&code, pc+1) as u32;
                let hi = u16_at(&code, pc+2) as u32;
                let off32 = ((hi << 16) | lo) as i32;
                let tgt = (pc as i32 + off32) as usize;
                labels.entry(tgt).or_insert_with(|| format!(":addr_{:x}", tgt));
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

        let (mut line, size_cu) = format_instruction_line(opdef, &code, pc, res, regmap, Some(&labels));
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

pub fn decode(bytecode: &[u8], api:i32, art_version: i32) -> Result<Vec<SmaliOp>, DexError> {
    let resolver = PlaceholderResolver;
    decode_with_resolver(bytecode, api, art_version, &resolver)
}