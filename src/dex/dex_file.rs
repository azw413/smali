/* Dex file format structures */

use crate::dex::annotations::{
    AnnotationItem, AnnotationSetItem, AnnotationSetRefList, AnnotationsDirectoryItem,
};
use crate::dex::encoded_values::{EncodedValue, read_encoded_array};
use crate::dex::error::DexError;
use crate::dex::{
    read_sleb128, read_u1, read_u2, read_u4, read_uleb128, read_uleb128p1, read_x, write_sleb128,
    write_u1, write_u2, write_u4, write_uleb128, write_uleb128p1, write_x,
};
use crate::types::{
    AnnotationElement as SmaliAnnElement, AnnotationValue as SmaliAnnValue,
    AnnotationVisibility as SmaliAnnVis, MethodSignature, Modifiers, ObjectIdentifier,
    SmaliAnnotation, SmaliClass, SmaliField, SmaliMethod, SmaliParam, TypeSignature,
};
use cesu8::to_java_cesu8;
use log::{error, info, warn};

use crate::dex::opcode_format::{RefResolver, RegMapper, decode_with_ctx};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
/* Constants */
pub const DEX_FILE_MAGIC: [u8; 8] = [0x64, 0x65, 0x78, 0x0a, 0x30, 0x33, 0x39, 0x00];
pub const ENDIAN_CONSTANT: u32 = 0x12345678;
pub const REVERSE_ENDIAN_CONSTANT: u32 = 0x78563412;
pub const NO_INDEX: usize = 0xffffffff;

/* Access flags */
pub const ACC_PUBLIC: u32 = 0x1;
pub const ACC_PRIVATE: u32 = 0x2;
pub const ACC_PROTECTED: u32 = 0x4;
pub const ACC_STATIC: u32 = 0x8;
pub const ACC_FINAL: u32 = 0x10;
pub const ACC_SYNCHRONIZED: u32 = 0x20;
pub const ACC_VOLATILE: u32 = 0x40;
pub const ACC_BRIDGE: u32 = 0x40;
pub const ACC_TRANSIENT: u32 = 0x80;
pub const ACC_VARARGS: u32 = 0x80;
pub const ACC_NATIVE: u32 = 0x100;
pub const ACC_INTERFACE: u32 = 0x200;
pub const ACC_ABSTRACT: u32 = 0x400;
pub const ACC_STRICT: u32 = 0x800;
pub const ACC_SYNTHETIC: u32 = 0x1000;
pub const ACC_ANNOTATION: u32 = 0x2000;
pub const ACC_ENUM: u32 = 0x4000;
pub const ACC_CONSTRUCTOR: u32 = 0x10000;
pub const ACC_DECLARED_SYNCHRONIZED: u32 = 0x20000;

type StringId = usize;
type TypeId = StringId;
type ProtoId = usize;
type FieldId = usize;
type MethodId = usize;

#[derive(Debug)]
pub struct TypeList(Vec<TypeId>);
impl TypeList {
    pub fn from_type_ids(ids: Vec<TypeId>) -> Self {
        TypeList(ids)
    }

    pub fn items(&self) -> &[TypeId] {
        &self.0
    }

    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<crate::dex::dex_file::TypeList, DexError> {
        let mut v = vec![];
        let size = read_u4(bytes, ix)?;
        for _ in 0..size {
            v.push(read_u2(bytes, ix)? as TypeId);
        }
        Ok(TypeList(v))
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_u4(bytes, self.0.len() as u32);
        for i in &self.0 {
            c += write_u2(bytes, *i as u16);
        }
        if (self.0.len() & 1) != 0 {
            c += write_u2(bytes, 0); // 4-byte alignment padding per spec
        }
        c
    }
}

#[derive(Debug)]
pub struct PrototypeItem {
    // The proto_id_item struct
    pub shorty_idx: StringId,
    pub return_type_idx: TypeId,
    pub parameters: TypeList,
}

impl PrototypeItem {
    pub fn to_string(&self, dex_file: &DexFile) -> Result<String, DexError> {
        let mut s = dex_file.strings[self.shorty_idx].to_string()?;
        s.push_str(" (");
        for t in self.parameters.items() {
            s.push_str(&dex_file.strings[dex_file.types[*t]].to_string()?);
        }
        s.push(')');
        s.push_str(&dex_file.strings[dex_file.types[self.return_type_idx]].to_string()?);
        Ok(s)
    }

    /// Write the `proto_id_item` entry. `parameters_off` must be the file-absolute offset to the
    /// associated `type_list` in the data section (or 0 if the prototype has no parameters).
    pub fn write(&self, bytes: &mut Vec<u8>, parameters_off: u32) -> usize {
        let mut c = 0;
        c += write_u4(bytes, self.shorty_idx as u32);
        c += write_u4(bytes, self.return_type_idx as u32);
        c += write_u4(bytes, parameters_off);
        c
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FieldItem {
    // The field_id_item struct
    pub class_idx: TypeId,
    pub type_idx: TypeId,
    pub name_idx: StringId,
}

impl FieldItem {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<crate::dex::dex_file::FieldItem, DexError> {
        Ok(FieldItem {
            class_idx: read_u2(bytes, ix)? as TypeId,
            type_idx: read_u2(bytes, ix)? as TypeId,
            name_idx: read_u4(bytes, ix)? as StringId,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_u2(bytes, self.class_idx as u16);
        c += write_u2(bytes, self.type_idx as u16);
        c += write_u4(bytes, self.name_idx as u32);
        c
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MethodItem {
    // The field_id_item struct
    pub class_idx: TypeId,
    pub proto_idx: ProtoId,
    pub name_idx: StringId,
}

impl crate::dex::dex_file::MethodItem {
    pub fn read(
        bytes: &[u8],
        ix: &mut usize,
    ) -> Result<crate::dex::dex_file::MethodItem, DexError> {
        Ok(crate::dex::dex_file::MethodItem {
            class_idx: read_u2(bytes, ix)? as TypeId,
            proto_idx: read_u2(bytes, ix)? as ProtoId,
            name_idx: read_u4(bytes, ix)? as StringId,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_u2(bytes, self.class_idx as u16);
        c += write_u2(bytes, self.proto_idx as u16);
        c += write_u4(bytes, self.name_idx as u32);
        c
    }
}

#[derive(Debug)]
pub struct EncodedField {
    pub field_idx: FieldId,
    pub access_flags: u32,
}

impl EncodedField {
    /// Write this encoded field, updating `last_index` per DEX diff encoding rules.
    pub fn write(&self, bytes: &mut Vec<u8>, last_index: &mut FieldId) -> usize {
        let mut c = 0;
        let diff = self.field_idx - *last_index;
        c += write_uleb128(bytes, diff as u32);
        *last_index = self.field_idx;
        c += write_uleb128(bytes, self.access_flags);
        c
    }
}

pub const DBG_END_SEQUENCE: u8 = 0x00;
pub const DBG_ADVANCE_PC: u8 = 0x01;
pub const DBG_ADVANCE_LINE: u8 = 0x02;
pub const DBG_START_LOCAL: u8 = 0x03;
pub const DBG_START_LOCAL_EXTENDED: u8 = 0x04;
pub const DBG_END_LOCAL: u8 = 0x05;
pub const DBG_RESTART_LOCAL: u8 = 0x06;
pub const DBG_SET_PROLOGUE_END: u8 = 0x07;
pub const DBG_SET_EPILOGUE_BEGIN: u8 = 0x08;
pub const DBG_SET_FILE: u8 = 0x09;
pub const DBG_FIRST_SPECIAL: u8 = 0x0a;
pub const DBG_LINE_BASE: i32 = -4;
pub const DBG_LINE_RANGE: u8 = 15;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DebugInfo {
    pub line_start: u32,
    pub parameter_names: Vec<Option<u32>>,
    pub debug_opcodes: Vec<u8>,
}

impl crate::dex::dex_file::DebugInfo {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<crate::dex::dex_file::DebugInfo, DexError> {
        let line_start = read_uleb128(bytes, ix)?;

        let parameters_size = read_uleb128(bytes, ix)?;
        let mut parameter_names = Vec::with_capacity(parameters_size as usize);

        for _ in 0..parameters_size {
            let idx = read_uleb128p1(bytes, ix)?; // -1 => NO_INDEX
            parameter_names.push(if idx < 0 { None } else { Some(idx as u32) });
        }

        let start = *ix;
        skip_debug_stream(bytes, ix)?;
        let debug_opcodes = bytes[start..*ix].to_vec();

        Ok(DebugInfo {
            line_start,
            parameter_names,
            debug_opcodes,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_uleb128(bytes, self.line_start);
        c += write_uleb128(bytes, self.parameter_names.len() as u32);
        for p in &self.parameter_names {
            let idx = match p {
                Some(val) => *val as i32,
                None => -1,
            };
            c += write_uleb128p1(bytes, idx);
        }

        c += write_x(bytes, &self.debug_opcodes);
        if self.debug_opcodes.last().copied() != Some(DBG_END_SEQUENCE) {
            c += write_u1(bytes, DBG_END_SEQUENCE);
        }
        c
    }
}

fn skip_debug_stream(bytes: &[u8], ix: &mut usize) -> Result<(), DexError> {
    loop {
        let opcode = read_u1(bytes, ix)?;
        match opcode {
            DBG_END_SEQUENCE => break,
            DBG_ADVANCE_PC => {
                read_uleb128(bytes, ix)?;
            }
            DBG_ADVANCE_LINE => {
                read_sleb128(bytes, ix)?;
            }
            DBG_START_LOCAL => {
                read_uleb128(bytes, ix)?;
                read_uleb128p1(bytes, ix)?;
                read_uleb128p1(bytes, ix)?;
            }
            DBG_START_LOCAL_EXTENDED => {
                read_uleb128(bytes, ix)?;
                read_uleb128p1(bytes, ix)?;
                read_uleb128p1(bytes, ix)?;
                read_uleb128p1(bytes, ix)?;
            }
            DBG_END_LOCAL | DBG_RESTART_LOCAL => {
                read_uleb128(bytes, ix)?;
            }
            DBG_SET_PROLOGUE_END | DBG_SET_EPILOGUE_BEGIN => {}
            DBG_SET_FILE => {
                read_uleb128p1(bytes, ix)?;
            }
            other => {
                if other >= DBG_FIRST_SPECIAL {
                    // no payload
                } else {
                    return Err(DexError::new(&format!(
                        "unknown debug opcode 0x{:x}",
                        other
                    )));
                }
            }
        }
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EncodedTypeAddrPair {
    pub type_idx: TypeId,
    pub addr: u32,
}

impl EncodedTypeAddrPair {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<EncodedTypeAddrPair, DexError> {
        let type_idx = read_uleb128(bytes, ix)? as usize;
        let addr = read_uleb128(bytes, ix)? as u32;
        Ok(EncodedTypeAddrPair { type_idx, addr })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_uleb128(bytes, self.type_idx as u32);
        c += write_uleb128(bytes, self.addr);
        c
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TryItem {
    pub start_addr: u32,
    pub insn_count: u16,
    pub handler_off: u16, // offset (in bytes) into the encoded_catch_handler_list
    pub handler_idx: Option<usize>, // helper for re-encoding handler_off
}

impl TryItem {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<TryItem, DexError> {
        Ok(TryItem {
            start_addr: read_u4(bytes, ix)?,
            insn_count: read_u2(bytes, ix)?,
            handler_off: read_u2(bytes, ix)?,
            handler_idx: None,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_u4(bytes, self.start_addr);
        c += write_u2(bytes, self.insn_count);
        c += write_u2(bytes, self.handler_off);
        c
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EncodedCatchHandler {
    pub handlers: Vec<EncodedTypeAddrPair>,
    pub catch_all_addr: Option<u32>,
}

impl EncodedCatchHandler {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<EncodedCatchHandler, DexError> {
        let size = read_sleb128(bytes, ix)?;
        let count = if size >= 0 {
            size as usize
        } else {
            (-size) as usize
        };
        let mut pairs = Vec::with_capacity(count);
        for _ in 0..count {
            pairs.push(EncodedTypeAddrPair::read(bytes, ix)?);
        }
        let catch_all_addr = if size < 0 {
            Some(read_uleb128(bytes, ix)? as u32)
        } else {
            None
        };
        Ok(EncodedCatchHandler {
            handlers: pairs,
            catch_all_addr,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        let size: i32 = if self.catch_all_addr.is_some() {
            -(self.handlers.len() as i32)
        } else {
            self.handlers.len() as i32
        };
        c += write_sleb128(bytes, size);
        for p in &self.handlers {
            c += p.write(bytes);
        }
        if let Some(addr) = self.catch_all_addr {
            c += write_uleb128(bytes, addr);
        }
        c
    }
}

#[derive(Debug)]
pub struct CodeItem {
    registers_size: u16,
    args_in_size: u16,
    args_out_size: u16,
    tries_size: u16,
    debug_info: Option<DebugInfo>,
    instructions: Vec<u16>,
    padding: u16,
    // New: actual data for try blocks and catch handlers
    tries: Vec<TryItem>,
    handlers: Vec<EncodedCatchHandler>,
}

impl crate::dex::dex_file::CodeItem {
    pub fn debug_info(&self) -> Option<&DebugInfo> {
        self.debug_info.as_ref()
    }

    pub fn new(
        registers_size: u16,
        args_in_size: u16,
        args_out_size: u16,
        instructions: Vec<u16>,
        tries: Vec<TryItem>,
        handlers: Vec<EncodedCatchHandler>,
        debug_info: Option<DebugInfo>,
    ) -> Self {
        CodeItem {
            registers_size,
            args_in_size,
            args_out_size,
            tries_size: tries.len() as u16,
            debug_info,
            instructions,
            padding: 0,
            tries,
            handlers,
        }
    }

    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<crate::dex::dex_file::CodeItem, DexError> {
        let code_item_start = *ix;
        let registers_size = read_u2(bytes, ix)?;
        let args_in_size = read_u2(bytes, ix)?;
        let args_out_size = read_u2(bytes, ix)?;
        let tries_size = read_u2(bytes, ix)?;

        let mut debug_offset = read_u4(bytes, ix)? as usize;
        let debug_info = if debug_offset > 0 {
            Some(DebugInfo::read(bytes, &mut debug_offset)?)
        } else {
            None
        };

        let instructions_size = read_u4(bytes, ix)?;
        let mut instructions = vec![];
        for _ in 0..instructions_size {
            instructions.push(read_u2(bytes, ix)?);
        }

        // Optional 2-byte padding if there are tries and insns_size is odd
        let mut padding: u16 = 0;
        let mut tries: Vec<TryItem> = vec![];
        let mut handlers: Vec<EncodedCatchHandler> = vec![];

        if tries_size > 0 {
            if (instructions_size & 1) != 0 {
                padding = read_u2(bytes, ix)?;
            }
            if (instructions_size & 1) != 0 && padding != 0 {
                warn!(
                    "[codeitem] non-zero padding 0x{:04x} at 0x{:x} (code_item_start=0x{:x})",
                    padding,
                    *ix as usize - 2,
                    code_item_start
                );
            }
            for _ in 0..tries_size {
                tries.push(TryItem::read(bytes, ix)?);
            }
            // encoded_catch_handler_list starts here
            let handlers_size = read_uleb128(bytes, ix)? as usize;
            if handlers_size > 1_000_000 {
                return Err(DexError::new(
                    "encoded_catch_handler_list size is implausibly large",
                ));
            }
            let handlers_base = *ix; // first handler entry will start here

            // Sanity: verify each try's handler_off points somewhere within file bounds
            for (ti, t) in tries.iter().enumerate() {
                let abs = handlers_base.saturating_add(t.handler_off as usize);
                if abs >= bytes.len() {
                    warn!(
                        "[codeitem] TryItem#{} handler_off {} -> OOB abs=0x{:x} (base=0x{:x}, file_size=0x{:x})",
                        ti,
                        t.handler_off,
                        abs,
                        handlers_base,
                        bytes.len()
                    );
                }
            }

            // Read all handlers with a temporary cursor so we can produce good error context
            let mut scan = handlers_base;
            let mut handler_offsets = Vec::with_capacity(handlers_size);
            for i in 0..handlers_size {
                let entry_off = scan;
                match EncodedCatchHandler::read(bytes, &mut scan) {
                    Ok(h) => handlers.push(h),
                    Err(e) => {
                        let ctx = format!(
                            "while reading EncodedCatchHandler #{}/{} at 0x{:x} (base=0x{:x}, code_item_start=0x{:x}, tries_size={}, insns_size={})",
                            i + 1,
                            handlers_size,
                            entry_off,
                            handlers_base,
                            code_item_start,
                            tries_size,
                            instructions_size
                        );
                        return Err(DexError::with_context(e, ctx));
                    }
                }
                if scan <= entry_off {
                    return Err(DexError::new(
                        "EncodedCatchHandler did not advance cursor (corrupt data)",
                    ));
                }
                handler_offsets.push((entry_off - handlers_base) as u32);
            }
            // Advance main cursor only after successful scan
            *ix = scan;

            let mut offset_to_index = HashMap::with_capacity(handler_offsets.len());
            for (idx, off) in handler_offsets.iter().enumerate() {
                offset_to_index.entry(*off).or_insert(idx);
            }
            for (ti, t) in tries.iter_mut().enumerate() {
                let abs_off = t.handler_off as u32;
                if let Some(&idx) = offset_to_index.get(&abs_off) {
                    t.handler_idx = Some(idx);
                } else {
                    warn!(
                        "[codeitem] could not resolve handler offset {} for try #{} (handlers_size={})",
                        abs_off,
                        ti,
                        handler_offsets.len()
                    );
                }
            }
        }

        Ok(CodeItem {
            registers_size,
            args_in_size,
            args_out_size,
            tries_size,
            debug_info,
            instructions,
            padding,
            tries,
            handlers,
        })
    }

    // Written to the data section
    pub fn write(&self, bytes: &mut Vec<u8>, code_item_base: u32) -> Result<usize, DexError> {
        let mut c = 0;
        let start = bytes.len();
        c += write_u2(bytes, self.registers_size);
        c += write_u2(bytes, self.args_in_size);
        c += write_u2(bytes, self.args_out_size);
        let tries_len = self.tries.len() as u16;
        c += write_u2(bytes, tries_len);

        // Reserve space for debug_info_off; we'll patch it after writing the debug block
        let debug_off_pos = bytes.len();
        c += write_u4(bytes, 0);

        c += write_u4(bytes, self.instructions.len() as u32);
        for i in &self.instructions {
            c += write_u2(bytes, *i);
        }

        if !self.tries.is_empty() && (self.instructions.len() & 1) != 0 {
            // single 16-bit 0 padding to make tries start on a 4-byte boundary
            c += write_u2(bytes, 0);
        }

        if !self.tries.is_empty() {
            let mut handler_offsets: Vec<u16> = Vec::with_capacity(self.handlers.len());
            let mut handler_entries: Vec<Vec<u8>> = Vec::with_capacity(self.handlers.len());
            for h in &self.handlers {
                let mut entry_buf = Vec::new();
                h.write(&mut entry_buf);
                handler_entries.push(entry_buf);
            }

            let mut prefix_buf = Vec::new();
            write_uleb128(&mut prefix_buf, self.handlers.len() as u32);
            let mut running = prefix_buf.len();
            for entry in &handler_entries {
                if running > u16::MAX as usize {
                    return Err(DexError::new(
                        "encoded_catch_handler_list exceeds u16 offset range",
                    ));
                }
                handler_offsets.push(running as u16);
                running += entry.len();
            }

            for (idx, t) in self.tries.iter().enumerate() {
                let resolved = if let Some(handler_index) = t.handler_idx {
                    handler_offsets.get(handler_index).copied().ok_or_else(|| {
                        DexError::new(&format!(
                            "handler_idx {} out of range for try #{} (handlers={})",
                            handler_index,
                            idx,
                            handler_offsets.len()
                        ))
                    })?
                } else if self.handlers.is_empty() {
                    t.handler_off
                } else {
                    return Err(DexError::new(
                        "TryItem missing handler_idx while handlers present",
                    ));
                };
                let mut try_record = t.clone();
                try_record.handler_off = resolved;
                c += try_record.write(bytes);
            }

            c += write_uleb128(bytes, self.handlers.len() as u32);
            for entry in handler_entries {
                let before = bytes.len();
                bytes.extend_from_slice(&entry);
                c += bytes.len() - before;
            }
        }

        // Patch debug_info_off and append the debug block (if any)
        if let Some(di) = &self.debug_info {
            let debug_info_start = bytes.len();
            let absolute = code_item_base
                .checked_add((debug_info_start - start) as u32)
                .ok_or_else(|| DexError::new("debug_info offset overflow"))?;
            let mut tmp = Vec::with_capacity(4);
            write_u4(&mut tmp, absolute);
            bytes[debug_off_pos..debug_off_pos + 4].copy_from_slice(&tmp);
            c += di.write(bytes);
        }

        Ok(c)
    }
}

#[derive(Debug)]
pub struct EncodedMethod {
    pub method_idx: MethodId,
    pub access_flags: u32,
    pub code_off: u32,
    pub code: Option<CodeItem>,
}

impl EncodedMethod {
    pub fn write(&self, bytes: &mut Vec<u8>, last_index: &mut MethodId) -> usize {
        let mut c = 0;
        let diff = self.method_idx - *last_index;
        c += write_uleb128(bytes, diff as u32);
        *last_index = self.method_idx;
        c += write_uleb128(bytes, self.access_flags);
        c += write_uleb128(bytes, self.code_off);
        c
    }
}

#[derive(Debug)]
pub struct ClassDataItem {
    // The class_def_item struct
    pub static_fields: Vec<EncodedField>,
    pub instance_fields: Vec<EncodedField>,
    pub direct_methods: Vec<EncodedMethod>,
    pub virtual_methods: Vec<EncodedMethod>,
}

impl ClassDataItem {
    pub fn read(
        bytes: &[u8],
        ix: &mut usize,
    ) -> Result<crate::dex::dex_file::ClassDataItem, DexError> {
        let static_field_size = read_uleb128(bytes, ix)?;
        let instance_field_size = read_uleb128(bytes, ix)?;
        let direct_method_size = read_uleb128(bytes, ix)?;
        let virtual_method_size = read_uleb128(bytes, ix)?;

        let mut static_fields = vec![];
        let mut instance_fields = vec![];
        let mut direct_methods = vec![];
        let mut virtual_methods = vec![];

        let mut offset = 0;
        for _ in 0..static_field_size {
            offset += read_uleb128(bytes, ix)?;
            static_fields.push(EncodedField {
                field_idx: offset as FieldId,
                access_flags: read_uleb128(bytes, ix)?,
            })
        }

        offset = 0;
        for _ in 0..instance_field_size {
            offset += read_uleb128(bytes, ix)?;
            instance_fields.push(EncodedField {
                field_idx: offset as FieldId,
                access_flags: read_uleb128(bytes, ix)?,
            })
        }

        offset = 0;
        for _ in 0..direct_method_size {
            offset += read_uleb128(bytes, ix)?;
            let access_flags = read_uleb128(bytes, ix)?;
            let code_off = read_uleb128(bytes, ix)?;
            let mut code_cursor = code_off as usize;
            let code = if code_off > 0 {
                Some(CodeItem::read(bytes, &mut code_cursor)?)
            } else {
                None
            };
            direct_methods.push(EncodedMethod {
                method_idx: offset as MethodId,
                access_flags,
                code_off,
                code,
            });
        }

        offset = 0;
        for _ in 0..virtual_method_size {
            offset += read_uleb128(bytes, ix)?;
            let access_flags = read_uleb128(bytes, ix)?;
            let code_off = read_uleb128(bytes, ix)?;
            let mut code_cursor = code_off as usize;
            let code = if code_off > 0 {
                Some(CodeItem::read(bytes, &mut code_cursor)?)
            } else {
                None
            };
            virtual_methods.push(EncodedMethod {
                method_idx: offset as MethodId,
                access_flags,
                code_off,
                code,
            });
        }

        Ok(ClassDataItem {
            static_fields,
            instance_fields,
            direct_methods,
            virtual_methods,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_uleb128(bytes, self.static_fields.len() as u32);
        c += write_uleb128(bytes, self.instance_fields.len() as u32);
        c += write_uleb128(bytes, self.direct_methods.len() as u32);
        c += write_uleb128(bytes, self.virtual_methods.len() as u32);

        let mut last_field: FieldId = 0;
        for field in &self.static_fields {
            c += field.write(bytes, &mut last_field);
        }

        last_field = 0;
        for field in &self.instance_fields {
            c += field.write(bytes, &mut last_field);
        }

        let mut last_method: MethodId = 0;
        for method in &self.direct_methods {
            c += method.write(bytes, &mut last_method);
        }

        last_method = 0;
        for method in &self.virtual_methods {
            c += method.write(bytes, &mut last_method);
        }

        c
    }
}

#[derive(Debug)]
pub struct ClassDefItem {
    // The class_def_item struct
    pub class_idx: TypeId,
    pub access_flags: u32,
    pub superclass_idx: TypeId,
    pub interfaces: Option<TypeList>,
    pub source_file_idx: StringId,
    pub annotations: Option<AnnotationsDirectoryItem>,
    pub class_data: Option<ClassDataItem>,
    pub static_values: Option<Vec<EncodedValue>>,
}

impl ClassDefItem {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<ClassDefItem, DexError> {
        let class_idx = read_u4(bytes, ix)? as TypeId;
        let access_flags = read_u4(bytes, ix)?;
        let superclass_idx = read_u4(bytes, ix)? as TypeId;
        let mut interface_offset = read_u4(bytes, ix)? as usize;
        let interfaces = if interface_offset > 0 {
            Some(TypeList::read(bytes, &mut interface_offset)?)
        } else {
            None
        };
        let source_file_idx = read_u4(bytes, ix)? as StringId;
        let mut annotations_offset = read_u4(bytes, ix)? as usize;
        let annotations = if annotations_offset > 0 {
            Some(AnnotationsDirectoryItem::read(
                bytes,
                &mut annotations_offset,
            )?)
        } else {
            None
        };
        let mut class_data_offset = read_u4(bytes, ix)? as usize;
        let class_data = if class_data_offset > 0 {
            let cd = ClassDataItem::read(bytes, &mut class_data_offset);
            match cd {
                Ok(cd) => Some(cd),
                Err(e) => {
                    error!("Error reading ClassDataItem: {:?}", e);
                    None
                }
            }
        } else {
            None
        };
        let mut static_values_offset = read_u4(bytes, ix)? as usize;
        let static_values = if static_values_offset > 0 {
            Some(read_encoded_array(bytes, &mut static_values_offset)?)
        } else {
            None
        };

        Ok(ClassDefItem {
            class_idx,
            access_flags,
            superclass_idx,
            interfaces,
            source_file_idx,
            annotations,
            class_data,
            static_values,
        })
    }

    /// Write a `class_def_item` using provided offsets for referenced sections.
    /// This does not serialize the referenced sections themselves.
    pub fn write_with_offsets(
        &self,
        bytes: &mut Vec<u8>,
        interfaces_off: u32,
        annotations_off: u32,
        class_data_off: u32,
        static_values_off: u32,
    ) -> usize {
        let mut c = 0;
        c += write_u4(bytes, self.class_idx as u32);
        c += write_u4(bytes, self.access_flags);
        c += write_u4(bytes, self.superclass_idx as u32);
        c += write_u4(bytes, interfaces_off);
        c += write_u4(bytes, self.source_file_idx as u32);
        c += write_u4(bytes, annotations_off);
        c += write_u4(bytes, class_data_off);
        c += write_u4(bytes, static_values_off);
        c
    }

    pub fn write(&self, _bytes: &mut Vec<u8>) -> usize {
        // ClassDefItem participates in a sectioned layout; a higher-level builder should
        // compute and supply the offsets. Use `write_with_offsets` instead of this method.
        0
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallSiteId {
    // The call_site_id_item struct
}

#[derive(Debug, PartialEq, Eq)]
pub struct MethodHandle {
    // The method_handle_item struct
}

#[derive(Debug)]
pub struct DexFile {
    pub header: Header,
    pub strings: Vec<DexString>,
    pub types: Vec<TypeId>,
    pub prototypes: Vec<PrototypeItem>,
    pub fields: Vec<FieldItem>,
    pub methods: Vec<MethodItem>,
    pub class_defs: Vec<ClassDefItem>,
    pub call_site_ids: Vec<CallSiteId>,
    pub method_handles: Vec<MethodHandle>,
    pub data: Vec<u8>,
    pub link_data: Vec<u8>,
}

impl DexFile {
    fn read(bytes: &[u8], ix: &mut usize) -> Result<DexFile, DexError> {
        let header = Header::read(bytes, ix)?;

        let mut dex = DexFile {
            header,
            strings: vec![],
            types: vec![],
            prototypes: vec![],
            fields: vec![],
            methods: vec![],
            class_defs: vec![],
            call_site_ids: vec![],
            method_handles: vec![],
            data: bytes.to_vec(),
            link_data: vec![],
        };

        // Read the strings
        *ix = dex.header.string_ids_off as usize;
        for _ in 0..dex.header.string_ids_size {
            let mut string_id = read_u4(bytes, ix)? as usize;
            let ds = DexString::read(bytes, &mut string_id)?;
            dex.strings.push(ds);
        }

        // Read the type_ids
        *ix = dex.header.type_ids_off as usize;
        for _ in 0..dex.header.type_ids_size {
            let type_id: TypeId = read_u4(bytes, ix)? as usize;
            if let DexString::Decoded(_s) = &dex.strings[type_id] {
                dex.types.push(type_id);
            } else {
                fail!("Invalid type description: {:?}", &dex.strings[type_id]);
            }
        }

        // Read the prototypes
        *ix = dex.header.proto_ids_off as usize;
        for _ in 0..dex.header.proto_ids_size {
            let shorty_idx = read_u4(bytes, ix)? as crate::dex::dex_file::StringId;
            let return_type_idx = read_u4(bytes, ix)? as crate::dex::dex_file::TypeId;
            let mut parameter_offset = read_u4(bytes, ix)? as usize;
            let p = PrototypeItem {
                shorty_idx,
                return_type_idx,
                parameters: if parameter_offset == 0 {
                    TypeList(vec![])
                } else {
                    TypeList::read(bytes, &mut parameter_offset)?
                },
            };
            dex.prototypes.push(p);
        }

        // Read the Field ids
        *ix = dex.header.field_ids_off as usize;
        for _ in 0..dex.header.field_ids_size {
            dex.fields.push(FieldItem::read(bytes, ix)?);
        }

        // Read the Methods ids
        *ix = dex.header.method_ids_off as usize;
        for _ in 0..dex.header.method_ids_size {
            dex.methods.push(MethodItem::read(bytes, ix)?);
        }

        // Read the Class Defs
        *ix = dex.header.class_defs_off as usize;
        for _ in 0..dex.header.class_defs_size {
            dex.class_defs.push(ClassDefItem::read(bytes, ix)?);
        }

        Ok(dex)
    }

    fn get_string(&self, id: StringId) -> Result<String, DexError> {
        let name_string = &self.strings[id];
        let name = match name_string {
            DexString::Decoded(s) => s.to_string(),
            DexString::Raw(_, _) => return Err(DexError::new("Invalid string in class name.")),
        };

        Ok(name)
    }

    fn get_type(&self, id: TypeId) -> Result<ObjectIdentifier, DexError> {
        let type_str = self.get_string(self.types[id])?;
        Ok(ObjectIdentifier::from_jni_type(&type_str))
    }

    // Helper: get type descriptor string from type_idx
    fn type_desc(&self, type_idx: TypeId) -> Result<String, DexError> {
        self.strings[self.types[type_idx]].to_string()
    }

    // Helper: read all AnnotationItem at a set offset
    fn read_annotation_set_items(&self, off: u32) -> Result<Vec<AnnotationItem>, DexError> {
        if off == 0 {
            return Ok(vec![]);
        }
        let mut ix = off as usize;
        let set = AnnotationSetItem::read(&self.data, &mut ix)?;
        let mut items = Vec::with_capacity(set.entries.len());
        for entry_off in set.entries {
            if entry_off == 0 {
                continue;
            }
            let mut j = entry_off as usize;
            let item = AnnotationItem::read(&self.data, &mut j)?;
            items.push(item);
        }
        Ok(items)
    }

    // Helper: convert DEX annotation visibility to SmaliAnnVis
    fn convert_visibility(v: u8) -> SmaliAnnVis {
        match v {
            0x00 => SmaliAnnVis::Build,
            0x01 => SmaliAnnVis::Runtime,
            0x02 => SmaliAnnVis::System,
            _ => SmaliAnnVis::Runtime,
        }
    }

    fn encoded_value_atom_to_smali(
        &self,
        v: &crate::dex::encoded_values::EncodedValue,
    ) -> Result<String, DexError> {
        use crate::dex::encoded_values::EncodedValue as EV;

        match v {
            EV::String(sid) => {
                // Quote + escape (incl. newlines) so SourceDebugExtension is correct.
                let raw = self.get_string(*sid as usize)?;
                Ok(format!("\"{}\"", DexRefResolver::escape_smali_string(&raw)))
            }
            EV::Type(tid) => {
                // JNI descriptor for classes: Lcom/...;  (this fixes MemberClasses arrays)
                let desc = self.type_desc(*tid as usize)?;
                Ok(desc)
            }
            EV::Boolean(b) => Ok(if *b { "true" } else { "false" }.to_string()),
            EV::Byte(x) => Ok(format!("{}", x)),
            EV::Short(x) => Ok(format!("{}", x)),
            EV::Char(x) => Ok(format!("{}", *x as u32)), // print as number
            EV::Int(x) => Ok(format!("{}", x)),
            EV::Long(x) => Ok(format!("{}", x)),
            EV::Float(f) => Ok(format!("{}", f)),
            EV::Double(d) => Ok(format!("{}", d)),
            EV::Null => Ok(String::from("null")),
            EV::Enum(field_idx) => {
                // Render as Lpkg/Enum;->NAME:Lpkg/Enum;
                if let Some(fi) = self.fields.get(*field_idx as usize) {
                    let class_desc = self.type_desc(fi.class_idx)?;
                    let name = self.get_string(fi.name_idx)?;
                    let ty_desc = self.type_desc(fi.type_idx)?;
                    Ok(format!("{}->{}:{}", class_desc, name, ty_desc))
                } else {
                    Ok(format!("enum@{}", field_idx))
                }
            }

            // Complex (arrays / sub-annotations) aren’t stringified here;
            // caller handles them specially so we can keep structure.
            other => Ok(other.to_string(&self.strings)),
        }
    }

    fn convert_annotation(&self, item: &AnnotationItem) -> Result<SmaliAnnotation, DexError> {
        use crate::dex::encoded_values::EncodedValue as EV;

        let vis = Self::convert_visibility(item.visibility);
        let ann_type_desc = self.type_desc(item.annotation.type_idx as usize)?;
        let ann_type = TypeSignature::from_jni(&ann_type_desc);

        let mut elements: Vec<SmaliAnnElement> = Vec::with_capacity(item.annotation.elements.len());
        for e in &item.annotation.elements {
            let name = self.strings[e.name_idx as usize].to_string()?;

            let value_av = match &e.value {
                EV::Array(vals) => {
                    // Arrays are rendered as a list of atoms (strings already quoted, types as L...;).
                    // This fixes MemberClasses: you’ll now get the real class names, not numeric ids.
                    let mut out: Vec<String> = Vec::with_capacity(vals.len());
                    for v in vals {
                        out.push(self.encoded_value_atom_to_smali(v)?);
                    }
                    SmaliAnnValue::Array(out)
                }

                EV::Annotation(sub) => {
                    // Nested annotation in a value: convert with a default visibility.
                    // (Visibility of nested annotation values isn’t printed; using Runtime is safe.)
                    let sub_type_desc = self.type_desc(sub.type_idx as usize)?;
                    let sub_type = TypeSignature::from_jni(&sub_type_desc);
                    let mut sub_elems: Vec<SmaliAnnElement> =
                        Vec::with_capacity(sub.elements.len());
                    for se in &sub.elements {
                        let sname = self.strings[se.name_idx as usize].to_string()?;
                        let sval = match &se.value {
                            EV::Array(vals) => {
                                let mut arr = Vec::with_capacity(vals.len());
                                for v in vals {
                                    arr.push(self.encoded_value_atom_to_smali(v)?);
                                }
                                SmaliAnnValue::Array(arr)
                            }
                            EV::Annotation(_) => {
                                // Nested-nested: recurse by building a pseudo AnnotationItem
                                let pseudo = AnnotationItem {
                                    visibility: 0x01,
                                    annotation: se.value.as_annotation().unwrap().clone(),
                                };
                                SmaliAnnValue::SubAnnotation(self.convert_annotation(&pseudo)?)
                            }
                            EV::Enum(field_idx) => {
                                if let Some(fi) = self.fields.get(*field_idx as usize) {
                                    let class_id = self.get_type(fi.class_idx)?; // ObjectIdentifier
                                    let ename = self.get_string(fi.name_idx)?;
                                    SmaliAnnValue::Enum(class_id, ename)
                                } else {
                                    SmaliAnnValue::Single(
                                        self.encoded_value_atom_to_smali(&se.value)?,
                                    )
                                }
                            }
                            // Strings and Types (and other primitives) → Single textual atom
                            _ => {
                                SmaliAnnValue::Single(self.encoded_value_atom_to_smali(&se.value)?)
                            }
                        };
                        sub_elems.push(SmaliAnnElement {
                            name: sname,
                            value: sval,
                        });
                    }
                    let sub_ann = SmaliAnnotation {
                        visibility: SmaliAnnVis::Runtime,
                        annotation_type: sub_type,
                        elements: sub_elems,
                    };
                    SmaliAnnValue::SubAnnotation(sub_ann)
                }

                EV::Enum(field_idx) => {
                    // Enum as top-level element value
                    if let Some(fi) = self.fields.get(*field_idx as usize) {
                        let class_id = self.get_type(fi.class_idx)?;
                        let ename = self.get_string(fi.name_idx)?;
                        SmaliAnnValue::Enum(class_id, ename)
                    } else {
                        SmaliAnnValue::Single(self.encoded_value_atom_to_smali(&e.value)?)
                    }
                }

                // Strings: ensure escaped/quoted (fixes SourceDebugExtension).
                EV::String(_) => SmaliAnnValue::Single(self.encoded_value_atom_to_smali(&e.value)?),

                // Single type refs (rare, but valid)
                EV::Type(_) => SmaliAnnValue::Single(self.encoded_value_atom_to_smali(&e.value)?),

                // All other primitives / fallbacks
                _ => SmaliAnnValue::Single(self.encoded_value_atom_to_smali(&e.value)?),
            };

            elements.push(SmaliAnnElement {
                name,
                value: value_av,
            });
        }

        Ok(SmaliAnnotation {
            visibility: vis,
            annotation_type: ann_type,
            elements,
        })
    }

    pub fn to_smali(&self) -> Result<Vec<SmaliClass>, DexError> {
        let mut smali_classes = vec![];
        // Derive API/ART from this DEX once, reuse for all method decodes
        let (api_level, art_version) = self.detect_api_and_art();

        for c in &self.class_defs {
            let mut smali = SmaliClass {
                name: self.get_type(c.class_idx)?,
                modifiers: Modifiers::from_u32(c.access_flags),
                source: if c.source_file_idx != NO_INDEX {
                    Some(self.get_string(c.source_file_idx)?)
                } else {
                    None
                },
                super_class: if c.superclass_idx != NO_INDEX {
                    self.get_type(c.superclass_idx)?
                } else {
                    ObjectIdentifier::from_jni_type("Ljava/lang/Object;")
                },
                implements: vec![],
                annotations: vec![],
                fields: vec![],
                methods: vec![],
                file_path: None,
            };

            // Any interfaces?
            if let Some(tl) = &c.interfaces {
                for t in &tl.0 {
                    smali.implements.push(self.get_type(*t)?);
                }
            }

            // Class annotations
            if let Some(dir) = &c.annotations {
                if dir.class_annotations_off != 0 {
                    let items = self.read_annotation_set_items(dir.class_annotations_off)?;
                    smali.annotations = items
                        .iter()
                        .map(|it| self.convert_annotation(it))
                        .collect::<Result<_, _>>()?;
                }
            }

            if let Some(class_data) = &c.class_data {
                // Static fields
                for (i, f) in class_data.static_fields.iter().enumerate() {
                    let dex_field = &self.fields[f.field_idx];

                    let mut field_annotations = vec![];
                    if let Some(dir) = &c.annotations {
                        for fa in &dir.field_annotations {
                            if fa.field_idx as usize == f.field_idx {
                                let items = self.read_annotation_set_items(fa.annotations_off)?;
                                field_annotations = items
                                    .iter()
                                    .map(|it| self.convert_annotation(it))
                                    .collect::<Result<_, _>>()?;
                                break;
                            }
                        }
                    }

                    smali.fields.push(SmaliField {
                        name: self.get_string(dex_field.name_idx)?,
                        modifiers: Modifiers::from_u32(f.access_flags),
                        signature: TypeSignature::from_jni(
                            &self.get_string(self.types[dex_field.type_idx])?,
                        ),
                        initial_value: if let Some(s) = &c.static_values {
                            if i < s.len() {
                                match s[i] {
                                    EncodedValue::Null => None,
                                    EncodedValue::String(sid) => {
                                        let raw = self.get_string(sid as usize)?;
                                        Some(format!(
                                            "\"{}\"",
                                            DexRefResolver::escape_smali_string(&raw)
                                        ))
                                    }
                                    _ => Some(s[i].to_string(&self.strings)),
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        },
                        annotations: field_annotations,
                    });
                }

                // Instance fields
                for (_i, f) in class_data.instance_fields.iter().enumerate() {
                    let dex_field = &self.fields[f.field_idx];

                    let mut field_annotations = vec![];
                    if let Some(dir) = &c.annotations {
                        for fa in &dir.field_annotations {
                            if fa.field_idx as usize == f.field_idx {
                                let items = self.read_annotation_set_items(fa.annotations_off)?;
                                field_annotations = items
                                    .iter()
                                    .map(|it| self.convert_annotation(it))
                                    .collect::<Result<_, _>>()?;
                                break;
                            }
                        }
                    }

                    smali.fields.push(SmaliField {
                        name: self.get_string(dex_field.name_idx)?,
                        modifiers: Modifiers::from_u32(f.access_flags),
                        signature: TypeSignature::from_jni(
                            &self.get_string(self.types[dex_field.type_idx])?,
                        ),
                        initial_value: None,
                        annotations: field_annotations,
                    });
                }
            }

            // Methods (direct + virtual), no disassembly yet
            let mut all_methods: Vec<&EncodedMethod> = vec![];
            if let Some(class_data) = &c.class_data {
                for m in &class_data.direct_methods {
                    all_methods.push(m);
                }
                for m in &class_data.virtual_methods {
                    all_methods.push(m);
                }
            }

            // Build quick lookup maps for method and parameter annotations for this class
            let mut method_ann_map: HashMap<usize, Vec<SmaliAnnotation>> = HashMap::new();
            let mut param_ann_map: HashMap<usize, Vec<Vec<SmaliAnnotation>>> = HashMap::new();
            if let Some(dir) = &c.annotations {
                // Method annotations
                for ma in &dir.method_annotations {
                    let items = self.read_annotation_set_items(ma.annotations_off)?;
                    let converted = items
                        .iter()
                        .map(|it| self.convert_annotation(it))
                        .collect::<Result<Vec<_>, _>>()?;
                    method_ann_map.insert(ma.method_idx as usize, converted);
                }
                // Parameter annotations
                for pa in &dir.parameter_annotations {
                    if pa.annotations_off != 0 {
                        let mut ixp = pa.annotations_off as usize;
                        let ref_list = AnnotationSetRefList::read(&self.data, &mut ixp)?;
                        let mut per_param: Vec<Vec<SmaliAnnotation>> =
                            Vec::with_capacity(ref_list.list.len());
                        for &set_off in &ref_list.list {
                            if set_off == 0 {
                                per_param.push(vec![]);
                                continue;
                            }
                            let items = self.read_annotation_set_items(set_off)?;
                            let converted = items
                                .iter()
                                .map(|it| self.convert_annotation(it))
                                .collect::<Result<Vec<_>, _>>()?;
                            per_param.push(converted);
                        }
                        param_ann_map.insert(pa.method_idx as usize, per_param);
                    }
                }
            }

            for m in all_methods {
                let mi = &self.methods[m.method_idx];
                let name = self.get_string(mi.name_idx)?;
                let proto = &self.prototypes[mi.proto_idx];

                // Build JNI method signature string
                let mut sig = String::new();
                sig.push('(');
                for &t in &proto.parameters.0 {
                    sig.push_str(&self.get_string(self.types[t])?);
                }
                sig.push(')');
                sig.push_str(&self.get_string(self.types[proto.return_type_idx])?);
                let signature = MethodSignature::from_jni(&sig);

                let is_static = (m.access_flags & ACC_STATIC) != 0;
                let mut params: Vec<SmaliParam> = Vec::with_capacity(proto.parameters.0.len());
                for (i, _t) in proto.parameters.0.iter().enumerate() {
                    let reg_name = format!("p{}", if is_static { i } else { i + 1 });
                    params.push(SmaliParam {
                        name: None,
                        register: reg_name,
                        annotations: vec![],
                    });
                }

                // Attach method annotations
                let annotations = method_ann_map.remove(&m.method_idx).unwrap_or_default();

                // Attach parameter annotations where available
                if let Some(per_param) = param_ann_map.remove(&m.method_idx) {
                    for (i, annos) in per_param.into_iter().enumerate() {
                        if i < params.len() {
                            params[i].annotations = annos;
                        }
                    }
                }

                // Compute locals from code item if present: registers_size - ins_size
                let (locals, ops) = if let Some(ci) = &m.code {
                    let locals_calc = ci.registers_size.saturating_sub(ci.args_in_size) as u32;
                    // Convert code units (u16 LE) back into raw bytes for the decoder
                    let mut bc: Vec<u8> = Vec::with_capacity(ci.instructions.len() * 2);
                    for &u in &ci.instructions {
                        bc.push((u & 0x00FF) as u8);
                        bc.push((u >> 8) as u8);
                    }
                    // Decode and propagate errors with method/class context
                    let class_desc = self.get_string(self.types[mi.class_idx])?;

                    let resolver = DexRefResolver { dex: self };
                    let regmap = RegMapper {
                        registers_size: ci.registers_size,
                        ins_size: ci.args_in_size,
                    };

                    let decoded =
                        decode_with_ctx(&bc, api_level, art_version, &resolver, Some(&regmap))
                            .map_err(|e| {
                                DexError::with_context(
                                    e,
                                    format!("while decoding {}->{}{}", class_desc, name, sig),
                                )
                            })?;
                    (locals_calc, decoded)
                } else {
                    (0, Vec::new())
                };

                smali.methods.push(SmaliMethod {
                    name,
                    modifiers: Modifiers::from_u32(m.access_flags),
                    constructor: false, // could set true if name == "<init>", keep false for now
                    signature,
                    locals,
                    params,
                    annotations,
                    ops,
                });
            }

            smali_classes.push(smali);
        }

        Ok(smali_classes)
    }

    /// Return the numeric DEX version from the header magic, e.g. 35, 37, 38, 39, 40, 41.
    fn dex_version(&self) -> u32 {
        let d0 = self.header.magic[4];
        let d1 = self.header.magic[5];
        let d2 = self.header.magic[6];
        if d0.is_ascii_digit() && d1.is_ascii_digit() && d2.is_ascii_digit() {
            ((d0 - b'0') as u32) * 100 + ((d1 - b'0') as u32) * 10 + ((d2 - b'0') as u32)
        } else {
            35
        }
    }

    /// Best-effort mapping from DEX version to Android API level, plus detection of quickened opcodes.
    /// ART version is 0 for normal DEX, 1 if quickened/odex-style instructions are detected.
    fn detect_api_and_art(&self) -> (i32, i32) {
        let dex_ver = self.dex_version();
        // Conservative mapping; good enough for opcode tables
        let api = match dex_ver {
            35 => 19, // pre-L up to KitKat
            36 => 20, // if observed
            37 => 21, // Lollipop
            38 => 24, // Nougat
            39 => 26, // Oreo
            40 => 28, // Pie
            41 => 29, // Android 10
            _ => 33,  // default to recent
        };

        // Heuristic quick/odex detection: look for known quick opcodes in low byte
        let mut has_quick = false;
        'scan: for cdef in &self.class_defs {
            if let Some(cd) = &cdef.class_data {
                for m in cd.direct_methods.iter().chain(cd.virtual_methods.iter()) {
                    if let Some(code) = &m.code {
                        for &cu in &code.instructions {
                            let opc = (cu & 0x00FF) as u8;
                            match opc {
                                0x60..=0x65   // iget/iput quick family
                                | 0x90..=0x93 // invoke-virtual/super quick (+/range)
                                | 0xEC..=0xEF // execute-inline, throw-verification-error (odexy)
                                => { has_quick = true; break 'scan; }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }
        let art_version = if has_quick { 1 } else { 0 };
        (api, art_version)
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<DexFile, DexError> {
        let mut ix = 0;
        DexFile::read(bytes, &mut ix)
    }

    pub fn from_file(path: &Path) -> Result<DexFile, DexError> {
        let bytes = fs::read(path).map_err(|e| DexError::new(&format!("io Error: {}", e)))?;
        DexFile::from_bytes(&bytes)
    }
}

struct DexRefResolver<'a> {
    dex: &'a DexFile,
}

impl DexRefResolver<'_> {
    fn escape_smali_string(s: &str) -> String {
        let mut out = String::with_capacity(s.len() + 8);
        for ch in s.chars() {
            match ch {
                '\\' => out.push_str("\\\\"),
                '"' => out.push_str("\\\""),
                '\n' => out.push_str("\\n"),
                '\r' => out.push_str("\\r"),
                '\t' => out.push_str("\\t"),
                // ASCII control chars (C0)
                c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
                // C1 control block 0x7F..0x9F
                c if (0x7F..=0x9F).contains(&(c as u32)) => {
                    out.push_str(&format!("\\u{:04x}", c as u32))
                }
                // Any Unicode whitespace other than regular ASCII space should be escaped
                c if c.is_whitespace() && c != ' ' => out.push_str(&format!("\\u{:04x}", c as u32)),
                // Safe to emit directly
                c => out.push(c),
            }
        }
        out
    }

    fn string_idx(&self, sid: usize) -> String {
        self.dex
            .strings
            .get(sid)
            .and_then(|ds| ds.to_string().ok())
            .unwrap_or_else(|| format!("string@{}", sid))
    }

    fn type_desc_idx(&self, tid: usize) -> String {
        self.dex
            .types
            .get(tid)
            .and_then(|sid| self.dex.strings.get(*sid))
            .and_then(|ds| ds.to_string().ok())
            .unwrap_or_else(|| format!("Ltype@{};", tid))
    }

    fn proto_desc(&self, pid: usize) -> String {
        if let Some(p) = self.dex.prototypes.get(pid) {
            let mut s = String::from("(");
            for &t in &p.parameters.0 {
                s.push_str(&self.type_desc_idx(t));
            }
            s.push(')');
            s.push_str(&self.type_desc_idx(p.return_type_idx));
            s
        } else {
            String::from("()V")
        }
    }
}

impl RefResolver for DexRefResolver<'_> {
    fn string(&self, idx: u32) -> String {
        let raw = self.string_idx(idx as usize);
        format!("\"{}\"", Self::escape_smali_string(&raw))
    }
    fn type_desc(&self, idx: u32) -> String {
        self.type_desc_idx(idx as usize)
    }
    fn field_ref(&self, idx: u32) -> (String, String, String) {
        let i = idx as usize;
        if let Some(f) = self.dex.fields.get(i) {
            let class_desc = if f.class_idx < self.dex.types.len() {
                self.type_desc_idx(f.class_idx)
            } else {
                warn!(
                    "[resolver] field_ref {}: class_idx {} OOB (types.len={})",
                    idx,
                    f.class_idx,
                    self.dex.types.len()
                );
                format!("Ltype@{};", f.class_idx)
            };
            let name = if f.name_idx < self.dex.strings.len() {
                self.string_idx(f.name_idx)
            } else {
                warn!(
                    "[resolver] field_ref {}: name_idx {} OOB (strings.len={})",
                    idx,
                    f.name_idx,
                    self.dex.strings.len()
                );
                format!("field@{}", f.name_idx)
            };
            let ty_desc = if f.type_idx < self.dex.types.len() {
                self.type_desc_idx(f.type_idx)
            } else {
                warn!(
                    "[resolver] field_ref {}: type_idx {} OOB (types.len={})",
                    idx,
                    f.type_idx,
                    self.dex.types.len()
                );
                format!("Ltype@{};", f.type_idx)
            };
            (class_desc, name, ty_desc)
        } else {
            warn!(
                "[resolver] field_ref {} OOB (fields.len={})",
                idx,
                self.dex.fields.len()
            );
            (
                format!("Lclass@{};", idx),
                format!("field@{}", idx),
                String::from("Ljava/lang/Object;"),
            )
        }
    }
    fn method_ref(&self, idx: u32) -> (String, String, String) {
        let i = idx as usize;
        if let Some(m) = self.dex.methods.get(i) {
            let class_desc = if m.class_idx < self.dex.types.len() {
                self.type_desc_idx(m.class_idx)
            } else {
                warn!(
                    "[resolver] method_ref {}: class_idx {} OOB (types.len={})",
                    idx,
                    m.class_idx,
                    self.dex.types.len()
                );
                format!("Ltype@{};", m.class_idx)
            };
            let name = if m.name_idx < self.dex.strings.len() {
                self.string_idx(m.name_idx)
            } else {
                warn!(
                    "[resolver] method_ref {}: name_idx {} OOB (strings.len={})",
                    idx,
                    m.name_idx,
                    self.dex.strings.len()
                );
                format!("method@{}", m.name_idx)
            };
            let proto = if m.proto_idx < self.dex.prototypes.len() {
                self.proto_desc(m.proto_idx)
            } else {
                warn!(
                    "[resolver] method_ref {}: proto_idx {} OOB (protos.len={})",
                    idx,
                    m.proto_idx,
                    self.dex.prototypes.len()
                );
                String::from("()V")
            };
            (class_desc, name, proto)
        } else {
            warn!(
                "[resolver] method_ref {} OOB (methods.len={})",
                idx,
                self.dex.methods.len()
            );
            (
                format!("Lclass@{};", idx),
                format!("method@{}", idx),
                String::from("()V"),
            )
        }
    }
    fn call_site(&self, idx: u32) -> String {
        format!("callsite@{}", idx)
    }
    fn method_handle(&self, idx: u32) -> String {
        format!("handle@{}", idx)
    }
    fn proto(&self, idx: u32) -> String {
        self.proto_desc(idx as usize)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Header {
    pub magic: [u8; 8],
    pub checksum: u32,
    pub signature: [u8; 20],
    pub file_size: u32,
    pub header_size: u32,
    pub endian_tag: u32,
    pub link_size: u32,
    pub link_off: u32,
    pub map_off: u32,
    pub string_ids_size: u32,
    pub string_ids_off: u32,
    pub type_ids_size: u32,
    pub type_ids_off: u32,
    pub proto_ids_size: u32,
    pub proto_ids_off: u32,
    pub field_ids_size: u32,
    pub field_ids_off: u32,
    pub method_ids_size: u32,
    pub method_ids_off: u32,
    pub class_defs_size: u32,
    pub class_defs_off: u32,
    pub data_size: u32,
    pub data_off: u32,
}

impl Header {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<Header, DexError> {
        if bytes.len() < 0x70 {
            return Err(DexError::new("Not enough bytes for header"));
        }

        let magic = <[u8; 8]>::try_from(read_x(bytes, ix, 8)?).unwrap();
        if magic[0] != 0x64 || magic[1] != 0x65 || magic[2] != 0x78 {
            return Err(DexError::new("Invalid magic value"));
        }

        Ok(Header {
            magic,
            checksum: read_u4(bytes, ix)?,
            signature: <[u8; 20]>::try_from(read_x(bytes, ix, 20)?).unwrap(),
            file_size: read_u4(bytes, ix)?,
            header_size: read_u4(bytes, ix)?,
            endian_tag: read_u4(bytes, ix)?,
            link_size: read_u4(bytes, ix)?,
            link_off: read_u4(bytes, ix)?,
            map_off: read_u4(bytes, ix)?,
            string_ids_size: read_u4(bytes, ix)?,
            string_ids_off: read_u4(bytes, ix)?,
            type_ids_size: read_u4(bytes, ix)?,
            type_ids_off: read_u4(bytes, ix)?,
            proto_ids_size: read_u4(bytes, ix)?,
            proto_ids_off: read_u4(bytes, ix)?,
            field_ids_size: read_u4(bytes, ix)?,
            field_ids_off: read_u4(bytes, ix)?,
            method_ids_size: read_u4(bytes, ix)?,
            method_ids_off: read_u4(bytes, ix)?,
            class_defs_size: read_u4(bytes, ix)?,
            class_defs_off: read_u4(bytes, ix)?,
            data_size: read_u4(bytes, ix)?,
            data_off: read_u4(bytes, ix)?,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_x(bytes, &self.magic);
        c += write_u4(bytes, self.checksum);
        c += write_x(bytes, &self.signature);
        c += write_u4(bytes, self.file_size);
        c += write_u4(bytes, self.header_size);
        c += write_u4(bytes, self.endian_tag);
        c += write_u4(bytes, self.link_size);
        c += write_u4(bytes, self.link_off);
        c += write_u4(bytes, self.map_off);
        c += write_u4(bytes, self.string_ids_size);
        c += write_u4(bytes, self.string_ids_off);
        c += write_u4(bytes, self.type_ids_size);
        c += write_u4(bytes, self.type_ids_off);
        c += write_u4(bytes, self.proto_ids_size);
        c += write_u4(bytes, self.proto_ids_off);
        c += write_u4(bytes, self.field_ids_size);
        c += write_u4(bytes, self.field_ids_off);
        c += write_u4(bytes, self.method_ids_size);
        c += write_u4(bytes, self.method_ids_off);
        c += write_u4(bytes, self.class_defs_size);
        c += write_u4(bytes, self.class_defs_off);
        c += write_u4(bytes, self.data_size);
        c += write_u4(bytes, self.data_off);
        c
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum DexString {
    Decoded(String),
    Raw(u32, Vec<u8>),
}

impl DexString {
    pub fn from_string(s: &str) -> DexString {
        DexString::Decoded(s.to_string())
    }

    pub fn to_string(&self) -> Result<String, DexError> {
        match &self {
            DexString::Decoded(s) => Ok(s.to_string()),
            DexString::Raw(_, _) => Err(DexError::new("DexString failed conversion")),
        }
    }

    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<DexString, DexError> {
        let utf16_size = read_uleb128(bytes, ix)?;
        let mut v = vec![];

        loop {
            let u = read_u1(bytes, ix)?;
            if u != 0 {
                v.push(u);
            } else {
                break;
            }
        }

        Ok(match cesu8::from_java_cesu8(v.as_slice()) {
            Ok(converted_str) => DexString::Decoded(converted_str.to_string()),
            _ => DexString::Raw(utf16_size, v),
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;

        match self {
            DexString::Raw(utf16_size, v) => {
                c += write_uleb128(bytes, *utf16_size);
                c += write_x(bytes, v);
                c += write_u1(bytes, 0);
            }

            DexString::Decoded(s) => {
                let encoded = to_java_cesu8(s).to_vec();
                let utf16_len = s.encode_utf16().count() as u32;
                c += write_uleb128(bytes, utf16_len);
                c += write_x(bytes, encoded.as_slice());
                c += write_u1(bytes, 0);
            }
        }
        c
    }

    pub fn is_decoded(&self) -> bool {
        matches!(self, DexString::Decoded(_))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::read;

    #[test]
    fn test_header_from_bytes() {
        let dex_path = "tests/classes.dex";
        let dex_bytes = read(dex_path).expect("Failed to read DEX file");
        let mut ix = 0;
        let header = Header::read(&dex_bytes, &mut ix).expect("Failed to parse DEX header");
        let mut encoded_bytes = vec![];
        header.write(&mut encoded_bytes);
        ix = 0;
        let decoded = Header::read(encoded_bytes.as_slice(), &mut ix).unwrap();

        println!("{:x?}", header);

        assert_eq!(encoded_bytes.len(), 0x70);
        assert_eq!(header, decoded);
    }

    #[test]
    fn test_decode_dexfile() {
        let dex_path = "tests/classes.dex";
        let dex_bytes = read(dex_path).expect("Failed to read DEX file");
        let mut ix = 0;
        let dex = DexFile::read(dex_bytes.as_slice(), &mut ix).expect("Failed read");
        println!(
            "Strings: {:} [header: {:}]",
            dex.strings.len(),
            dex.header.string_ids_size
        );
        println!(
            "Types: {:} [header: {:}]",
            dex.types.len(),
            dex.header.type_ids_size
        );
        println!(
            "Prototypes: {:} [header: {:}]",
            dex.prototypes.len(),
            dex.header.proto_ids_size
        );
        println!(
            "Fields: {:} [header: {:}]",
            dex.fields.len(),
            dex.header.field_ids_size
        );
        println!(
            "Methods: {:} [header: {:}]",
            dex.methods.len(),
            dex.header.method_ids_size
        );
        println!(
            "Classes: {:} [header: {:}]",
            dex.class_defs.len(),
            dex.header.class_defs_size
        );

        let smali = dex.to_smali().expect("Failed to generate smali");
    }
    #[test]
    fn test_try_item_roundtrip() {
        let t = TryItem {
            start_addr: 0x12345678,
            insn_count: 0x0102,
            handler_off: 0x2030,
            handler_idx: None,
        };
        let mut bytes = vec![];
        let written = t.write(&mut bytes);
        assert_eq!(written, 8); // 4 + 2 + 2

        let mut ix = 0;
        let t2 = TryItem::read(&bytes, &mut ix).expect("TryItem read failed");
        assert_eq!(ix, bytes.len());
        assert_eq!(t, t2);
    }

    #[test]
    fn test_encoded_catch_handler_roundtrip_no_catch_all() {
        let h = EncodedCatchHandler {
            handlers: vec![
                EncodedTypeAddrPair {
                    type_idx: 3,
                    addr: 0x00000100,
                },
                EncodedTypeAddrPair {
                    type_idx: 7,
                    addr: 0x00002222,
                },
            ],
            catch_all_addr: None,
        };

        let mut bytes = vec![];
        let _ = h.write(&mut bytes);

        let mut ix = 0;
        let h2 =
            EncodedCatchHandler::read(&bytes, &mut ix).expect("EncodedCatchHandler read failed");
        assert_eq!(ix, bytes.len());
        assert_eq!(h, h2);
    }

    #[test]
    fn test_encoded_catch_handler_roundtrip_with_catch_all() {
        let h = EncodedCatchHandler {
            handlers: vec![EncodedTypeAddrPair {
                type_idx: 42,
                addr: 0x0000ABCD,
            }],
            catch_all_addr: Some(0x00001234),
        };

        let mut bytes = vec![];
        let _ = h.write(&mut bytes);

        // First varint should encode a negative count since catch_all is present
        // Decode with the crate's SLEB128 to ensure sign is negative
        let (size_signed, used0) = crate::dex::leb::decode_sleb128(&bytes);
        assert!(
            size_signed < 0,
            "first SLEB128 should be negative when catch_all present"
        );
        assert_eq!((-size_signed) as usize, h.handlers.len());
        assert!((1..=5).contains(&used0));

        let mut ix = 0;
        let h2 =
            EncodedCatchHandler::read(&bytes, &mut ix).expect("EncodedCatchHandler read failed");
        assert_eq!(ix, bytes.len());
        assert_eq!(h, h2);
    }

    #[test]
    fn test_debug_info_write_encodes_params_and_opcodes() {
        let di = DebugInfo {
            line_start: 123,
            parameter_names: vec![Some(5), None],
            debug_opcodes: vec![0x0A, 0x0B],
        };
        let mut buf = Vec::new();
        let written = di.write(&mut buf);
        assert_eq!(written, buf.len());

        let mut ix = 0;
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 123);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 2);
        assert_eq!(read_uleb128p1(&buf, &mut ix).unwrap(), 5);
        assert_eq!(read_uleb128p1(&buf, &mut ix).unwrap(), -1);
        assert_eq!(&buf[ix..ix + 2], &[0x0A, 0x0B]);
        ix += 2;
        assert_eq!(buf[ix], DBG_END_SEQUENCE);
    }

    #[test]
    fn test_debug_info_write_does_not_duplicate_end_sequence() {
        let di = DebugInfo {
            line_start: 0,
            parameter_names: vec![],
            debug_opcodes: vec![DBG_END_SEQUENCE],
        };
        let mut buf = Vec::new();
        di.write(&mut buf);

        assert_eq!(buf.last().copied(), Some(DBG_END_SEQUENCE));
        assert_eq!(buf.len(), 3); // line_start + parameters_size + terminator
        assert_eq!(buf[2], DBG_END_SEQUENCE);
    }

    #[test]
    fn test_prototype_item_write_records_offsets() {
        let proto = PrototypeItem {
            shorty_idx: 3,
            return_type_idx: 5,
            parameters: TypeList(vec![1, 2, 3]),
        };
        let mut buf = Vec::new();
        let written = proto.write(&mut buf, 0x1122_3344);
        assert_eq!(written, 12);

        let mut ix = 0;
        assert_eq!(read_u4(&buf, &mut ix).unwrap(), 3);
        assert_eq!(read_u4(&buf, &mut ix).unwrap(), 5);
        assert_eq!(read_u4(&buf, &mut ix).unwrap(), 0x1122_3344);
    }

    #[test]
    fn test_encoded_field_write_differential_encoding() {
        let f1 = EncodedField {
            field_idx: 2,
            access_flags: 0x1,
        };
        let f2 = EncodedField {
            field_idx: 5,
            access_flags: 0x2,
        };
        let mut last: FieldId = 0;
        let mut buf = Vec::new();
        f1.write(&mut buf, &mut last);
        f2.write(&mut buf, &mut last);

        let mut ix = 0;
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 2);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0x1);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 3);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0x2);
    }

    #[test]
    fn test_encoded_method_write_differential_encoding() {
        let m1 = EncodedMethod {
            method_idx: 4,
            access_flags: 0x100,
            code_off: 0x30,
            code: None,
        };
        let m2 = EncodedMethod {
            method_idx: 7,
            access_flags: 0x200,
            code_off: 0,
            code: None,
        };
        let mut last: MethodId = 0;
        let mut buf = Vec::new();
        m1.write(&mut buf, &mut last);
        m2.write(&mut buf, &mut last);

        let mut ix = 0;
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 4);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0x100);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0x30);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 3);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0x200);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0);
    }

    #[test]
    fn test_code_item_write_computes_handler_offsets_and_debug_info_base() {
        let code = CodeItem {
            registers_size: 2,
            args_in_size: 0,
            args_out_size: 0,
            tries_size: 0,
            debug_info: Some(DebugInfo {
                line_start: 1,
                parameter_names: vec![],
                debug_opcodes: vec![],
            }),
            instructions: vec![0x0001, 0x0002, 0x0003],
            padding: 0,
            tries: vec![TryItem {
                start_addr: 0,
                insn_count: 1,
                handler_off: 0,
                handler_idx: Some(0),
            }],
            handlers: vec![EncodedCatchHandler {
                handlers: vec![EncodedTypeAddrPair {
                    type_idx: 1,
                    addr: 0x10,
                }],
                catch_all_addr: None,
            }],
        };

        let mut buf = Vec::new();
        let base = 0x2000;
        let written = code.write(&mut buf, base).expect("write succeeds");
        assert_eq!(written, buf.len());

        let mut ix = 0;
        assert_eq!(read_u2(&buf, &mut ix).unwrap(), 2); // registers_size
        assert_eq!(read_u2(&buf, &mut ix).unwrap(), 0); // args_in_size
        assert_eq!(read_u2(&buf, &mut ix).unwrap(), 0); // args_out_size
        assert_eq!(read_u2(&buf, &mut ix).unwrap(), 1); // tries_size

        let debug_info_off = read_u4(&buf, &mut ix).unwrap();
        let insn_count = read_u4(&buf, &mut ix).unwrap() as usize;
        assert_eq!(insn_count, 3);
        ix += insn_count * 2; // instructions

        let padding = read_u2(&buf, &mut ix).unwrap();
        assert_eq!(padding, 0);

        // try_item
        assert_eq!(read_u4(&buf, &mut ix).unwrap(), 0);
        assert_eq!(read_u2(&buf, &mut ix).unwrap(), 1);
        let handler_off = read_u2(&buf, &mut ix).unwrap();
        assert_eq!(
            handler_off, 1,
            "expected handler offset after handlers_size prefix"
        );

        let handler_count = read_uleb128(&buf, &mut ix).unwrap();
        assert_eq!(handler_count, 1);
        EncodedCatchHandler::read(&buf, &mut ix).expect("handler read");

        let debug_info_start = ix;
        assert_eq!(debug_info_off, base + debug_info_start as u32);

        // Validate debug info payload: line_start=1, parameters_size=0, terminator
        let mut di_ix = debug_info_start;
        assert_eq!(read_uleb128(&buf, &mut di_ix).unwrap(), 1);
        assert_eq!(read_uleb128(&buf, &mut di_ix).unwrap(), 0);
        assert_eq!(buf[di_ix], DBG_END_SEQUENCE);
    }

    #[test]
    fn test_class_data_item_write_encodes_fields_and_methods() {
        let class_data = ClassDataItem {
            static_fields: vec![EncodedField {
                field_idx: 2,
                access_flags: 0x1,
            }],
            instance_fields: vec![EncodedField {
                field_idx: 4,
                access_flags: 0x2,
            }],
            direct_methods: vec![EncodedMethod {
                method_idx: 5,
                access_flags: 0x0101,
                code_off: 0x40,
                code: None,
            }],
            virtual_methods: vec![EncodedMethod {
                method_idx: 7,
                access_flags: 0x0200,
                code_off: 0,
                code: None,
            }],
        };

        let mut buf = Vec::new();
        let written = class_data.write(&mut buf);
        assert_eq!(written, buf.len());

        let mut ix = 0;
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 1); // static fields
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 1); // instance fields
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 1); // direct methods
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 1); // virtual methods

        // static field diffs
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 2);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0x1);

        // instance field diffs
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 4);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0x2);

        // direct method
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 5);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0x0101);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0x40);

        // virtual method
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 7);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0x0200);
        assert_eq!(read_uleb128(&buf, &mut ix).unwrap(), 0);

        assert_eq!(ix, buf.len());
    }

    #[test]
    fn test_typelist_write_includes_padding_for_odd_length() {
        let list = TypeList(vec![1, 2, 3]);
        let mut buf = Vec::new();
        let written = list.write(&mut buf);

        assert_eq!(written, buf.len());
        assert_eq!(buf.len() % 4, 0, "type_list must be 4-byte aligned");

        let mut ix = 0;
        let size = read_u4(&buf, &mut ix).expect("size read");
        assert_eq!(size, 3);
        // consume items
        ix += 2 * 3;
        assert_eq!(&buf[ix..], &[0x00, 0x00]);
    }

    #[test]
    fn test_dexstring_write_reports_utf16_length() {
        let s = "😀"; // surrogate pair → 2 UTF-16 code units
        let ds = DexString::from_string(s);
        let mut buf = Vec::new();
        ds.write(&mut buf);

        let mut ix = 0;
        let utf16_len = read_uleb128(&buf, &mut ix).expect("utf16 len");
        assert_eq!(utf16_len, 2);
    }
}
