#[cfg(test)]
mod tests {
    use crate::dex::builder::{build_dex_file_bytes, debug_info_offsets_by_method, DexLayoutPlan};
    use crate::dex::dex_file::DexFile;
    use crate::dex::{read_u1, read_u4, read_uleb128, read_uleb128p1};
    use crate::find_smali_files;
    use std::path::PathBuf;

    #[test]
    fn debug_info_streams_are_valid() {
        let dir = PathBuf::from("tests/smali_classes11");
        let classes = find_smali_files(&dir).expect("load smali classes");
        let plan = DexLayoutPlan::from_classes(&classes).expect("build dex layout plan");
        let expected_offsets = debug_info_offsets_by_method(&plan);
        let bytes = build_dex_file_bytes(&classes).expect("build dex bytes");
        let dex = DexFile::from_bytes(&bytes).expect("parse dex");
        if dex.types.len() != plan.ids.type_ids.len() {
            panic!(
                "type_ids size mismatch: dex={}, plan={}",
                dex.types.len(),
                plan.ids.type_ids.len()
            );
        }
        if dex.strings.len() != plan.ids.strings.len() {
            panic!(
                "string_ids size mismatch: dex={}, plan={}",
                dex.strings.len(),
                plan.ids.strings.len()
            );
        }

        for class_def in &dex.class_defs {
            let class_data = match &class_def.class_data {
                Some(data) => data,
                None => continue,
            };
            for method in class_data
                .direct_methods
                .iter()
                .chain(class_data.virtual_methods.iter())
            {
                if method.code_off == 0 {
                    continue;
                }
                let debug_info_off = read_debug_info_off(&bytes, method.code_off as usize)
                    .expect("read debug_info_off");
                let method_id = method_id_string(&dex, method.method_idx);
                let expected = expected_offsets.get(&method.method_idx).copied().unwrap_or(0);
                if expected == 0 && debug_info_off == 0 {
                    continue;
                }
                if expected == 0 {
                    panic!(
                        "unexpected debug_info_off {} for {} (no debug info chunk)",
                        debug_info_off, method_id
                    );
                }
                if debug_info_off == 0 {
                    panic!(
                        "missing debug_info_off for {} (expected {})",
                        method_id, expected
                    );
                }
                if debug_info_off != expected {
                    panic!(
                        "debug_info_off mismatch for {}: got {}, expected {}",
                        method_id, debug_info_off, expected
                    );
                }
                validate_debug_info_at_offset(&bytes, &dex, debug_info_off as usize)
                    .unwrap_or_else(|err| {
                        panic!(
                            "invalid debug info stream for {} at offset {}: {}",
                            method_id, debug_info_off, err
                        )
                    });
            }
        }
    }

    fn read_debug_info_off(bytes: &[u8], code_off: usize) -> Result<u32, crate::dex::error::DexError> {
        if code_off + 12 > bytes.len() {
            return Err(crate::dex::error::DexError::new("code_off out of bounds"));
        }
        let mut ix = code_off + 8;
        read_u4(bytes, &mut ix)
    }

    fn validate_debug_info_at_offset(
        bytes: &[u8],
        dex: &DexFile,
        offset: usize,
    ) -> Result<(), crate::dex::error::DexError> {
        if offset >= bytes.len() {
            return Err(crate::dex::error::DexError::new("debug_info_off out of bounds"));
        }
        let mut ix = offset;
        let _line_start = read_uleb128(bytes, &mut ix)?;
        let parameters_size = read_uleb128(bytes, &mut ix)?;
        for _ in 0..parameters_size {
            let idx = read_uleb128p1(bytes, &mut ix)?;
            validate_string_index(dex, idx, "parameter name")?;
        }
        let mut op_index = 0usize;
        loop {
            let opcode = read_u1(bytes, &mut ix)?;
            op_index += 1;
            match opcode {
                0x00 => break, // DBG_END_SEQUENCE
                0x01 => {
                    let _ = read_uleb128(bytes, &mut ix)?;
                }
                0x02 => {
                    let _ = read_sleb128_local(bytes, &mut ix, "advance_line");
                }
                0x03 => {
                    let reg = read_uleb128(bytes, &mut ix)?;
                    let name_idx = read_uleb128p1(bytes, &mut ix)?;
                    let type_idx = read_uleb128p1(bytes, &mut ix)?;
                    validate_string_index(dex, name_idx, "local name")?;
                    validate_type_index(dex, type_idx, "local type").map_err(|e| {
                        let name = if name_idx >= 0 {
                            dex.strings
                                .get(name_idx as usize)
                                .and_then(|s| s.to_string().ok())
                                .unwrap_or_else(|| "<invalid>".to_string())
                        } else {
                            "<none>".to_string()
                        };
                        crate::dex::error::DexError::new(&format!(
                            "{} (reg {}, name '{}', op {}, offset {})",
                            e, reg, name, op_index, ix
                        ))
                    })?;
                }
                0x04 => {
                    let reg = read_uleb128(bytes, &mut ix)?;
                    let name_idx = read_uleb128p1(bytes, &mut ix)?;
                    let type_idx = read_uleb128p1(bytes, &mut ix)?;
                    let sig_idx = read_uleb128p1(bytes, &mut ix)?;
                    validate_string_index(dex, name_idx, "local name")?;
                    validate_type_index(dex, type_idx, "local type").map_err(|e| {
                        let name = if name_idx >= 0 {
                            dex.strings
                                .get(name_idx as usize)
                                .and_then(|s| s.to_string().ok())
                                .unwrap_or_else(|| "<invalid>".to_string())
                        } else {
                            "<none>".to_string()
                        };
                        crate::dex::error::DexError::new(&format!(
                            "{} (reg {}, name '{}', op {}, offset {})",
                            e, reg, name, op_index, ix
                        ))
                    })?;
                    validate_string_index(dex, sig_idx, "local signature")?;
                }
                0x05 | 0x06 => {
                    let _ = read_uleb128(bytes, &mut ix)?;
                }
                0x07 | 0x08 => {}
                0x09 => {
                    let idx = read_uleb128p1(bytes, &mut ix)?;
                    validate_string_index(dex, idx, "source file")?;
                }
                other => {
                    if other >= 0x0a {
                        // special opcode, no payload
                    } else {
                        return Err(crate::dex::error::DexError::new(&format!(
                            "unknown debug opcode 0x{other:02x}"
                        )));
                    }
                }
            }
        }
        Ok(())
    }


    fn validate_string_index(
        dex: &DexFile,
        idx: i32,
        context: &str,
    ) -> Result<(), crate::dex::error::DexError> {
        if idx < 0 {
            return Ok(());
        }
        let idx = idx as usize;
        if idx >= dex.strings.len() {
            return Err(crate::dex::error::DexError::new(&format!(
                "{context} index out of range: {idx} >= {}",
                dex.strings.len()
            )));
        }
        Ok(())
    }

    fn validate_type_index(
        dex: &DexFile,
        idx: i32,
        context: &str,
    ) -> Result<(), crate::dex::error::DexError> {
        if idx < 0 {
            return Ok(());
        }
        let idx = idx as usize;
        if idx >= dex.types.len() {
            let extra = if idx < dex.strings.len() {
                format!(
                    " (string[{idx}]='{}')",
                    dex.strings[idx].to_string().unwrap_or_else(|_| "<invalid>".to_string())
                )
            } else {
                String::new()
            };
            return Err(crate::dex::error::DexError::new(&format!(
                "{context} index out of range: {idx} >= {}{extra}",
                dex.types.len()
            )));
        }
        Ok(())
    }

    fn read_sleb128_local(bytes: &[u8], ix: &mut usize, context: &str) -> i32 {
        if *ix >= bytes.len() {
            panic!("sleb128 out of bounds for {context}");
        }
        let mut value: i32 = 0;
        let mut shift: u32 = 0;
        let mut count: usize = 0;
        let mut last_byte: u8 = 0;
        let mut cursor = *ix;
        while cursor < bytes.len() {
            let byte = bytes[cursor];
            cursor += 1;
            count += 1;
            last_byte = byte;
            let low = (byte & 0x7F) as i32;
            if shift < 32 {
                value |= low.wrapping_shl(shift);
            }
            let cont = (byte & 0x80) != 0;
            shift = shift.saturating_add(7);
            if !cont || count == 5 {
                break;
            }
        }
        if (last_byte & 0x40) != 0 && shift < 32 {
            value |= (-1i32).wrapping_shl(shift);
        }
        if count == 0 || cursor > bytes.len() {
            panic!("invalid sleb128 for {context}");
        }
        *ix = cursor;
        value
    }

    fn dex_string(dex: &DexFile, idx: usize) -> String {
        dex.strings[idx].to_string().unwrap_or_else(|_| "<invalid>".to_string())
    }

    fn type_descriptor(dex: &DexFile, type_id: usize) -> String {
        let string_id = dex.types[type_id];
        dex_string(dex, string_id)
    }

    fn method_signature(dex: &DexFile, proto_idx: usize) -> String {
        let proto = &dex.prototypes[proto_idx];
        let mut sig = String::from("(");
        for ty in proto.parameters.items() {
            sig.push_str(&type_descriptor(dex, *ty));
        }
        sig.push(')');
        sig.push_str(&type_descriptor(dex, proto.return_type_idx));
        sig
    }

    fn method_id_string(dex: &DexFile, method_idx: usize) -> String {
        let method = &dex.methods[method_idx];
        let class_desc = type_descriptor(dex, method.class_idx);
        let name = dex_string(dex, method.name_idx);
        let sig = method_signature(dex, method.proto_idx);
        format!("{class_desc}->{name}{sig}")
    }
}
