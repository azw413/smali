use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::dex::dex_file::DexRefResolver;
use crate::dex::DexFile;
use crate::dex::opcode_format::decode_with_ctx_full;
use crate::types::{AnnotationValue, CatchDirective, Modifier, SmaliClass, SmaliOp, TypeSignature};
use crate::smali_ops::{parse_literal_int, unescape_smali_string};

type MethodCodeFingerprint = Option<(String, Vec<(u32, u16, String)>)>;
type MethodCodeFingerprintMap = BTreeMap<String, MethodCodeFingerprint>;

fn class_smali_map(classes: &[SmaliClass]) -> BTreeMap<String, String> {
    let mut map = BTreeMap::new();
    for class in classes {
        let desc = class.name.as_jni_type();
        map.insert(desc, class.to_smali());
    }
    map
}

fn normalize_modifiers(modifiers: &[Modifier]) -> Vec<String> {
    let mut values: Vec<String> = modifiers.iter().map(|m| m.to_str().to_string()).collect();
    values.sort();
    values
}

fn normalize_annotation_literal(value: &str) -> String {
    let trimmed = value.trim();
    if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
        let inner = &trimmed[1..trimmed.len() - 1];
        return format!("str:{}", unescape_smali_string(inner));
    }
    let numeric = trimmed
        .strip_suffix('L')
        .or_else(|| trimmed.strip_suffix('l'))
        .unwrap_or(trimmed);
    if let Ok((rest, value)) = parse_literal_int::<i64>(numeric)
        && rest.trim().is_empty()
    {
        return format!("int:{value}");
    }
    trimmed.to_string()
}

fn normalize_field_initial_value(signature: &TypeSignature, value: Option<&str>) -> Option<String> {
    let value = value?;
    let trimmed = value.trim();
    Some(match signature {
        TypeSignature::Bool => {
            if trimmed == "true" || trimmed == "false" {
                trimmed.to_string()
            } else if let Ok((rest, parsed)) = parse_literal_int::<i32>(trimmed) {
                if rest.trim().is_empty() {
                    if parsed == 0 { "false".to_string() } else { "true".to_string() }
                } else {
                    trimmed.to_string()
                }
            } else {
                trimmed.to_string()
            }
        }
        TypeSignature::Byte => {
            if let Ok((rest, parsed)) = parse_literal_int::<i8>(trimmed.trim_end_matches(['t', 'T'])) {
                if rest.trim().is_empty() { format!("byte:{parsed}") } else { trimmed.to_string() }
            } else {
                trimmed.to_string()
            }
        }
        TypeSignature::Short => {
            if let Ok((rest, parsed)) = parse_literal_int::<i16>(trimmed.trim_end_matches(['s', 'S'])) {
                if rest.trim().is_empty() { format!("short:{parsed}") } else { trimmed.to_string() }
            } else {
                trimmed.to_string()
            }
        }
        TypeSignature::Char => {
            if trimmed.starts_with('\'') && trimmed.ends_with('\'') && trimmed.len() >= 2 {
                let inner = &trimmed[1..trimmed.len() - 1];
                let unescaped = unescape_smali_string(inner);
                let mut chars = unescaped.chars();
                if let (Some(ch), None) = (chars.next(), chars.next()) {
                    format!("char:{}", ch as u32)
                } else {
                    trimmed.to_string()
                }
            } else if let Ok((rest, parsed)) = parse_literal_int::<i32>(trimmed) {
                if rest.trim().is_empty() { format!("char:{parsed}") } else { trimmed.to_string() }
            } else {
                trimmed.to_string()
            }
        }
        TypeSignature::Int => {
            if let Ok((rest, parsed)) = parse_literal_int::<i32>(trimmed) {
                if rest.trim().is_empty() { format!("int:{parsed}") } else { trimmed.to_string() }
            } else {
                trimmed.to_string()
            }
        }
        TypeSignature::Long => {
            let numeric = trimmed.strip_suffix('L').or_else(|| trimmed.strip_suffix('l')).unwrap_or(trimmed);
            if let Ok((rest, parsed)) = parse_literal_int::<i64>(numeric) {
                if rest.trim().is_empty() { format!("long:{parsed}") } else { trimmed.to_string() }
            } else {
                trimmed.to_string()
            }
        }
        TypeSignature::Float | TypeSignature::Double | TypeSignature::Array(_) | TypeSignature::Object(_) | TypeSignature::Void | TypeSignature::TypeParameters(_, _) | TypeSignature::TypeParameter(_, _) | TypeSignature::TypeVariableSignature(_) | TypeSignature::WildcardPlus | TypeSignature::WildcardMinus | TypeSignature::WildcardStar => {
            normalize_annotation_literal(trimmed)
        }
    })
}

fn field_default_value(signature: &TypeSignature) -> Option<String> {
    Some(match signature {
        TypeSignature::Bool => "false".to_string(),
        TypeSignature::Byte => "byte:0".to_string(),
        TypeSignature::Short => "short:0".to_string(),
        TypeSignature::Char => "char:0".to_string(),
        TypeSignature::Int => "int:0".to_string(),
        TypeSignature::Long => "long:0".to_string(),
        TypeSignature::Float => "int:0".to_string(),
        TypeSignature::Double => "long:0".to_string(),
        TypeSignature::Array(_) | TypeSignature::Object(_) => "null".to_string(),
        TypeSignature::Void
        | TypeSignature::TypeParameters(_, _)
        | TypeSignature::TypeParameter(_, _)
        | TypeSignature::TypeVariableSignature(_)
        | TypeSignature::WildcardPlus
        | TypeSignature::WildcardMinus
        | TypeSignature::WildcardStar => return None,
    })
}

fn field_initial_values_equivalent(
    signature: &TypeSignature,
    lhs: Option<&str>,
    rhs: Option<&str>,
) -> bool {
    let lhs_norm = normalize_field_initial_value(signature, lhs);
    let rhs_norm = normalize_field_initial_value(signature, rhs);
    if lhs_norm == rhs_norm {
        return true;
    }
    let default = field_default_value(signature);
    (lhs_norm.is_none() && rhs_norm == default) || (rhs_norm.is_none() && lhs_norm == default)
}

fn annotation_value_fingerprint(value: &AnnotationValue) -> String {
    match value {
        AnnotationValue::Single(value) => {
            format!("single:{}", normalize_annotation_literal(value))
        }
        AnnotationValue::Array(values) => {
            let values = values
                .iter()
                .map(|value| normalize_annotation_literal(value))
                .collect::<Vec<_>>();
            format!("array:[{}]", values.join(","))
        }
        AnnotationValue::SubAnnotation(annotation) => {
            format!("sub:{}", annotation_fingerprint_inner(annotation, false))
        }
        AnnotationValue::Enum(ty, name) => format!("enum:{}:{}", ty.as_jni_type(), name),
    }
}

fn annotation_fingerprint_inner(
    annotation: &crate::types::SmaliAnnotation,
    include_visibility: bool,
) -> String {
    let mut elements: Vec<String> = annotation
        .elements
        .iter()
        .map(|el| format!("{}={}", el.name, annotation_value_fingerprint(&el.value)))
        .collect();
    elements.sort();
    if include_visibility {
        format!(
            "{} {} {{{}}}",
            annotation.visibility.to_str(),
            annotation.annotation_type.to_jni(),
            elements.join(",")
        )
    } else {
        format!("{} {{{}}}", annotation.annotation_type.to_jni(), elements.join(","))
    }
}

fn annotation_fingerprint(annotation: &crate::types::SmaliAnnotation) -> String {
    annotation_fingerprint_inner(annotation, true)
}

fn annotation_fingerprints(annotations: &[crate::types::SmaliAnnotation]) -> Vec<String> {
    let mut values: Vec<String> = annotations.iter().map(annotation_fingerprint).collect();
    values.sort();
    values
}

fn class_index_map(classes: &[SmaliClass]) -> BTreeMap<String, usize> {
    let mut map = BTreeMap::new();
    for (idx, class) in classes.iter().enumerate() {
        let desc = class.name.as_jni_type();
        map.insert(desc, idx);
    }
    map
}

fn class_stats(class: &SmaliClass) -> String {
    format!(
        "fields {} methods {} annotations {}",
        class.fields.len(),
        class.methods.len(),
        class.annotations.len()
    )
}

fn method_key(method: &crate::types::SmaliMethod) -> String {
    format!("{}{}", method.name, method.signature.to_jni())
}

fn method_index_map(class: &SmaliClass) -> BTreeMap<String, usize> {
    let mut map = BTreeMap::new();
    for (idx, method) in class.methods.iter().enumerate() {
        map.insert(method_key(method), idx);
    }
    map
}

fn method_smali_map(class: &SmaliClass) -> BTreeMap<String, String> {
    let mut map = BTreeMap::new();
    for method in &class.methods {
        map.insert(method_key(method), format!("{method}"));
    }
    map
}

fn is_debug_op(op: &crate::types::SmaliOp) -> bool {
    matches!(
        op,
        crate::types::SmaliOp::Line(_)
            | crate::types::SmaliOp::Local { .. }
            | crate::types::SmaliOp::EndLocal { .. }
            | crate::types::SmaliOp::RestartLocal { .. }
            | crate::types::SmaliOp::Prologue
            | crate::types::SmaliOp::Epilogue
    )
}

fn debug_op_fingerprint(op: &crate::types::SmaliOp) -> String {
    match op {
        crate::types::SmaliOp::Line(value) => format!("line:{value}"),
        crate::types::SmaliOp::Local {
            register,
            name,
            descriptor,
            signature,
        } => format!(
            "local:{register:?}:{:?}:{:?}:{:?}",
            name, descriptor, signature
        ),
        crate::types::SmaliOp::EndLocal { register } => format!("end:{register:?}"),
        crate::types::SmaliOp::RestartLocal { register } => format!("restart:{register:?}"),
        crate::types::SmaliOp::Prologue => "prologue".to_string(),
        crate::types::SmaliOp::Epilogue => "epilogue".to_string(),
        _ => "non-debug".to_string(),
    }
}

fn debug_ops(method: &crate::types::SmaliMethod) -> Vec<String> {
    method
        .ops
        .iter()
        .filter(|op| is_debug_op(op))
        .map(debug_op_fingerprint)
        .collect()
}

fn canonicalize_label_tokens(line: &str, labels: &BTreeMap<String, usize>) -> String {
    let bytes = line.as_bytes();
    let mut out = String::with_capacity(line.len());
    let mut i = 0usize;
    while i < bytes.len() {
        let is_label_context = i == 0
            || matches!(bytes[i.saturating_sub(1)] as char, ' ' | '\t' | '{' | '}' | ',');
        if bytes[i] == b':' && is_label_context {
            let start = i + 1;
            let mut end = start;
            while end < bytes.len() {
                let ch = bytes[end] as char;
                if ch.is_ascii_alphanumeric() || ch == '_' || ch == '$' || ch == '.' {
                    end += 1;
                } else {
                    break;
                }
            }
            if end > start {
                let name = &line[start..end];
                if let Some(idx) = labels.get(name) {
                    out.push_str(&format!(":L{idx}"));
                    i = end;
                    continue;
                }
            }
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}

fn smali_op_text(op: &SmaliOp) -> String {
    match op {
        SmaliOp::Label(label) => format!("{label}"),
        SmaliOp::Line(value) => format!(".line {value}"),
        SmaliOp::Op(op) => op.to_string(),
        SmaliOp::Catch(catch) => format!("{catch}"),
        SmaliOp::ArrayData(data) => format!("{data}"),
        SmaliOp::PackedSwitch(data) => format!("{data}"),
        SmaliOp::SparseSwitch(data) => format!("{data}"),
        SmaliOp::Prologue => ".prologue".to_string(),
        SmaliOp::Epilogue => ".epilogue".to_string(),
        SmaliOp::Local {
            register,
            name,
            descriptor,
            signature,
        } => format!(
            ".local {register:?}, {:?}, {:?}, {:?}",
            name, descriptor, signature
        ),
        SmaliOp::EndLocal { register } => format!(".end local {register:?}"),
        SmaliOp::RestartLocal { register } => format!(".restart local {register:?}"),
    }
}

fn canonical_method_ops_text(method: &crate::types::SmaliMethod, include_debug: bool) -> String {
    let mut labels = BTreeMap::new();
    let mut op_index = 0usize;
    for op in &method.ops {
        if !include_debug && is_debug_op(op) {
            continue;
        }
        if let SmaliOp::Label(label) = op {
            labels.insert(label.0.clone(), op_index);
            continue;
        }
        op_index += 1;
    }

    let mut main_lines = Vec::new();
    let mut catch_lines = Vec::new();
    for op in &method.ops {
        if !include_debug && is_debug_op(op) {
            continue;
        }
        if matches!(op, SmaliOp::Label(_)) {
            continue;
        }
        let canonical = canonicalize_label_tokens(&smali_op_text(op), &labels);
        if matches!(op, SmaliOp::Catch(_)) {
            catch_lines.push(canonical);
        } else {
            main_lines.push(canonical);
        }
    }
    catch_lines.sort();
    main_lines.extend(catch_lines);
    main_lines.join("\n")
}

fn canonical_ops_from_decoded(decoded: &[SmaliOp]) -> String {
    let mut labels = BTreeMap::new();
    let mut op_index = 0usize;
    for op in decoded {
        if let SmaliOp::Label(label) = op {
            labels.insert(label.0.clone(), op_index);
            continue;
        }
        op_index += 1;
    }

    let mut main_lines = Vec::new();
    let mut catch_lines = Vec::new();
    for op in decoded {
        if matches!(op, SmaliOp::Label(_)) {
            continue;
        }
        let canonical = canonicalize_label_tokens(&smali_op_text(op), &labels);
        if matches!(op, SmaliOp::Catch(_)) {
            catch_lines.push(canonical);
        } else {
            main_lines.push(canonical);
        }
    }
    catch_lines.sort();
    main_lines.extend(catch_lines);
    main_lines.join("\n")
}

fn assembled_method_code_fingerprints(
    class: &SmaliClass,
) -> Result<MethodCodeFingerprintMap, crate::dex::error::DexError> {
    let dex = DexFile::from_smali(std::slice::from_ref(class))?;
    let class_def = dex
        .class_defs
        .first()
        .ok_or_else(|| crate::dex::error::DexError::new("assembled dex missing class"))?;
    let class_data = class_def
        .class_data
        .as_ref()
        .ok_or_else(|| crate::dex::error::DexError::new("assembled dex missing class data"))?;

    let mut map = BTreeMap::new();
    let resolver = DexRefResolver { dex: &dex };
    for encoded in class_data
        .direct_methods
        .iter()
        .chain(class_data.virtual_methods.iter())
    {
        let method = &dex.methods[encoded.method_idx];
        let name = dex.strings[method.name_idx].to_string()?;
        let proto = &dex.prototypes[method.proto_idx];
        let mut sig = String::from("(");
        for &t in proto.parameters.items() {
            sig.push_str(&dex.strings[dex.types[t]].to_string()?);
        }
        sig.push(')');
        sig.push_str(&dex.strings[dex.types[proto.return_type_idx]].to_string()?);
        let key = format!("{name}{sig}");
        let bytes = if let Some(code) = &encoded.code {
            let mut tries = Vec::new();
            for try_item in code.tries() {
                let handler_idx = try_item.handler_idx.expect("assembled try item missing handler_idx");
                let handler = &code.handlers()[handler_idx];
                let mut typed = handler
                    .handlers
                    .iter()
                    .map(|pair| {
                        format!(
                            "{}@{}",
                            dex.strings[dex.types[pair.type_idx]].to_string().unwrap_or_default(),
                            pair.addr
                        )
                    })
                    .collect::<Vec<_>>();
                typed.sort();
                let handler_sig = format!("typed:[{}];catchall:{:?}", typed.join(","), handler.catch_all_addr);
                tries.push((try_item.start_addr, try_item.insn_count, handler_sig));
            }
            tries.sort();
            let mut bytecode = Vec::with_capacity(code.instructions().len() * 2);
            for &word in code.instructions() {
                bytecode.extend_from_slice(&word.to_le_bytes());
            }
            let decoded = decode_with_ctx_full(&bytecode, 33, 0, &resolver, None)?;
            Some((canonical_ops_from_decoded(&decoded.ops), tries))
        } else {
            None
        };
        map.insert(key, bytes);
    }
    Ok(map)
}

fn debug_op_diff_detail(
    original: &crate::types::SmaliMethod,
    rebuilt: &crate::types::SmaliMethod,
) -> String {
    let orig = debug_ops(original);
    let rebuilt = debug_ops(rebuilt);
    let min_len = orig.len().min(rebuilt.len());
    for idx in 0..min_len {
        if orig[idx] != rebuilt[idx] {
            return format!(
                "debug op mismatch at {idx} (orig {} vs rebuilt {})",
                orig[idx], rebuilt[idx]
            );
        }
    }
    if orig.len() != rebuilt.len() {
        return format!(
            "debug op length mismatch (orig {} vs rebuilt {})",
            orig.len(),
            rebuilt.len()
        );
    }
    "debug op mismatch (unknown)".to_string()
}

fn non_debug_op_fingerprint(method: &crate::types::SmaliMethod) -> String {
    let mut parts = Vec::new();
    for op in &method.ops {
        if is_debug_op(op) {
            continue;
        }
        parts.push(format!("{op:?}"));
    }
    parts.join("|")
}

fn compare_class_structures(original: &SmaliClass, rebuilt: &SmaliClass) -> Vec<String> {
    let mut diffs = Vec::new();
    let assembled_original_methods = assembled_method_code_fingerprints(original).ok();
    let assembled_rebuilt_methods = assembled_method_code_fingerprints(rebuilt).ok();
    if normalize_modifiers(&original.modifiers) != normalize_modifiers(&rebuilt.modifiers) {
        diffs.push("class modifiers".to_string());
    }
    if original.source != rebuilt.source {
        diffs.push("class source".to_string());
    }
    if original.super_class != rebuilt.super_class {
        diffs.push("class superclass".to_string());
    }
    if original.implements != rebuilt.implements {
        diffs.push("class implements".to_string());
    }
    if annotation_fingerprints(&original.annotations)
        != annotation_fingerprints(&rebuilt.annotations)
    {
        diffs.push("class annotations".to_string());
    }

    let mut original_fields = BTreeMap::new();
    for field in &original.fields {
        let key = format!("{}:{}", field.name, field.signature.to_jni());
        original_fields.insert(key, field);
    }
    let mut rebuilt_fields = BTreeMap::new();
    for field in &rebuilt.fields {
        let key = format!("{}:{}", field.name, field.signature.to_jni());
        rebuilt_fields.insert(key, field);
    }
    for (key, field) in &original_fields {
        match rebuilt_fields.get(key) {
            Some(rebuilt_field) => {
                if normalize_modifiers(&field.modifiers)
                    != normalize_modifiers(&rebuilt_field.modifiers)
                {
                    diffs.push(format!("field {key} modifiers"));
                }
                if !field_initial_values_equivalent(
                    &field.signature,
                    field.initial_value.as_deref(),
                    rebuilt_field.initial_value.as_deref(),
                ) {
                    diffs.push(format!("field {key} initial value"));
                }
                if annotation_fingerprints(&field.annotations)
                    != annotation_fingerprints(&rebuilt_field.annotations)
                {
                    diffs.push(format!("field {key} annotations"));
                }
            }
            None => diffs.push(format!("field {key} missing in rebuilt")),
        }
    }
    for key in rebuilt_fields.keys() {
        if !original_fields.contains_key(key) {
            diffs.push(format!("field {key} missing in original"));
        }
    }

    let original_methods = method_index_map(original);
    let rebuilt_methods = method_index_map(rebuilt);
    for (key, orig_idx) in &original_methods {
        match rebuilt_methods.get(key) {
            Some(rebuilt_idx) => {
                let orig_method = &original.methods[*orig_idx];
                let rebuilt_method = &rebuilt.methods[*rebuilt_idx];
                if normalize_modifiers(&orig_method.modifiers)
                    != normalize_modifiers(&rebuilt_method.modifiers)
                {
                    diffs.push(format!("method {key} modifiers"));
                }
                if orig_method.constructor != rebuilt_method.constructor {
                    diffs.push(format!("method {key} constructor flag"));
                }
                if orig_method.locals != rebuilt_method.locals
                    || orig_method.registers != rebuilt_method.registers
                {
                    diffs.push(format!("method {key} locals/registers"));
                }
                let orig_semantic = canonical_method_ops_text(orig_method, false);
                let rebuilt_semantic = canonical_method_ops_text(rebuilt_method, false);
                if orig_semantic != rebuilt_semantic {
                    let assembled_equal = assembled_original_methods
                        .as_ref()
                        .and_then(|methods| methods.get(key))
                        == assembled_rebuilt_methods
                            .as_ref()
                            .and_then(|methods| methods.get(key));
                    if !assembled_equal {
                        diffs.push(format!("method {key} ops"));
                    }
                }
            }
            None => diffs.push(format!("method {key} missing in rebuilt")),
        }
    }
    for key in rebuilt_methods.keys() {
        if !original_methods.contains_key(key) {
            diffs.push(format!("method {key} missing in original"));
        }
    }

    diffs
}

fn compare_method_registers(
    original: &SmaliClass,
    rebuilt: &SmaliClass,
    sample: &mut Vec<String>,
    diff_count: &mut usize,
) {
    let original_index = method_index_map(original);
    let rebuilt_index = method_index_map(rebuilt);
    for (key, orig_idx) in &original_index {
        let Some(rebuilt_idx) = rebuilt_index.get(key) else {
            continue;
        };
        let orig_method = &original.methods[*orig_idx];
        let rebuilt_method = &rebuilt.methods[*rebuilt_idx];
        if orig_method.locals != rebuilt_method.locals || orig_method.registers != rebuilt_method.registers {
            *diff_count += 1;
            if sample.len() < 5 {
                sample.push(format!(
                    "{}->{} locals/registers mismatch (orig locals {} regs {:?} vs rebuilt locals {} regs {:?})",
                    original.name.as_jni_type(),
                    key,
                    orig_method.locals,
                    orig_method.registers,
                    rebuilt_method.locals,
                    rebuilt_method.registers
                ));
            }
        }
    }
}

fn method_diff_summary(original: &SmaliClass, rebuilt: &SmaliClass) -> String {
    let original_map = method_smali_map(original);
    let rebuilt_map = method_smali_map(rebuilt);
    let original_index = method_index_map(original);
    let rebuilt_index = method_index_map(rebuilt);
    let mut diffs = Vec::new();
    let mut debug_detail = None;
    for (key, orig) in &original_map {
        match rebuilt_map.get(key) {
            Some(rebuilt) if rebuilt == orig => {}
            Some(_) => {
                let mut suffix = "";
                if let (Some(orig_idx), Some(rebuilt_idx)) =
                    (original_index.get(key), rebuilt_index.get(key))
                {
                    let orig_method = &original.methods[*orig_idx];
                    let rebuilt_method = &rebuilt.methods[*rebuilt_idx];
                    if non_debug_op_fingerprint(orig_method)
                        == non_debug_op_fingerprint(rebuilt_method)
                    {
                        suffix = " (debug-only)";
                        if debug_detail.is_none() {
                            debug_detail =
                                Some(debug_op_diff_detail(orig_method, rebuilt_method));
                        }
                    } else {
                        suffix = " (body mismatch)";
                    }
                }
                diffs.push(format!("{key}{suffix}"));
            }
            None => diffs.push(format!("{key} missing in rebuilt")),
        }
    }
    for key in rebuilt_map.keys() {
        if !original_map.contains_key(key) {
            diffs.push(format!("{key} missing in original"));
        }
    }
    if diffs.is_empty() {
        "no method diffs".to_string()
    } else {
        let sample = diffs.into_iter().take(3).collect::<Vec<_>>().join(", ");
        if let Some(detail) = debug_detail {
            format!("method diffs: {sample}; {detail}")
        } else {
            format!("method diffs: {sample}")
        }
    }
}

fn collect_smali_files(root: &Path, files: &mut Vec<PathBuf>) {
    let entries = match fs::read_dir(root) {
        Ok(entries) => entries,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_smali_files(&path, files);
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("smali") {
            files.push(path);
        }
    }
}

fn load_smali_classes(root: &Path) -> Vec<SmaliClass> {
    let mut files = Vec::new();
    collect_smali_files(root, &mut files);
    files.sort();
    let mut classes = Vec::with_capacity(files.len());
    for path in files {
        let content = fs::read_to_string(&path)
            .unwrap_or_else(|_| panic!("read smali fixture {:?}", path));
        let class = SmaliClass::from_smali(&content)
            .unwrap_or_else(|_| panic!("parse smali fixture {:?}", path));
        classes.push(class);
    }
    classes
}

#[test]
fn rebuilds_rootbeer_dex_roundtrip() {
    let bytes = fs::read("tests/rootbeer.dex").expect("read rootbeer fixture");
    let dex = DexFile::from_bytes(&bytes).expect("parse rootbeer dex");
    let classes = dex.to_smali().expect("convert to smali classes");

    let rebuilt_dex =
        DexFile::from_smali(&classes).expect("rebuild dex from smali classes and parse result");
    let rebuilt_classes = rebuilt_dex
        .to_smali()
        .expect("convert rebuilt dex to smali classes");

    let original_map = class_smali_map(&classes);
    let rebuilt_map = class_smali_map(&rebuilt_classes);

    let mut sample = Vec::new();
    let mut diff_count = 0usize;
    for (desc, original) in &original_map {
        match rebuilt_map.get(desc) {
            Some(rebuilt) if rebuilt == original => {}
            Some(rebuilt) => {
                diff_count += 1;
                if sample.len() < 5 {
                    sample.push(format!(
                        "{desc} (original {} bytes vs rebuilt {} bytes)",
                        original.len(),
                        rebuilt.len()
                    ));
                }
            }
            None => {
                diff_count += 1;
                if sample.len() < 5 {
                    sample.push(format!("{desc} missing from rebuilt output"));
                }
            }
        }
    }
    for desc in rebuilt_map.keys() {
        if !original_map.contains_key(desc) {
            diff_count += 1;
            if sample.len() < 5 {
                sample.push(format!("{desc} missing from original output"));
            }
        }
    }
    assert!(
        diff_count == 0,
        "smali text mismatch after rebuild: {diff_count} differing classes (examples: {})",
        sample.join(", ")
    );
}

#[test]
fn roundtrips_smali_classes11() {
    let classes = load_smali_classes(Path::new("tests/smali_classes11"));
    let dex = DexFile::from_smali(&classes).expect("assemble smali_classes11");
    let rebuilt_classes = dex.to_smali().expect("roundtrip smali_classes11");

    let original_index = class_index_map(&classes);
    let rebuilt_index = class_index_map(&rebuilt_classes);

    let mut sample = Vec::new();
    let mut diff_count = 0usize;
    for (desc, orig_idx) in &original_index {
        if let Some(rebuilt_idx) = rebuilt_index.get(desc) {
            compare_method_registers(
                &classes[*orig_idx],
                &rebuilt_classes[*rebuilt_idx],
                &mut sample,
                &mut diff_count,
            );
        }
    }
    for (desc, orig_idx) in &original_index {
        match rebuilt_index.get(desc) {
            Some(rebuilt_idx) => {
                let diffs =
                    compare_class_structures(&classes[*orig_idx], &rebuilt_classes[*rebuilt_idx]);
                if !diffs.is_empty() {
                    diff_count += diffs.len();
                    if sample.len() < 5 {
                        let original_stats = class_stats(&classes[*orig_idx]);
                        let rebuilt_stats = class_stats(&rebuilt_classes[*rebuilt_idx]);
                        let method_diffs =
                            method_diff_summary(&classes[*orig_idx], &rebuilt_classes[*rebuilt_idx]);
                        let diff_sample = diffs.iter().take(3).cloned().collect::<Vec<_>>().join(", ");
                        sample.push(format!(
                            "{desc} ({original_stats} vs {rebuilt_stats}, {method_diffs}, diffs: {diff_sample})"
                        ));
                    }
                }
            }
            None => {
                diff_count += 1;
                if sample.len() < 5 {
                    sample.push(format!("{desc} missing from rebuilt output"));
                }
            }
        }
    }
    for desc in rebuilt_index.keys() {
        if !original_index.contains_key(desc) {
            diff_count += 1;
            if sample.len() < 5 {
                sample.push(format!("{desc} missing from original output"));
            }
        }
    }
    assert!(
        diff_count == 0,
        "roundtrip mismatch: {diff_count} differences (examples: {})",
        sample.join(", ")
    );
}

const TRY_CATCH_CLASS: &str = r#"
.class public Lroundtrip/TryCatchSample;
.super Ljava/lang/Object;

.method public static safeLoad()V
    .registers 2
    const-string v0, "kotlinx/coroutines/test/internal/TestMainDispatcherFactory"
    :try_start
    invoke-static {v0}, Ljava/lang/Class;->forName(Ljava/lang/String;)Ljava/lang/Class;
    :try_end
    return-void
    .catch Ljava/lang/ClassNotFoundException; {:try_start .. :try_end} :handler

    :handler
    return-void
.end method

.method public constructor <init>()V
    .registers 1
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
"#;

#[test]
fn preserves_try_catch_directives() {
    let original = SmaliClass::from_smali(TRY_CATCH_CLASS).expect("parse sample class");
    let assembled = SmaliClass::from_smali(TRY_CATCH_CLASS).expect("parse sample class");
    let dex = DexFile::from_smali(&[assembled]).expect("assemble sample class");
    let rebuilt = dex.to_smali().expect("roundtrip to smali");
    let rebuilt_class = rebuilt
        .iter()
        .find(|c| c.name == original.name)
        .expect("class present after roundtrip");

    let count_catches = |cls: &SmaliClass| {
        cls.methods
            .iter()
            .find(|m| m.name == "safeLoad")
            .expect("safeLoad present")
            .ops
            .iter()
            .filter(|op| matches!(op, SmaliOp::Catch(_)))
            .count()
    };

    let original = count_catches(&original);
    let roundtrip = count_catches(rebuilt_class);

    assert_eq!(
        original, roundtrip,
        "catch directives changed after roundtrip"
    );
}

#[test]
fn fast_service_loader_catches_present() {
    let bytes = fs::read("tests/rootbeer.dex").expect("read rootbeer fixture");
    let dex = DexFile::from_bytes(&bytes).expect("parse rootbeer dex");
    let classes = dex.to_smali().expect("convert dex to smali");
    let class = classes
        .iter()
        .find(|c| c.name.as_jni_type() == "Lkotlinx/coroutines/internal/FastServiceLoader;")
        .expect("FastServiceLoader present");
    let method = class
        .methods
        .iter()
        .find(|m| m.name == "loadMainDispatcherFactory$kotlinx_coroutines_core")
        .expect("loadMainDispatcherFactory present");
    let typed_exceptions: Vec<_> = method
        .ops
        .iter()
        .filter_map(|op| {
            if let SmaliOp::Catch(CatchDirective::Catch { exception, .. }) = op {
                Some(exception.clone())
            } else {
                None
            }
        })
        .collect();
    let class_not_found = typed_exceptions
        .iter()
        .filter(|exc| exc == &&"Ljava/lang/ClassNotFoundException;".to_string())
        .count();
    assert!(
        class_not_found >= 2,
        "expected at least two ClassNotFoundException catches, found {class_not_found}"
    );
}

const INVOKE_CUSTOM_SMALI: &str = r#"
.class public Lcom/example/CustomCalls;
.super Ljava/lang/Object;

.method public static concat(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
    .registers 2
    invoke-custom {p0, p1}, "Lcom/example/Bootstrap;->bootstrap(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;::makeConcat(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"
    move-result-object v0
    return-object v0
.end method

.method public static concatRange(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
    .registers 3
    invoke-custom/range {p0 .. p2}, "Lcom/example/Bootstrap;->bootstrap(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;::makeConcat3(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"
    move-result-object v0
    return-object v0
.end method

.method public static dynamic(Ljava/lang/invoke/MethodHandle;Ljava/lang/Object;)Ljava/lang/Object;
    .registers 2
    invoke-polymorphic {p0, p1}, Ljava/lang/invoke/MethodHandle;->invokeExact(Ljava/lang/Object;)Ljava/lang/Object;, (Ljava/lang/Object;)Ljava/lang/Object;
    move-result-object v0
    return-object v0
.end method

.method public static dynamicRange(Ljava/lang/invoke/MethodHandle;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;
    .registers 3
    invoke-polymorphic/range {p0 .. p2}, Ljava/lang/invoke/MethodHandle;->invoke(Ljava/lang/Object;)Ljava/lang/Object;, (Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;
    move-result-object v0
    return-object v0
.end method

.method public static makeHandle()Ljava/lang/invoke/MethodHandle;
    .registers 1
    const-method-handle v0, "invoke-static {}, Lcom/example/Target;->doIt(Ljava/lang/String;)Ljava/lang/Object;"
    return-object v0
.end method
"#;

#[test]
fn invoke_custom_polymorphic_handle_roundtrip() {
    use crate::dex::builder::build_dex_file_bytes;
    use crate::types::SmaliClass;

    let class = SmaliClass::from_smali(INVOKE_CUSTOM_SMALI).expect("parse class");

    // smali -> DEX bytes -> reparse
    let bytes = build_dex_file_bytes(std::slice::from_ref(&class)).expect("assemble to DEX");
    let reparsed = DexFile::from_bytes(&bytes).expect("reparse DEX from bytes");

    // Map list must contain a call_site_id_item entry (count > 0).
    assert!(
        !reparsed.call_site_ids.is_empty(),
        "expected call_site_ids to be present after roundtrip, got empty"
    );
    assert!(
        !reparsed.method_handles.is_empty(),
        "expected method_handles to be present after roundtrip"
    );

    let rebuilt_classes = reparsed.to_smali().expect("convert reparsed DEX to smali");
    assert_eq!(rebuilt_classes.len(), 1);
    let rebuilt = &rebuilt_classes[0];

    fn collect_dex_ops(class: &SmaliClass) -> Vec<String> {
        let mut out = Vec::new();
        for m in &class.methods {
            for op in &m.ops {
                if let SmaliOp::Op(dex_op) = op {
                    out.push(dex_op.to_string());
                }
            }
        }
        out
    }

    let original_ops = collect_dex_ops(&class);
    let rebuilt_ops = collect_dex_ops(rebuilt);

    fn has_op(ops: &[String], prefix: &str) -> bool {
        ops.iter().any(|o| o.starts_with(prefix))
    }
    for prefix in [
        "invoke-custom ",
        "invoke-custom/range ",
        "invoke-polymorphic ",
        "invoke-polymorphic/range ",
        "const-method-handle ",
    ] {
        assert!(
            has_op(&original_ops, prefix),
            "original missing {prefix}: {original_ops:?}"
        );
        assert!(
            has_op(&rebuilt_ops, prefix),
            "rebuilt missing {prefix}: {rebuilt_ops:?}"
        );
    }

    // Ensure each invoke-custom literal's bootstrap descriptor survived the roundtrip
    // (the substring is the user-meaningful payload of the call_site_item).
    for needle in ["makeConcat(", "makeConcat3(", "Lcom/example/Bootstrap;"] {
        assert!(
            rebuilt_ops.iter().any(|o| o.contains(needle)),
            "expected '{needle}' to survive roundtrip in {rebuilt_ops:?}"
        );
    }

    // Sanity: ensure the rebuilt dex has the same number of dex ops per method.
    assert_eq!(
        original_ops.len(),
        rebuilt_ops.len(),
        "dex op count differs after roundtrip"
    );

}

// Helpers for the link_data / hiddenapi pass-through tests below: take a freshly
// built DEX byte stream and append a synthetic section of `payload` bytes, updating
// the relevant header fields and the map list to point at it. This lets us verify
// the reader without needing a real-world fixture that exercises these features.
fn append_section_and_update_map(
    bytes: &[u8],
    payload: &[u8],
    map_type: u16,
    map_size: u32,
    update_link: bool,
) -> Vec<u8> {
    // Header field offsets per DEX spec.
    const FILE_SIZE_OFF: usize = 0x20;
    const LINK_SIZE_OFF: usize = 0x2c;
    const LINK_OFF_OFF: usize = 0x30;
    const MAP_OFF_OFF: usize = 0x34;

    fn read_u4_le(b: &[u8], off: usize) -> u32 {
        u32::from_le_bytes(b[off..off + 4].try_into().unwrap())
    }
    fn write_u4_le(b: &mut [u8], off: usize, v: u32) {
        b[off..off + 4].copy_from_slice(&v.to_le_bytes());
    }

    let original_map_off = read_u4_le(bytes, MAP_OFF_OFF) as usize;
    let map_size_count = read_u4_le(bytes, original_map_off);

    // Layout: [original bytes up to old map] [appended payload, 4-aligned] [new map]
    let mut out = bytes[..original_map_off].to_vec();
    while out.len() % 4 != 0 {
        out.push(0);
    }
    let payload_off = out.len() as u32;
    out.extend_from_slice(payload);
    while out.len() % 4 != 0 {
        out.push(0);
    }

    let new_map_off = out.len() as u32;
    out.extend_from_slice(&(map_size_count + 1).to_le_bytes());
    // Copy original map entries.
    let original_map_entries_start = original_map_off + 4;
    let original_map_entries_end = original_map_entries_start + (map_size_count as usize) * 12;
    out.extend_from_slice(&bytes[original_map_entries_start..original_map_entries_end]);
    // Append our new map item: u2 type, u2 unused, u4 size, u4 offset.
    out.extend_from_slice(&map_type.to_le_bytes());
    out.extend_from_slice(&0u16.to_le_bytes());
    out.extend_from_slice(&map_size.to_le_bytes());
    out.extend_from_slice(&payload_off.to_le_bytes());

    // Patch header.
    write_u4_le(&mut out, MAP_OFF_OFF, new_map_off);
    let final_len = out.len() as u32;
    write_u4_le(&mut out, FILE_SIZE_OFF, final_len);
    if update_link {
        write_u4_le(&mut out, LINK_OFF_OFF, payload_off);
        write_u4_le(&mut out, LINK_SIZE_OFF, payload.len() as u32);
    }
    // Recompute SHA-1 (header bytes 32..) and Adler-32 (bytes 12..). Using the
    // same crates the builder uses keeps the reader from rejecting the file.
    let signature: [u8; 20] = {
        use sha1::{Digest, Sha1};
        let mut hasher = Sha1::new();
        hasher.update(&out[32..]);
        hasher.finalize().into()
    };
    out[12..32].copy_from_slice(&signature);
    let checksum = adler::adler32_slice(&out[12..]);
    write_u4_le(&mut out, 8, checksum);
    out
}

#[test]
fn link_data_round_trip_preserves_bytes() {
    use crate::dex::DexFile;
    use crate::dex::builder::build_dex_file_bytes;
    use crate::types::SmaliClass;

    let smali = r#"
.class public Lcom/example/L;
.super Ljava/lang/Object;

.method public static noop()V
    .registers 0
    return-void
.end method
"#;
    let class = SmaliClass::from_smali(smali).expect("parse");
    let dex_bytes = build_dex_file_bytes(std::slice::from_ref(&class)).expect("build dex");

    // Sanity: a freshly built DEX has no link_data.
    let baseline = DexFile::from_bytes(&dex_bytes).expect("parse baseline");
    assert!(
        baseline.link_data.is_empty(),
        "fresh build should have empty link_data"
    );

    // Inject 16 distinctive bytes as the link_data section.
    let payload: Vec<u8> = (0u8..16).map(|i| 0xA0 ^ i).collect();
    // link_data isn't tracked in the map list, but we still synthesize a map entry
    // so that overall offsets stay consistent and we exercise the same patcher.
    let patched = append_section_and_update_map(
        &dex_bytes,
        &payload,
        TYPE_HEADER_ITEM_PLACEHOLDER, // any unused type code is fine; reader keys on header link_off
        1,
        true,
    );

    let parsed = DexFile::from_bytes(&patched).expect("parse patched dex");
    assert_eq!(parsed.link_data, payload, "link_data must round-trip verbatim");
    assert_eq!(parsed.header.link_size as usize, payload.len());
    assert!(parsed.header.link_off > 0);
}

#[test]
fn hiddenapi_class_data_round_trip_preserves_bytes() {
    use crate::dex::DexFile;
    use crate::dex::builder::build_dex_file_bytes;
    use crate::types::SmaliClass;

    let smali = r#"
.class public Lcom/example/H;
.super Ljava/lang/Object;

.method public static noop()V
    .registers 0
    return-void
.end method
"#;
    let class = SmaliClass::from_smali(smali).expect("parse");
    let dex_bytes = build_dex_file_bytes(std::slice::from_ref(&class)).expect("build dex");

    let baseline = DexFile::from_bytes(&dex_bytes).expect("parse baseline");
    assert!(
        baseline.hiddenapi_class_data.is_empty(),
        "fresh build should have empty hiddenapi_class_data"
    );

    // Build a minimal hiddenapi_class_data_item: the first u4 is the section size
    // (including itself), then per-class data. With one class, the simplest valid
    // payload is `{ size, offset_for_class_0=0 }` meaning "no flags for any member".
    // Total size = 4 (size field) + 4 (one offset) = 8 bytes.
    let mut payload = Vec::new();
    payload.extend_from_slice(&8u32.to_le_bytes());
    payload.extend_from_slice(&0u32.to_le_bytes());
    let patched = append_section_and_update_map(
        &dex_bytes,
        &payload,
        TYPE_HIDDENAPI_CLASS_DATA_PLACEHOLDER,
        1,
        false,
    );

    let parsed = DexFile::from_bytes(&patched).expect("parse patched dex");
    assert_eq!(
        parsed.hiddenapi_class_data, payload,
        "hiddenapi_class_data must round-trip verbatim"
    );
}

const TYPE_HEADER_ITEM_PLACEHOLDER: u16 = 0x0000;
const TYPE_HIDDENAPI_CLASS_DATA_PLACEHOLDER: u16 = 0xF000;
