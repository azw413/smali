#[cfg(test)]
mod tests {

    use crate::dex::error::DexError;
    use crate::dex::opcode_format::assemble::{AssemblerIndexResolver, LineEvent, MethodAssembler};
    use crate::dex::opcode_format::decode;
    use crate::smali_ops::{MethodRef, SmaliRegister};
    use crate::types::{DexOp, MethodSignature, SmaliClass, SmaliOp};
    use std::collections::HashMap;

    struct DummyResolver;

    impl AssemblerIndexResolver for DummyResolver {
        fn string_index(&self, _value: &str) -> Result<u32, DexError> {
            Ok(0)
        }

        fn type_index(&self, _descriptor: &str) -> Result<u32, DexError> {
            Ok(0)
        }

        fn field_index(
            &self,
            _class_desc: &str,
            _name: &str,
            _type_desc: &str,
        ) -> Result<u32, DexError> {
            Ok(0)
        }

        fn method_index(
            &self,
            _class_desc: &str,
            _name: &str,
            _proto: &MethodSignature,
        ) -> Result<u32, DexError> {
            Ok(0)
        }

        fn proto_index(&self, _proto: &MethodSignature) -> Result<u32, DexError> {
            Ok(0)
        }

        fn call_site_index(&self, _name: &str) -> Result<u32, DexError> {
            Ok(0)
        }

        fn method_handle_index(&self, _literal: &str) -> Result<u32, DexError> {
            Ok(0)
        }
    }

    struct RecordingResolver {
        string_map: HashMap<String, u32>,
    }

    impl RecordingResolver {
        fn with_string(name: &str, idx: u32) -> Self {
            let mut map = HashMap::new();
            map.insert(name.to_string(), idx);
            RecordingResolver { string_map: map }
        }
    }

    impl AssemblerIndexResolver for RecordingResolver {
        fn string_index(&self, value: &str) -> Result<u32, DexError> {
            self.string_map
                .get(value)
                .copied()
                .ok_or_else(|| DexError::new("string not found in resolver"))
        }

        fn type_index(&self, _descriptor: &str) -> Result<u32, DexError> {
            Ok(0)
        }

        fn field_index(
            &self,
            _class_desc: &str,
            _name: &str,
            _type_desc: &str,
        ) -> Result<u32, DexError> {
            Ok(0)
        }

        fn method_index(
            &self,
            _class_desc: &str,
            _name: &str,
            _proto: &MethodSignature,
        ) -> Result<u32, DexError> {
            Ok(0)
        }

        fn proto_index(&self, _proto: &MethodSignature) -> Result<u32, DexError> {
            Ok(0)
        }

        fn call_site_index(&self, _name: &str) -> Result<u32, DexError> {
            Ok(0)
        }

        fn method_handle_index(&self, _literal: &str) -> Result<u32, DexError> {
            Ok(0)
        }
    }

    #[test]
    fn test_decode_bytecode_1() {
        let bc: Vec<u8> = vec![
            84, 32, 63, 26, 51, 3, 4, 0, 18, 0, 17, 0, 84, 32, 63, 26, 56, 0, 16, 0, 84, 33, 61,
            26, 56, 1, 5, 0, 114, 32, 152, 2, 16, 0, 84, 33, 65, 26, 56, 1, 5, 0, 114, 32, 153, 2,
            16, 0, 91, 35, 63, 26, 56, 3, 31, 0, 84, 33, 61, 26, 56, 1, 5, 0, 114, 32, 149, 2, 19,
            0, 84, 33, 65, 26, 56, 1, 5, 0, 114, 32, 150, 2, 19, 0, 26, 1, 145, 30, 114, 32, 137,
            2, 19, 0, 10, 1, 89, 33, 68, 26, 18, 17, 92, 33, 66, 26, 110, 16, 218, 47, 2, 0, 40,
            10, 18, 241, 89, 33, 68, 26, 18, 1, 92, 33, 66, 26, 110, 16, 219, 47, 2, 0, 17, 0,
        ];
        let ops = decode(bc.as_slice(), 33, 0).unwrap();

        //println!("{:?}", ops);
    }

    #[test]
    fn test_decode_bytecode_2() {
        let bc: Vec<u8> = vec![
            84, 16, 75, 26, 112, 48, 253, 47, 33, 0, 111, 32, 245, 47, 33, 0, 12, 0, 17, 0,
        ];
        let ops = decode(bc.as_slice(), 33, 0).unwrap();

        //println!("{:?}", ops);
    }

    #[test]
    fn bytecode_assembler_handles_arithmetic_ops() {
        let smali = r#"
.class public Lcom/example/Arith;
.super Ljava/lang/Object;

.method public static op(II)I
    .locals 1
    add-int v0, p0, p1
    add-int/2addr v0, p0
    sub-long p0, p0, p1
    mul-float v0, v0, v0
    div-double p0, p0, p0
    return v0
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "op")
            .expect("method present");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");
        let ops = decode(&code_units_to_bytes(&result.encoded_code_units), 33, 200)
            .expect("decode result");
        assert!(
            ops.iter()
                .any(|op| matches!(op, SmaliOp::Op(DexOp::AddInt { .. })))
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, SmaliOp::Op(DexOp::AddInt2Addr { .. })))
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, SmaliOp::Op(DexOp::SubLong { .. })))
        );
    }

    #[test]
    fn bytecode_assembler_handles_conversions_and_unary_ops() {
        let smali = r#"
.class public Lcom/example/Convert;
.super Ljava/lang/Object;

.method public static convert(D)I
    .locals 4
    neg-double v0, p0
    double-to-int v2, v0
    int-to-long v0, v2
    return v2
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "convert")
            .expect("method present");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");
        let ops = decode(&code_units_to_bytes(&result.encoded_code_units), 33, 200)
            .expect("decode result");
        assert!(
            ops.iter()
                .any(|op| matches!(op, SmaliOp::Op(DexOp::NegDouble { .. })))
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, SmaliOp::Op(DexOp::DoubleToInt { .. })))
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, SmaliOp::Op(DexOp::IntToLong { .. })))
        );
    }

    #[test]
    fn bytecode_assembler_handles_const_wide_high16() {
        let smali = r#"
.class public Lcom/example/Lit;
.super Ljava/lang/Object;

.method public static wide()J
    .locals 2
    const-wide/high16 v0, 0x1000000000000L
    return-wide v0
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "wide")
            .expect("method present");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");
        assert_eq!(result.encoded_code_units[0] & 0x00ff, 0x19);
        assert_eq!(result.encoded_code_units[1], 0x0001);
    }

    #[test]
    fn bytecode_assembler_emits_quick_field_accessors() {
        let smali = r#"
.class public Lcom/example/QuickField;
.super Ljava/lang/Object;

.method public static load()V
    .locals 2
    iget v0, v1, Lquick;->field@3:I
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "load")
            .expect("method present");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");
        assert_eq!(result.encoded_code_units[0] & 0x00ff, 0xf2);
    }

    #[test]
    fn bytecode_assembler_emits_quick_invoke() {
        let smali = r#"
.class public Lcom/example/QuickInvoke;
.super Ljava/lang/Object;

.method public static call()V
    .locals 1
    invoke-virtual {v0}, Lquick;->virtual@5()V
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "call")
            .expect("method present");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");
        assert_eq!(result.encoded_code_units[0] & 0x00ff, 0xf8);
    }

    #[test]
    fn bytecode_assembler_emits_throw_verification_error() {
        let smali = r#"
.class public Lcom/example/Verify;
.super Ljava/lang/Object;

.method public static broken()V
    .locals 0
    throw-verification-error 7, Ljava/lang/Object;
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "broken")
            .expect("method present");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");
        assert_eq!(result.encoded_code_units[0] & 0x00ff, 0xed);
        assert_eq!(result.encoded_code_units[0] >> 8, 7);
    }

    #[test]
    fn bytecode_assembler_emits_execute_inline_variants() {
        let smali = r#"
.class public Lcom/example/Inline;
.super Ljava/lang/Object;

.method public static call()V
    .locals 3
    execute-inline {v0, v1}, inline@0x21
    execute-inline/range {v0 .. v2}, inline@5
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "call")
            .expect("method present");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");
        assert_eq!(result.encoded_code_units[0] & 0x00ff, 0xee);
        assert_eq!(result.encoded_code_units[1], 0x21);
        assert_eq!(result.encoded_code_units[3] & 0x00ff, 0xef);
        assert_eq!(result.encoded_code_units[4], 5);
    }

    #[test]
    fn bytecode_assembler_handles_trivial_return() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public static bridge()V
.locals 0
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "bridge")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");
        assert_eq!(result.registers_size, result.ins_size);
        assert_eq!(result.encoded_code_units.len(), 1);
    }

    #[test]
    fn bytecode_assembler_encodes_basic_const_and_branch() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public static bridge()V
    .locals 1
    const/4 v0, 0x1
    goto :done
:done
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "bridge")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");

        assert_eq!(result.registers_size, 1);
        assert_eq!(result.ins_size, 0);
        assert_eq!(result.outs_size, 0);
        assert!(!result.encoded_code_units.is_empty());

        let bytes = code_units_to_bytes(&result.encoded_code_units);

        let decoded = decode(bytes.as_slice(), 33, 0).expect("decode assembled bytes");
        let ops: Vec<_> = decoded
            .into_iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();

        assert!(matches!(ops.first(), Some(DexOp::Const4 { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::Goto { .. }))); // label gets normalized
        assert!(matches!(ops.last(), Some(DexOp::ReturnVoid)));
    }

    #[test]
    fn bytecode_assembler_encodes_const_string_and_return_object() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public static bridge()Ljava/lang/String;
    .locals 1
    const-string v0, "hi"
    return-object v0
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "bridge")
            .expect("method exists");
        let resolver = RecordingResolver::with_string("hi", 1);
        let assembler = MethodAssembler::new(&resolver, 33, 0);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");

        assert_eq!(result.registers_size, 1);
        assert_eq!(result.encoded_code_units.len(), 3);

        let bytes = code_units_to_bytes(&result.encoded_code_units);
        let decoded = decode(bytes.as_slice(), 33, 0).expect("decode assembled bytes");
        let ops: Vec<DexOp> = decoded
            .into_iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();
        assert!(matches!(ops.first(), Some(DexOp::ConstString { .. }))); // placeholder string value
        assert!(matches!(ops.last(), Some(DexOp::ReturnObject { .. })));
    }

    #[test]
    fn bytecode_assembler_handles_moves_and_wide_consts() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public wide()J
    .locals 4
    const-wide/32 v0, 0x1
    move-wide/16 v2, v0
    return-wide v2
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "wide")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");

        assert!(result.encoded_code_units.len() >= 3);
        let bytes = code_units_to_bytes(&result.encoded_code_units);
        let decoded = decode(bytes.as_slice(), 33, 200).expect("decode assembled");
        let ops: Vec<DexOp> = decoded
            .into_iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();
        assert!(ops.iter().any(|op| matches!(
            op,
            DexOp::Const { .. } | DexOp::ConstWide { .. } | DexOp::ConstWide32 { .. }
        ))); // ensure constant present
        assert!(ops.iter().any(|op| matches!(op, DexOp::MoveWide16 { .. })));
        assert!(matches!(ops.last(), Some(DexOp::ReturnWide { .. })));
    }

    #[test]
    fn bytecode_assembler_encodes_if_branches() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public static branchy(I)I
    .locals 2
    const/4 v0, 0x0
    if-eq p0, p0, :equal
    const/4 v0, 0x1
    goto :done
:equal
    const/4 v0, 0x2
:done
    if-eqz v0, :zero
    if-eq v0, v0, :equal
    return v0
:zero
    return v0
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "branchy")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");

        let bytes = code_units_to_bytes(&result.encoded_code_units);
        let decoded = decode(bytes.as_slice(), 33, 200).expect("decode assembled");
        let ops: Vec<DexOp> = decoded
            .into_iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();

        assert!(ops.iter().any(|op| matches!(op, DexOp::IfEq { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::IfEqz { .. })));
    }

    #[test]
    fn bytecode_assembler_enforces_if_register_limits() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public static too_many_locals()V
    .locals 17
    if-eq v16, v0, :exit
    return-void
:exit
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "too_many_locals")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let err = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect_err("register overflow should fail");
        assert!(err.to_string().contains("first operand register index"));
    }

    #[test]
    fn bytecode_assembler_encodes_cmp_family() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public static cmp()V
    .locals 6
    cmp-long v0, v2, v4
    cmpl-double v1, v2, v4
    cmpg-double v2, v2, v4
    cmpl-float v3, v0, v1
    cmpg-float v4, v0, v1
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "cmp")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");

        let bytes = code_units_to_bytes(&result.encoded_code_units);
        let decoded = decode(bytes.as_slice(), 33, 200).expect("decode assembled");
        let ops: Vec<DexOp> = decoded
            .into_iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();

        assert!(ops.iter().any(|op| matches!(op, DexOp::CmpLong { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::CmplDouble { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::CmpgDouble { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::CmplFloat { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::CmpgFloat { .. })));
    }

    #[test]
    fn bytecode_assembler_handles_array_ops() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public static arrays([I[J[Ljava/lang/Object;[Z)V
    .locals 6
    const/4 v0, 0x0
    const/4 v1, 0x1
    aget v2, p0, v0
    aget-wide v3, p1, v0
    aget-object v5, p2, v1
    aget-boolean v1, p3, v0
    aput v2, p0, v0
    aput-wide v3, p1, v0
    aput-object v5, p2, v1
    aput-boolean v1, p3, v0
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "arrays")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");

        let bytes = code_units_to_bytes(&result.encoded_code_units);
        let decoded = decode(bytes.as_slice(), 33, 200).expect("decode assembled");
        let ops: Vec<DexOp> = decoded
            .into_iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();

        assert!(ops.iter().any(|op| matches!(op, DexOp::AGet { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::AGetWide { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::APutObject { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::APutBoolean { .. })));
    }

    #[test]
    fn bytecode_assembler_handles_field_access() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.field public value:I
.field public static counter:I

.method public fieldOps(I)V
    .locals 4
    iget v0, p0, Lcom/example/Asm;->value:I
    iput p1, p0, Lcom/example/Asm;->value:I
    sget v1, Lcom/example/Asm;->counter:I
    sput v0, Lcom/example/Asm;->counter:I
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "fieldOps")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");

        let bytes = code_units_to_bytes(&result.encoded_code_units);
        let decoded = decode(bytes.as_slice(), 33, 200).expect("decode assembled");
        let ops: Vec<DexOp> = decoded
            .into_iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();

        assert!(ops.iter().any(|op| matches!(op, DexOp::IGet { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::IPut { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::SGet { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::SPut { .. })));
    }

    #[test]
    fn bytecode_assembler_handles_type_ops() {
        let smali = r#"
.class public Lcom/example/TypeOps;
.super Ljava/lang/Object;

.method public static build()V
    .locals 8
    const-class v0, Ljava/lang/String;
    const-method-type v1, ([Ljava/lang/Object;)Ljava/lang/Object;
    const-method-handle v2, "invoke-static {v0}, Ljava/lang/Object;->notify()V"
    const/4 v5, 0x3
    new-instance v3, Ljava/lang/StringBuilder;
    new-array v4, v5, [I
    check-cast v3, Ljava/lang/CharSequence;
    instance-of v6, v3, Ljava/lang/String;
    monitor-enter v3
    monitor-exit v3
    filled-new-array {v0, v1, v6}, [Ljava/lang/Object;
    filled-new-array/range {v0 .. v2}, [Ljava/lang/String;
    throw v3
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "build")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");
        let bytes = code_units_to_bytes(&result.encoded_code_units);
        let decoded = decode(bytes.as_slice(), 33, 200).expect("decode assembled");
        let ops: Vec<DexOp> = decoded
            .into_iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();

        assert!(ops.iter().any(|op| matches!(op, DexOp::ConstClass { .. })));
        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::ConstMethodType { .. }))
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::ConstMethodHandle { .. }))
        );
        assert!(ops.iter().any(|op| matches!(op, DexOp::NewInstance { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::NewArray { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::CheckCast { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::InstanceOf { .. })));
        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::MonitorEnter { .. }))
        );
        assert!(ops.iter().any(|op| matches!(op, DexOp::MonitorExit { .. })));
        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::FilledNewArray { .. }))
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::FilledNewArrayRange { .. }))
        );
        assert!(ops.iter().any(|op| matches!(op, DexOp::Throw { .. })));
    }

    #[test]
    fn bytecode_assembler_handles_payload_ops() {
        let smali = r#"
.class public Lcom/example/Payloads;
.super Ljava/lang/Object;

.method public static payload(I)V
    .locals 3
    const/4 v0, 0x0
    fill-array-data v0, :array_data
    packed-switch v0, :packed_table
    sparse-switch v0, :sparse_table
    return-void

:case_a
    return-void

:case_b
    return-void

    nop
:array_data
.array-data 0x2
    0x1s
    0x2s
.end array-data

    nop
:packed_table
.packed-switch 0x0
    :case_a
    :case_b
.end packed-switch

    nop
:sparse_table
.sparse-switch
    0x5 -> :case_a
    0x6 -> :case_b
.end sparse-switch
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "payload")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");
        let bytes = code_units_to_bytes(&result.encoded_code_units);
        let decoded = decode(bytes.as_slice(), 33, 200).expect("decode assembled");

        let op_refs: Vec<&DexOp> = decoded
            .iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();
        assert!(
            op_refs
                .iter()
                .any(|op| matches!(op, DexOp::FillArrayData { .. }))
        );
        assert!(
            op_refs
                .iter()
                .any(|op| matches!(op, DexOp::PackedSwitch { .. }))
        );
        assert!(
            op_refs
                .iter()
                .any(|op| matches!(op, DexOp::SparseSwitch { .. }))
        );

        let array_dir = decoded.iter().find_map(|op| match op {
            SmaliOp::ArrayData(dir) => Some(dir),
            _ => None,
        });
        assert!(array_dir.is_some());
        if let Some(dir) = array_dir {
            assert_eq!(dir.elements.len(), 2);
        }

        let packed_dir = decoded.iter().find_map(|op| match op {
            SmaliOp::PackedSwitch(dir) => Some(dir),
            _ => None,
        });
        assert!(packed_dir.is_some());
        if let Some(dir) = packed_dir {
            assert_eq!(dir.targets.len(), 2);
        }

        let sparse_dir = decoded.iter().find_map(|op| match op {
            SmaliOp::SparseSwitch(dir) => Some(dir),
            _ => None,
        });
        assert!(sparse_dir.is_some());
        if let Some(dir) = sparse_dir {
            assert_eq!(dir.entries.len(), 2);
        }
    }

    #[test]
    fn bytecode_assembler_enforces_field_register_limits() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.field public value:I

.method public static bad()V
    .locals 17
    iget v16, v15, Lcom/example/Asm;->value:I
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "bad")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let err = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect_err("register overflow should fail");
        assert!(err.to_string().contains("iget dest"));
    }

    #[test]
    fn bytecode_assembler_handles_literal_ops() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public static litops(I)I
    .locals 3
    add-int/lit8 v0, p0, 0x7
    rsub-int v0, v0, 0x1234
    and-int/lit16 v1, v0, 0xff
    shl-int/lit8 v2, v1, 0x1
    return v2
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "litops")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");

        let bytes = code_units_to_bytes(&result.encoded_code_units);
        let decoded = decode(bytes.as_slice(), 33, 200).expect("decode assembled");
        let ops: Vec<DexOp> = decoded
            .into_iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();

        assert!(ops.iter().any(|op| matches!(op, DexOp::AddIntLit8 { .. })));
        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::RSubIntLit16 { .. }))
        );
        assert!(ops.iter().any(|op| matches!(op, DexOp::AndIntLit16 { .. })));
        assert!(ops.iter().any(|op| matches!(op, DexOp::ShlIntLit8 { .. })));
    }

    #[test]
    fn bytecode_assembler_rejects_lit16_register_overflow() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public static badlit()V
    .locals 17
    add-int/lit16 v16, v0, 0x1
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "badlit")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 0);
        let err = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect_err("register overflow should fail");
        assert!(err.to_string().contains("add-int/lit16 dest"));
    }

    #[test]
    fn bytecode_assembler_handles_invoke_variants() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public static calls(Ljava/lang/String;)V
    .locals 4
    invoke-static {p0}, Lcom/example/Asm;->helper(Ljava/lang/String;)V
    invoke-direct {p0}, Lcom/example/Asm;->init()V
    invoke-virtual {p0}, Ljava/lang/String;->length()I
    invoke-virtual/range {p0 .. p0}, Ljava/lang/String;->toString()Ljava/lang/String;
    invoke-custom {p0}, "CallSite0"
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "calls")
            .expect("method exists");
        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");

        let bytes = code_units_to_bytes(&result.encoded_code_units);
        let decoded = decode(bytes.as_slice(), 33, 200).expect("decode assembled");
        let ops: Vec<DexOp> = decoded
            .into_iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();

        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::InvokeStatic { .. }))
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::InvokeDirect { .. }))
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::InvokeVirtual { .. }))
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::InvokeVirtualRange { .. }))
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::InvokeCustom { .. }))
        );
    }

    #[test]
    fn bytecode_assembler_handles_polymorphic_invokes() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public bridge()V
    .locals 6
    invoke-polymorphic {v0, v1}, Ljava/lang/invoke/MethodHandle;->invokeExact([Ljava/lang/Object;)Ljava/lang/Object;, ([Ljava/lang/Object;)V
    invoke-polymorphic/range {v2 .. v4}, Ljava/lang/invoke/MethodHandle;->invoke(Ljava/lang/Object;)Ljava/lang/Object;, (II)I
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter()
            .find(|m| m.name == "bridge")
            .expect("method exists");

        // Ensure the parser captured the full method proto descriptors.
        assert!(matches!(
            method.ops.get(0),
            Some(SmaliOp::Op(DexOp::InvokePolymorphic { proto, .. }))
                if proto == "([Ljava/lang/Object;)V"
        ));
        assert!(matches!(
            method.ops.get(1),
            Some(SmaliOp::Op(DexOp::InvokePolymorphicRange { proto, .. }))
                if proto == "(II)I"
        ));

        let assembler = MethodAssembler::new(&DummyResolver, 33, 200);
        let result = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect("assemble");
        let bytes = code_units_to_bytes(&result.encoded_code_units);
        let decoded = decode(bytes.as_slice(), 33, 200).expect("decode assembled");
        let ops: Vec<DexOp> = decoded
            .into_iter()
            .filter_map(|op| match op {
                SmaliOp::Op(inner) => Some(inner),
                _ => None,
            })
            .collect();

        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::InvokePolymorphic { .. }))
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, DexOp::InvokePolymorphicRange { .. }))
        );
    }

    #[test]
    fn bytecode_assembler_rejects_invoke_register_overflow() {
        let smali = r#"
.class public Lcom/example/Asm;
.super Ljava/lang/Object;

.method public static spill()V
    .locals 10
    return-void
.end method
"#;

        let mut class = SmaliClass::from_smali(smali).expect("parse class");
        let method = class
            .methods
            .iter_mut()
            .find(|m| m.name == "spill")
            .expect("method exists");
        let invoke = DexOp::InvokeVirtual {
            registers: vec![
                SmaliRegister::Local(0),
                SmaliRegister::Local(1),
                SmaliRegister::Local(2),
                SmaliRegister::Local(3),
                SmaliRegister::Local(4),
                SmaliRegister::Local(5),
            ],
            method: MethodRef {
                class: "Lcom/example/Asm;".to_string(),
                name: "helper".to_string(),
                descriptor: "(I)V".to_string(),
            },
        };
        method.ops = vec![SmaliOp::Op(invoke), SmaliOp::Op(DexOp::ReturnVoid)];

        let assembler = MethodAssembler::new(&DummyResolver, 33, 0);
        let err = assembler
            .assemble_method(&class.name.as_jni_type(), method)
            .expect_err("invoke should fail due to register overflow");
        assert!(err.to_string().contains("maximum for this form is 5"));
    }

    fn code_units_to_bytes(units: &[u16]) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(units.len() * 2);
        for cu in units {
            bytes.push((*cu & 0x00ff) as u8);
            bytes.push((*cu >> 8) as u8);
        }
        bytes
    }
}
