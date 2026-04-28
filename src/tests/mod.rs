mod dex_roundtrip;
mod opcode_tests;
mod dexdump_cases;
mod dex_debug;

#[cfg(test)]
mod parser_tests {
    use crate::types::{MethodSignature, ObjectIdentifier, SmaliClass, TypeSignature};
    use std::path::Path;

    #[test]
    fn object_identifier_to_jni() {
        let o = ObjectIdentifier::from_java_type("com.basic.Test");
        assert_eq!(o.as_java_type(), "com.basic.Test");
        assert_eq!(o.as_jni_type(), "Lcom/basic/Test;");
    }

    #[test]
    fn object_identifier_to_java() {
        let o = ObjectIdentifier::from_jni_type("Lcom/basic/Test;");
        assert_eq!(o.as_jni_type(), "Lcom/basic/Test;");
        assert_eq!(o.as_java_type(), "com.basic.Test");
    }

    #[test]
    fn signatures() {
        let t = TypeSignature::Bool;
        assert_eq!(t.to_jni(), "Z");
        let m = MethodSignature::from_jni("([I)V");
        assert_eq!(m.result, TypeSignature::Void);
    }

    #[test]
    fn parse_write() {
        let dex = SmaliClass::read_from_file(Path::new("tests/OkHttpClient.smali")).unwrap();
        let smali = dex.to_smali();

        // Attempt to parse the output
        let _dex = SmaliClass::from_smali(&smali).unwrap();
        //println!("{}\n", dex.to_smali());
    }

    #[test]
    fn roundtrip_registers_directive() {
        let smali = r#"
.class public Lcom/example/Test;
.super Ljava/lang/Object;

.method public foo(I)V
    .registers 2
    return-void
.end method
"#;
        let class = SmaliClass::from_smali(smali).unwrap();
        let out = class.to_smali();
        assert!(out.contains(".registers 2"));
        assert!(!out.contains(".locals"));
        let _ = SmaliClass::from_smali(&out).unwrap();
    }

    #[test]
    fn roundtrip_param_name_only() {
        let smali = r#"
.class public Lcom/example/Test;
.super Ljava/lang/Object;

.method public foo(I)V
    .registers 2
    .param p1, "value"
    return-void
.end method
"#;
        let class = SmaliClass::from_smali(smali).unwrap();
        let out = class.to_smali();
        assert!(out.contains(".param p1, \"value\""));
        let _ = SmaliClass::from_smali(&out).unwrap();
    }
}
