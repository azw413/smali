mod dex_roundtrip;
mod opcode_tests;
mod dexdump_cases;

#[cfg(test)]
mod tests {
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
        let dex = SmaliClass::from_smali(&smali).unwrap();
        println!("{}\n", dex.to_smali());
    }
}
