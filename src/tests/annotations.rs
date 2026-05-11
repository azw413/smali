//! End-to-end annotation round-trip coverage.
//!
//! Each test builds a class with a specific annotation shape, runs it through
//! `smali -> DEX bytes -> smali`, and asserts the meaningful payload survives.
//! The tests aim to cover every EncodedValue variant the DEX format defines.

use crate::dex::DexFile;
use crate::dex::builder::build_dex_file_bytes;
use crate::types::{AnnotationValue, SmaliAnnotation, SmaliClass};

/// Build a class from smali, assemble to DEX, parse back, and return the
/// recovered SmaliClass. Asserts a single class survived the trip.
fn round_trip(smali: &str) -> SmaliClass {
    let class = SmaliClass::from_smali(smali).expect("parse smali");
    let bytes = build_dex_file_bytes(std::slice::from_ref(&class)).expect("assemble dex");
    let parsed = DexFile::from_bytes(&bytes).expect("reparse dex");
    let mut classes = parsed.to_smali().expect("decode classes");
    assert_eq!(classes.len(), 1, "expected exactly one class after round-trip");
    classes.remove(0)
}

fn find_annotation<'a>(class: &'a SmaliClass, type_desc: &str) -> Option<&'a SmaliAnnotation> {
    class
        .annotations
        .iter()
        .find(|a| a.annotation_type.to_jni() == type_desc)
}

fn find_element<'a>(ann: &'a SmaliAnnotation, name: &str) -> Option<&'a AnnotationValue> {
    ann.elements
        .iter()
        .find(|e| e.name == name)
        .map(|e| &e.value)
}

/// Assert `value` is `Single` and equals one of the acceptable spellings.
/// Different code paths may legitimately re-emit a literal slightly
/// differently (e.g. `0x2a` vs `42`), so we accept any of the supplied forms.
fn assert_single_matches(value: &AnnotationValue, accepted: &[&str], what: &str) {
    match value {
        AnnotationValue::Single(s) => {
            let trimmed = s.trim();
            assert!(
                accepted.iter().any(|a| *a == trimmed),
                "{what}: expected one of {:?}, got {:?}",
                accepted,
                trimmed
            );
        }
        other => panic!("{what}: expected Single, got {:?}", other),
    }
}

const PRIMITIVES_CLASS: &str = r#"
.class public abstract Lcom/example/PrimAnn;
.super Ljava/lang/Object;
.implements Ljava/lang/annotation/Annotation;

.annotation runtime Ljava/lang/annotation/Retention;
    value = .enum Ljava/lang/annotation/RetentionPolicy;->RUNTIME:Ljava/lang/annotation/RetentionPolicy;
.end annotation

.method public abstract b()B
.end method
.method public abstract s()S
.end method
.method public abstract c()C
.end method
.method public abstract i()I
.end method
.method public abstract j()J
.end method
.method public abstract f()F
.end method
.method public abstract d()D
.end method
.method public abstract z()Z
.end method
.method public abstract str()Ljava/lang/String;
.end method
"#;

const USES_PRIMITIVES_CLASS: &str = r#"
.class public Lcom/example/UsesPrim;
.super Ljava/lang/Object;

.annotation runtime Lcom/example/PrimAnn;
    b = -7
    s = 1000
    c = 65
    i = 0x2a
    j = 0x100000000L
    f = 1.5f
    d = 3.5
    z = true
    str = "hi"
.end annotation
"#;

#[test]
fn primitive_annotation_values_round_trip() {
    // The annotation declaration provides the schema (return types) so that
    // the type-directed encoder picks the right EncodedValue width.
    let ann_decl = SmaliClass::from_smali(PRIMITIVES_CLASS).expect("parse annotation decl");
    let user = SmaliClass::from_smali(USES_PRIMITIVES_CLASS).expect("parse user class");

    let bytes = build_dex_file_bytes(&[ann_decl, user]).expect("assemble");
    let parsed = DexFile::from_bytes(&bytes).expect("reparse");
    let classes = parsed.to_smali().expect("decode");
    let user_rt = classes
        .iter()
        .find(|c| c.name.as_jni_type() == "Lcom/example/UsesPrim;")
        .expect("recover user class");

    let ann = find_annotation(user_rt, "Lcom/example/PrimAnn;")
        .expect("PrimAnn annotation present");
    assert_single_matches(find_element(ann, "b").unwrap(), &["-7"], "b");
    assert_single_matches(find_element(ann, "s").unwrap(), &["1000"], "s");
    assert_single_matches(find_element(ann, "c").unwrap(), &["65"], "c");
    assert_single_matches(find_element(ann, "i").unwrap(), &["42", "0x2a"], "i");
    assert_single_matches(
        find_element(ann, "j").unwrap(),
        &["4294967296", "0x100000000"],
        "j",
    );
    assert_single_matches(find_element(ann, "f").unwrap(), &["1.5"], "f");
    assert_single_matches(find_element(ann, "d").unwrap(), &["3.5"], "d");
    assert_single_matches(find_element(ann, "z").unwrap(), &["true"], "z");
    assert_single_matches(find_element(ann, "str").unwrap(), &["\"hi\""], "str");
}

#[test]
fn nested_subannotation_round_trips() {
    let inner_decl = r#"
.class public abstract Lcom/example/Inner;
.super Ljava/lang/Object;
.implements Ljava/lang/annotation/Annotation;
.method public abstract name()Ljava/lang/String;
.end method
"#;
    let outer_decl = r#"
.class public abstract Lcom/example/Outer;
.super Ljava/lang/Object;
.implements Ljava/lang/annotation/Annotation;
.method public abstract child()Lcom/example/Inner;
.end method
"#;
    let user = r#"
.class public Lcom/example/Holder;
.super Ljava/lang/Object;

.annotation runtime Lcom/example/Outer;
    child = .subannotation Lcom/example/Inner;
        name = "deep"
    .end subannotation
.end annotation
"#;
    let inner = SmaliClass::from_smali(inner_decl).expect("inner");
    let outer = SmaliClass::from_smali(outer_decl).expect("outer");
    let holder = SmaliClass::from_smali(user).expect("holder");
    let bytes = build_dex_file_bytes(&[inner, outer, holder]).expect("assemble");
    let parsed = DexFile::from_bytes(&bytes).expect("parse");
    let classes = parsed.to_smali().expect("decode");

    let holder_rt = classes
        .iter()
        .find(|c| c.name.as_jni_type() == "Lcom/example/Holder;")
        .expect("holder back");
    let outer_ann = find_annotation(holder_rt, "Lcom/example/Outer;").expect("outer ann");
    let child_val = find_element(outer_ann, "child").expect("child element");
    match child_val {
        AnnotationValue::SubAnnotation(sub) => {
            assert_eq!(sub.annotation_type.to_jni(), "Lcom/example/Inner;");
            let name = find_element(sub, "name").expect("name element");
            assert_single_matches(name, &["\"deep\""], "inner.name");
        }
        other => panic!("expected SubAnnotation, got {:?}", other),
    }
}

#[test]
fn enum_annotation_value_round_trips() {
    // Standard pattern: an enum field. `.enum` literal in the value position.
    let enum_decl = r#"
.class public final enum Lcom/example/Color;
.super Ljava/lang/Enum;

.field public static final RED:Lcom/example/Color;
.field public static final BLUE:Lcom/example/Color;
"#;
    let ann_decl = r#"
.class public abstract Lcom/example/Hue;
.super Ljava/lang/Object;
.implements Ljava/lang/annotation/Annotation;
.method public abstract color()Lcom/example/Color;
.end method
"#;
    let user = r#"
.class public Lcom/example/Box;
.super Ljava/lang/Object;

.annotation runtime Lcom/example/Hue;
    color = .enum Lcom/example/Color;->RED:Lcom/example/Color;
.end annotation
"#;
    let bytes = build_dex_file_bytes(&[
        SmaliClass::from_smali(enum_decl).expect("enum"),
        SmaliClass::from_smali(ann_decl).expect("ann decl"),
        SmaliClass::from_smali(user).expect("user"),
    ])
    .expect("assemble");
    let parsed = DexFile::from_bytes(&bytes).expect("reparse");
    let classes = parsed.to_smali().expect("decode");
    let user_rt = classes
        .iter()
        .find(|c| c.name.as_jni_type() == "Lcom/example/Box;")
        .expect("user back");
    let hue = find_annotation(user_rt, "Lcom/example/Hue;").expect("hue ann");
    let color = find_element(hue, "color").expect("color element");
    match color {
        AnnotationValue::Enum(class_id, name) => {
            assert_eq!(class_id.as_jni_type(), "Lcom/example/Color;");
            assert_eq!(name, "RED");
        }
        other => panic!("expected Enum, got {:?}", other),
    }
}

#[test]
fn array_annotation_value_round_trips() {
    let ann_decl = r#"
.class public abstract Lcom/example/Labels;
.super Ljava/lang/Object;
.implements Ljava/lang/annotation/Annotation;
.method public abstract names()[Ljava/lang/String;
.end method
"#;
    let user = r#"
.class public Lcom/example/Tag;
.super Ljava/lang/Object;

.annotation runtime Lcom/example/Labels;
    names = { "alpha", "beta", "gamma" }
.end annotation
"#;
    let bytes = build_dex_file_bytes(&[
        SmaliClass::from_smali(ann_decl).expect("ann"),
        SmaliClass::from_smali(user).expect("user"),
    ])
    .expect("assemble");
    let parsed = DexFile::from_bytes(&bytes).expect("reparse");
    let classes = parsed.to_smali().expect("decode");
    let user_rt = classes
        .iter()
        .find(|c| c.name.as_jni_type() == "Lcom/example/Tag;")
        .expect("user back");
    let labels = find_annotation(user_rt, "Lcom/example/Labels;").expect("labels");
    let names = find_element(labels, "names").expect("names element");
    match names {
        AnnotationValue::Array(items) => {
            let strs: Vec<&str> = items.iter().map(|s| s.trim()).collect();
            assert!(strs.contains(&"\"alpha\""));
            assert!(strs.contains(&"\"beta\""));
            assert!(strs.contains(&"\"gamma\""));
        }
        other => panic!("expected Array, got {:?}", other),
    }
}

#[test]
fn type_value_in_annotation_round_trips() {
    // Annotation methods returning Class<?> show up as VALUE_TYPE.
    let ann_decl = r#"
.class public abstract Lcom/example/Boxed;
.super Ljava/lang/Object;
.implements Ljava/lang/annotation/Annotation;
.method public abstract clazz()Ljava/lang/Class;
.end method
"#;
    let user = r#"
.class public Lcom/example/UseBoxed;
.super Ljava/lang/Object;

.annotation runtime Lcom/example/Boxed;
    clazz = Ljava/lang/String;
.end annotation
"#;
    let user_rt = {
        let bytes = build_dex_file_bytes(&[
            SmaliClass::from_smali(ann_decl).expect("ann"),
            SmaliClass::from_smali(user).expect("user"),
        ])
        .expect("assemble");
        let parsed = DexFile::from_bytes(&bytes).expect("reparse");
        let classes = parsed.to_smali().expect("decode");
        classes
            .into_iter()
            .find(|c| c.name.as_jni_type() == "Lcom/example/UseBoxed;")
            .expect("user back")
    };
    let ann = find_annotation(&user_rt, "Lcom/example/Boxed;").expect("ann");
    assert_single_matches(
        find_element(ann, "clazz").unwrap(),
        &["Ljava/lang/String;"],
        "clazz",
    );
}

#[test]
fn null_annotation_value_round_trips() {
    let ann_decl = r#"
.class public abstract Lcom/example/MaybeStr;
.super Ljava/lang/Object;
.implements Ljava/lang/annotation/Annotation;
.method public abstract value()Ljava/lang/String;
.end method
"#;
    let user = r#"
.class public Lcom/example/Nullable;
.super Ljava/lang/Object;

.annotation runtime Lcom/example/MaybeStr;
    value = null
.end annotation
"#;
    let user_rt = {
        let bytes = build_dex_file_bytes(&[
            SmaliClass::from_smali(ann_decl).expect("ann"),
            SmaliClass::from_smali(user).expect("user"),
        ])
        .expect("assemble");
        let parsed = DexFile::from_bytes(&bytes).expect("reparse");
        let classes = parsed.to_smali().expect("decode");
        classes
            .into_iter()
            .find(|c| c.name.as_jni_type() == "Lcom/example/Nullable;")
            .expect("user back")
    };
    let ann = find_annotation(&user_rt, "Lcom/example/MaybeStr;").expect("ann");
    assert_single_matches(find_element(ann, "value").unwrap(), &["null"], "value");
}

#[test]
fn method_handle_annotation_value_round_trips() {
    // VALUE_METHOD_HANDLE in an annotation. The bootstrap kind is invoke-static.
    let smali = r#"
.class public Lcom/example/HandleAnn;
.super Ljava/lang/Object;

.annotation system Lcom/example/HandleHolder;
    value = invoke-static@Lcom/example/T;->bootstrap(Ljava/lang/Object;)Ljava/lang/Object;
.end annotation

.method public static bootstrap(Ljava/lang/Object;)Ljava/lang/Object;
    .registers 1
    return-object p0
.end method
"#;
    let class = SmaliClass::from_smali(smali).expect("parse");
    let bytes = build_dex_file_bytes(std::slice::from_ref(&class)).expect("assemble");
    let parsed = DexFile::from_bytes(&bytes).expect("reparse");
    assert!(
        !parsed.method_handles.is_empty(),
        "method handle pool should be populated"
    );
    let classes = parsed.to_smali().expect("decode");
    let rt = classes
        .iter()
        .find(|c| c.name.as_jni_type() == "Lcom/example/HandleAnn;")
        .expect("class back");
    let ann = find_annotation(rt, "Lcom/example/HandleHolder;").expect("annotation back");
    // The disassembler emits the same `kind@member` form the smali parser
    // accepts, so the literal must round-trip without distortion.
    let value = find_element(ann, "value").expect("value element");
    let recovered = match value {
        AnnotationValue::Single(s) => s.trim().to_string(),
        other => panic!("expected Single for method handle, got {:?}", other),
    };
    assert!(
        recovered.contains("invoke-static@") && recovered.contains("Lcom/example/T;->bootstrap"),
        "method handle literal lost detail: {recovered}"
    );
}

#[test]
fn method_type_annotation_value_round_trips() {
    // VALUE_METHOD_TYPE: an annotation method returning java.lang.invoke.MethodType.
    let ann_decl = r#"
.class public abstract Lcom/example/HasType;
.super Ljava/lang/Object;
.implements Ljava/lang/annotation/Annotation;
.method public abstract sig()Ljava/lang/invoke/MethodType;
.end method
"#;
    let user = r#"
.class public Lcom/example/Owner;
.super Ljava/lang/Object;

.method public stub()V
    .registers 1
    return-void
.end method

.annotation runtime Lcom/example/HasType;
    sig = (Ljava/lang/String;)V
.end annotation
"#;
    let bytes = build_dex_file_bytes(&[
        SmaliClass::from_smali(ann_decl).expect("ann"),
        SmaliClass::from_smali(user).expect("user"),
    ])
    .expect("assemble");
    let parsed = DexFile::from_bytes(&bytes).expect("reparse");
    let classes = parsed.to_smali().expect("decode");
    let rt = classes
        .iter()
        .find(|c| c.name.as_jni_type() == "Lcom/example/Owner;")
        .expect("owner back");
    let ann = find_annotation(rt, "Lcom/example/HasType;").expect("annotation back");
    assert_single_matches(
        find_element(ann, "sig").unwrap(),
        &["(Ljava/lang/String;)V"],
        "sig",
    );
}

#[test]
fn field_and_method_annotations_attach_to_their_owners() {
    let smali = r#"
.class public Lcom/example/Targets;
.super Ljava/lang/Object;

.field public f:I
    .annotation runtime Lcom/example/F;
        v = 1
    .end annotation
.end field

.method public m()V
    .registers 1
    .annotation runtime Lcom/example/M;
        v = 2
    .end annotation
    return-void
.end method
"#;
    let recovered = round_trip(smali);
    let field = recovered
        .fields
        .iter()
        .find(|f| f.name == "f")
        .expect("field present");
    assert!(
        field.annotations.iter().any(|a| a.annotation_type.to_jni() == "Lcom/example/F;"),
        "field annotation lost"
    );
    let method = recovered
        .methods
        .iter()
        .find(|m| m.name == "m")
        .expect("method present");
    assert!(
        method.annotations.iter().any(|a| a.annotation_type.to_jni() == "Lcom/example/M;"),
        "method annotation lost"
    );
}

#[test]
fn parameter_annotations_attach_to_their_parameters() {
    let smali = r#"
.class public Lcom/example/PAnn;
.super Ljava/lang/Object;

.method public m(II)V
    .registers 3
    .param p1
        .annotation runtime Lcom/example/A;
            v = 1
        .end annotation
    .end param
    .param p2
        .annotation runtime Lcom/example/B;
            v = 2
        .end annotation
    .end param
    return-void
.end method
"#;
    let recovered = round_trip(smali);
    let method = recovered
        .methods
        .iter()
        .find(|m| m.name == "m")
        .expect("method present");
    let p1 = method.params.iter().find(|p| p.register == "p1").expect("p1");
    assert!(
        p1.annotations.iter().any(|a| a.annotation_type.to_jni() == "Lcom/example/A;"),
        "p1 annotation lost"
    );
    let p2 = method.params.iter().find(|p| p.register == "p2").expect("p2");
    assert!(
        p2.annotations.iter().any(|a| a.annotation_type.to_jni() == "Lcom/example/B;"),
        "p2 annotation lost"
    );
}

#[test]
fn all_visibility_levels_round_trip() {
    use crate::types::AnnotationVisibility;
    let smali = r#"
.class public Lcom/example/Vis;
.super Ljava/lang/Object;

.annotation build Lcom/example/Build;
    v = 0
.end annotation

.annotation runtime Lcom/example/Runtime;
    v = 1
.end annotation

.annotation system Lcom/example/System;
    v = 2
.end annotation
"#;
    let recovered = round_trip(smali);
    let build_ann = find_annotation(&recovered, "Lcom/example/Build;").expect("build ann");
    let runtime_ann = find_annotation(&recovered, "Lcom/example/Runtime;").expect("runtime ann");
    let system_ann = find_annotation(&recovered, "Lcom/example/System;").expect("system ann");
    assert!(matches!(build_ann.visibility, AnnotationVisibility::Build));
    assert!(matches!(runtime_ann.visibility, AnnotationVisibility::Runtime));
    assert!(matches!(system_ann.visibility, AnnotationVisibility::System));
}

#[test]
fn annotation_default_values_round_trip() {
    // The AnnotationDefault pattern: an `@AnnotationName(...)` declaration carries
    // a system annotation `Ldalvik/annotation/AnnotationDefault;` whose `value`
    // is a subannotation of the same type containing the default for each
    // abstract method. This is how `javac` emits `@interface Foo { int x()
    // default 5; ... }` and how baksmali surfaces it. Verifying it round-trips
    // covers a major real-world case (Kotlin/Java annotation libraries lean on
    // this heavily — JetBrains, AndroidX, greenrobot, etc.).
    let enum_decl = r#"
.class public final enum Lcom/example/Mode;
.super Ljava/lang/Enum;
.field public static final POSTING:Lcom/example/Mode;
.field public static final MAIN:Lcom/example/Mode;
"#;
    let ann_decl = r#"
.class public interface abstract annotation Lcom/example/Subscribe;
.super Ljava/lang/Object;
.implements Ljava/lang/annotation/Annotation;

.annotation system Ldalvik/annotation/AnnotationDefault;
    value = .subannotation Lcom/example/Subscribe;
        priority = 0x0
        sticky = false
        threadMode = .enum Lcom/example/Mode;->POSTING:Lcom/example/Mode;
        label = "default"
    .end subannotation
.end annotation

.annotation runtime Ljava/lang/annotation/Retention;
    value = .enum Ljava/lang/annotation/RetentionPolicy;->RUNTIME:Ljava/lang/annotation/RetentionPolicy;
.end annotation

.method public abstract priority()I
.end method
.method public abstract sticky()Z
.end method
.method public abstract threadMode()Lcom/example/Mode;
.end method
.method public abstract label()Ljava/lang/String;
.end method
"#;

    let bytes = build_dex_file_bytes(&[
        SmaliClass::from_smali(enum_decl).expect("enum"),
        SmaliClass::from_smali(ann_decl).expect("annotation decl"),
    ])
    .expect("assemble");
    let parsed = DexFile::from_bytes(&bytes).expect("reparse");
    let classes = parsed.to_smali().expect("decode");

    let rt = classes
        .iter()
        .find(|c| c.name.as_jni_type() == "Lcom/example/Subscribe;")
        .expect("annotation decl present after round-trip");

    let default_ann = find_annotation(rt, "Ldalvik/annotation/AnnotationDefault;")
        .expect("AnnotationDefault annotation must survive");

    // The `value` element is a subannotation of Lcom/example/Subscribe; itself,
    // containing one element per method with a default.
    let defaults = match find_element(default_ann, "value").expect("value element") {
        AnnotationValue::SubAnnotation(sub) => sub,
        other => panic!("expected SubAnnotation, got {:?}", other),
    };
    assert_eq!(
        defaults.annotation_type.to_jni(),
        "Lcom/example/Subscribe;",
        "inner annotation type must match the declaring annotation"
    );

    // Each default must be present with the right shape.
    assert_single_matches(
        find_element(defaults, "priority").expect("priority default"),
        &["0", "0x0"],
        "priority default",
    );
    assert_single_matches(
        find_element(defaults, "sticky").expect("sticky default"),
        &["false"],
        "sticky default",
    );
    assert_single_matches(
        find_element(defaults, "label").expect("label default"),
        &["\"default\""],
        "label default",
    );
    match find_element(defaults, "threadMode").expect("threadMode default") {
        AnnotationValue::Enum(class_id, name) => {
            assert_eq!(class_id.as_jni_type(), "Lcom/example/Mode;");
            assert_eq!(name, "POSTING");
        }
        other => panic!("expected Enum for threadMode default, got {:?}", other),
    }
}

#[test]
fn dalvik_system_signature_annotation_round_trips() {
    // dalvik.annotation.Signature: an array-of-strings holding the original
    // generic type signature. Critical for Kotlin/Java generics preservation.
    let smali = r#"
.class public Lcom/example/Generic;
.super Ljava/lang/Object;

.annotation system Ldalvik/annotation/Signature;
    value = {
        "Ljava/util/List<",
        "Ljava/lang/String;",
        ">;"
    }
.end annotation
"#;
    let recovered = round_trip(smali);
    let sig = find_annotation(&recovered, "Ldalvik/annotation/Signature;").expect("signature ann");
    let parts = match find_element(sig, "value").expect("value element") {
        AnnotationValue::Array(parts) => parts.clone(),
        other => panic!("expected Array, got {:?}", other),
    };
    let joined: String = parts.iter().map(|s| s.trim()).collect::<Vec<_>>().join("|");
    assert!(joined.contains("Ljava/util/List<"));
    assert!(joined.contains("Ljava/lang/String;"));
    assert!(joined.contains(">;"));
}
