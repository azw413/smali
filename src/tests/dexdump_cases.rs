use crate::dex::DexFile;
use crate::types::SmaliClass;
use std::fs;
use std::process::Command;

const CASES: &[(&str, &[&str])] = &[
    (
        "static_field",
        &[r".class public Lfoo/StaticField;
.super Ljava/lang/Object;

.field public static test:I

.method public constructor <init>()V
    .registers 2
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
"],
    ),
    (
        "static_and_instance",
        &[r".class public Lfoo/StaticInstance;
.super Ljava/lang/Object;

.field public static test:I
.field public instance:I

.method public constructor <init>()V
    .registers 2
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
"],
    ),
    (
        "static_with_value",
        &[r".class public Lfoo/StaticValue;
.super Ljava/lang/Object;

.field public static test:I = 0x1

.method public constructor <init>()V
    .registers 2
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
"],
    ),
    (
        "method_annotations",
        &[r".class public Lfoo/Annotated;
.super Ljava/lang/Object;

.field public static test:I

.method public static foo()V
    .locals 1
    const/4 v0, 0x0
    return-void
.end method

.method public constructor <init>()V
    .registers 2
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
"],
    ),
    (
        "param_annotations",
        &[
            r".class public interface abstract annotation Lfoo/Marker;
.super Ljava/lang/Object;
.implements Ljava/lang/annotation/Annotation;
",
            r#".class public Lfoo/WithParams;
.super Ljava/lang/Object;

.method public static tagged(I)V
    .registers 1
    .param p0, "count"
        .annotation runtime Lfoo/Marker;
        .end annotation
    .end param
    return-void
.end method

.method public constructor <init>()V
    .registers 1
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
"#,
        ],
    ),
    (
        "annotation_elements",
        &[
            r".class public interface abstract annotation Lfoo/Pair;
.super Ljava/lang/Object;
.implements Ljava/lang/annotation/Annotation;

.method public abstract a()Ljava/lang/String;
.end method

.method public abstract b()Ljava/lang/String;
.end method
",
            r#".class public Lfoo/AnnotatedPair;
.super Ljava/lang/Object;

.field public static dummy:I
    .annotation runtime Lfoo/Pair;
        value = "one"
        key = "two"
    .end annotation
.end field

.method public constructor <init>()V
    .registers 1
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
"#,
        ],
    ),
    (
        "array_data_two_classes",
        &[
            r".class public Lfoo/ArrayOne;
.super Ljava/lang/Object;

.method public static build()[I
    .registers 2
    const/4 v0, 0x2
    new-array v0, v0, [I
    fill-array-data v0, :data
    return-object v0

    :data
    .array-data 4
        0x1
        0x2
    .end array-data
.end method

.method public constructor <init>()V
    .registers 1
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
",
            r".class public Lfoo/ArrayTwo;
.super Ljava/lang/Object;

.method public static build()[I
    .registers 2
    const/4 v0, 0x3
    new-array v0, v0, [I
    fill-array-data v0, :data2
    return-object v0

    :data2
    .array-data 4
        0x3
        0x4
        0x5
    .end array-data
.end method

.method public constructor <init>()V
    .registers 1
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
",
        ],
    ),
    (
        "two_classes",
        &[
            r".class public Lfoo/A;
.super Ljava/lang/Object;

.field public static foo:I

.method public constructor <init>()V
    .registers 2
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
",
            r".class public Lfoo/B;
.super Ljava/lang/Object;

.field public static bar:I
.field public baz:I

.method public constructor <init>()V
    .registers 2
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
",
        ],
    ),
    (
        "static_methods",
        &[r".class public Lfoo/Statics;
.super Ljava/lang/Object;

.field public static foo:I

.method public static one()I
    .locals 1
    const/4 v0, 0x1
    return v0
.end method

.method public static two()I
    .locals 1
    const/4 v0, 0x2
    return v0
.end method

.method public constructor <init>()V
    .registers 1
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
"],
    ),
];

fn run_case(name: &str, smali: &[&str]) {
    let classes: Vec<_> = smali
        .iter()
        .map(|src| SmaliClass::from_smali(src).unwrap())
        .collect();
    let dex = DexFile::from_smali(&classes).unwrap();

    let path = std::env::temp_dir()
        .join(format!("smali-test-{}-{name}.dex", std::process::id()));
    fs::write(&path, dex.to_bytes()).unwrap();
    let keep_output = std::env::var_os("KEEP_DEX").is_some();

    let output = Command::new("dexdump")
        .arg(&path)
        .output()
        .unwrap_or_else(|err| panic!("failed to spawn dexdump for {name}: {err}"));

    assert!(
        output.status.success(),
        "{name}: dexdump failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    if keep_output {
        eprintln!("kept dex for {name}: {}", path.display());
    } else {
        let _ = fs::remove_file(&path);
    }
}

#[test]
fn dexdump_minimal_cases() {
    let filter = std::env::var("DEXDUMP_CASE").ok();
    let mut ran = false;
    for (name, smali) in CASES {
        if let Some(target) = filter.as_deref() {
            if *name != target {
                continue;
            }
        }
        ran = true;
        run_case(name, smali);
    }
    if let Some(target) = filter {
        assert!(ran, "DEXDUMP_CASE '{target}' did not match any scenarios");
    }
}
