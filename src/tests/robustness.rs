//! Malformed-input robustness guards.
//!
//! The library parses files that come from the wild — APKs pulled off devices,
//! obfuscated DEXes, hand-edited `.smali`. A malformed input must produce an
//! `Err`, never a panic, because a panic unwinds out of a library call and
//! takes the caller's process with it.
//!
//! These tests feed deliberately corrupted input through the public entry
//! points and assert that nothing panics. They are deterministic: the "random"
//! corruption uses a fixed-seed LCG rather than a real RNG, so a failure is
//! always reproducible and a passing run always covers the same cases.
//!
//! Historical note: when these were written, corrupting a single byte of a
//! small DEX panicked on 457 of 3000 attempts (15%), and 7 of 7 hand-written
//! malformed `.smali` files panicked. The dominant causes were unchecked
//! index reads into the DEX id tables, `read_uleb128` slicing at an offset past
//! the end of the buffer, and `parse_op` turning recoverable nom errors into
//! `panic!`.

use crate::dex::DexFile;
use crate::dex::builder::build_dex_file_bytes;
use crate::types::SmaliClass;

/// Deterministic linear congruential generator (Knuth's MMIX constants).
/// Used instead of a real RNG so failures reproduce exactly.
fn lcg(state: &mut u64) -> u64 {
    *state = state
        .wrapping_mul(6364136223846793005)
        .wrapping_add(1442695040888963407);
    *state
}

/// A small class exercising annotations, static and instance fields, a
/// constructor, wide constants and a method with parameters — enough distinct
/// DEX structures that random byte corruption reaches most of the reader.
const FIXTURE_SMALI: &str = r#".class public Lcom/example/Fixture;
.super Ljava/lang/Object;
.source "Fixture.java"

.annotation runtime Lcom/example/Ann;
    value = 0x2a
    name = "hello"
.end annotation

.field public static final K:I = 0x7
.field public name:Ljava/lang/String;

.method public constructor <init>()V
    .registers 2
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    const/4 v0, 0x3
    return-void
.end method

.method public add(II)J
    .registers 6
    const-wide v0, 0x100L
    return-wide v0
.end method
"#;

fn fixture_dex() -> Vec<u8> {
    let class = SmaliClass::from_smali(FIXTURE_SMALI).expect("fixture parses");
    build_dex_file_bytes(&[class]).expect("fixture assembles")
}

/// Run `f` and report whether it panicked, keeping the panic message off the
/// test log (we expect to trigger none, but a hook keeps output readable if we do).
fn panicked(f: impl FnOnce() + std::panic::UnwindSafe) -> bool {
    let prev = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let result = std::panic::catch_unwind(f);
    std::panic::set_hook(prev);
    result.is_err()
}

#[test]
fn truncated_dex_never_panics() {
    let base = fixture_dex();
    let mut failures = Vec::new();
    for cut in 0..base.len() {
        let bytes = base[..cut].to_vec();
        if panicked(move || {
            let _ = DexFile::from_bytes(&bytes).and_then(|d| d.to_smali());
        }) {
            failures.push(cut);
        }
    }
    assert!(
        failures.is_empty(),
        "truncating the fixture DEX panicked at {} of {} lengths (first: {:?}); \
         malformed input must return Err",
        failures.len(),
        base.len(),
        &failures[..failures.len().min(10)]
    );
}

#[test]
fn corrupted_dex_never_panics() {
    let base = fixture_dex();
    let mut state = 0x0bad_c0de_1234_5678u64;
    let mut failures = Vec::new();
    for _ in 0..3000 {
        let r = lcg(&mut state);
        let pos = (r >> 33) as usize % base.len();
        let value = (r >> 11) as u8;
        let mut bytes = base.clone();
        bytes[pos] = value;
        if panicked(move || {
            let _ = DexFile::from_bytes(&bytes).and_then(|d| d.to_smali());
        }) {
            failures.push((pos, value));
        }
    }
    assert!(
        failures.is_empty(),
        "single-byte DEX corruption panicked {} times in 3000 (first: {:?}); \
         malformed input must return Err",
        failures.len(),
        &failures[..failures.len().min(10)]
    );
}

#[test]
fn malformed_smali_never_panics() {
    const HDR: &str = ".class public Lcom/example/T;\n.super Ljava/lang/Object;\n";
    let cases: Vec<(&str, String)> = vec![
        ("unterminated super descriptor", format!("{HDR}.super LFoo\n")),
        (
            "class name is not a descriptor",
            ".class public Foo\n.super Ljava/lang/Object;\n".to_string(),
        ),
        (
            "primitive where object expected",
            ".class public I\n.super Ljava/lang/Object;\n".to_string(),
        ),
        (
            "non-numeric .locals",
            format!("{HDR}.method public f()V\n    .locals x\n    return-void\n.end method\n"),
        ),
        (
            "non-numeric .registers",
            format!("{HDR}.method public f()V\n    .registers zz\n    return-void\n.end method\n"),
        ),
        (
            "non-numeric .line",
            format!(
                "{HDR}.method public f()V\n    .locals 0\n    .line xx\n    return-void\n.end method\n"
            ),
        ),
        (
            "unknown mnemonic",
            format!("{HDR}.method public f()V\n    .locals 0\n    frobnicate v0\n.end method\n"),
        ),
        (
            "odex-only opcode",
            format!(
                "{HDR}.method public f()V\n    .locals 1\n    iget-volatile v0, v0, Lcom/example/T;->x:I\n.end method\n"
            ),
        ),
        (
            "malformed operand",
            format!("{HDR}.method public f()V\n    .locals 1\n    const/4 v0, nope\n.end method\n"),
        ),
        (
            "register number overflows u16",
            format!(
                "{HDR}.method public f()V\n    .locals 1\n    const/4 v99999, 0x0\n.end method\n"
            ),
        ),
        ("bad field descriptor", format!("{HDR}.field public x:Q\n")),
        ("empty input", String::new()),
    ];

    let mut failures = Vec::new();
    for (name, src) in &cases {
        let src = src.clone();
        if panicked(move || {
            let _ = SmaliClass::from_smali(&src);
        }) {
            failures.push(*name);
        }
    }
    assert!(
        failures.is_empty(),
        "malformed .smali panicked instead of returning Err: {failures:?}"
    );
}

#[test]
fn corrupted_smali_text_never_panics() {
    // Mutating bytes of a valid class into other syntactically plausible
    // characters reaches operand parsers that hand-written cases miss.
    let base = SmaliClass::from_smali(FIXTURE_SMALI)
        .expect("fixture parses")
        .to_smali();
    let bytes = base.as_bytes();
    let alphabet = b"0123456789abcdefLv/;{}()[]-, \n.:";
    let mut state = 0x1111_2222_3333_4444u64;
    let mut failures = 0usize;
    for _ in 0..2000 {
        let r = lcg(&mut state);
        let pos = (r >> 33) as usize % bytes.len();
        let mut v = bytes.to_vec();
        v[pos] = alphabet[((r >> 11) as usize) % alphabet.len()];
        let Ok(text) = String::from_utf8(v) else {
            continue;
        };
        if panicked(move || {
            let _ = SmaliClass::from_smali(&text);
        }) {
            failures += 1;
        }
    }
    assert_eq!(
        failures, 0,
        "corrupting valid smali text panicked {failures} times in 2000"
    );
}

#[test]
fn parse_errors_carry_a_line_number() {
    // A parse failure must say *where*. Before this, callers got
    // `ErrorKind::Fail` plus the entire remainder of the file inlined into the
    // message — no line, no column, potentially megabytes long.
    let src = ".class public Lcom/example/T;\n.super Ljava/lang/Object;\n\n\n.super LBroken\n";
    let err = SmaliClass::from_smali(src).expect_err("should not parse");
    assert!(
        err.details.contains("line 5"),
        "expected the error to name line 5, got: {}",
        err.details
    );
    assert!(
        err.details.len() < 200,
        "error should be an excerpt, not the whole file: {} chars",
        err.details.len()
    );
}

#[test]
fn parse_errors_inside_a_method_point_at_the_offending_line() {
    // `parse_class` tries each construct in turn. A nom `Error` means "not this
    // construct, try the next"; a `Failure` means "this *is* a method/field/
    // annotation and it is broken". Failures must propagate so the reported
    // position is the bad instruction, not the enclosing `.method` line.
    let src = ".class public Lcom/example/T;\n\
               .super Ljava/lang/Object;\n\
               .method public f()V\n\
               \x20   .locals 0\n\
               \x20   frobnicate v0\n\
               .end method\n";
    let err = SmaliClass::from_smali(src).expect_err("should not parse");
    assert!(
        err.details.contains("line 5"),
        "expected the offending instruction's line (5), not the .method line (3); got: {}",
        err.details
    );
    assert!(
        err.details.contains("frobnicate"),
        "expected the excerpt to show the offending text, got: {}",
        err.details
    );
}
