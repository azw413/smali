//! Kotlin-specific structures embedded inside DEX/JVM class files.
//!
//! Kotlin compiles each class with a `@kotlin.Metadata` annotation that
//! carries a Protobuf-encoded view of the original source-level declarations
//! (Kotlin names, generics, properties, extension functions — everything the
//! JVM type system can't represent). Tools that rewrite DEX files need to
//! keep this metadata in sync with their changes; otherwise Kotlin
//! reflection, IDE tooling, `kotlinx.serialization`, DI frameworks, etc.
//! return the pre-rewrite view of the world.
//!
//! This module is deliberately low-level: it decodes and re-encodes the
//! annotation, exposes its renameable string pool (`d2`) as a plain mutable
//! `Vec<String>`, and leaves the policy of how/when/what to rename to the
//! caller.
//!
//! # Inspecting metadata
//!
//! ```
//! use smali::types::SmaliClass;
//!
//! let class = SmaliClass::from_smali(r#"
//! .class public Lcom/example/Greeter;
//! .super Ljava/lang/Object;
//!
//! .annotation runtime Lkotlin/Metadata;
//!     k = 0x1
//!     mv = { 0x1, 0x9, 0x0 }
//!     d1 = { " " }
//!     d2 = { "Lcom/example/Greeter;", "greet" }
//!     xi = 0x30
//! .end annotation
//! "#).unwrap();
//!
//! let meta = class.kotlin_metadata().unwrap().expect("kotlin class");
//! assert_eq!(meta.kind, Some(smali::kotlin::KotlinMetadataKind::Class));
//! assert_eq!(meta.metadata_version, vec![1, 9, 0]);
//! assert!(meta.string_pool.contains(&"greet".to_string()));
//! ```
//!
//! # Editing the string pool
//!
//! Renames performed at the DEX level (class, method, property names) need
//! the corresponding strings updated in `d2` so Kotlin reflection stays
//! consistent. The `proto_bytes` (`d1`) reference `string_pool` (`d2`) by
//! index, so substituting an entry is enough — the protobuf doesn't change.
//!
//! ```
//! use smali::types::SmaliClass;
//!
//! let mut class = SmaliClass::from_smali(r#"
//! .class public Lcom/example/Greeter;
//! .super Ljava/lang/Object;
//!
//! .annotation runtime Lkotlin/Metadata;
//!     k = 0x1
//!     mv = { 0x1, 0x9, 0x0 }
//!     d1 = { " " }
//!     d2 = { "Lcom/example/Greeter;", "greet" }
//!     xi = 0x30
//! .end annotation
//! "#).unwrap();
//!
//! let mut meta = class.kotlin_metadata().unwrap().unwrap();
//! for s in meta.string_pool.iter_mut() {
//!     if s == "greet" {
//!         *s = "salute".into();
//!     }
//! }
//! class.set_kotlin_metadata(&meta);
//!
//! // The change is visible after re-decoding.
//! let recovered = class.kotlin_metadata().unwrap().unwrap();
//! assert!(recovered.string_pool.contains(&"salute".to_string()));
//! ```
//!
//! # Things to leave alone
//!
//! - **`metadata_version`**: Kotlin reflection refuses metadata whose
//!   version is newer than its own runtime. Don't bump it.
//! - **`extra_int`**: holds compiler bit flags (pre-release, IR backend, K2
//!   frontend, strict semantics). Preserve verbatim unless you know the
//!   target runtime accepts whatever you're setting.
//! - **`proto_bytes`**: an encoded protobuf message. Editing raw bytes is
//!   only safe if you've decoded the schema (see `metadata.proto` and
//!   `jvm_metadata.proto` in the JetBrains/kotlin repo). The string-pool
//!   approach above covers the common rename cases without touching this.

pub mod bit_encoding;
pub mod metadata;

pub use metadata::{KOTLIN_METADATA_TYPE, KotlinMetadata, KotlinMetadataKind};
