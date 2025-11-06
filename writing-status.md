# DEX File Writing Status

## Overview
The crate can currently decode DEX files into `Vec<SmaliClass>` and exposes a number of low-level writers, but there is no end-to-end pipeline that rebuilds a DEX file. This document captures the present state of the writing code, highlights gaps that must be closed before we can emit well-formed binaries, and lays out an implementation plan.

## Implementation Snapshot

### Implemented Foundations
- `src/dex/mod.rs`: Little-endian integer writers, LEB128/SLEB128 encoders, and raw byte helpers are in place and have tests.
- `src/dex/dex_file.rs`: ID table writers (`FieldItem`, `MethodItem`, `PrototypeItem`) emit spec-compliant rows; `TypeList::write` now pads to 4-byte boundaries, `DexString::write` counts UTF-16 code units correctly, and low-level exception data writers (`EncodedTypeAddrPair`, `TryItem`, `EncodedCatchHandler`) round-trip.
- `DebugInfo`, `CodeItem`, and `ClassDataItem` writers respect DEX encoding rules: parameter indices use `uleb128p1`, opcode streams honour caller-provided sequences, code items recompute handler offsets / debug-info pointers, and class data emits differential field/method indices alongside code offsets.
- `src/dex/annotations.rs` & `src/dex/encoded_values.rs`: Annotation directory structures and encoded value writers cover the full set of value tags, with round-trip tests.
- `Header::write` mirrors the layout of the 0x70-byte header, enabling patch-up once real sizes, checksum, and signature are available.
- `src/dex/builder.rs`: `DexIndexPools` converts `Vec<SmaliClass>` inputs into canonical string/type/proto/field/method tables with deterministic ordering, crawls method bodies to register literal/Invoke metadata, and can now emit `DexIdTables` plus index-lookup helpers so future layout code can translate descriptors into concrete ID rows.
- `DexLayoutPlan` (new in `src/dex/builder.rs`) consumes the pools and ID tables to assign section offsets, encode string/type-list payloads into a data-section plan, precompute `ClassDefItem` records (w/ deterministic ordering), emit encoded-array payloads for simple static-value initializers, and drives `build_dex_file_bytes`, which now writes header/ID/data sections, map list, and checksum/signature so we can materialize a skeletal (code-free) DEX file.

### Partially Implemented / Requires Fixes
- `CodeItem::write` still assumes 16-bit handler offsets; very large handler tables require defensive checks during layout.
- `ClassDataItem` emits method code offsets but does not yet integrate static value arrays or annotation directories—higher layers must still provide those offsets.
- `Header::write` depends on pre-populated sizes, offsets, Adler-32 checksum, and SHA-1 signature—no code computes these yet.

### Missing Functionality
- `ClassDefItem::write` remains a thin helper that expects precomputed offsets; the layout plan now emits offsets for interfaces/static values and the new writer drains the planned buffers, but annotation directories, class data, and actual code/static-value blobs still need to be hooked up.
- Index pools exist (`DexIndexPools`) and can output ID-table payloads, but literal harvesting currently only covers strings/booleans; numeric/static-object literals from encoded arrays still need parser coverage, and the layout plan only handles string data + type lists + encoded static strings/nulls (no annotations, static numeric arrays, or code yet).
- No builder stitches together class annotations/static value arrays with the offsets that `ClassDataItem`/`ClassDefItem` expect; the new writer emits empty placeholders for those offsets until real payloads exist.
- Call-site/method-handle section support and link-section emission are still missing.
- No Smali → bytecode lowering: register allocation, instruction encoding, debug info construction, static initializer emission, or annotation translation onto the DEX structures.
- No Smali → bytecode lowering: register allocation, instruction encoding, debug info construction, static initializer emission, or annotation translation onto the DEX structures.

## Key Technical Gaps
- **Alignment & sizing:** Type lists now pad correctly, but other data-area payloads (annotation sets, encoded arrays, map list, etc.) still need alignment helpers.
- **Offset management:** Every structure written into the data section needs its file-absolute offset recorded so ID tables and class_defs can reference it. Helper writers must either return offsets or accept callbacks for patching.
- **Index pools:** A deterministic scheme for assigning indices to strings, types, protos, fields, and methods is required so that cross-references line up with the generated Smali metadata.
- **Code generation:** Translating `SmaliOp` into encoded instructions (with proper register layouts) is non-existent, as is emitting debug opcodes beyond placeholders.
- **Validation:** There are no tests that assert the serialized output re-loads successfully or matches known-good binaries.

## Detailed Implementation Plan

### Phase 0 – Writer Repairs (Completed)
The foundational writers now emit canonical encodings:
1. `DexString::write` reports UTF-16 code unit counts and `TypeList::write` pads to 4-byte boundaries.
2. `DebugInfo::write` encodes parameter indices individually and preserves caller-provided opcode streams.
3. `CodeItem::write` recalculates try-handler offsets, honours instruction padding, and patches `debug_info_off` using a supplied base address.
4. `PrototypeItem`, `EncodedField`, and `EncodedMethod` provide proper writers; `ClassDataItem::write` emits differential indices plus method `code_off` values.
5. Targeted unit tests cover the repaired components.

Remaining writer work before layout: integrate static values/annotation offsets when class defs are assembled.

### Phase 1 – Build Index Pools
1. ✅ `DexIndexPools` gathers strings, type descriptors, prototypes, fields, and methods from `SmaliClass` inputs, producing sorted tables that map descriptors to their eventual indices.
2. 🔄 Pools now also scan `DexOp` bodies for const-string/class usage plus field/method references (including invoke-polymorphic/custom prototypes) and normalize string literal harvesting from field initializers; literal collection still needs to hook into encoded arrays/static values/annotation lowering for numeric/object cases, but the builder now exposes helpers to do so without reindexing.
3. 🔄 `DexLayoutPlan` allocates deterministic offsets for the ID sections, encodes string/type-list payloads into a planned data section, prepares `ClassDefItem` stubs (including encoded-array blobs for simple static string initializers), and now feeds `build_dex_file_bytes`. Next steps: add annotation directories/class data/static values for other literal types and plug in code/debug info emission.

### Phase 2 – Layout & Serialization Pipeline
1. ✅ Define section models for the core ID tables plus class_defs, and stage data payload builders for string data + TypeList-backed structures (now also covering encoded-array static values for strings/nulls).
2. ✅ Compute sizes and alignment requirements, then assign file offsets in canonical DEX order (captured in `SectionOffsets`).
3. ✅ Serialize each section into a single contiguous buffer, recording offsets for later references. `build_dex_file_bytes` now emits header + ID sections + data, appends map list entries for string/type/encoded-array chunks, and patches checksum/signature so the bytes can be parsed as a real (code-less) DEX file.
4. ✅ Generate the map list covering every emitted section and append it to the data area.
5. ✅ Finalize the header: populate sizes/offsets, then compute Adler-32 (excluding the first 12 bytes) and SHA-1 (bytes 32..end) checksums and patch them into the header.
6. 🔜 Add optional link-data and API-26+ sections when inputs require them.

### Phase 3 – Smali Lowering
1. Convert `SmaliClass` metadata into DEX class_def/class_data/annotation/static_value structures using the index pools.
2. Implement a bytecode assembler that maps `SmaliOp` plus operands into encoded instructions, handling registers, literal pools, switch payloads, and wide/narrow variants.
3. Generate `DebugInfo` streams (parameter names, line number programs, local variable tracking) tied to the assembled code.
4. Support encoded array initializers for static fields and annotation payloads, reusing the encoded value writer.

### Phase 4 – Builder API & Tooling
1. Introduce a `DexBuilder` (or similar) facade that accepts `Vec<SmaliClass>`, produces a `DexFile`, and optionally writes directly to disk.
2. Expose ergonomic constructors (`DexFile::from_smali`, `DexFile::write_to_file`, CLI example) with error reporting through `DexError`.
3. Provide feature flags for API-level extensions (call sites, method handles, quickened opcodes) so emitters can target specific Android versions.

### Phase 5 – Validation & Regression Safety
1. Unit-test every writer against hand-crafted fixtures (including edge cases for padding, large indices, UTF-16 surrogate pairs).
2. Add golden integration tests: parse an existing DEX → convert to Smali → rebuild DEX → compare headers, map list, and class data with the original (allowing for checksum/signature differences).
3. Property-based tests that fuzz Smali inputs to ensure generated DEX files can be parsed by ART/dexdump without errors.
4. Benchmark serialization to detect performance regressions once the builder is in place.

## Validation Strategy
- **Round-trip Tests:** Reuse the decode path to load generated bytes and assert structural equality of strings/types/protos/class defs.
- **External Tooling:** Optionally feed emitted DEX files into `dexdump`/`vdexExtractor` or an emulator’s ART verifier for additional assurance.
- **Static Analysis:** Enforce section alignment and offset monotonicity via assertions during build.

## Risks & Unknowns
- Register assignment and parameter register ordering for methods with wide arguments can be tricky; incorrect layouts will corrupt bytecode verification.
- Supporting odex/quickened opcodes or API-26+ features (invoke-polymorphic, call-site/method-handle sections) requires additional table writers beyond the core pipeline.
- Debug info emission is specification-heavy; misencoding the state machine will make debuggers unhappy even if the code executes correctly.
