//! Payload alignment across branch promotion.
//!
//! `packed-switch`, `sparse-switch` and `array-data` payloads must begin on a
//! 4-byte boundary — an even offset in the stream of 2-byte code units. ART
//! rejects the whole class otherwise:
//!
//! ```text
//! java.lang.VerifyError: Verifier rejected class ...:
//!   [0xFFFFFFFF] unaligned switch table: at 236, switch offset 229
//! ```
//!
//! The padding `nop` used to be decided once, when the payload directive was
//! pushed. The assembler then lays the method out in a retry loop, widening any
//! branch whose target has drifted out of range — and both promotions
//! (`goto` -> `goto/16` and `goto/16` -> `goto/32`) grow by a *single* code unit,
//! flipping the parity of every offset after them. The padding was therefore
//! either missing or spurious, depending on which side of the boundary the
//! method happened to fall.
//!
//! Each test below sweeps a range of method sizes that spans a promotion
//! boundary, so both directions are covered for every payload kind.

#[cfg(test)]
mod tests {
    use crate::dex::error::DexError;
    use crate::dex::opcode_format::assemble::{
        AssemblerIndexResolver, MethodAssembler, MethodAssemblyResult,
    };
    use crate::types::{MethodSignature, SmaliClass};

    /// The assembler only asks the resolver to turn symbolic references into pool
    /// indices. The fixtures here contain none that matter, so any constant does.
    struct StubResolver;

    impl AssemblerIndexResolver for StubResolver {
        fn string_index(&self, _value: &str) -> Result<u32, DexError> {
            Ok(0)
        }
        fn type_index(&self, _descriptor: &str) -> Result<u32, DexError> {
            Ok(0)
        }
        fn field_index(&self, _: &str, _: &str, _: &str) -> Result<u32, DexError> {
            Ok(0)
        }
        fn method_index(&self, _: &str, _: &str, _: &MethodSignature) -> Result<u32, DexError> {
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

    // The builder's own defaults.
    const API_LEVEL: i32 = 33;
    const ART_VERSION: i32 = 200;

    /// First code unit of each payload — the pseudo-opcode identifying it.
    const PACKED_SWITCH_IDENT: u16 = 0x0100;
    const SPARSE_SWITCH_IDENT: u16 = 0x0200;
    const ARRAY_DATA_IDENT: u16 = 0x0300;

    fn assemble(body: &str) -> MethodAssemblyResult {
        let source =
            format!(".class public Lsw/Align;\n.super Ljava/lang/Object;\n\n{body}\n.end method\n");
        let class = SmaliClass::from_smali(&source).expect("fixture parses");
        let method = class.methods.first().expect("fixture defines a method");
        MethodAssembler::new(&StubResolver, API_LEVEL, ART_VERSION)
            .assemble_method("Lsw/Align;", method)
            .expect("fixture assembles")
    }

    /// Assert the payload labelled `:pl` is 4-byte aligned, and that the label
    /// resolves to the payload itself rather than to its padding `nop`.
    fn assert_payload_aligned(result: &MethodAssemblyResult, ident: u16, context: &str) {
        let offset = *result
            .label_offsets
            .get("pl")
            .unwrap_or_else(|| panic!("{context}: payload label :pl not resolved"))
            as usize;

        assert_eq!(
            offset % 2,
            0,
            "{context}: payload at code unit {offset} is not 4-byte aligned; \
             ART rejects the class with `unaligned switch table`"
        );

        let unit = result
            .encoded_code_units
            .get(offset)
            .copied()
            .unwrap_or_else(|| panic!("{context}: payload offset {offset} is past end of method"));
        assert_eq!(
            unit, ident,
            "{context}: label :pl points at code unit {unit:#06x}, not the payload {ident:#06x} \
             (padding is 0x0000, so this is the label landing on the padding instead)"
        );
    }

    /// A forward `goto` over `filler` code units of `nop`, then a switch and its
    /// payload.
    ///
    /// Sweeping `filler` walks the branch delta across the Format10t boundary
    /// (signed 8-bit) and, much further out, the Format20t boundary (signed
    /// 16-bit). Each promotion adds one code unit, so consecutive values of
    /// `filler` land the payload on alternating parities — which is why the
    /// original bug showed up for some method sizes and not others.
    fn switch_fixture(filler: usize, payload: &str) -> String {
        let nops = vec!["    nop"; filler].join("\n");
        format!(
            r#".method public static sw(I)I
    .locals 1
    goto :far
{nops}
:far
    packed-switch p0, :pl
    const/4 v0, 0x0
    return v0
:t0
    const/4 v0, 0x1
    return v0
:t1
    const/4 v0, 0x2
    return v0
:pl
{payload}
"#
        )
    }

    /// Where the payload of [`switch_fixture`] belongs, and how long the whole
    /// method body should be, worked out from the DEX spec rather than from the
    /// assembler — so the two can disagree.
    ///
    /// Returns `(payload_offset_cu, total_cu)`.
    fn expected_switch_layout(filler: usize, payload_cu: usize) -> (usize, usize) {
        // Smallest `goto` encoding whose delta to :far fits, taken to a fixpoint:
        // widening the branch also moves :far one further away, which can push it
        // over the next boundary in turn.
        let mut goto_cu = 1usize;
        while goto_cu < 3 {
            let delta = (goto_cu + filler) as i64;
            let fits = match goto_cu {
                1 => (i8::MIN as i64..=i8::MAX as i64).contains(&delta),
                _ => (i16::MIN as i64..=i16::MAX as i64).contains(&delta),
            };
            if fits {
                break;
            }
            goto_cu += 1;
        }
        // goto + nops + packed-switch (3) + three `const/4` + `return` pairs (6).
        let payload = (goto_cu + filler + 9).next_multiple_of(2);
        (payload, payload + payload_cu)
    }

    #[test]
    fn packed_switch_payload_stays_aligned_across_promotion() {
        // 120..=140 spans the Format10t boundary: at 127 filler nops the delta no
        // longer fits, the goto is promoted to goto/16, and everything after it
        // shifts by one code unit.
        for filler in 120..=140 {
            let body = switch_fixture(
                filler,
                "    .packed-switch 0x0\n        :t0\n        :t1\n    .end packed-switch",
            );
            let result = assemble(&body);
            assert_payload_aligned(
                &result,
                PACKED_SWITCH_IDENT,
                &format!("packed-switch, {filler} filler nops"),
            );
        }
    }

    #[test]
    fn sparse_switch_payload_stays_aligned_across_promotion() {
        for filler in 120..=140 {
            let body = switch_fixture(
                filler,
                "    .sparse-switch\n        0x0 -> :t0\n        0x5 -> :t1\n    .end sparse-switch",
            );
            let result = assemble(&body);
            assert_payload_aligned(
                &result,
                SPARSE_SWITCH_IDENT,
                &format!("sparse-switch, {filler} filler nops"),
            );
        }
    }

    #[test]
    fn array_data_payload_stays_aligned_across_promotion() {
        for filler in 120..=140 {
            let nops = vec!["    nop"; filler].join("\n");
            let body = format!(
                r#".method public static arr()[I
    .locals 2
    goto :far
{nops}
:far
    const/4 v1, 0x2
    new-array v0, v1, [I
    fill-array-data v0, :pl
    return-object v0
:pl
    .array-data 4
        0x1
        0x2
    .end array-data
"#
            );
            let result = assemble(&body);
            assert_payload_aligned(
                &result,
                ARRAY_DATA_IDENT,
                &format!("array-data, {filler} filler nops"),
            );
        }
    }

    #[test]
    fn payload_stays_aligned_across_the_second_promotion() {
        // Past 32767 code units a goto/16 cannot reach either, so the branch is
        // promoted again to goto/32 — another single code unit, another parity
        // flip. These fillers step either side of that boundary.
        for filler in [32764usize, 32765, 32766, 32767] {
            let body = switch_fixture(
                filler,
                "    .packed-switch 0x0\n        :t0\n        :t1\n    .end packed-switch",
            );
            let result = assemble(&body);
            assert_payload_aligned(
                &result,
                PACKED_SWITCH_IDENT,
                &format!("goto/16 -> goto/32, {filler} filler nops"),
            );
        }
    }

    #[test]
    fn no_padding_is_emitted_when_the_payload_is_already_aligned() {
        // The mirror image of the reported bug: a promotion can just as easily
        // make a padded payload correctly aligned, leaving a `nop` that should no
        // longer be there. Checking the exact body length catches padding that is
        // missing *and* padding that is surplus.
        //
        // The packed-switch payload is ident + size + first_key (2) + 2 targets
        // (2 each) = 8 code units.
        for filler in 120..=140 {
            let body = switch_fixture(
                filler,
                "    .packed-switch 0x0\n        :t0\n        :t1\n    .end packed-switch",
            );
            let result = assemble(&body);
            let (expected_payload, expected_total) = expected_switch_layout(filler, 8);

            assert_eq!(
                result.label_offsets.get("pl").copied(),
                Some(expected_payload as u32),
                "{filler} filler nops: payload is not where the spec puts it"
            );
            assert_eq!(
                result.encoded_code_units.len(),
                expected_total,
                "{filler} filler nops: method body is {} code units, expected {expected_total} \
                 (a surplus unit is a padding nop in front of an already-aligned payload)",
                result.encoded_code_units.len()
            );
        }
    }

    #[test]
    fn switch_instruction_branches_exactly_onto_its_payload() {
        // Alignment alone is not enough: the payload base recorded for the switch
        // has to agree with where the payload actually ended up, or every target
        // is resolved against the wrong origin.
        for filler in 120..=140 {
            let body = switch_fixture(
                filler,
                "    .packed-switch 0x0\n        :t0\n        :t1\n    .end packed-switch",
            );
            let result = assemble(&body);
            let code = &result.encoded_code_units;
            let payload = *result.label_offsets.get("pl").expect("payload label") as usize;

            // packed-switch is opcode 0x2b in Format31t: AA|op, then a signed
            // 32-bit branch offset across two code units. It is the first unit in
            // the method with that low byte — the filler is all `nop` (0x0000) and
            // the payload comes later.
            let at = (0..code.len())
                .find(|&i| code[i] & 0x00ff == 0x2b)
                .unwrap_or_else(|| panic!("{filler}: no packed-switch instruction emitted"));
            let delta = (((code[at + 2] as u32) << 16) | code[at + 1] as u32) as i32;
            let target = at as i32 + delta;

            assert_eq!(
                target, payload as i32,
                "{filler}: packed-switch at {at} branches to {target}, \
                 but the payload is at {payload}"
            );
        }
    }

    #[test]
    fn debug_line_offsets_survive_branch_promotion() {
        // `.line` directives are anchored to the instruction they precede. A
        // promotion shifts every later instruction by one code unit, so offsets
        // captured before the retry loop ran would misattribute every line after
        // the widened branch.
        let filler = 130;
        let nops = vec!["    nop"; filler].join("\n");
        let body = format!(
            r#".method public static sw(I)I
    .locals 1
    .line 1
    goto :far
{nops}
:far
    .line 2
    const/4 v0, 0x0
    .line 3
    return v0
"#
        );
        let result = assemble(&body);

        // The goto had to widen to goto/16, so :far — and the `.line 2` sitting on
        // it — is at filler + 2, not filler + 1.
        let far = *result.label_offsets.get("far").expect("label :far") as usize;
        assert_eq!(far, filler + 2, "goto should have widened to two code units");

        let lines: Vec<(u32, u32)> = result
            .line_events
            .iter()
            .map(|event| (event.offset_cu, event.line))
            .collect();
        assert_eq!(
            lines,
            vec![(0, 1), (far as u32, 2), (far as u32 + 1, 3)],
            "line offsets must be the ones actually emitted, not the pre-promotion ones"
        );
    }
}
