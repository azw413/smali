//! Index-section ordering in the emitted DEX.
//!
//! `string_ids` must be sorted "by string contents, using UTF-16 code point
//! values". That wording reads like Unicode scalar order — which is what a
//! `BTreeSet<String>` gives, because Rust's `str: Ord` is UTF-8 byte order —
//! but ART's `DexFileVerifier` compares UTF-16 *code units*.
//!
//! The two orders agree across the whole BMP and diverge in exactly one place:
//! a supplementary character (>= U+10000) is encoded in UTF-16 as a surrogate
//! pair starting in `0xD800..=0xDBFF`, which is *below* the BMP range
//! `0xE000..=0xFFFF`, while its scalar value is far above it. So for any pair
//! of strings starting one from each side, the two orders are opposite, and a
//! single such pair rejects the entire file:
//!
//! ```text
//! java.io.IOException: Failed to open dex files from .../base.apk because:
//!   Failure to verify dex file '.../base.apk': Out-of-order string_ids: '<a>' then '<b>'
//! ```
//!
//! ART reports that as a `ClassNotFoundException` on the app's `Application`
//! class, so nothing in the visible symptom points at the string table.
//!
//! `type_ids` and `field_ids` are ordered by indices *into* that pool, so they
//! are exposed by the same divergence whenever a type descriptor or member name
//! reaches past the BMP. The checks below verify every index section the
//! verifier tests, not just `string_ids`.

#[cfg(test)]
mod tests {
    use crate::dex::builder::build_multi_dex_bytes;
    use crate::types::SmaliClass;

    /// BMP, private-use area. One UTF-16 code unit: `0xE97E`.
    ///
    /// This is the pair that broke a real app: `U+E97E` arrived from string
    /// encryption, `U+DFFFD` was already the last entry in the unprotected
    /// app's pool, and scalar order put them the wrong way round.
    const BMP: char = '\u{e97e}';
    /// Supplementary, plane 13. Two UTF-16 code units: `0xDB3F 0xDFFD`.
    const SUPPLEMENTARY: char = '\u{dfffd}';

    fn build(source: &str) -> Vec<u8> {
        let class = SmaliClass::from_smali(source).expect("fixture parses");
        let mut dexes = build_multi_dex_bytes(&[class]).expect("fixture builds");
        assert_eq!(dexes.len(), 1, "fixture should not need splitting");
        dexes.remove(0)
    }

    fn u32_at(dex: &[u8], off: usize) -> usize {
        u32::from_le_bytes(dex[off..off + 4].try_into().unwrap()) as usize
    }

    fn u16_at(dex: &[u8], off: usize) -> usize {
        u16::from_le_bytes(dex[off..off + 2].try_into().unwrap()) as usize
    }

    /// `(size, offset)` of a section, from its fixed slot in the header.
    fn section(dex: &[u8], header_off: usize) -> (usize, usize) {
        (u32_at(dex, header_off), u32_at(dex, header_off + 4))
    }

    const STRING_IDS: usize = 0x38;
    const TYPE_IDS: usize = 0x40;
    const PROTO_IDS: usize = 0x48;
    const FIELD_IDS: usize = 0x50;
    const METHOD_IDS: usize = 0x58;

    /// Decode a `string_data_item` to UTF-16 code units.
    ///
    /// Deliberately does not go via `String`: comparing decoded `String`s would
    /// reintroduce the scalar ordering that is the bug under test.
    fn units_at(dex: &[u8], mut o: usize) -> Vec<u16> {
        let mut len = 0usize;
        let mut shift = 0;
        loop {
            let b = dex[o];
            o += 1;
            len |= ((b & 0x7f) as usize) << shift;
            if b & 0x80 == 0 {
                break;
            }
            shift += 7;
        }

        let mut units = Vec::with_capacity(len);
        while units.len() < len {
            let c = dex[o] as u16;
            o += 1;
            units.push(if c < 0x80 {
                c
            } else if c & 0xe0 == 0xc0 {
                let u = ((c & 0x1f) << 6) | (dex[o] as u16 & 0x3f);
                o += 1;
                u
            } else {
                // Three bytes per code *unit*: a supplementary character is two
                // such sequences (MUTF-8 is CESU-8), so the four-byte form of
                // real UTF-8 never appears here.
                let u = ((c & 0x0f) << 12)
                    | ((dex[o] as u16 & 0x3f) << 6)
                    | (dex[o + 1] as u16 & 0x3f);
                o += 2;
                u
            });
        }
        units
    }

    fn string_units(dex: &[u8]) -> Vec<Vec<u16>> {
        let (size, off) = section(dex, STRING_IDS);
        (0..size)
            .map(|i| units_at(dex, u32_at(dex, off + 4 * i)))
            .collect()
    }

    /// The parameter type indices of a `proto_id_item`, from its `type_list`.
    fn type_list(dex: &[u8], off: usize) -> Vec<usize> {
        if off == 0 {
            return Vec::new();
        }
        let size = u32_at(dex, off);
        (0..size).map(|i| u16_at(dex, off + 4 + 2 * i)).collect()
    }

    /// Check every ordering rule `DexFileVerifier` enforces on the index
    /// sections, and panic naming the offending neighbours.
    ///
    /// Each section is required to be *strictly* increasing — duplicates are
    /// rejected as firmly as inversions are.
    fn assert_index_sections_sorted(dex: &[u8], context: &str) {
        let strings = string_units(dex);
        assert_strictly_sorted(&strings, context, "string_ids", |units| {
            format!("{units:04x?}")
        });

        // `type_ids` is ordered by the string index of its descriptor.
        let (size, off) = section(dex, TYPE_IDS);
        let types: Vec<usize> = (0..size).map(|i| u32_at(dex, off + 4 * i)).collect();
        assert_strictly_sorted(&types, context, "type_ids", |idx| {
            format!("descriptor_idx {idx} {:04x?}", strings[*idx])
        });

        // `proto_ids` is ordered by return type, then by parameter list.
        let (size, off) = section(dex, PROTO_IDS);
        let protos: Vec<(usize, Vec<usize>)> = (0..size)
            .map(|i| {
                let item = off + 12 * i;
                (u32_at(dex, item + 4), type_list(dex, u32_at(dex, item + 8)))
            })
            .collect();
        assert_strictly_sorted(&protos, context, "proto_ids", |(ret, params)| {
            format!("return_type_idx {ret}, parameters {params:?}")
        });

        // `field_ids` is ordered by defining class, then name, then type.
        let (size, off) = section(dex, FIELD_IDS);
        let fields: Vec<(usize, usize, usize)> = (0..size)
            .map(|i| {
                let item = off + 8 * i;
                (
                    u16_at(dex, item),
                    u32_at(dex, item + 4),
                    u16_at(dex, item + 2),
                )
            })
            .collect();
        assert_strictly_sorted(&fields, context, "field_ids", |(class, name, ty)| {
            format!(
                "class_idx {class}, name_idx {name} {:04x?}, type_idx {ty}",
                strings[*name]
            )
        });

        // `method_ids` is ordered by defining class, then name, then prototype.
        let (size, off) = section(dex, METHOD_IDS);
        let methods: Vec<(usize, usize, usize)> = (0..size)
            .map(|i| {
                let item = off + 8 * i;
                (
                    u16_at(dex, item),
                    u32_at(dex, item + 4),
                    u16_at(dex, item + 2),
                )
            })
            .collect();
        assert_strictly_sorted(&methods, context, "method_ids", |(class, name, proto)| {
            format!("class_idx {class}, name_idx {name}, proto_idx {proto}")
        });
    }

    fn assert_strictly_sorted<T: Ord>(
        items: &[T],
        context: &str,
        section: &str,
        describe: impl Fn(&T) -> String,
    ) {
        for (i, pair) in items.windows(2).enumerate() {
            assert!(
                pair[0] < pair[1],
                "{context}: out-of-order {section} at [{i}] and [{}]\n  \
                 [{i}] {}\n  [{}] {}\nART rejects the whole file, and reports it as a \
                 ClassNotFoundException on the app's Application class.",
                i + 1,
                describe(&pair[0]),
                i + 1,
                describe(&pair[1]),
            );
        }
    }

    /// Index of `needle` in the emitted pool, by UTF-16 units so the lookup
    /// cannot itself depend on scalar ordering.
    fn index_of(dex: &[u8], needle: &str) -> usize {
        let wanted: Vec<u16> = needle.encode_utf16().collect();
        string_units(dex)
            .iter()
            .position(|units| *units == wanted)
            .unwrap_or_else(|| {
                panic!("{:?} is not in the emitted string pool", needle.escape_unicode())
            })
    }

    #[test]
    fn string_ids_are_sorted_by_utf16_code_units() {
        // Sanity: the two orderings really do disagree on this pair. If this
        // fails the fixture is wrong, not the writer.
        let bmp = BMP.to_string();
        let supplementary = SUPPLEMENTARY.to_string();
        assert!(bmp < supplementary, "scalar order puts the BMP string first");
        assert!(
            supplementary.encode_utf16().lt(bmp.encode_utf16()),
            "UTF-16 order puts the supplementary string first"
        );

        let dex = build(&format!(
            r#".class public LOrder;
.super Ljava/lang/Object;

.method public static run()V
    .registers 1
    const-string v0, "{BMP}"
    const-string v0, "{SUPPLEMENTARY}"
    return-void
.end method
"#
        ));

        // Not just "sorted": state the direction, so the test still means
        // something if the fixture is ever reduced to one of the two strings.
        assert!(
            index_of(&dex, &supplementary) < index_of(&dex, &bmp),
            "the supplementary string sorts first by UTF-16 code unit \
             (0xDB3F < 0xE97E), even though its scalar value is far higher"
        );
        assert_index_sections_sorted(&dex, "const-string pair");
    }

    #[test]
    fn string_pool_stays_sorted_across_the_surrogate_boundary() {
        // A spread either side of the divergence, plus prefix pairs that
        // exercise the shorter-is-less tie-break, and ASCII to confirm the
        // common case is untouched.
        let strings = [
            "a",
            "ab",
            "\u{d7ff}",       // last BMP scalar below the surrogate range
            "\u{e000}",       // first BMP scalar above it
            "\u{e97e}",       // the reported private-use string
            "\u{e97e}\u{01}", // same, with a suffix
            "\u{ffff}",       // last BMP scalar
            "\u{10000}",      // first supplementary: 0xD800 0xDC00
            "\u{dfffd}",      // the reported plane-13 string
            "\u{10ffff}",     // last supplementary: 0xDBFF 0xDFFF
        ];
        let loads = strings
            .iter()
            .map(|s| format!("    const-string v0, \"{s}\""))
            .collect::<Vec<_>>()
            .join("\n");
        let dex = build(&format!(
            r#".class public LSpread;
.super Ljava/lang/Object;

.method public static run()V
    .registers 1
{loads}
    return-void
.end method
"#
        ));

        assert_index_sections_sorted(&dex, "surrogate boundary spread");

        // Every supplementary string must precede every BMP string at U+E000
        // or above — the inversion scalar order produces.
        let mut supplementary_max = 0;
        let mut high_bmp_min = usize::MAX;
        for s in strings {
            let idx = index_of(&dex, s);
            match s.chars().next().unwrap() {
                c if c as u32 >= 0x10000 => supplementary_max = supplementary_max.max(idx),
                c if c as u32 >= 0xe000 => high_bmp_min = high_bmp_min.min(idx),
                _ => {}
            }
        }
        assert!(
            supplementary_max < high_bmp_min,
            "supplementary strings end at [{supplementary_max}] but the U+E000+ BMP \
             strings start at [{high_bmp_min}]; they must not interleave"
        );
    }

    #[test]
    fn non_ascii_class_names_keep_type_ids_sorted() {
        // `type_ids` is ordered by `descriptor_idx`, an index into the pool.
        // Sorting the descriptors as Rust strings agrees with that only while
        // they stay ASCII — which a renaming pass need not respect.
        let dex = build(&format!(
            r#".class public L{BMP};
.super Ljava/lang/Object;

.field public static other:L{SUPPLEMENTARY};
"#
        ));

        assert_index_sections_sorted(&dex, "non-ASCII class names");
        assert!(
            index_of(&dex, &format!("L{SUPPLEMENTARY};"))
                < index_of(&dex, &format!("L{BMP};")),
            "the two descriptors differ first at the character after `L`"
        );
    }

    #[test]
    fn non_ascii_member_names_keep_field_and_method_ids_sorted() {
        // Both sections are ordered by `name_idx` within a class, so the names
        // have to be placed by pool index rather than by scalar order.
        let dex = build(&format!(
            r#".class public LMembers;
.super Ljava/lang/Object;

.field public static {BMP}:I

.field public static {SUPPLEMENTARY}:I

.method public static {BMP}()V
    .registers 1
    return-void
.end method

.method public static {SUPPLEMENTARY}()V
    .registers 1
    return-void
.end method
"#
        ));

        assert_index_sections_sorted(&dex, "non-ASCII member names");
    }

    #[test]
    fn ordinary_ascii_classes_satisfy_every_ordering_rule() {
        // The checker above is new, so pin it against an ordinary class: it
        // must pass on code that has always been accepted, or the non-ASCII
        // failures above prove nothing.
        let dex = build(
            r#".class public Lcom/example/Ascii;
.super Ljava/lang/Object;
.implements Ljava/lang/Runnable;

.field private zebra:Ljava/lang/String;

.field private alpha:I

.method public constructor <init>()V
    .registers 1
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method

.method public run()V
    .registers 2
    const-string v0, "hello"
    iput-object v0, p0, Lcom/example/Ascii;->zebra:Ljava/lang/String;
    return-void
.end method

.method public compute(ILjava/lang/String;)J
    .registers 4
    const-wide/16 v0, 0x0
    return-wide v0
.end method
"#,
        );

        assert_index_sections_sorted(&dex, "ordinary ASCII class");
    }
}
