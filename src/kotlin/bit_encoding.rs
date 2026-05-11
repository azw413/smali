//! Encoder/decoder for the bit-packing scheme that the Kotlin compiler uses
//! to embed protobuf bytes inside the `d1` field of `@kotlin.Metadata`.
//!
//! `d1` is declared `String[]` because JVM annotation values can only hold
//! strings (and a few other non-binary types). The Kotlin compiler ships
//! arbitrary protobuf bytes through that field by mapping each byte to a
//! `Char`. To stay within the 64 KiB-per-`CONSTANT_Utf8` limit, the byte
//! stream is split into chunks whose modified-UTF-8 encoded length stays
//! under that limit.
//!
//! Two on-disk encodings exist. The current Kotlin compiler always emits the
//! "UTF-8 mode" form, but artifacts from older or specially-configured builds
//! may use the older "8-to-7" form, so the decoder accepts both. The encoder
//! only emits the UTF-8 form, matching what `kotlinc` produces today.
//!
//! The reference implementation lives in `kotlinx-metadata-jvm`'s
//! `BitEncoding.java` and `utfEncoding.kt` (Apache 2.0).

/// Maximum bytes a `CONSTANT_Utf8_info` entry can hold (JVMS §4.4.7).
const MAX_UTF8_INFO_LENGTH: usize = 65535;

/// Marker char prefixed to UTF-8-mode payloads. A NUL char encodes as two
/// bytes in modified UTF-8, which doubles as our format discriminator.
const UTF8_MODE_MARKER: char = '\0';

/// Marker char prefixed to 8-to-7-mode payloads (legacy compiler output).
/// `(char) -1` in Java widens to `0xFFFF`.
const BIT7_MODE_MARKER: char = '\u{FFFF}';

/// Decode a `d1` field back to its raw protobuf bytes.
///
/// Detects which on-disk encoding the producer used by inspecting the leading
/// marker char of the first non-empty string.
pub fn decode_d1(strings: &[String]) -> Vec<u8> {
    if let Some(first) = strings.first() {
        if let Some(marker) = first.chars().next() {
            if marker == UTF8_MODE_MARKER {
                let dropped = drop_marker(strings);
                return strings_to_bytes(&dropped);
            }
            if marker == BIT7_MODE_MARKER {
                let dropped = drop_marker(strings);
                let mut bytes = combine_low_bytes(&dropped);
                add_modulo_byte(&mut bytes, 0x7f); // inverse of +1 modulo 0x80
                return decode_7_to_8(&bytes);
            }
        }
    }
    // No recognised marker — assume legacy 8-to-7 stream without marker.
    let mut bytes = combine_low_bytes(strings);
    add_modulo_byte(&mut bytes, 0x7f);
    decode_7_to_8(&bytes)
}

/// Encode raw protobuf bytes into a `d1` field, using the modern UTF-8 mode
/// that current Kotlin compilers emit.
pub fn encode_d1(bytes: &[u8]) -> Vec<String> {
    bytes_to_strings(bytes)
}

// ---- UTF-8 mode codec ------------------------------------------------------

/// Encode bytes using the trivial "byte = Char codepoint" scheme. The
/// resulting strings, when serialised as modified UTF-8, fit inside
/// `CONSTANT_Utf8` entries.
fn bytes_to_strings(bytes: &[u8]) -> Vec<String> {
    let mut result: Vec<String> = Vec::with_capacity(1);
    let mut buffer = String::new();
    // 0-char marker takes 2 modified-UTF-8 bytes.
    let mut bytes_in_buffer: usize = 2;
    buffer.push(UTF8_MODE_MARKER);

    for &b in bytes {
        // Each char's code point is the byte value (0..=255).
        // `from_u32` is safe because all values are valid Unicode scalars.
        let c = char::from_u32(b as u32).expect("byte fits in U+0000..U+00FF");
        buffer.push(c);
        // Bytes 0x01..0x7F are 1 modified-UTF-8 byte. 0x00 and 0x80..0xFF are 2.
        bytes_in_buffer += if (1..=127).contains(&b) { 1 } else { 2 };

        if bytes_in_buffer >= MAX_UTF8_INFO_LENGTH - 1 {
            result.push(std::mem::take(&mut buffer));
            bytes_in_buffer = 0;
        }
    }

    if !buffer.is_empty() {
        result.push(buffer);
    }
    result
}

/// Decode UTF-8-mode strings: take the low 8 bits of each char's code point.
fn strings_to_bytes(strings: &[String]) -> Vec<u8> {
    let total: usize = strings.iter().map(|s| s.chars().count()).sum();
    let mut out = Vec::with_capacity(total);
    for s in strings {
        for c in s.chars() {
            out.push((c as u32 & 0xff) as u8);
        }
    }
    out
}

// ---- 8-to-7 mode codec (legacy decode only) --------------------------------

/// Combine the (already marker-stripped) strings into one byte vector by
/// taking the low 8 bits of each char.
fn combine_low_bytes(strings: &[String]) -> Vec<u8> {
    let total: usize = strings.iter().map(|s| s.chars().count()).sum();
    let mut out = Vec::with_capacity(total);
    for s in strings {
        for c in s.chars() {
            out.push((c as u32 & 0xff) as u8);
        }
    }
    out
}

/// Add `inc` to every byte modulo 128. Used by the 8-to-7 path to re-bias
/// bytes back into a useful range.
fn add_modulo_byte(data: &mut [u8], inc: u8) {
    for b in data.iter_mut() {
        *b = b.wrapping_add(inc) & 0x7f;
    }
}

/// Inverse of Kotlin's `encode8to7`. Treats the input as a stream of 7-bit
/// groups (low bits first) and packs them back into 8-bit bytes.
fn decode_7_to_8(data: &[u8]) -> Vec<u8> {
    let result_length = 7 * data.len() / 8;
    let mut result = vec![0u8; result_length];

    let mut byte_index = 0usize;
    let mut bit = 0u32;
    for slot in result.iter_mut() {
        let first = (data[byte_index] as u32) >> bit;
        byte_index += 1;
        let mask = (1u32 << (bit + 1)) - 1;
        let second = (data[byte_index] as u32 & mask) << (7 - bit);
        *slot = (first + second) as u8;
        if bit == 6 {
            byte_index += 1;
            bit = 0;
        } else {
            bit += 1;
        }
    }
    result
}

// ---- Helpers ---------------------------------------------------------------

fn drop_marker(strings: &[String]) -> Vec<String> {
    let mut out = strings.to_vec();
    if let Some(first) = out.first_mut() {
        let mut chars = first.chars();
        chars.next();
        *first = chars.as_str().to_string();
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_round_trips() {
        let encoded = encode_d1(&[]);
        // The marker still occupies one char in the only emitted string.
        assert_eq!(encoded.len(), 1);
        let decoded = decode_d1(&encoded);
        assert!(decoded.is_empty());
    }

    #[test]
    fn small_payload_round_trips() {
        let bytes: Vec<u8> = (0..=255u8).collect();
        let encoded = encode_d1(&bytes);
        let decoded = decode_d1(&encoded);
        assert_eq!(decoded, bytes);
    }

    #[test]
    fn nul_bytes_survive() {
        // NUL bytes inside the payload (not the marker) must round-trip.
        let bytes = vec![0x00, 0x01, 0x00, 0x02, 0x00];
        let encoded = encode_d1(&bytes);
        let decoded = decode_d1(&encoded);
        assert_eq!(decoded, bytes);
    }

    #[test]
    fn high_bytes_survive() {
        let bytes = vec![0x80, 0xff, 0xc0, 0x7f, 0x81];
        let encoded = encode_d1(&bytes);
        let decoded = decode_d1(&encoded);
        assert_eq!(decoded, bytes);
    }

    #[test]
    fn payload_chunked_when_long() {
        // Force chunking by emitting enough zero bytes that the modified-UTF-8
        // budget overflows. Each NUL costs 2 bytes in the buffer.
        let bytes = vec![0x00; 40_000];
        let encoded = encode_d1(&bytes);
        assert!(
            encoded.len() >= 2,
            "expected multiple chunks for {} zero bytes, got {}",
            bytes.len(),
            encoded.len()
        );
        let decoded = decode_d1(&encoded);
        assert_eq!(decoded, bytes);
    }

    #[test]
    fn encoded_form_starts_with_utf8_marker() {
        let bytes = vec![1, 2, 3];
        let encoded = encode_d1(&bytes);
        assert_eq!(encoded[0].chars().next(), Some(UTF8_MODE_MARKER));
    }

    #[test]
    fn decodes_legacy_8to7_with_marker() {
        // Build a payload via the 8-to-7 path manually: take some bytes,
        // encode them using the inverse of decode_7_to_8 and add the marker.
        // We don't expose the encoder for 8-to-7 (the production path no
        // longer uses it), but we can still verify the decoder accepts it
        // by round-tripping through our own encoder, then re-wrapping with
        // the legacy marker after a manual transform.
        let original = vec![0x12u8, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde];
        // Build bits LSB-first, then take 7-bit groups.
        let mut bit_stream = Vec::with_capacity(original.len() * 8);
        for &b in &original {
            for i in 0..8 {
                bit_stream.push((b >> i) & 1);
            }
        }
        // Pad to a multiple of 7.
        while bit_stream.len() % 7 != 0 {
            bit_stream.push(0);
        }
        let mut packed = Vec::new();
        for chunk in bit_stream.chunks(7) {
            let mut byte = 0u8;
            for (i, bit) in chunk.iter().enumerate() {
                byte |= bit << i;
            }
            packed.push(byte);
        }
        // Apply the +1 modulo step the encoder runs before splitting.
        for b in packed.iter_mut() {
            *b = b.wrapping_add(1) & 0x7f;
        }
        // Wrap with the 8-to-7 marker.
        let mut s = String::new();
        s.push(BIT7_MODE_MARKER);
        for &b in &packed {
            s.push(char::from_u32(b as u32).unwrap());
        }
        let decoded = decode_d1(&[s]);
        // The 8-to-7 path produces floor(7*N/8) output bytes, which may be
        // one fewer than the original if padding bits were added. Confirm the
        // shared prefix matches.
        assert!(decoded.starts_with(&original[..decoded.len().min(original.len())]));
        assert!(!decoded.is_empty());
    }
}
