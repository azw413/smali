pub(crate) fn encode_uleb128(value: u32) -> Vec<u8> {
    let mut result = Vec::new();
    let mut remaining = value;

    if remaining == 0 {
        result.push(0);
        return result;
    }

    while remaining != 0 {
        let mut byte = (remaining & 0x7F) as u8;
        remaining >>= 7;

        if remaining != 0 {
            byte |= 0x80;
        }

        result.push(byte);
    }

    result
}

pub(crate) fn decode_uleb128(encoded: &[u8]) -> (u32, usize) {
    let mut value: u32 = 0;
    let mut shift: u32 = 0;
    let mut count: usize = 0;

    for &byte in encoded {
        count += 1;

        let low = (byte & 0x7F) as u32;
        if shift < 32 {
            // guard against UB: saturate the shift and use wrapping to avoid panic
            value = value.wrapping_add(low.wrapping_shl(shift));
        }

        let cont = (byte & 0x80) != 0;
        shift = shift.saturating_add(7);

        // DEX uleb128 values are 32-bit — valid encodings are ≤ 5 bytes.
        if !cont || count == 5 {
            break;
        }
    }

    (value, count)
}

pub(crate) fn encode_sleb128(value: i32) -> Vec<u8> {
    let mut result = Vec::new();
    let mut remaining = value;

    loop {
        let mut byte = (remaining & 0x7F) as u8;
        remaining >>= 7;

        let is_more =
            !((remaining == 0 && (byte & 0x40) == 0) || (remaining == -1 && (byte & 0x40) != 0));
        if is_more {
            byte |= 0x80;
        }

        result.push(byte);

        if !is_more {
            break;
        }
    }

    result
}

pub(crate) fn decode_sleb128(encoded: &[u8]) -> (i32, usize) {
    let mut value: i32 = 0;
    let mut shift: u32 = 0;
    let mut count: usize = 0;
    let mut last_byte: u8 = 0;

    for &byte in encoded {
        count += 1;
        last_byte = byte;

        let low = (byte & 0x7F) as i32;
        if shift < 32 {
            value |= low.wrapping_shl(shift);
        }

        let cont = (byte & 0x80) != 0;
        shift = shift.saturating_add(7);

        // i32 sleb128 likewise fits within 5 bytes
        if !cont || count == 5 {
            break;
        }
    }

    // Sign-extend if needed and we didn't fill all 32 bits
    if (last_byte & 0x40) != 0 && shift < 32 {
        value |= (-1i32).wrapping_shl(shift);
    }

    (value, count)
}

pub(crate) fn encode_uleb128p1(value: i32) -> Vec<u8> {
    encode_uleb128((value + 1) as u32)
}

pub(crate) fn decode_uleb128p1(encoded: &[u8]) -> (i32, usize) {
    let (v, c) = decode_uleb128(encoded);
    (v as i32 - 1, c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_uleb128() {
        let cases = vec![
            (0, vec![0x00]),
            (1, vec![0x01]),
            (127, vec![0x7F]),
            (128, vec![0x80, 0x01]),
            (16256, vec![0x80, 0x7F]),
            (624485, vec![0xE5, 0x8E, 0x26]),
        ];

        for (value, expected) in cases {
            assert_eq!(encode_uleb128(value), expected);
        }
    }

    #[test]
    fn test_decode_uleb128() {
        let cases = vec![
            (vec![0x00], 0),
            (vec![0x01], 1),
            (vec![0x7F], 127),
            (vec![0x80, 0x01], 128),
            (vec![0x80, 0x7F], 16256),
            (vec![0xE5, 0x8E, 0x26], 624485),
        ];

        for (encoded, expected) in cases {
            let (v, _) = decode_uleb128(&encoded);
            assert_eq!(v, expected);
        }
    }

    #[test]
    fn test_encode_sleb128() {
        let cases = vec![
            (0, vec![0x00]),
            (1, vec![0x01]),
            (-1, vec![0x7F]),
            (-123456, vec![0xC0, 0xBB, 0x78]),
            (-128, vec![0x80, 0x7F]),
        ];

        for (value, expected) in cases {
            assert_eq!(encode_sleb128(value), expected);
        }
    }

    #[test]
    fn test_decode_sleb128() {
        let cases = vec![
            (vec![0x00], 0),
            (vec![0x01], 1),
            (vec![0x7F], -1),
            (vec![0xFF, 0x00], 127),
            (vec![0x80, 0x7F], -128),
            (vec![0xC0, 0xBB, 0x78], -123456),
        ];

        for (encoded, expected) in cases {
            let (v, _) = decode_sleb128(&encoded);
            assert_eq!(v, expected);
        }
    }
}
