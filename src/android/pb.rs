//! Minimal protocol-buffers wire-format codec.
//!
//! Android App Bundles store their manifest and targeting metadata as protobuf
//! (`aapt.pb.XmlNode`, `Targeting`) rather than the binary XML used inside an
//! APK. The schemas involved are small and fixed, so rather than take on a
//! protobuf runtime plus code generation, this module implements the wire
//! format directly — the same approach the crate already takes for DEX and
//! AXML.
//!
//! Only what the bundle schemas actually use is implemented: varints,
//! length-delimited fields, and fixed-width 32/64-bit fields. Groups
//! (deprecated wire types 3 and 4) are rejected.
//!
//! See <https://protobuf.dev/programming-guides/encoding/> for the format.

use super::binary_xml::{BinaryXmlError, BinaryXmlResult};

/// Wire types, as encoded in the low three bits of a field key.
pub(crate) const WIRE_VARINT: u32 = 0;
pub(crate) const WIRE_FIXED64: u32 = 1;
pub(crate) const WIRE_LEN: u32 = 2;
pub(crate) const WIRE_FIXED32: u32 = 5;

fn malformed(msg: impl Into<String>) -> BinaryXmlError {
    BinaryXmlError::MalformedDocument(msg.into())
}

/// A cursor over a protobuf message body.
///
/// Every read is bounds-checked and returns `Err` rather than panicking; the
/// bytes come from a file that may be truncated or hostile.
pub(crate) struct PbReader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> PbReader<'a> {
    pub(crate) fn new(data: &'a [u8]) -> Self {
        PbReader { data, pos: 0 }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.pos >= self.data.len()
    }

    /// Read a base-128 varint. Protobuf varints are up to 10 bytes for a u64.
    pub(crate) fn varint(&mut self) -> BinaryXmlResult<u64> {
        let mut result: u64 = 0;
        let mut shift = 0u32;
        loop {
            let byte = *self
                .data
                .get(self.pos)
                .ok_or_else(|| malformed("unexpected end of protobuf stream in varint"))?;
            self.pos += 1;
            if shift >= 64 {
                return Err(malformed("protobuf varint exceeds 64 bits"));
            }
            result |= ((byte & 0x7f) as u64) << shift;
            if byte & 0x80 == 0 {
                return Ok(result);
            }
            shift += 7;
        }
    }

    /// Read a field key, returning `(field_number, wire_type)`.
    pub(crate) fn key(&mut self) -> BinaryXmlResult<(u32, u32)> {
        let key = self.varint()?;
        let field = (key >> 3) as u32;
        let wire = (key & 7) as u32;
        if field == 0 {
            return Err(malformed("protobuf field number 0 is not valid"));
        }
        Ok((field, wire))
    }

    /// Read a length-delimited field's payload.
    pub(crate) fn bytes(&mut self) -> BinaryXmlResult<&'a [u8]> {
        let len = self.varint()? as usize;
        let end = self
            .pos
            .checked_add(len)
            .filter(|e| *e <= self.data.len())
            .ok_or_else(|| {
                malformed(format!(
                    "protobuf length-delimited field of {len} bytes overruns the message"
                ))
            })?;
        let slice = &self.data[self.pos..end];
        self.pos = end;
        Ok(slice)
    }

    /// Read a length-delimited field as UTF-8.
    pub(crate) fn string(&mut self) -> BinaryXmlResult<String> {
        let raw = self.bytes()?;
        String::from_utf8(raw.to_vec())
            .map_err(|e| malformed(format!("protobuf string is not valid UTF-8: {e}")))
    }

    /// Skip a field whose number we do not recognise, preserving forward
    /// compatibility with newer bundle tooling.
    pub(crate) fn skip(&mut self, wire: u32) -> BinaryXmlResult<()> {
        match wire {
            WIRE_VARINT => {
                self.varint()?;
            }
            WIRE_FIXED64 => self.advance(8)?,
            WIRE_LEN => {
                self.bytes()?;
            }
            WIRE_FIXED32 => self.advance(4)?,
            other => {
                return Err(malformed(format!(
                    "unsupported protobuf wire type {other} (groups are not supported)"
                )));
            }
        }
        Ok(())
    }

    fn advance(&mut self, n: usize) -> BinaryXmlResult<()> {
        let end = self
            .pos
            .checked_add(n)
            .filter(|e| *e <= self.data.len())
            .ok_or_else(|| malformed("unexpected end of protobuf stream"))?;
        self.pos = end;
        Ok(())
    }
}

/// Builder for a protobuf message body.
#[derive(Default)]
pub(crate) struct PbWriter {
    buf: Vec<u8>,
}

impl PbWriter {
    pub(crate) fn new() -> Self {
        PbWriter { buf: Vec::new() }
    }

    pub(crate) fn into_bytes(self) -> Vec<u8> {
        self.buf
    }

    #[cfg(test)]
    pub(crate) fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    fn write_varint(&mut self, mut value: u64) {
        loop {
            let byte = (value & 0x7f) as u8;
            value >>= 7;
            if value == 0 {
                self.buf.push(byte);
                return;
            }
            self.buf.push(byte | 0x80);
        }
    }

    fn write_key(&mut self, field: u32, wire: u32) {
        self.write_varint(((field as u64) << 3) | wire as u64);
    }

    /// Write a `uint32`/`uint64` field. Zero is the protobuf default and is
    /// omitted, which is what aapt2 does and what keeps output compact.
    pub(crate) fn varint_field(&mut self, field: u32, value: u64) {
        if value == 0 {
            return;
        }
        self.write_key(field, WIRE_VARINT);
        self.write_varint(value);
    }

    /// Write a length-delimited field. Empty payloads are omitted, matching the
    /// proto3 default-value convention.
    pub(crate) fn bytes_field(&mut self, field: u32, value: &[u8]) {
        if value.is_empty() {
            return;
        }
        self.write_key(field, WIRE_LEN);
        self.write_varint(value.len() as u64);
        self.buf.extend_from_slice(value);
    }

    /// Write a string field, omitting it when empty.
    pub(crate) fn string_field(&mut self, field: u32, value: &str) {
        self.bytes_field(field, value.as_bytes());
    }

    /// Write a nested message, always emitting it even when the body is empty —
    /// presence is meaningful for submessages (an empty `Item` still selects a
    /// `oneof` arm, for example).
    pub(crate) fn message_field(&mut self, field: u32, body: PbWriter) {
        let bytes = body.into_bytes();
        self.write_key(field, WIRE_LEN);
        self.write_varint(bytes.len() as u64);
        self.buf.extend_from_slice(&bytes);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn varint_round_trips_boundary_values() {
        for value in [0u64, 1, 127, 128, 300, u32::MAX as u64, u64::MAX] {
            let mut w = PbWriter::new();
            w.write_varint(value);
            let bytes = w.into_bytes();
            let mut r = PbReader::new(&bytes);
            assert_eq!(r.varint().unwrap(), value, "round trip {value}");
            assert!(r.is_empty());
        }
    }

    #[test]
    fn truncated_varint_errors_rather_than_panicking() {
        // 0x80 sets the continuation bit with no following byte.
        let mut r = PbReader::new(&[0x80]);
        assert!(r.varint().is_err());
    }

    #[test]
    fn overlong_varint_is_rejected() {
        let mut r = PbReader::new(&[0x80; 12]);
        assert!(r.varint().is_err());
    }

    #[test]
    fn length_delimited_overrun_errors() {
        // Claims 50 bytes but only supplies 2.
        let mut r = PbReader::new(&[50, b'a', b'b']);
        assert!(r.bytes().is_err());
    }

    #[test]
    fn key_encodes_field_and_wire_type() {
        let mut w = PbWriter::new();
        w.string_field(3, "manifest");
        let bytes = w.into_bytes();
        // field 3, wire type 2 => (3 << 3) | 2 == 0x1a
        assert_eq!(bytes[0], 0x1a);
        assert_eq!(bytes[1], 8);
        let mut r = PbReader::new(&bytes);
        assert_eq!(r.key().unwrap(), (3, WIRE_LEN));
        assert_eq!(r.string().unwrap(), "manifest");
    }

    #[test]
    fn skip_handles_every_supported_wire_type() {
        let mut w = PbWriter::new();
        w.varint_field(1, 300);
        w.string_field(2, "hello");
        let bytes = w.into_bytes();
        let mut r = PbReader::new(&bytes);
        while !r.is_empty() {
            let (_, wire) = r.key().unwrap();
            r.skip(wire).unwrap();
        }
    }

    #[test]
    fn groups_are_rejected() {
        let mut r = PbReader::new(&[]);
        assert!(r.skip(3).is_err());
        assert!(r.skip(4).is_err());
    }

    #[test]
    fn zero_and_empty_fields_are_omitted() {
        let mut w = PbWriter::new();
        w.varint_field(5, 0);
        w.string_field(6, "");
        assert!(w.is_empty(), "proto3 defaults should not be emitted");
    }
}
