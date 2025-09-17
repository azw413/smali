use std::cmp::max;
use crate::dex::error::DexError;
use crate::dex::{read_u1, read_uleb128, write_u1, write_uleb128, write_x};
use crate::dex::dex_file::DexString;

#[derive(Debug, PartialEq, Clone)]
pub struct EncodedAnnotation {
    pub type_idx: u32,
    pub elements: Vec<AnnotationElement>,
}

impl EncodedAnnotation {
    // Read an EncodedAnnotation from bytes
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<EncodedAnnotation, DexError> {
        let type_idx = read_uleb128(bytes, ix)?;
        let size = read_uleb128(bytes, ix)? as usize;
        let mut elements = Vec::with_capacity(size);

        for _ in 0..size {
            let element = AnnotationElement::read(bytes, ix)?;
            elements.push(element);
        }

        Ok(EncodedAnnotation { type_idx, elements })
    }

    // Write an EncodedAnnotation to bytes
    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut written_bytes = 0;

        written_bytes += write_uleb128(bytes, self.type_idx);
        written_bytes += write_uleb128(bytes, self.elements.len() as u32);

        for element in &self.elements {
            written_bytes += element.write(bytes);
        }

        written_bytes
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct AnnotationElement {
    pub name_idx: u32,
    pub value: EncodedValue,
}

impl AnnotationElement {
    // Read an AnnotationElement from bytes
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<AnnotationElement, DexError> {
        let name_idx = read_uleb128(bytes, ix)?;
        let value = EncodedValue::read(bytes, ix)?;

        Ok(AnnotationElement { name_idx, value })
    }

    // Write an AnnotationElement to bytes
    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut written_bytes = 0;

        written_bytes += write_uleb128(bytes, self.name_idx);
        written_bytes += self.value.write(bytes);

        written_bytes
    }
}


#[derive(Debug, PartialEq, Clone)]
pub enum EncodedValue {
    Byte(i8),
    Short(i16),
    Char(u16),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    MethodType(u32),
    MethodHandle(u32),
    String(u32),
    Type(u32),
    Field(u32),
    Method(u32),
    Enum(u32),
    Array(Vec<EncodedValue>),
    Annotation(EncodedAnnotation),
    Null,
    Boolean(bool),
}

impl EncodedValue
{

    /// Return a shared reference to the inner `EncodedAnnotation` if this value is `EncodedValue::Annotation`.
    #[inline]
    pub fn as_annotation(&self) -> Option<&EncodedAnnotation> {
        match self {
            EncodedValue::Annotation(ann) => Some(ann),
            _ => None,
        }
    }

    pub fn to_string(&self, strings: &Vec<DexString>) -> String
    {
        match self
        {
            EncodedValue::Byte(x) => { format!("{:}", *x) }
            EncodedValue::Short(x) => { format!("{:}", *x) }
            EncodedValue::Char(x) => { format!("{:}", *x) }
            EncodedValue::Int(x) => { format!("{:}", *x) }
            EncodedValue::Long(x) => { format!("{:}", *x) }
            EncodedValue::Float(x) => { format!("{:}", *x) }
            EncodedValue::Double(x) => { format!("{:}", *x) }
            EncodedValue::MethodType(x) => { format!("{:?}", *x) }
            EncodedValue::MethodHandle(x) => { format!("{:?}", *x) }
            EncodedValue::String(x) => { format!("\"{}\"", match &strings[*x as usize] {
                DexString::Decoded(s) => s.to_string(),
                DexString::Raw(_, _) => format!("{:?}", *x)
            } ) }
            EncodedValue::Type(x) => { format!("{:?}", *x) }
            EncodedValue::Field(x) => { format!("{:?}", *x) }
            EncodedValue::Method(x) => { format!("{:?}", *x) }
            EncodedValue::Enum(x) => { format!("{:?}", *x) }
            EncodedValue::Array(v) => {
                let mut s = "{ ".to_string();
                for i in v { s.push_str(&format!("{},", i.to_string(strings))); }
                s.push_str(" }");
                s
            }
            EncodedValue::Annotation(ea) => { format!("{:?}", *ea) }
            EncodedValue::Null => { "null".to_string() }
            EncodedValue::Boolean(b) => { if *b { "true".to_string() } else { "false".to_string() } }
        }
    }

    // Read the EncodedValue from bytes
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<EncodedValue, DexError> {
        let header_byte = read_u1(bytes, ix)?;
        let value_arg = header_byte >> 5;
        let value_type = header_byte & 0x1F;
        let size = (value_arg + 1) as usize;

        match value_type {
            0x00 => {
                let val = read_u1(bytes, ix)? as i8;
                Ok(EncodedValue::Byte(val))
            },
            0x02 => {
                let val = read_var_u16(bytes, ix, size)? as i16;
                Ok(EncodedValue::Short(val))
            },
            0x03 => {
                let val = read_var_u16(bytes, ix, size)? as i16;
                Ok(EncodedValue::Char(val as u16))
            },
            0x04 => {
                let val = read_var_u32(bytes, ix, size)? as i32;
                Ok(EncodedValue::Int(val))
            },
            0x06 => {
                let val = read_var_i64(bytes, ix, size)?;
                Ok(EncodedValue::Long(val))
            },
            0x10 => {
                let val = read_var_f32(bytes, ix, size)?;
                Ok(EncodedValue::Float(val))
            },
            0x11 => {
                let val = read_var_f64(bytes, ix, size)?;
                Ok(EncodedValue::Double(val))
            },
            0x15 => {
                let val = read_var_u32(bytes, ix, size)?;
                Ok(EncodedValue::MethodType(val))
            },
            0x16 => {
                let val = read_var_u32(bytes, ix, size)?;
                Ok(EncodedValue::MethodHandle(val))
            },
            0x17 => {
                let val = read_var_u32(bytes, ix, size)?;
                Ok(EncodedValue::String(val))
            },
            0x18 => {
                let val = read_var_u32(bytes, ix, size)?;
                Ok(EncodedValue::Type(val))
            },
            0x19 => {
                let val = read_var_u32(bytes, ix, size)?;
                Ok(EncodedValue::Field(val))
            },
            0x1A => {
                let val = read_var_u32(bytes, ix, size)?;
                Ok(EncodedValue::Method(val))
            },
            0x1B => {
                let val = read_var_u32(bytes, ix, size)?;
                Ok(EncodedValue::Enum(val))
            },
            0x1C => {
                Ok(EncodedValue::Array(read_encoded_array(bytes, ix)?))
            },
            0x1D => {
                Ok(EncodedValue::Annotation(EncodedAnnotation::read(bytes, ix)?))
            },
            0x1E => Ok(EncodedValue::Null),
            0x1F => Ok(EncodedValue::Boolean(value_arg != 0)),
            _ => Err(DexError::new("Unknown EncodedValue type")),
        }
    }

    // Write the EncodedValue to bytes
    pub fn write(&self, bytes: &mut Vec<u8>) -> usize
    {
        let mut c = 0;

        match self {
            EncodedValue::Byte(val) => {
                c += write_u1(bytes, 0x00); // value_type = 0x00, value_arg = 0
                c += write_u1(bytes, *val as u8)
            },
            EncodedValue::Short(value) => {
                c += write_u1(bytes, ((byte_size_i16(*value) - 1) << 5) | 0x02);
                let x = &value.to_le_bytes()[..byte_size_i16(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::Char(value) => {
                c += write_u1(bytes, ((byte_size_u16(*value) - 1) << 5) | 0x03);
                let x = &value.to_le_bytes()[..byte_size_u16(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::Int(value) => {
                c += write_u1(bytes, ((byte_size_i32(*value) - 1) << 5) | 0x04);
                let x = &value.to_le_bytes()[..byte_size_i32(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::Long(value) => {
                c += write_u1(bytes, ((byte_size_i64(*value) - 1) << 5) | 0x06);
                let x = &value.to_le_bytes()[..byte_size_i64(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::Float(value) => {
                c += write_u1(bytes, ((byte_size_f32(*value) - 1) << 5) | 0x10);
                let x = &value.to_le_bytes()[..byte_size_f32(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::Double(value) => {
                c += write_u1(bytes, ((byte_size_f64(*value) - 1) << 5) | 0x11);
                let x = &value.to_le_bytes()[..byte_size_f64(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::MethodType(value) => {
                c += write_u1(bytes, ((byte_size_u32(*value) - 1) << 5) | 0x15);
                let x = &value.to_le_bytes()[..byte_size_u32(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::MethodHandle(value) => {
                c += write_u1(bytes, ((byte_size_u32(*value) - 1) << 5) | 0x16);
                let x = &value.to_le_bytes()[..byte_size_u32(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::String(value) => {
                c += write_u1(bytes, ((byte_size_u32(*value) - 1) << 5) | 0x17);
                let x = &value.to_le_bytes()[..byte_size_u32(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::Type(value) => {
                c += write_u1(bytes, ((byte_size_u32(*value) - 1) << 5) | 0x18);
                let x = &value.to_le_bytes()[..byte_size_u32(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::Field(value) => {
                c += write_u1(bytes, ((byte_size_u32(*value) - 1) << 5) | 0x19);
                let x = &value.to_le_bytes()[..byte_size_u32(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::Method(value) => {
                c += write_u1(bytes, ((byte_size_u32(*value) - 1) << 5) | 0x1a);
                let x = &value.to_le_bytes()[..byte_size_u32(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::Enum(value) => {
                c += write_u1(bytes, ((byte_size_u32(*value) - 1) << 5) | 0x1b);
                let x = &value.to_le_bytes()[..byte_size_u32(*value) as usize];
                c += write_x(bytes, x);
            },
            EncodedValue::Array(value) => {
                c += write_u1(bytes, 0x1c);
                c += write_encoded_array(value, bytes);
            },
            EncodedValue::Annotation(value) => {
                c += write_u1(bytes, 0x1d);
                c += value.write(bytes);
            },
            EncodedValue::Null => {
                c += write_u1(bytes, 0x1e);
            },
            EncodedValue::Boolean(val) => {
                let v = match *val { true => 1, false => 0 };
                c += write_u1(bytes, 0x1f | (v << 5));
            }
        }
        c
    }
}

fn read_var_u16(bytes: &[u8], ix: &mut usize, size: usize) -> Result<u16, DexError> {
    let mut result = 0;
    for i in 0..size {
        let byte = read_u1(bytes, ix)?;
        result |= (byte as u16) << (8 * i);
    }
    if size < 2 && (result & (1 << (8 * size - 1))) != 0 {
        result |= !((1 << (8 * size)) - 1);
    }
    Ok(result)
}

fn read_var_u32(bytes: &[u8], ix: &mut usize, size: usize) -> Result<u32, DexError> {
    let mut result = 0;
    for i in 0..size {
        let byte = read_u1(bytes, ix)?;
        result |= (byte as u32) << (8 * i);
    }

    Ok(result)
}

fn read_var_i64(bytes: &[u8], ix: &mut usize, size: usize) -> Result<i64, DexError> {
    let mut result = 0i64;
    for i in 0..size {
        let byte = read_u1(bytes, ix)?;
        result |= (byte as i64) << (8 * i);
    }
    if size < 8 && (result & (1 << (8 * size - 1))) != 0 {
        result |= !((1 << (8 * size)) - 1);
    }
    Ok(result)
}

fn read_var_f32(bytes: &[u8], ix: &mut usize, size: usize) -> Result<f32, DexError> {
    let mut result = 0u32;
    for i in 0..size {
        let byte = read_u1(bytes, ix)?;
        result |= (byte as u32) << (8 * i);
    }
    Ok(f32::from_bits(result))
}

fn read_var_f64(bytes: &[u8], ix: &mut usize, size: usize) -> Result<f64, DexError> {
    let mut result = 0u64;
    for i in 0..size {
        let byte = read_u1(bytes, ix)?;
        result |= (byte as u64) << (8 * i);
    }
    Ok(f64::from_bits(result))
}

fn byte_size_i16(v: i16) -> u8
{
    let s = (v.leading_zeros() / 8) as u8;
    max(1, 2 - s)
}

fn byte_size_u16(v: u16) -> u8
{
    let s = (v.leading_zeros() / 8) as u8;
    max(1, 2 - s)
}

fn byte_size_i32(v: i32) -> u8
{
    let s = (v.leading_zeros() / 8) as u8;
    max(1, 4 - s)
}

fn byte_size_u32(v: u32) -> u8
{
    let s = (v.leading_zeros() / 8) as u8;
    max(1, 4 - s)
}

fn byte_size_i64(v: i64) -> u8
{
    let s = (v.leading_zeros() / 8) as u8;
    max(1, 8 - s)
}

fn byte_size_f32(v: f32) -> u8
{
    let u = u32::from_le_bytes(v.to_le_bytes());
    let s = (u.leading_zeros() / 8) as u8;
    max(1, 4 - s)
}

fn byte_size_f64(v: f64) -> u8
{
    let u = u64::from_le_bytes(v.to_le_bytes());
    let s = (u.leading_zeros() / 8) as u8;
    max(1, 8 - s)
}



pub fn write_encoded_array(encoded_array: &[EncodedValue], bytes: &mut Vec<u8>) -> usize
{
    let mut c = 0;
    c += write_uleb128(bytes, encoded_array.len() as u32);

    for value in encoded_array {
        c += value.write(bytes);
    }

    c
}

pub fn read_encoded_array(bytes: &[u8], ix: &mut usize) -> Result<Vec<EncodedValue>, DexError>
{
    let size = read_uleb128(bytes, ix)? as usize;

    let mut values = Vec::with_capacity(size);
    for _ in 0..size
    {
        values.push(EncodedValue::read(bytes, ix)?);
    }

    Ok(values)
}


#[cfg(test)]
mod tests {
    use super::*;

    // Unfortunately GPT-4 screwed up on these test cases: the input vectors need calculating properly for most of them to work.

    #[test]
    fn test_encoded_value_byte() {
        let mut bytes = vec![0x00, 0x7F];  // 0x00 is the header, 0x7F (127) is the value
        let mut ix = 0;
        let encoded_value = EncodedValue::read(&bytes, &mut ix).expect("Failed to read EncodedValue");
        match encoded_value {
            EncodedValue::Byte(val) => assert_eq!(val, 127),
            _ => panic!("Unexpected variant"),
        }

        let mut output_bytes = vec![];
        let bytes_written = encoded_value.write(&mut output_bytes);
        assert_eq!(bytes_written, 2);
        assert_eq!(output_bytes, bytes);
    }

    #[test]
    fn test_encoded_value_short() {
        let mut bytes = vec![34, 0x34, 0x12];  // 0x10 is the header, 0x1234 in little-endian
        let mut ix = 0;
        let encoded_value = EncodedValue::read(&bytes, &mut ix).expect("Failed to read EncodedValue");
        match encoded_value {
            EncodedValue::Short(val) => assert_eq!(val, 0x1234),
            _ => panic!("Unexpected variant"),
        }

        let mut output_bytes = vec![];
        let bytes_written = encoded_value.write(&mut output_bytes);
        assert_eq!(bytes_written, 3);
        assert_eq!(output_bytes, bytes);
    }

    #[test]
    fn test_encoded_value_char() {
        let mut bytes = vec![35, 0x34, 0x12];  // 0x30 is the header, 0x1234 in little-endian
        let mut ix = 0;
        let encoded_value = EncodedValue::read(&bytes, &mut ix).expect("Failed to read EncodedValue");
        match encoded_value {
            EncodedValue::Char(val) => assert_eq!(val, 0x1234),
            _ => panic!("Unexpected variant"),
        }

        let mut output_bytes = vec![];
        let bytes_written = encoded_value.write(&mut output_bytes);
        assert_eq!(bytes_written, 3);
        assert_eq!(output_bytes, bytes);
    }

    #[test]
    fn test_encoded_value_int() {
        let mut bytes = vec![0x64, 0x78, 0x56, 0x34, 0x12];  // 0x40 is the header, 0x12345678 in little-endian
        let mut ix = 0;
        let encoded_value = EncodedValue::read(&bytes, &mut ix).expect("Failed to read EncodedValue");
        match encoded_value {
            EncodedValue::Int(val) => assert_eq!(val, 0x12345678),
            _ => panic!("Unexpected variant"),
        }

        let mut output_bytes = vec![];
        let bytes_written = encoded_value.write(&mut output_bytes);
        assert_eq!(bytes_written, 5);
        assert_eq!(output_bytes, bytes);
    }

    #[test]
    fn test_encoded_value_null() {
        let mut bytes = vec![0x1E];  // 0x1E is the header for null
        let mut ix = 0;
        let encoded_value = EncodedValue::read(&bytes, &mut ix).expect("Failed to read EncodedValue");
        match encoded_value {
            EncodedValue::Null => (),
            _ => panic!("Unexpected variant"),
        }

        let mut output_bytes = vec![];
        let bytes_written = encoded_value.write(&mut output_bytes);
        assert_eq!(bytes_written, 1);
        assert_eq!(output_bytes, bytes);
    }

    #[test]
    fn test_encoded_value_boolean_true() {
        let mut bytes = vec![0x1F | (1 << 5)];  // 0x1F is the header for boolean, 1 << 5 is the value_arg
        let mut ix = 0;
        let encoded_value = EncodedValue::read(&bytes, &mut ix).expect("Failed to read EncodedValue");
        match encoded_value {
            EncodedValue::Boolean(val) => assert_eq!(val, true),
            _ => panic!("Unexpected variant"),
        }

        let mut output_bytes = vec![];
        let bytes_written = encoded_value.write(&mut output_bytes);
        assert_eq!(bytes_written, 1);
        assert_eq!(output_bytes, bytes);
    }

    #[test]
    fn test_encoded_value_boolean_false() {
        let mut bytes = vec![0x1F];  // 0x1F is the header for boolean with value_arg = 0
        let mut ix = 0;
        let encoded_value = EncodedValue::read(&bytes, &mut ix).expect("Failed to read EncodedValue");
        match encoded_value {
            EncodedValue::Boolean(val) => assert_eq!(val, false),
            _ => panic!("Unexpected variant"),
        }

        let mut output_bytes = vec![];
        let bytes_written = encoded_value.write(&mut output_bytes);
        assert_eq!(bytes_written, 1);
        assert_eq!(output_bytes, bytes);
    }

    #[test]
    fn test_encoded_annotation_read_write() {
        let annotation = EncodedAnnotation {
            type_idx: 1,
            elements: vec![
                AnnotationElement {
                    name_idx: 2,
                    value: EncodedValue::Boolean(true),
                },
                AnnotationElement {
                    name_idx: 3,
                    value: EncodedValue::Int(42),
                },
            ],
        };

        let mut bytes = vec![];
        annotation.write(&mut bytes);

        let mut ix = 0;
        let read_annotation = EncodedAnnotation::read(&bytes, &mut ix).expect("Failed to read EncodedAnnotation");

        assert_eq!(annotation, read_annotation);
    }
}
