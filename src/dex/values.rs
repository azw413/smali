use std::cmp::{max};
use std::io::{Cursor, Read};
use byteorder::{ReadBytesExt};
use crate::dex::DexError;
use crate::leb::{decode_uleb128, encode_uleb128};

#[derive(Debug, PartialEq)]
pub struct EncodedAnnotation {
    pub type_idx: u32,
    pub elements: Vec<AnnotationElement>,
}

#[derive(Debug, PartialEq)]
pub struct AnnotationElement {
    pub name_idx: u32,
    pub value: EncodedValue,
}

#[derive(Debug, PartialEq)]
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
    Annotation(EncodedAnnotation), // You'll need to define the EncodedAnnotation type
    Null,
    Boolean(bool),
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

impl EncodedValue {
    pub fn encode(&self) -> Vec<u8> {
        let mut data = Vec::new();

        match self {
            EncodedValue::Byte(value) => {
                data.push(0x00);
                data.push(*value as u8);
            }
            EncodedValue::Short(value) => {
                data.push(0x02 | ((byte_size_i16(*value) - 1) << 5) as u8) ;
                data.extend(&value.to_le_bytes()[..byte_size_i16(*value) as usize]);
            }
            EncodedValue::Char(value) => {
                data.push(0x03 | ((byte_size_u16(*value) - 1) << 5) as u8);
                data.extend(&value.to_le_bytes()[..byte_size_u16(*value) as usize]);
            }
            EncodedValue::Int(value) => {
                data.push(0x04 | ((byte_size_i32(*value) - 1) << 5) as u8);
                data.extend(&value.to_le_bytes()[..byte_size_i32(*value) as usize]);
            }
            EncodedValue::Long(value) => {
                data.push(0x06 | ((byte_size_i64(*value) - 1) << 5) as u8);
                data.extend(&value.to_le_bytes()[..byte_size_i64(*value) as usize]);
            }
            EncodedValue::Float(value) => {
                data.push(0x10 | ((byte_size_f32(*value) - 1) << 5) as u8);
                data.extend(&value.to_bits().to_le_bytes()[..byte_size_f32(*value) as usize]);
            }
            EncodedValue::Double(value) => {
                data.push(0x11 | ((byte_size_f64(*value) - 1) << 5) as u8);
                data.extend(&value.to_bits().to_le_bytes()[..byte_size_f64(*value) as usize]);
            }
            EncodedValue::MethodType(value) => {
                data.push(0x15 | ((byte_size_u32(*value) - 1) << 5) as u8);
                data.extend(&value.to_le_bytes()[..byte_size_u32(*value) as usize]);
            }
            EncodedValue::MethodHandle(value) => {
                data.push(0x16 | ((byte_size_u32(*value) - 1) << 5) as u8);
                data.extend(&value.to_le_bytes()[..byte_size_u32(*value) as usize]);
            }
            EncodedValue::String(value) => {
                data.push(0x17 | ((byte_size_u32(*value) - 1) << 5) as u8);
                data.extend(&value.to_le_bytes()[..byte_size_u32(*value) as usize]);
            }
            EncodedValue::Type(value) => {
                data.push(0x18 | ((byte_size_u32(*value) - 1) << 5) as u8);
                data.extend(&value.to_le_bytes()[..byte_size_u32(*value) as usize]);
            }
            EncodedValue::Field(value) => {
                data.push(0x19 | ((byte_size_u32(*value) - 1) << 5) as u8);
                data.extend(&value.to_le_bytes()[..byte_size_u32(*value) as usize]);
            }
            EncodedValue::Method(value) => {
                data.push(0x1a | ((byte_size_u32(*value) - 1) << 5) as u8);
                data.extend(&value.to_le_bytes()[..byte_size_u32(*value) as usize]);
            }
            EncodedValue::Enum(value) => {
                data.push(0x1b | ((byte_size_u32(*value) - 1) << 5) as u8);
                data.extend(&value.to_le_bytes()[..byte_size_u32(*value) as usize]);
            }
            EncodedValue::Array(value) => {
                data.push(0x1c);
                data.extend(encode_encoded_array(value));
            }
            EncodedValue::Annotation(value) => {
                data.push(0x1d);
                data.extend(encode_encoded_annotation(value));
            }
            EncodedValue::Null => {
                data.push(0x1e);
            }
            EncodedValue::Boolean(value) => {
                data.push(0x1f | ((*value as u8) << 5));
            }
        }

        data
    }

    pub fn decode(data: &[u8]) -> Result<(Self, usize), DexError> {
        let mut cursor = Cursor::new(data);

        let value_type_and_arg = cursor.read_u8().map_err(|_| DexError::new("Failed to read encoded value type and arg"))?;
        let value_type = value_type_and_arg & 0x1F;
        let value_arg = value_type_and_arg >> 5;
        let s = (value_arg + 1) as usize;

        match value_type {
            0x00 => {
                let value = cursor.read_i8().map_err(|_| DexError::new("Failed to read ValueByte"))?;
                Ok((EncodedValue::Byte(value), 2))
            }
            0x02 => {
                let mut bytes = vec![0; 2];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueShort"))?;
                let value = i16::from_le_bytes(bytes.try_into().unwrap());
                Ok((EncodedValue::Short(value), s + 1))
            }
            0x03 => {
                let mut bytes = vec![0; 2];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueChar"))?;
                let value = u16::from_le_bytes(bytes.try_into().unwrap());
                Ok((EncodedValue::Char(value), s + 1))
            }
            0x04 => {
                let mut bytes = vec![0; 4];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueInt"))?;
                let value = i32::from_le_bytes(bytes.try_into().unwrap());
                Ok((EncodedValue::Int(value), s + 1))
            }
            0x06 => {
                let mut bytes = vec![0; 8];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueLong"))?;
                let value = i64::from_le_bytes(bytes.try_into().unwrap());
                Ok((EncodedValue::Long(value), s + 1))
            }
            0x10 => {
                let mut bytes = vec![0; 4];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueFloat"))?;
                let value = f32::from_bits(u32::from_le_bytes(bytes.try_into().unwrap()));
                Ok((EncodedValue::Float(value), s + 1))
            }
            0x11 => {
                let mut bytes = vec![0; 8];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueDouble"))?;
                let value = f64::from_bits(u64::from_le_bytes(bytes.try_into().unwrap()));
                Ok((EncodedValue::Double(value), s + 1))
            }
            0x15 => {
                let mut bytes = vec![0; 4];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueMethodType"))?;
                let value = u32::from_le_bytes(bytes.try_into().unwrap());
                Ok((EncodedValue::MethodType(value), s + 1))
            }
            0x16 => {
                let mut bytes = vec![0; 4];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueMethodHandle"))?;
                let value = u32::from_le_bytes(bytes.try_into().unwrap());
                Ok((EncodedValue::MethodHandle(value), s + 1))
            }
            0x17 => {
                let mut bytes = vec![0; 4];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueString"))?;
                let value = u32::from_le_bytes(bytes.try_into().unwrap());
                Ok((EncodedValue::String(value), s + 1))
            }
            0x18 => {
                let mut bytes = vec![0; 4];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueType"))?;
                let value = u32::from_le_bytes(bytes.try_into().unwrap());
                Ok((EncodedValue::Type(value), s + 1))
            }
            0x19 => {
                let mut bytes = vec![0; 4];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueField"))?;
                let value = u32::from_le_bytes(bytes.try_into().unwrap());
                Ok((EncodedValue::Field(value), s + 1))
            }
            0x1a => {
                let mut bytes = vec![0; 4];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueMethod"))?;
                let value = u32::from_le_bytes(bytes.try_into().unwrap());
                Ok((EncodedValue::Method(value), s + 1))
            }
            0x1b => {
                let mut bytes = vec![0; 4];
                cursor.take(s as u64).read(&mut bytes).map_err(|_| DexError::new("Failed to read ValueEnum"))?;
                let value = u32::from_le_bytes(bytes.try_into().unwrap());
                Ok((EncodedValue::Enum(value), s + 1))
            }
            0x1c => {
                let (values, offset) = decode_encoded_array(&data[1..]).map_err(|_| DexError::new("Failed to read ValueArray"))?;
                Ok((EncodedValue::Array(values), offset + 1))
            }
            0x1d => {
                let (encoded_annotation, offset) = decode_encoded_annotation(&data[1..]).map_err(|_| DexError::new("Failed to read ValueAnnotation"))?;
                Ok((EncodedValue::Annotation(encoded_annotation), offset + 1))
            }
            0x1e => Ok((EncodedValue::Null, 1)),
            0x1f => Ok((EncodedValue::Boolean(value_arg != 0), 1)),
            _ => Err(DexError::new("Invalid encoded value type"))
        }
    }
}

    pub fn encode_encoded_array(encoded_array: &[EncodedValue]) -> Vec<u8> {
    let mut encoded = Vec::new();
    encoded.extend(encode_uleb128(encoded_array.len() as u32));

    for value in encoded_array {
        encoded.extend(EncodedValue::encode(value));
    }

    encoded
}

pub fn decode_encoded_array(data: &[u8]) -> Result<(Vec<EncodedValue>, usize), DexError> {
    let (size, mut offset) = decode_uleb128(data);
    let size = size as usize;

    let mut values = Vec::with_capacity(size);
    for _ in 0..size {
        let (value, new_offset) = EncodedValue::decode(&data[offset..])?;
        values.push(value);
        offset += new_offset;
    }

    Ok((values, offset))
}

pub fn encode_encoded_annotation(annotation: &EncodedAnnotation) -> Vec<u8> {
    let mut encoded = Vec::new();
    encoded.extend(encode_uleb128(annotation.type_idx));
    encoded.extend(encode_uleb128(annotation.elements.len() as u32));

    for element in &annotation.elements {
        encoded.extend(encode_annotation_element(element));
    }

    encoded
}

pub fn decode_encoded_annotation(data: &[u8]) -> Result<(EncodedAnnotation, usize), DexError> {
    let (type_idx, mut offset) = decode_uleb128(data);
    let (size, new_offset) = decode_uleb128(&data[offset..]);
    offset += new_offset;

    let mut elements = Vec::with_capacity(size as usize);
    for _ in 0..size {
        let (element, new_offset) = decode_annotation_element(&data[offset..])?;
        elements.push(element);
        offset += new_offset;
    }

    Ok((EncodedAnnotation { type_idx: type_idx as u32, elements },
        offset))
}

pub fn encode_annotation_element(element: &AnnotationElement) -> Vec<u8> {
    let mut encoded = Vec::new();
    encoded.extend(encode_uleb128(element.name_idx as u32));
    encoded.extend(EncodedValue::encode(&element.value));

    encoded
}

pub fn decode_annotation_element(data: &[u8]) -> Result<(AnnotationElement, usize), DexError> {
    let (name_idx, mut offset) = decode_uleb128(data);
    let (value, new_offset) = EncodedValue::decode(&data[offset..])?;

    Ok((AnnotationElement { name_idx: name_idx as u32, value },
        offset + new_offset))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::Rng;

    #[test]
    fn test_encode_encoded_array() {
        let values = vec![
            EncodedValue::Boolean(true),
            EncodedValue::Byte(127),
            EncodedValue::Float(314159.0),
        ];

        let expected = vec![3, 63, 0, 127, 112, 224, 101, 153, 72];

        let encoded = encode_encoded_array(&values);
        assert_eq!(encoded, expected);
    }

    #[test]
    fn test_encode_encoded_annotation() {
        let annotation = EncodedAnnotation {
            type_idx: 3,
            elements: vec![
                AnnotationElement {
                    name_idx: 1,
                    value: EncodedValue::Boolean(false),
                },
                AnnotationElement {
                    name_idx: 2,
                    value: EncodedValue::Byte(127),
                },
            ],
        };

        let expected = vec![
            0x03, // type_idx
            0x02, // size
            0x01, 0x1f, // AnnotationElement { name_idx: 1, value: ValueBoolean(false) }
            0x02, 0x00, 0x7f, // AnnotationElement { name_idx: 2, value: ValueByte(127) }
        ];

        let encoded = encode_encoded_annotation(&annotation);
        assert_eq!(encoded, expected);
    }

    #[test]
    fn test_decode_encoded_array() {
        let data = vec![3, 63, 0, 127, 112, 224, 101, 153, 72];

        let expected = vec![
            EncodedValue::Boolean(true),
            EncodedValue::Byte(127),
            EncodedValue::Float(314159.0),
        ];

        let (decoded, _) = decode_encoded_array(&data).expect("Failed to decode encoded array");
        assert_eq!(decoded, expected);
    }

    #[test]
    fn test_decode_encoded_annotation() {
        let data = vec![
            0x03, // type_idx
            0x02, // size
            0x01, 0x1f | 1 << 5,       // AnnotationElement { name_idx: 1, value: ValueBoolean(true) }
            0x02, 0x00, 0x7f, // AnnotationElement { name_idx: 2, value: ValueByte(127) }
        ];

        let expected = EncodedAnnotation {
            type_idx: 3,
            elements: vec![
                AnnotationElement {
                    name_idx: 1,
                    value: EncodedValue::Boolean(true),
                },
                AnnotationElement {
                    name_idx: 2,
                    value: EncodedValue::Byte(127),
                },
            ],
        };

        let (decoded, _) = decode_encoded_annotation(&data).expect("Failed to decode encoded annotation");
        assert_eq!(decoded, expected);
    }

    #[test]
    fn test_simple_values()
    {
        let mut rng = rand::thread_rng();

        let v_byte = EncodedValue::Byte(rng.gen::<i8>());
        let (r, _) = EncodedValue::decode(v_byte.encode().as_slice()).unwrap();
        assert_eq!(v_byte, r);

        let v_short = EncodedValue::Short(rng.gen::<i16>());
        let e_short = v_short.encode();
        let (r, _) = EncodedValue::decode(e_short.as_slice()).unwrap();
        assert_eq!(v_short, r);

        let v_char = EncodedValue::Char(rng.gen::<u16>());
        let (r, _) = EncodedValue::decode(v_char.encode().as_slice()).unwrap();
        assert_eq!(v_char, r);

        let v_int = EncodedValue::Int(rng.gen::<i32>());
        let (r, _) = EncodedValue::decode(v_int.encode().as_slice()).unwrap();
        assert_eq!(v_int, r);

        let v_long = EncodedValue::Long(rng.gen::<i64>());
        let (r, _) = EncodedValue::decode(v_long.encode().as_slice()).unwrap();
        assert_eq!(v_long, r);

        let v_float = EncodedValue::Float(rng.gen::<f32>());
        let (r, _) = EncodedValue::decode(v_float.encode().as_slice()).unwrap();
        assert_eq!(v_float, r);

        let v_double = EncodedValue::Double(rng.gen::<f64>());
        let (r, _) = EncodedValue::decode(v_double.encode().as_slice()).unwrap();
        assert_eq!(v_double, r);

        let v_method_type = EncodedValue::MethodType(rng.gen::<u32>());
        let (r, _) = EncodedValue::decode(v_method_type.encode().as_slice()).unwrap();
        assert_eq!(v_method_type, r);

        let v_method_handle = EncodedValue::MethodHandle(rng.gen::<u32>());
        let (r, _) = EncodedValue::decode(v_method_handle.encode().as_slice()).unwrap();
        assert_eq!(v_method_handle, r);

        let v_string = EncodedValue::String(rng.gen::<u32>());
        let (r, _) = EncodedValue::decode(v_string.encode().as_slice()).unwrap();
        assert_eq!(v_string, r);

        let v_type = EncodedValue::Type(rng.gen::<u32>());
        let (r, _) = EncodedValue::decode(v_type.encode().as_slice()).unwrap();
        assert_eq!(v_type, r);

        let v_field = EncodedValue::Field(rng.gen::<u32>());
        let (r, _) = EncodedValue::decode(v_byte.encode().as_slice()).unwrap();
        assert_eq!(v_byte, r);

        let v_method = EncodedValue::Method(rng.gen::<u32>());
        let (r, _) = EncodedValue::decode(v_method.encode().as_slice()).unwrap();
        assert_eq!(v_method, r);

        let v_enum = EncodedValue::Enum(rng.gen::<u32>());
        let (r, _) = EncodedValue::decode(v_enum.encode().as_slice()).unwrap();
        assert_eq!(v_enum, r);

        let v_null = EncodedValue::Null;
        let (r, _) = EncodedValue::decode(v_null.encode().as_slice()).unwrap();
        assert_eq!(v_null, r);

        let v_boolean = EncodedValue::Boolean(rng.gen::<bool>());
        let (r, _) = EncodedValue::decode(v_boolean.encode().as_slice()).unwrap();
        assert_eq!(v_boolean, r);
    }

    #[test]
    fn test_byte_size_i16()
    {
        assert_eq!(byte_size_i16(0), 1);
        assert_eq!(byte_size_i16(1), 1);
        assert_eq!(byte_size_i16(1000), 2);
        assert_eq!(byte_size_i16(-56), 2);
    }

    #[test]
    fn test_byte_size_u16()
    {
        assert_eq!(byte_size_u16(0), 1);
        assert_eq!(byte_size_u16(26), 1);
        assert_eq!(byte_size_u16(1000), 2);
        assert_eq!(byte_size_u16(256), 2);
    }

    #[test]
    fn test_byte_size_i32()
    {
        assert_eq!(byte_size_i32(-1), 4);
        assert_eq!(byte_size_i32(0), 1);
        assert_eq!(byte_size_i32(26), 1);
        assert_eq!(byte_size_i32(1000), 2);
        assert_eq!(byte_size_i32(500), 2);
        assert_eq!(byte_size_i32(65537), 3);
    }

    #[test]
    fn test_byte_size_u32()
    {
        assert_eq!(byte_size_i32(0), 1);
        assert_eq!(byte_size_i32(26), 1);
        assert_eq!(byte_size_i32(1000), 2);
        assert_eq!(byte_size_i32(500), 2);
        assert_eq!(byte_size_i32(65537), 3);
    }

    #[test]
    fn test_byte_size_i64()
    {
        assert_eq!(byte_size_i32(0), 1);
        assert_eq!(byte_size_i32(26), 1);
        assert_eq!(byte_size_i32(1000), 2);
        assert_eq!(byte_size_i32(500), 2);
        assert_eq!(byte_size_i32(65537), 3);
    }

}

