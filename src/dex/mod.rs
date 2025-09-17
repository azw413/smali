#[macro_use]
pub mod error;

pub(crate) mod dex_file;
pub use dex_file::DexFile;
mod leb;
mod annotations;
mod instructions;
mod encoded_values;
mod opcode_format;
mod opcodes;

use crate::dex::error::DexError;
use crate::dex::leb::{decode_uleb128, decode_uleb128p1, encode_uleb128, encode_uleb128p1};

// Basic type reading and writing
pub(crate) fn read_u1(bytes: &[u8], ix: &mut usize) -> Result<u8, DexError>
{
    if bytes.len() < *ix + 1
    {
        fail!("Unexpected end of stream reading u1 at index {}", *ix);
    }
    let result = bytes[*ix];
    *ix += 1;
    Ok(result)
}

pub(crate) fn read_u2(bytes: &[u8], ix: &mut usize) -> Result<u16, DexError>
{
    if bytes.len() < *ix + 2
    {
        fail!("Unexpected end of stream reading u2 at index {}", *ix);
    }
    let result = ((bytes[*ix + 1] as u16) << 8) | (bytes[*ix] as u16);
    *ix += 2;
    Ok(result)
}

pub(crate) fn read_u4(bytes: &[u8], ix: &mut usize) -> Result<u32, DexError>
{
    if bytes.len() < *ix + 4
    {
        fail!("Unexpected end of stream reading u4 at index {}", *ix);
    }
    let result =
        ((bytes[*ix + 3] as u32) << 24) | ((bytes[*ix + 2] as u32) << 16) | ((bytes[*ix + 1] as u32) << 8) | (bytes[*ix] as u32);
    *ix += 4;
    Ok(result)
}

pub(crate) fn read_uleb128(bytes: &[u8], ix: &mut usize) -> Result<u32, DexError>
{
    let (val, size) = decode_uleb128(&bytes[*ix..]);
    *ix += size;
    Ok(val)
}

pub(crate) fn read_uleb128p1(bytes: &[u8], ix: &mut usize) -> Result<i32, DexError>
{
    let (val, size) = decode_uleb128p1(&bytes[*ix..]);
    *ix += size;
    Ok(val)
}

pub(crate) fn read_x(bytes: &[u8], ix: &mut usize, length: usize) -> Result<Vec<u8>, DexError>
{
    if bytes.len() - *ix >= length
    {
        let mut v = Vec::with_capacity(length + 1);
        v.extend_from_slice(&bytes[*ix..*ix + length]);
        *ix += length;
        Ok(v)
    }
    else
    {
        Err(DexError::new(
            "buffer too short for array read",
        ))
    }
}

pub(crate) fn write_u1(buffer: &mut Vec<u8>, val: u8) -> usize
{
    buffer.push(val);
    1
}

pub(crate) fn write_u2(buffer: &mut Vec<u8>, val: u16) -> usize
{
    buffer.push(val as u8);
    buffer.push((val >> 8) as u8);
    2
}

pub(crate) fn write_u4(buffer: &mut Vec<u8>, val: u32) -> usize
{
    for i in 0..4
    {
        buffer.push((val >> (i * 8)) as u8);
    }
    4
}

pub(crate) fn write_uleb128(buffer: &mut Vec<u8>, val: u32) -> usize
{
    let encoded = encode_uleb128(val);
    let c = encoded.len();
    buffer.extend(encoded);
    c
}

pub(crate) fn write_uleb128p1(buffer: &mut Vec<u8>, val: i32) -> usize
{
    let encoded = encode_uleb128p1(val);
    let c = encoded.len();
    buffer.extend(encoded);
    c
}

pub(crate) fn write_x(buffer: &mut Vec<u8>, val: &[u8]) -> usize
{
    let len = val.len();
    buffer.extend(val);
    len
}

// Local helpers for signed LEB128 used by encoded_catch_handler.size
fn read_sleb128(bytes: &[u8], ix: &mut usize) -> Result<i32, DexError> {
    let mut result: i32 = 0;
    let mut shift = 0;
    let mut byte: u8;
    loop {
        byte = read_u1(bytes, ix)?;
        result |= ((byte & 0x7f) as i32) << shift;
        shift += 7;
        if (byte & 0x80) == 0 { break; }
    }
    // sign-extend if sign bit set and we didn't consume full i32 width
    if shift < 32 && (byte & 0x40) != 0 {
        result |= (!0) << shift;
    }
    Ok(result)
}

fn write_sleb128(bytes: &mut Vec<u8>, mut value: i32) -> usize {
    let mut written = 0;
    loop {
        let mut byte = (value as u8) & 0x7f;
        let sign = (byte & 0x40) != 0;
        value >>= 7; // arithmetic shift
        let done = (value == 0 && !sign) || (value == -1 && sign);
        if !done { byte |= 0x80; }
        written += write_u1(bytes, byte);
        if done { break; }
    }
    written
}
