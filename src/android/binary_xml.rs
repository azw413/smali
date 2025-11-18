use crate::android::zip::ApkEntry;
use quick_xml::events::{attributes::AttrError, BytesDecl, BytesEnd, BytesStart, BytesText, Event};
use quick_xml::{Error as QuickXmlError, Reader, Writer};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::str;

const RES_XML_TYPE: u16 = 0x0003;
const RES_STRING_POOL_TYPE: u16 = 0x0001;
const RES_XML_RESOURCE_MAP_TYPE: u16 = 0x0180;
const RES_XML_START_NAMESPACE_TYPE: u16 = 0x0100;
const RES_XML_END_NAMESPACE_TYPE: u16 = 0x0101;
const RES_XML_START_ELEMENT_TYPE: u16 = 0x0102;
const RES_XML_END_ELEMENT_TYPE: u16 = 0x0103;
const RES_XML_CDATA_TYPE: u16 = 0x0104;

const NO_ENTRY_INDEX: u32 = 0xFFFF_FFFF;
const STRING_FLAG_UTF8: u32 = 0x0000_0100;

const ANDROID_NAMESPACE_URI: &str = "http://schemas.android.com/apk/res/android";

const TYPE_NULL: u8 = 0x00;
const TYPE_REFERENCE: u8 = 0x01;
const TYPE_STRING: u8 = 0x03;
const TYPE_FLOAT: u8 = 0x04;
const TYPE_INT_DEC: u8 = 0x10;
const TYPE_INT_HEX: u8 = 0x11;
const TYPE_INT_BOOLEAN: u8 = 0x12;

struct ChunkHeader {
    chunk_type: u16,
    header_size: u16,
    chunk_size: u32,
    start: usize,
}

impl ChunkHeader {
    fn end(&self) -> usize {
        self.start + self.chunk_size as usize
    }
}

struct BinaryReader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> BinaryReader<'a> {
    fn new(data: &'a [u8]) -> Self {
        BinaryReader { data, pos: 0 }
    }

    fn position(&self) -> usize {
        self.pos
    }

    fn remaining(&self) -> usize {
        self.data.len().saturating_sub(self.pos)
    }

    fn read_u8(&mut self) -> BinaryXmlResult<u8> {
        if self.pos + 1 > self.data.len() {
            return Err(BinaryXmlError::MalformedDocument(
                "Unexpected end of binary XML".to_string(),
            ));
        }
        let value = self.data[self.pos];
        self.pos += 1;
        Ok(value)
    }

    fn read_u16(&mut self) -> BinaryXmlResult<u16> {
        if self.pos + 2 > self.data.len() {
            return Err(BinaryXmlError::MalformedDocument(
                "Unexpected end of binary XML".to_string(),
            ));
        }
        let value = u16::from_le_bytes([self.data[self.pos], self.data[self.pos + 1]]);
        self.pos += 2;
        Ok(value)
    }

    fn read_u32(&mut self) -> BinaryXmlResult<u32> {
        if self.pos + 4 > self.data.len() {
            return Err(BinaryXmlError::MalformedDocument(
                "Unexpected end of binary XML".to_string(),
            ));
        }
        let value = u32::from_le_bytes([
            self.data[self.pos],
            self.data[self.pos + 1],
            self.data[self.pos + 2],
            self.data[self.pos + 3],
        ]);
        self.pos += 4;
        Ok(value)
    }

    fn seek(&mut self, offset: usize) -> BinaryXmlResult<()> {
        if offset > self.data.len() {
            return Err(BinaryXmlError::MalformedDocument(
                "Attempted to seek past end of document".to_string(),
            ));
        }
        self.pos = offset;
        Ok(())
    }
}

struct StringPool {
    strings: Vec<String>,
}

impl StringPool {
    fn parse(reader: &mut BinaryReader<'_>, header: &ChunkHeader) -> BinaryXmlResult<Self> {
        let string_count = reader.read_u32()? as usize;
        let style_count = reader.read_u32()? as usize;
        let flags = reader.read_u32()?;
        let strings_start = reader.read_u32()? as usize;
        let styles_start = reader.read_u32()? as usize;

        let is_utf8 = (flags & STRING_FLAG_UTF8) != 0;

        let mut string_offsets = Vec::with_capacity(string_count);
        for _ in 0..string_count {
            string_offsets.push(reader.read_u32()? as usize);
        }

        for _ in 0..style_count {
            reader.read_u32()?; // skip style offsets
        }

        let strings_base = header.start + strings_start;
        let chunk_end = header.end();

        let mut strings = Vec::with_capacity(string_count);
        for offset in string_offsets {
            let absolute = strings_base + offset;
            let text = if is_utf8 {
                read_utf8_string(reader.data, absolute, chunk_end)?
            } else {
                read_utf16_string(reader.data, absolute, chunk_end)?
            };
            strings.push(text);
        }

        if styles_start != 0 {
            // Skip style data entirely by seeking to chunk end.
            reader.seek(chunk_end)?;
        }

        Ok(StringPool { strings })
    }

    fn get(&self, idx: u32) -> Option<&str> {
        if idx == NO_ENTRY_INDEX {
            return None;
        }
        self.strings.get(idx as usize).map(|s| s.as_str())
    }
}

#[derive(Clone, Debug)]
struct NamespaceFrame {
    prefix: Option<String>,
    uri: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct NamespaceDecl {
    prefix: String,
    uri: String,
}

#[derive(Clone, Debug)]
struct PendingAttribute {
    prefix: Option<String>,
    local_name: String,
    value: String,
}

struct StringPoolBuilder {
    strings: Vec<String>,
    indices: BTreeMap<String, u32>,
}

impl StringPoolBuilder {
    fn new() -> Self {
        StringPoolBuilder {
            strings: Vec::new(),
            indices: BTreeMap::new(),
        }
    }

    fn intern(&mut self, value: impl AsRef<str>) -> u32 {
        let value = value.as_ref();
        if let Some(&idx) = self.indices.get(value) {
            return idx;
        }
        let idx = self.strings.len() as u32;
        let owned = value.to_string();
        self.strings.push(owned.clone());
        self.indices.insert(owned, idx);
        idx
    }

    fn intern_cow(&mut self, value: Cow<'_, str>) -> u32 {
        if let Some(&idx) = self.indices.get(value.as_ref()) {
            return idx;
        }
        let idx = self.strings.len() as u32;
        let owned = value.into_owned();
        self.strings.push(owned.clone());
        self.indices.insert(owned, idx);
        idx
    }

    fn index_of(&self, value: &str) -> Option<u32> {
        self.indices.get(value).copied()
    }

    fn to_chunk(&self) -> Vec<u8> {
        let string_count = self.strings.len() as u32;
        let header_size = 28u16;
        let strings_start = header_size as u32 + string_count * 4;
        let mut string_data = Vec::new();
        let mut offsets = Vec::with_capacity(self.strings.len());
        for s in &self.strings {
            offsets.push(string_data.len() as u32);
            write_utf16_string(&mut string_data, s);
        }
        align_to_four(&mut string_data);

        let mut chunk = Vec::new();
        write_u16(&mut chunk, RES_STRING_POOL_TYPE);
        write_u16(&mut chunk, header_size);
        write_u32(&mut chunk, 0); // chunk size placeholder
        write_u32(&mut chunk, string_count);
        write_u32(&mut chunk, 0); // style count
        write_u32(&mut chunk, 0); // flags (UTF-16)
        write_u32(&mut chunk, strings_start);
        write_u32(&mut chunk, 0); // stylesStart
        for offset in offsets {
            write_u32(&mut chunk, offset);
        }
        chunk.extend_from_slice(&string_data);
        align_to_four(&mut chunk);
        let chunk_size = chunk.len() as u32;
        chunk[4..8].copy_from_slice(&chunk_size.to_le_bytes());
        chunk
    }
}

fn read_utf8_string(data: &[u8], offset: usize, limit: usize) -> BinaryXmlResult<String> {
    let mut cursor = offset;
    if cursor >= limit {
        return Err(BinaryXmlError::MalformedDocument(
            "String offset exceeds chunk bounds".to_string(),
        ));
    }
    let (char_len, len_bytes) = read_utf8_length(data, cursor, limit)?;
    cursor += len_bytes;
    let (byte_len, byte_len_size) = read_utf8_length(data, cursor, limit)?;
    cursor += byte_len_size;
    if cursor + byte_len > limit {
        return Err(BinaryXmlError::MalformedDocument(
            "UTF-8 string exceeds chunk bounds".to_string(),
        ));
    }
    let slice = &data[cursor..cursor + byte_len];
    let text = std::str::from_utf8(slice)
        .map_err(|err| BinaryXmlError::MalformedDocument(err.to_string()))?;
    cursor += byte_len;
    if cursor >= limit {
        return Err(BinaryXmlError::MalformedDocument(
            "Missing UTF-8 terminator".to_string(),
        ));
    }
    // `char_len` is unused but parsed for completeness.
    let _ = char_len;
    Ok(text.to_string())
}

fn read_utf16_string(data: &[u8], offset: usize, limit: usize) -> BinaryXmlResult<String> {
    let mut cursor = offset;
    let (char_count, header_bytes) = read_utf16_length(data, cursor, limit)?;
    cursor += header_bytes;
    let byte_len = char_count * 2;
    if cursor + byte_len > limit {
        return Err(BinaryXmlError::MalformedDocument(
            "UTF-16 string exceeds chunk bounds".to_string(),
        ));
    }
    let mut units = Vec::with_capacity(char_count);
    for chunk in data[cursor..cursor + byte_len].chunks_exact(2) {
        units.push(u16::from_le_bytes([chunk[0], chunk[1]]));
    }
    cursor += byte_len;
    if cursor + 2 > limit {
        return Err(BinaryXmlError::MalformedDocument(
            "Missing UTF-16 terminator".to_string(),
        ));
    }
    let terminator = u16::from_le_bytes([data[cursor], data[cursor + 1]]);
    if terminator != 0 {
        return Err(BinaryXmlError::MalformedDocument(
            "UTF-16 string missing terminator".to_string(),
        ));
    }
    let text = String::from_utf16(&units)
        .map_err(|err| BinaryXmlError::MalformedDocument(err.to_string()))?;
    Ok(text)
}

fn read_utf8_length(data: &[u8], offset: usize, limit: usize) -> BinaryXmlResult<(usize, usize)> {
    if offset >= limit {
        return Err(BinaryXmlError::MalformedDocument(
            "Invalid UTF-8 length offset".to_string(),
        ));
    }
    let first = data[offset];
    if (first & 0x80) == 0 {
        Ok((first as usize, 1))
    } else {
        if offset + 1 >= limit {
            return Err(BinaryXmlError::MalformedDocument(
                "Truncated UTF-8 length".to_string(),
            ));
        }
        let second = data[offset + 1];
        let length = (((first & 0x7F) as usize) << 8) | second as usize;
        Ok((length, 2))
    }
}

fn read_utf16_length(data: &[u8], offset: usize, limit: usize) -> BinaryXmlResult<(usize, usize)> {
    if offset + 2 > limit {
        return Err(BinaryXmlError::MalformedDocument(
            "Invalid UTF-16 length offset".to_string(),
        ));
    }
    let first = u16::from_le_bytes([data[offset], data[offset + 1]]);
    if (first & 0x8000) == 0 {
        Ok((first as usize, 2))
    } else {
        if offset + 4 > limit {
            return Err(BinaryXmlError::MalformedDocument(
                "Truncated UTF-16 length".to_string(),
            ));
        }
        let second = u16::from_le_bytes([data[offset + 2], data[offset + 3]]);
        let length = (((first & 0x7FFF) as usize) << 16) | second as usize;
        Ok((length, 4))
    }
}

fn read_chunk_header(reader: &mut BinaryReader<'_>) -> BinaryXmlResult<ChunkHeader> {
    let start = reader.position();
    if reader.remaining() < 8 {
        return Err(BinaryXmlError::MalformedDocument(
            "Truncated binary XML chunk header".to_string(),
        ));
    }
    let chunk_type = reader.read_u16()?;
    let header_size = reader.read_u16()?;
    let chunk_size = reader.read_u32()?;
    if chunk_size < header_size as u32 {
        return Err(BinaryXmlError::MalformedDocument(
            "Invalid chunk sizing in binary XML".to_string(),
        ));
    }
    let end = start
        .checked_add(chunk_size as usize)
        .ok_or_else(|| {
            BinaryXmlError::MalformedDocument("Chunk size overflow".to_string())
        })?;
    if end > reader.data.len() {
        return Err(BinaryXmlError::MalformedDocument(
            "Chunk extends past end of document".to_string(),
        ));
    }
    Ok(ChunkHeader {
        chunk_type,
        header_size,
        chunk_size,
        start,
    })
}

fn resolve_prefix(namespaces: &[NamespaceFrame], uri: Option<&str>) -> Option<String> {
    uri.and_then(|target| {
        namespaces
            .iter()
            .rev()
            .find(|frame| frame.uri.as_deref() == Some(target))
            .and_then(|frame| frame.prefix.clone())
    })
}

fn decode_value(
    strings: &StringPool,
    raw_value_idx: u32,
    data_type: u8,
    data: u32,
) -> BinaryXmlResult<ManifestValue> {
    if let Some(raw) = strings.get(raw_value_idx) {
        return Ok(ManifestValue::String(raw.to_string()));
    }

    match data_type {
        TYPE_NULL => Ok(ManifestValue::String(String::new())),
        TYPE_STRING => strings
            .get(data)
            .map(|s| ManifestValue::String(s.to_string()))
            .ok_or_else(|| {
                BinaryXmlError::MalformedDocument(
                    "String value references missing pool entry".to_string(),
                )
            }),
        TYPE_REFERENCE => Ok(ManifestValue::Reference(data)),
        TYPE_INT_BOOLEAN => Ok(ManifestValue::Boolean(data != 0)),
        TYPE_INT_DEC => Ok(ManifestValue::Integer(i64::from(data as i32))),
        TYPE_INT_HEX => Ok(ManifestValue::Hex(data)),
        TYPE_FLOAT => {
            let value = f32::from_bits(data);
            Ok(ManifestValue::String(value.to_string()))
        }
        _ => Ok(ManifestValue::Hex(data)),
    }
}

fn write_u16(buf: &mut Vec<u8>, value: u16) {
    buf.extend_from_slice(&value.to_le_bytes());
}

fn write_u32(buf: &mut Vec<u8>, value: u32) {
    buf.extend_from_slice(&value.to_le_bytes());
}

fn write_u8(buf: &mut Vec<u8>, value: u8) {
    buf.push(value);
}

fn write_utf16_string(buf: &mut Vec<u8>, text: &str) {
    let units: Vec<u16> = text.encode_utf16().collect();
    let len = units.len();
    if len < 0x8000 {
        write_u16(buf, len as u16);
    } else {
        let first = 0x8000 | ((len >> 16) as u16 & 0x7FFF);
        let second = (len & 0xFFFF) as u16;
        write_u16(buf, first);
        write_u16(buf, second);
    }
    for unit in units {
        write_u16(buf, unit);
    }
    write_u16(buf, 0);
}

fn align_to_four(buf: &mut Vec<u8>) {
    while buf.len() % 4 != 0 {
        buf.push(0);
    }
}

fn raw_value_text(value: &ManifestValue) -> Option<Cow<'_, str>> {
    match value {
        ManifestValue::String(text) => Some(Cow::Borrowed(text)),
        ManifestValue::Boolean(flag) => {
            if *flag {
                Some(Cow::Borrowed("true"))
            } else {
                Some(Cow::Borrowed("false"))
            }
        }
        ManifestValue::Integer(num) => Some(Cow::Owned(num.to_string())),
        ManifestValue::Hex(value) => Some(Cow::Owned(format!("0x{value:x}"))),
        ManifestValue::Reference(id) => Some(Cow::Owned(format!("@0x{id:08x}"))),
    }
}

fn manifest_value_to_text(value: &ManifestValue) -> String {
    raw_value_text(value)
        .unwrap_or_else(|| Cow::Borrowed(""))
        .into_owned()
}

fn parse_manifest_value(text: &str) -> ManifestValue {
    let trimmed = text.trim();
    if trimmed.eq_ignore_ascii_case("true") {
        ManifestValue::Boolean(true)
    } else if trimmed.eq_ignore_ascii_case("false") {
        ManifestValue::Boolean(false)
    } else if let Some(hex) = trimmed.strip_prefix("@0x").or_else(|| trimmed.strip_prefix("@0X")) {
        if let Ok(value) = u32::from_str_radix(hex, 16) {
            ManifestValue::Reference(value)
        } else {
            ManifestValue::String(trimmed.to_string())
        }
    } else if let Some(decimal) = trimmed.strip_prefix('@') {
        if decimal.chars().all(|c| c.is_ascii_digit()) {
            if let Ok(value) = decimal.parse::<u32>() {
                ManifestValue::Reference(value)
            } else {
                ManifestValue::String(trimmed.to_string())
            }
        } else {
            ManifestValue::String(trimmed.to_string())
        }
    } else if let Some(value) = trimmed.strip_prefix("0x").or_else(|| trimmed.strip_prefix("0X")) {
        if let Ok(number) = u32::from_str_radix(value, 16) {
            ManifestValue::Hex(number)
        } else {
            ManifestValue::String(trimmed.to_string())
        }
    } else if let Ok(number) = trimmed.parse::<i64>() {
        ManifestValue::Integer(number)
    } else {
        ManifestValue::String(trimmed.to_string())
    }
}

fn begin_chunk(buf: &mut Vec<u8>, chunk_type: u16, header_size: u16) -> usize {
    let start = buf.len();
    write_u16(buf, chunk_type);
    write_u16(buf, header_size);
    write_u32(buf, 0); // placeholder for chunk size
    start
}

fn finalize_chunk(buf: &mut Vec<u8>, chunk_start: usize) {
    align_to_four(buf);
    let size = (buf.len() - chunk_start) as u32;
    let size_bytes = size.to_le_bytes();
    buf[chunk_start + 4..chunk_start + 8].copy_from_slice(&size_bytes);
}


/// Result alias for binary XML operations.
pub type BinaryXmlResult<T> = Result<T, BinaryXmlError>;

/// Errors surfaced by the binary XML helpers.
#[derive(Debug)]
pub enum BinaryXmlError {
    /// The document is missing the expected structure.
    MalformedDocument(String),
    /// The current feature is not implemented yet.
    Unimplemented(&'static str),
    /// Text XML parsing/generation failure.
    Xml(String),
}

impl std::fmt::Display for BinaryXmlError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryXmlError::MalformedDocument(msg) => write!(f, "Malformed manifest: {msg}"),
            BinaryXmlError::Unimplemented(feature) => {
                write!(f, "Binary XML feature '{feature}' is not implemented yet")
            }
            BinaryXmlError::Xml(msg) => write!(f, "XML error: {msg}"),
        }
    }
}

impl std::error::Error for BinaryXmlError {}

impl From<QuickXmlError> for BinaryXmlError {
    fn from(value: QuickXmlError) -> Self {
        BinaryXmlError::Xml(value.to_string())
    }
}

impl From<AttrError> for BinaryXmlError {
    fn from(value: AttrError) -> Self {
        BinaryXmlError::Xml(value.to_string())
    }
}

/// Typed attribute values inside the manifest DOM representation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ManifestValue {
    String(String),
    Boolean(bool),
    Integer(i64),
    Hex(u32),
    Reference(u32),
}

impl ManifestValue {
    pub fn as_str(&self) -> Option<&str> {
        match self {
            ManifestValue::String(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_reference_id(&self) -> Option<u32> {
        match self {
            ManifestValue::Reference(id) => Some(*id),
            _ => None,
        }
    }
}

impl From<String> for ManifestValue {
    fn from(value: String) -> Self {
        ManifestValue::String(value)
    }
}

impl From<&str> for ManifestValue {
    fn from(value: &str) -> Self {
        ManifestValue::String(value.to_owned())
    }
}

impl From<bool> for ManifestValue {
    fn from(value: bool) -> Self {
        ManifestValue::Boolean(value)
    }
}

impl From<i64> for ManifestValue {
    fn from(value: i64) -> Self {
        ManifestValue::Integer(value)
    }
}

impl From<u32> for ManifestValue {
    fn from(value: u32) -> Self {
        ManifestValue::Hex(value)
    }
}

/// A single attribute attached to a manifest element.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ManifestAttribute {
    pub namespace_prefix: Option<String>,
    pub namespace_uri: Option<String>,
    pub resource_id: Option<u32>,
    pub name: String,
    pub value: ManifestValue,
}

impl ManifestAttribute {
    pub fn new(name: impl Into<String>, value: impl Into<ManifestValue>) -> Self {
        ManifestAttribute {
            namespace_prefix: None,
            namespace_uri: None,
            resource_id: None,
            name: name.into(),
            value: value.into(),
        }
    }

    pub fn with_namespace(
        namespace: impl Into<String>,
        name: impl Into<String>,
        value: impl Into<ManifestValue>,
    ) -> Self {
        ManifestAttribute {
            namespace_prefix: Some(namespace.into()),
            namespace_uri: None,
            resource_id: None,
            name: name.into(),
            value: value.into(),
        }
    }

    pub fn with_namespace_and_uri(
        prefix: impl Into<String>,
        uri: impl Into<String>,
        name: impl Into<String>,
        value: impl Into<ManifestValue>,
    ) -> Self {
        ManifestAttribute {
            namespace_prefix: Some(prefix.into()),
            namespace_uri: Some(uri.into()),
            resource_id: None,
            name: name.into(),
            value: value.into(),
        }
    }
}

/// DOM-style element node for the binary XML tree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ManifestElement {
    pub namespace_prefix: Option<String>,
    pub namespace_uri: Option<String>,
    pub tag: String,
    pub attributes: Vec<ManifestAttribute>,
    pub children: Vec<ManifestElement>,
    pub text: Option<String>,
}

impl ManifestElement {
    pub fn new(tag: impl Into<String>) -> Self {
        ManifestElement {
            namespace_prefix: None,
            namespace_uri: None,
            tag: tag.into(),
            attributes: Vec::new(),
            children: Vec::new(),
            text: None,
        }
    }

    pub fn with_namespace(tag: impl Into<String>, namespace: impl Into<String>) -> Self {
        ManifestElement {
            namespace_prefix: Some(namespace.into()),
            namespace_uri: None,
            ..ManifestElement::new(tag)
        }
    }

    pub fn with_namespace_and_uri(
        tag: impl Into<String>,
        prefix: impl Into<String>,
        uri: impl Into<String>,
    ) -> Self {
        ManifestElement {
            namespace_prefix: Some(prefix.into()),
            namespace_uri: Some(uri.into()),
            ..ManifestElement::new(tag)
        }
    }

    pub fn attribute_value(&self, name: &str) -> Option<&ManifestValue> {
        let (namespace, local) = split_attribute_query(name);
        self.attributes
            .iter()
            .find(|attr| attr.name == local && attr.namespace_prefix.as_deref() == namespace)
            .map(|attr| &attr.value)
    }

    pub fn attribute_value_mut(&mut self, name: &str) -> Option<&mut ManifestValue> {
        let (namespace, local) = split_attribute_query(name);
        self.attributes
            .iter_mut()
            .find(|attr| attr.name == local && attr.namespace_prefix.as_deref() == namespace)
            .map(|attr| &mut attr.value)
    }

    pub fn set_attribute(&mut self, attribute: ManifestAttribute) {
        if let Some(existing) = self
            .attributes
            .iter_mut()
            .find(|attr| {
                attr.name == attribute.name
                    && attr.namespace_prefix == attribute.namespace_prefix
            })
        {
            *existing = attribute;
        } else {
            self.attributes.push(attribute);
        }
    }

    pub fn remove_attribute(
        &mut self,
        name: &str,
        namespace: Option<&str>,
    ) -> Option<ManifestAttribute> {
        if let Some(idx) = self.attributes.iter().position(|attr| {
            attr.name == name && attr.namespace_prefix.as_deref() == namespace
        }) {
            Some(self.attributes.remove(idx))
        } else {
            None
        }
    }

    pub fn append_child(&mut self, child: ManifestElement) {
        self.children.push(child);
    }

    pub fn find_child(&self, tag: &str) -> Option<&ManifestElement> {
        self.children.iter().find(|child| child.tag == tag)
    }

    pub fn find_child_mut(&mut self, tag: &str) -> Option<&mut ManifestElement> {
        self.children.iter_mut().find(|child| child.tag == tag)
    }

    pub fn remove_children(&mut self, tag: &str) {
        self.children.retain(|child| child.tag != tag);
    }
}

fn split_attribute_query(name: &str) -> (Option<&str>, &str) {
    if let Some((ns, local)) = name.split_once(':') {
        (Some(ns), local)
    } else {
        (None, name)
    }
}

fn qualified_name(prefix: Option<&str>, local: &str) -> String {
    if let Some(prefix) = prefix {
        format!("{prefix}:{local}")
    } else {
        local.to_string()
    }
}

fn collect_namespace_declarations(root: &ManifestElement) -> Vec<NamespaceDecl> {
    let mut set = BTreeSet::new();
    gather_namespace_decls(root, &mut set);
    set.into_iter().collect()
}

fn gather_namespace_decls(element: &ManifestElement, set: &mut BTreeSet<NamespaceDecl>) {
    if let (Some(prefix), Some(uri)) = (&element.namespace_prefix, &element.namespace_uri) {
        set.insert(NamespaceDecl {
            prefix: prefix.clone(),
            uri: uri.clone(),
        });
    }
    for attr in &element.attributes {
        if let (Some(prefix), Some(uri)) = (&attr.namespace_prefix, &attr.namespace_uri) {
            set.insert(NamespaceDecl {
                prefix: prefix.clone(),
                uri: uri.clone(),
            });
        }
    }
    for child in &element.children {
        gather_namespace_decls(child, set);
    }
}

fn collect_element_strings(element: &ManifestElement, pool: &mut StringPoolBuilder) {
    pool.intern(&element.tag);
    if let Some(prefix) = &element.namespace_prefix {
        pool.intern(prefix);
    }
    if let Some(uri) = &element.namespace_uri {
        pool.intern(uri);
    }
    if let Some(text) = &element.text {
        pool.intern(text);
    }
    for attr in &element.attributes {
        pool.intern(&attr.name);
        if let Some(prefix) = &attr.namespace_prefix {
            pool.intern(prefix);
        }
        if let Some(uri) = &attr.namespace_uri {
            pool.intern(uri);
        }
        if let ManifestValue::String(value) = &attr.value {
            pool.intern(value);
        }
        if let Some(raw) = raw_value_text(&attr.value) {
            pool.intern_cow(raw);
        }
    }
    for child in &element.children {
        collect_element_strings(child, pool);
    }
}

fn attach_element(
    stack: &mut Vec<ManifestElement>,
    root: &mut Option<ManifestElement>,
    element: ManifestElement,
) -> BinaryXmlResult<()> {
    if let Some(parent) = stack.last_mut() {
        parent.children.push(element);
    } else if root.is_none() {
        *root = Some(element);
    } else {
        return Err(BinaryXmlError::MalformedDocument(
            "Multiple root elements in manifest".to_string(),
        ));
    }
    Ok(())
}

fn parse_text_content(raw: &str) -> Option<String> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_string())
    }
}

fn lookup_namespace_uri(
    stack: &[BTreeMap<String, String>],
    prefix: Option<&str>,
) -> Option<String> {
    let key = prefix.unwrap_or("");
    for frame in stack.iter().rev() {
        if let Some(uri) = frame.get(key) {
            return Some(uri.clone());
        }
    }
    None
}

fn split_qname_owned(name: &str) -> (Option<String>, String) {
    if let Some(idx) = name.find(':') {
        (
            Some(name[..idx].to_string()),
            name[idx + 1..].to_string(),
        )
    } else {
        (None, name.to_string())
    }
}

fn extract_attributes(
    start: &BytesStart<'_>,
) -> BinaryXmlResult<(BTreeMap<String, String>, Vec<PendingAttribute>)> {
    let mut namespaces = BTreeMap::new();
    let mut attrs = Vec::new();
    for attr in start.attributes().with_checks(false) {
        let attr = attr.map_err(BinaryXmlError::from)?;
        let key = std::str::from_utf8(attr.key.as_ref())
            .map_err(|err| BinaryXmlError::MalformedDocument(err.to_string()))?;
        let value = attr
            .unescape_value()
            .map_err(BinaryXmlError::from)?
            .into_owned();
        if key == "xmlns" {
            namespaces.insert(String::new(), value);
            continue;
        } else if let Some(rest) = key.strip_prefix("xmlns:") {
            namespaces.insert(rest.to_string(), value);
            continue;
        }
        let (prefix, local_name) = split_qname_owned(key);
        attrs.push(PendingAttribute {
            prefix,
            local_name,
            value,
        });
    }
    Ok((namespaces, attrs))
}

fn build_element_from_start(
    start: &BytesStart<'_>,
    ns_stack: &[BTreeMap<String, String>],
    attrs: Vec<PendingAttribute>,
) -> BinaryXmlResult<ManifestElement> {
    let name_ref = start.name();
    let raw_name = std::str::from_utf8(name_ref.as_ref())
        .map_err(|err| BinaryXmlError::MalformedDocument(err.to_string()))?;
    let (prefix, local) = split_qname_owned(raw_name);
    let namespace_uri = lookup_namespace_uri(ns_stack, prefix.as_deref());
    let mut element = ManifestElement::new(local);
    element.namespace_prefix = prefix;
    element.namespace_uri = namespace_uri;
    element.attributes = attrs
        .into_iter()
        .map(|attr| build_manifest_attribute(attr, ns_stack))
        .collect();
    Ok(element)
}

fn build_manifest_attribute(
    attr: PendingAttribute,
    ns_stack: &[BTreeMap<String, String>],
) -> ManifestAttribute {
    let namespace_uri = attr
        .prefix
        .as_deref()
        .and_then(|prefix| lookup_namespace_uri(ns_stack, Some(prefix)));
    ManifestAttribute {
        namespace_prefix: attr.prefix,
        namespace_uri,
        resource_id: None,
        name: attr.local_name,
        value: parse_manifest_value(&attr.value),
    }
}

fn write_namespace_chunk(
    buf: &mut Vec<u8>,
    pool: &StringPoolBuilder,
    decl: &NamespaceDecl,
    is_start: bool,
) -> BinaryXmlResult<()> {
    let chunk_type = if is_start {
        RES_XML_START_NAMESPACE_TYPE
    } else {
        RES_XML_END_NAMESPACE_TYPE
    };
    let chunk_start = begin_chunk(buf, chunk_type, 24);
    write_u32(buf, 0);
    write_u32(buf, NO_ENTRY_INDEX);
    let prefix_idx = pool
        .index_of(&decl.prefix)
        .ok_or_else(|| BinaryXmlError::MalformedDocument("Missing namespace prefix string".into()))?;
    let uri_idx = pool
        .index_of(&decl.uri)
        .ok_or_else(|| BinaryXmlError::MalformedDocument("Missing namespace URI string".into()))?;
    write_u32(buf, prefix_idx);
    write_u32(buf, uri_idx);
    finalize_chunk(buf, chunk_start);
    Ok(())
}

fn write_element_recursive(
    element: &ManifestElement,
    buf: &mut Vec<u8>,
    pool: &StringPoolBuilder,
) -> BinaryXmlResult<()> {
    write_start_element(buf, element, pool)?;
    if let Some(text) = &element.text {
        write_cdata(buf, text, pool)?;
    }
    for child in &element.children {
        write_element_recursive(child, buf, pool)?;
    }
    write_end_element(buf, element, pool)?;
    Ok(())
}

fn write_start_element(
    buf: &mut Vec<u8>,
    element: &ManifestElement,
    pool: &StringPoolBuilder,
) -> BinaryXmlResult<()> {
    let chunk_start = begin_chunk(buf, RES_XML_START_ELEMENT_TYPE, 36);
    write_u32(buf, 0);
    write_u32(buf, NO_ENTRY_INDEX);
    let ns_idx = if let Some(uri) = &element.namespace_uri {
        pool.index_of(uri).ok_or_else(|| {
            BinaryXmlError::MalformedDocument("Missing element namespace URI string".into())
        })?
    } else {
        NO_ENTRY_INDEX
    };
    let name_idx = pool
        .index_of(&element.tag)
        .ok_or_else(|| BinaryXmlError::MalformedDocument("Missing element tag string".into()))?;
    write_u32(buf, ns_idx);
    write_u32(buf, name_idx);
    write_u16(buf, 20); // attributeStart
    write_u16(buf, 20); // attributeSize
    write_u16(buf, element.attributes.len() as u16);
    write_u16(buf, 0); // idIndex
    write_u16(buf, 0); // classIndex
    write_u16(buf, 0); // styleIndex
    for attr in &element.attributes {
        write_attribute(buf, attr, pool)?;
    }
    finalize_chunk(buf, chunk_start);
    Ok(())
}

fn write_end_element(
    buf: &mut Vec<u8>,
    element: &ManifestElement,
    pool: &StringPoolBuilder,
) -> BinaryXmlResult<()> {
    let chunk_start = begin_chunk(buf, RES_XML_END_ELEMENT_TYPE, 24);
    write_u32(buf, 0);
    write_u32(buf, NO_ENTRY_INDEX);
    let ns_idx = if let Some(uri) = &element.namespace_uri {
        pool.index_of(uri).ok_or_else(|| {
            BinaryXmlError::MalformedDocument("Missing element namespace URI string".into())
        })?
    } else {
        NO_ENTRY_INDEX
    };
    let name_idx = pool
        .index_of(&element.tag)
        .ok_or_else(|| BinaryXmlError::MalformedDocument("Missing element tag string".into()))?;
    write_u32(buf, ns_idx);
    write_u32(buf, name_idx);
    finalize_chunk(buf, chunk_start);
    Ok(())
}

fn write_cdata(buf: &mut Vec<u8>, text: &str, pool: &StringPoolBuilder) -> BinaryXmlResult<()> {
    let idx = pool
        .index_of(text)
        .ok_or_else(|| BinaryXmlError::MalformedDocument("Missing CDATA text string".into()))?;
    let chunk_start = begin_chunk(buf, RES_XML_CDATA_TYPE, 28);
    write_u32(buf, 0);
    write_u32(buf, NO_ENTRY_INDEX);
    write_u32(buf, idx);
    write_u16(buf, 8);
    write_u8(buf, 0);
    write_u8(buf, TYPE_STRING);
    write_u32(buf, idx);
    finalize_chunk(buf, chunk_start);
    Ok(())
}

fn write_attribute(
    buf: &mut Vec<u8>,
    attr: &ManifestAttribute,
    pool: &StringPoolBuilder,
) -> BinaryXmlResult<()> {
    let ns_idx = if let Some(uri) = &attr.namespace_uri {
        pool.index_of(uri).ok_or_else(|| {
            BinaryXmlError::MalformedDocument("Missing attribute namespace URI string".into())
        })?
    } else {
        NO_ENTRY_INDEX
    };
    let name_idx = pool
        .index_of(&attr.name)
        .ok_or_else(|| BinaryXmlError::MalformedDocument("Missing attribute name string".into()))?;
    let raw = raw_value_text(&attr.value);
    let raw_idx = if let Some(text) = raw.as_ref() {
        pool.index_of(text.as_ref())
            .ok_or_else(|| BinaryXmlError::MalformedDocument("Missing raw attribute string".into()))?
    } else {
        NO_ENTRY_INDEX
    };
    let (data_type, data_value) = encode_typed_value(&attr.value, pool)?;
    write_u32(buf, ns_idx);
    write_u32(buf, name_idx);
    write_u32(buf, raw_idx);
    write_u16(buf, 8);
    write_u8(buf, 0);
    write_u8(buf, data_type);
    write_u32(buf, data_value);
    Ok(())
}

fn write_element_xml(
    element: &ManifestElement,
    writer: &mut Writer<Vec<u8>>,
    namespaces: &[NamespaceDecl],
    is_root: bool,
) -> BinaryXmlResult<()> {
    let element_name = qualified_name(element.namespace_prefix.as_deref(), &element.tag);
    let mut attr_storage = Vec::new();
    if is_root {
        for decl in namespaces {
            let attr_name = if decl.prefix.is_empty() {
                "xmlns".to_string()
            } else {
                format!("xmlns:{}", decl.prefix)
            };
            attr_storage.push((attr_name, decl.uri.clone()));
        }
    }
    for attr in &element.attributes {
        let name = qualified_name(attr.namespace_prefix.as_deref(), &attr.name);
        let value = manifest_value_to_text(&attr.value);
        attr_storage.push((name, value));
    }
    let mut start = BytesStart::new(element_name.as_str());
    for (key, value) in &attr_storage {
        start.push_attribute((key.as_str(), value.as_str()));
    }
    writer.write_event(Event::Start(start))?;
    if let Some(text) = &element.text {
        writer.write_event(Event::Text(BytesText::new(text)))?;
    }
    for child in &element.children {
        write_element_xml(child, writer, namespaces, false)?;
    }
    writer.write_event(Event::End(BytesEnd::new(element_name.as_str())))?;
    Ok(())
}

fn encode_typed_value(
    value: &ManifestValue,
    pool: &StringPoolBuilder,
) -> BinaryXmlResult<(u8, u32)> {
    match value {
        ManifestValue::String(text) => {
            let idx = pool
                .index_of(text)
                .ok_or_else(|| BinaryXmlError::MalformedDocument("Missing string value".into()))?;
            Ok((TYPE_STRING, idx))
        }
        ManifestValue::Boolean(flag) => Ok((TYPE_INT_BOOLEAN, if *flag { 1 } else { 0 })),
        ManifestValue::Integer(num) => {
            let value32 = *num as i32;
            Ok((TYPE_INT_DEC, value32 as u32))
        }
        ManifestValue::Hex(v) => Ok((TYPE_INT_HEX, *v)),
        ManifestValue::Reference(id) => Ok((TYPE_REFERENCE, *id)),
    }
}

/// High-level representation of `AndroidManifest.xml`.
#[derive(Clone, Debug)]
pub struct AndroidManifest {
    root: ManifestElement,
    attribute_resource_map: BTreeMap<(Option<String>, String), u32>,
}

impl PartialEq for AndroidManifest {
    fn eq(&self, other: &Self) -> bool {
        self.root == other.root
    }
}

impl Eq for AndroidManifest {}

impl AndroidManifest {
    pub fn new() -> Self {
        AndroidManifest {
            root: ManifestElement::new("manifest"),
            attribute_resource_map: BTreeMap::new(),
        }
    }

    pub fn from_root(root: ManifestElement) -> Self {
        AndroidManifest {
            root,
            attribute_resource_map: BTreeMap::new(),
        }
    }

    pub fn root(&self) -> &ManifestElement {
        &self.root
    }

    pub fn root_mut(&mut self) -> &mut ManifestElement {
        &mut self.root
    }

    pub fn package_name(&self) -> Option<&str> {
        self.root.attribute_value("package").and_then(|value| value.as_str())
    }

    pub fn set_package_name(&mut self, package: impl Into<String>) {
        let attr = ManifestAttribute::new("package", ManifestValue::from(package.into()));
        self.root.set_attribute(attr);
    }

    pub fn version_name(&self) -> Option<&str> {
        self.root
            .attribute_value("versionName")
            .and_then(|value| value.as_str())
    }

    pub fn set_version_name(&mut self, version: impl Into<String>) {
        let attr = ManifestAttribute::new("versionName", version.into());
        self.root.set_attribute(attr);
    }

    pub fn application(&self) -> Option<&ManifestElement> {
        self.root.find_child("application")
    }

    pub fn application_mut(&mut self) -> Option<&mut ManifestElement> {
        self.root.find_child_mut("application")
    }

    pub fn ensure_application(&mut self) -> &mut ManifestElement {
        if self.root.find_child("application").is_none() {
            self.root.append_child(ManifestElement::new("application"));
        }
        self.root.find_child_mut("application").expect("application node exists")
    }

    pub fn uses_permissions(&self) -> Vec<&ManifestElement> {
        self.root
            .children
            .iter()
            .filter(|child| child.tag == "uses-permission")
            .collect()
    }

    pub fn add_permission(&mut self, name: impl Into<String>) {
        let mut node = ManifestElement::new("uses-permission");
        node.set_attribute(ManifestAttribute::with_namespace_and_uri(
            "android",
            ANDROID_NAMESPACE_URI,
            "name",
            name.into(),
        ));
        self.root.append_child(node);
    }

    pub fn from_string(xml: &str) -> BinaryXmlResult<Self> {
        let mut reader = Reader::from_str(xml);
        reader.config_mut().trim_text(true);
        let mut buffer = Vec::new();
        let mut stack: Vec<ManifestElement> = Vec::new();
        let mut root: Option<ManifestElement> = None;
        let mut namespace_stack: Vec<BTreeMap<String, String>> = vec![BTreeMap::new()];

        loop {
            match reader.read_event_into(&mut buffer)? {
                Event::Start(start) => {
                    let start = start.into_owned();
                    let (decls, attrs) = extract_attributes(&start)?;
                    namespace_stack.push(decls);
                    let element = build_element_from_start(&start, &namespace_stack, attrs)?;
                    stack.push(element);
                }
                Event::Empty(start) => {
                    let start = start.into_owned();
                    let (decls, attrs) = extract_attributes(&start)?;
                    namespace_stack.push(decls);
                    let element = build_element_from_start(&start, &namespace_stack, attrs)?;
                    attach_element(&mut stack, &mut root, element)?;
                    if namespace_stack.len() <= 1 {
                        return Err(BinaryXmlError::MalformedDocument(
                            "Namespace stack underflow".to_string(),
                        ));
                    }
                    namespace_stack.pop();
                }
                Event::End(_) => {
                    let element = stack.pop().ok_or_else(|| {
                        BinaryXmlError::MalformedDocument(
                            "Unbalanced end tag in manifest".to_string(),
                        )
                    })?;
                    if namespace_stack.len() <= 1 {
                        return Err(BinaryXmlError::MalformedDocument(
                            "Namespace stack underflow".to_string(),
                        ));
                    }
                    namespace_stack.pop();
                    attach_element(&mut stack, &mut root, element)?;
                }
                Event::Text(text) => {
                    if let Some(current) = stack.last_mut() {
                        let raw = text.unescape()?.into_owned();
                        if let Some(content) = parse_text_content(&raw) {
                            match &mut current.text {
                                Some(existing) => {
                                    if !existing.is_empty() {
                                        existing.push(' ');
                                    }
                                    existing.push_str(&content);
                                }
                                None => current.text = Some(content),
                            }
                        }
                    }
                }
                Event::CData(text) => {
                    if let Some(current) = stack.last_mut() {
                        let raw = std::str::from_utf8(text.as_ref())
                            .map_err(|err| {
                                BinaryXmlError::MalformedDocument(err.to_string())
                            })?
                            .to_string();
                        if let Some(content) = parse_text_content(&raw) {
                            current.text = Some(content);
                        }
                    }
                }
                Event::Comment(_) | Event::Decl(_) | Event::PI(_) | Event::DocType(_) => {}
                Event::Eof => break,
            }
            buffer.clear();
        }

        if !stack.is_empty() {
            return Err(BinaryXmlError::MalformedDocument(
                "Unclosed XML elements in manifest".to_string(),
            ));
        }

        if namespace_stack.len() != 1 {
            return Err(BinaryXmlError::MalformedDocument(
                "Mismatched namespace scopes in manifest".to_string(),
            ));
        }

        let root = root.ok_or_else(|| {
            BinaryXmlError::MalformedDocument("Manifest does not have a root element".to_string())
        })?;

        Ok(AndroidManifest {
            root,
            attribute_resource_map: BTreeMap::new(),
        })
    }

    pub fn to_string(&self) -> BinaryXmlResult<String> {
        let mut writer = Writer::new(Vec::new());
        writer.write_event(Event::Decl(BytesDecl::new("1.0", Some("utf-8"), None)))?;
        let namespaces = collect_namespace_declarations(&self.root);
        write_element_xml(&self.root, &mut writer, &namespaces, true)?;
        let bytes = writer.into_inner();
        String::from_utf8(bytes)
            .map_err(|err| BinaryXmlError::MalformedDocument(err.to_string()))
    }

    pub fn from_bytes(bytes: &[u8]) -> BinaryXmlResult<Self> {
        let mut reader = BinaryReader::new(bytes);
        let xml_header = read_chunk_header(&mut reader)?;
        if xml_header.chunk_type != RES_XML_TYPE {
            return Err(BinaryXmlError::MalformedDocument(
                "Binary XML does not start with RES_XML_TYPE header".to_string(),
            ));
        }

        let xml_end = xml_header.end();
        reader.seek(xml_header.start + xml_header.header_size as usize)?;

        let mut resource_map = Vec::new();
        let mut attribute_resource_map: BTreeMap<(Option<String>, String), u32> = BTreeMap::new();
        let mut string_pool: Option<StringPool> = None;
        let mut namespaces: Vec<NamespaceFrame> = Vec::new();
        let mut element_stack: Vec<ManifestElement> = Vec::new();
        let mut root: Option<ManifestElement> = None;

        while reader.position() < xml_end {
            let chunk_header = read_chunk_header(&mut reader)?;
            let chunk_end = chunk_header.end();
            match chunk_header.chunk_type {
                RES_STRING_POOL_TYPE => {
                    string_pool = Some(StringPool::parse(&mut reader, &chunk_header)?);
                }
                RES_XML_RESOURCE_MAP_TYPE => {
                    let mut ids = Vec::new();
                    while reader.position() < chunk_end {
                        ids.push(reader.read_u32()?);
                    }
                    resource_map = ids;
                }
                RES_XML_START_NAMESPACE_TYPE => {
                    let pool = string_pool
                        .as_ref()
                        .ok_or_else(|| BinaryXmlError::MalformedDocument(
                            "Namespace chunk encountered before string pool".to_string(),
                        ))?;
                    reader.read_u32()?; // line number
                    reader.read_u32()?; // comment
                    let prefix_idx = reader.read_u32()?;
                    let uri_idx = reader.read_u32()?;
                    let prefix = pool.get(prefix_idx).map(|s| s.to_string());
                    let uri = pool.get(uri_idx).map(|s| s.to_string());
                    namespaces.push(NamespaceFrame { prefix, uri });
                }
                RES_XML_END_NAMESPACE_TYPE => {
                    reader.read_u32()?;
                    reader.read_u32()?;
                    reader.read_u32()?;
                    reader.read_u32()?;
                    namespaces.pop();
                }
                RES_XML_START_ELEMENT_TYPE => {
                    let pool = string_pool
                        .as_ref()
                        .ok_or_else(|| BinaryXmlError::MalformedDocument(
                            "Start element encountered before string pool".to_string(),
                        ))?;

                    reader.read_u32()?; // line number
                    reader.read_u32()?; // comment index
                    let ns_idx = reader.read_u32()?;
                    let name_idx = reader.read_u32()?;
                    reader.read_u16()?; // attributeStart
                    reader.read_u16()?; // attributeSize
                    let attr_count = reader.read_u16()? as usize;
                    reader.read_u16()?; // idIndex
                    reader.read_u16()?; // classIndex
                    reader.read_u16()?; // styleIndex

                    let tag_name = pool
                        .get(name_idx)
                        .ok_or_else(|| {
                            BinaryXmlError::MalformedDocument(
                                "Element references invalid string index".to_string(),
                            )
                        })?
                        .to_string();

                    let namespace_uri = pool.get(ns_idx).map(|s| s.to_string());
                    let namespace_prefix = resolve_prefix(&namespaces, namespace_uri.as_deref());
                    let mut element = ManifestElement::new(tag_name);
                    element.namespace_prefix = namespace_prefix;
                    element.namespace_uri = namespace_uri;

                    let mut attributes = Vec::with_capacity(attr_count);
                    for _ in 0..attr_count {
                        let attr_ns_idx = reader.read_u32()?;
                        let attr_name_idx = reader.read_u32()?;
                        let raw_value_idx = reader.read_u32()?;
                        let value_size = reader.read_u16()?;
                        reader.read_u8()?; // res0
                        let data_type = reader.read_u8()?;
                        let data = reader.read_u32()?;
                        if value_size != 8 {
                            return Err(BinaryXmlError::MalformedDocument(
                                "Attribute value size must be 8".to_string(),
                            ));
                        }
                        let attr_name = pool
                            .get(attr_name_idx)
                            .ok_or_else(|| {
                                BinaryXmlError::MalformedDocument(
                                    "Attribute name references invalid string index".to_string(),
                                )
                            })?
                            .to_string();
                        let attr_namespace_uri = pool.get(attr_ns_idx).map(|s| s.to_string());
                        let attr_namespace =
                            resolve_prefix(&namespaces, attr_namespace_uri.as_deref());
                        let value = decode_value(pool, raw_value_idx, data_type, data)?;
                        let resource_id = resource_map
                            .get(attr_name_idx as usize)
                            .copied()
                            .filter(|id| *id != 0);
                        if let Some(id) = resource_id {
                            attribute_resource_map.insert(
                                (attr_namespace_uri.clone(), attr_name.clone()),
                                id,
                            );
                        }
                        attributes.push(ManifestAttribute {
                            namespace_prefix: attr_namespace,
                            namespace_uri: attr_namespace_uri,
                            resource_id,
                            name: attr_name,
                            value,
                        });
                    }
                    element.attributes = attributes;
                    element_stack.push(element);
                }
                RES_XML_END_ELEMENT_TYPE => {
                    reader.read_u32()?;
                    reader.read_u32()?;
                    reader.read_u32()?;
                    reader.read_u32()?;
                    if let Some(element) = element_stack.pop() {
                        if let Some(parent) = element_stack.last_mut() {
                            parent.children.push(element);
                        } else {
                            root = Some(element);
                        }
                    } else {
                        return Err(BinaryXmlError::MalformedDocument(
                            "End element without matching start".to_string(),
                        ));
                    }
                }
                RES_XML_CDATA_TYPE => {
                    let pool = string_pool
                        .as_ref()
                        .ok_or_else(|| BinaryXmlError::MalformedDocument(
                            "CDATA encountered before string pool".to_string(),
                        ))?;
                    reader.read_u32()?;
                    reader.read_u32()?;
                    let data_idx = reader.read_u32()?;
                    let value_size = reader.read_u16()?;
                    reader.read_u8()?;
                    let data_type = reader.read_u8()?;
                    let data = reader.read_u32()?;
                    if value_size != 8 {
                        return Err(BinaryXmlError::MalformedDocument(
                            "CDATA value size must be 8".to_string(),
                        ));
                    }
                    if let Some(text) = pool
                        .get(data_idx)
                        .map(|s| s.to_string())
                        .or_else(|| {
                            if data_type == TYPE_STRING {
                                pool.get(data).map(|s| s.to_string())
                            } else {
                                None
                            }
                        })
                    {
                        if let Some(current) = element_stack.last_mut() {
                            current.text = Some(text);
                        }
                    }
                }
                _ => {
                    // Unknown chunk type; skip over it for forward compatibility.
                }
            }
            reader.seek(chunk_end)?;
        }

        if !element_stack.is_empty() {
            return Err(BinaryXmlError::MalformedDocument(
                "Unclosed XML elements at end of document".to_string(),
            ));
        }

        let root = root.ok_or_else(|| {
            BinaryXmlError::MalformedDocument("AndroidManifest is empty".to_string())
        })?;

        Ok(AndroidManifest {
            root,
            attribute_resource_map,
        })
    }

    pub fn to_bytes(&self) -> BinaryXmlResult<Vec<u8>> {
        let namespaces = collect_namespace_declarations(&self.root);
        let mut pool_builder = StringPoolBuilder::new();
        for decl in &namespaces {
            pool_builder.intern(&decl.prefix);
            pool_builder.intern(&decl.uri);
        }
        collect_element_strings(&self.root, &mut pool_builder);
        let string_chunk = pool_builder.to_chunk();

        let mut body = Vec::new();
        for decl in &namespaces {
            write_namespace_chunk(&mut body, &pool_builder, decl, true)?;
        }
        write_element_recursive(&self.root, &mut body, &pool_builder)?;
        for decl in namespaces.iter().rev() {
            write_namespace_chunk(&mut body, &pool_builder, decl, false)?;
        }

        let mut document = Vec::new();
        let xml_start = begin_chunk(&mut document, RES_XML_TYPE, 8);
        document.extend_from_slice(&string_chunk);
        document.extend_from_slice(&body);
        finalize_chunk(&mut document, xml_start);
        Ok(document)
    }

    pub fn from_apk_entry(entry: &ApkEntry) -> BinaryXmlResult<Self> {
        Self::from_bytes(&entry.data)
    }

    pub fn to_apk_entry(&self, template: Option<&ApkEntry>) -> BinaryXmlResult<ApkEntry> {
        let data = self.to_bytes()?;
        let mut entry = ApkEntry::new(data);
        if let Some(template) = template {
            entry.unix_mode = template.unix_mode;
            entry.compression = template.compression;
        }
        Ok(entry)
    }
}

// Roadmap for evolving the binary XML implementation:
// 1. Implement a chunk reader that can parse the Android binary XML stream, including
//    the string pool, resource map, and node chunks.
// 2. Map parsed nodes into [`ManifestElement`] structures, preserving namespaces and
//    attribute typing information (resource references, enums, etc.).
// 3. Implement the inverse serializer that can emit binary XML chunks from the DOM
//    representation while reusing the string pool where possible.
// 4. Add integration tests that load existing APK manifests, roundtrip them through
//    [`AndroidManifest`], and verify byte-for-byte stability when no modifications are
//    made.
// 5. Layer ergonomic helpers for common manifest edits (renaming packages, toggling
//    activities, injecting metadata) on top of the DOM primitives.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::android::zip::ApkFile;
    use std::fs;

    #[test]
    fn parses_rootbeer_manifest() {
        let apk = ApkFile::from_file("tests/rootbeer.apk").expect("read rootbeer apk fixture");
        let manifest_entry = apk
            .entry("AndroidManifest.xml")
            .expect("manifest entry exists");

        let manifest = AndroidManifest::from_apk_entry(manifest_entry)
            .expect("parse binary manifest");
        assert_eq!(
            manifest.package_name(),
            Some("com.scottyab.rootbeer.sample")
        );

        let application = manifest.application().expect("application element");
        let app_name = application
            .attribute_value("android:name")
            .and_then(|value| value.as_str())
            .expect("application android:name attribute");
        assert_eq!(app_name, "com.scottyab.rootbeer.sample.RootSampleApp");

        let activity = application
            .find_child("activity")
            .expect("main activity element");
        let activity_name = activity
            .attribute_value("android:name")
            .and_then(|value| value.as_str())
            .expect("activity android:name attribute");
        assert_eq!(activity_name, "com.scottyab.rootbeer.sample.MainActivity");

        match activity
            .attribute_value("android:label")
            .expect("activity label")
        {
            ManifestValue::Reference(value) => assert_ne!(*value, 0),
            other => panic!("unexpected label value: {:?}", other),
        }

        let rebuilt = manifest.to_bytes().expect("serialize manifest");
        let reparsed = AndroidManifest::from_bytes(&rebuilt).expect("roundtrip manifest");
        assert_eq!(manifest.package_name(), reparsed.package_name());
        let original_app = manifest.application().expect("original app node");
        let roundtrip_app = reparsed.application().expect("roundtrip app node");
        assert_eq!(original_app.attributes.len(), roundtrip_app.attributes.len());

        let xml_text = fs::read_to_string("tests/rootbeer/AndroidManifest.xml")
            .expect("read textual manifest");
        let text_manifest = AndroidManifest::from_string(&xml_text).expect("parse xml text");
        assert_eq!(
            text_manifest.package_name(),
            Some("com.scottyab.rootbeer.sample")
        );
        let xml_out = text_manifest.to_string().expect("emit xml text");
        let reparsed_text = AndroidManifest::from_string(&xml_out).expect("reparse xml");
        assert_eq!(text_manifest.package_name(), reparsed_text.package_name());
    }
}
