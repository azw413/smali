use crate::dex::dex_file::{
    ClassDefItem, DEX_FILE_MAGIC, DexString, ENDIAN_CONSTANT, FieldItem, Header, MethodItem,
    NO_INDEX, PrototypeItem, TypeList,
};
use crate::dex::encoded_values::{EncodedValue, write_encoded_array};
use crate::dex::error::DexError;
use crate::dex::{write_u2, write_u4};
use crate::smali_ops::{DexOp, FieldRef, MethodRef, parse_literal_int};
use crate::types::{
    AnnotationValue, CatchDirective, MethodSignature, Modifier, ObjectIdentifier, SmaliAnnotation,
    SmaliClass, SmaliField, SmaliMethod, SmaliOp, TypeSignature,
};

use std::collections::{BTreeSet, HashMap};
use std::convert::TryFrom;

use adler::adler32_slice;
use sha1::{Digest, Sha1};

const TYPE_HEADER_ITEM: u16 = 0x0000;
const TYPE_STRING_ID_ITEM: u16 = 0x0001;
const TYPE_TYPE_ID_ITEM: u16 = 0x0002;
const TYPE_PROTO_ID_ITEM: u16 = 0x0003;
const TYPE_FIELD_ID_ITEM: u16 = 0x0004;
const TYPE_METHOD_ID_ITEM: u16 = 0x0005;
const TYPE_CLASS_DEF_ITEM: u16 = 0x0006;
const TYPE_MAP_LIST: u16 = 0x1000;
const TYPE_TYPE_LIST: u16 = 0x1001;
const TYPE_STRING_DATA_ITEM: u16 = 0x2002;
const TYPE_ENCODED_ARRAY_ITEM: u16 = 0x2005;

/// Canonicalized string/type/proto/field/method tables built from a set of `SmaliClass` items.
///
/// The entries are stored in DEX-sorted order (lexicographic by descriptor/name), ready to be
/// turned into on-disk sections once offsets are known.
#[derive(Debug, Default)]
pub struct DexIndexPools {
    pub strings: Vec<String>,
    pub string_index: HashMap<String, u32>,

    pub types: Vec<TypeEntry>,
    pub type_index: HashMap<String, u32>,

    pub protos: Vec<ProtoEntry>,
    proto_index: HashMap<PrototypeKey, u32>,

    pub fields: Vec<FieldEntry>,
    field_index: HashMap<FieldKey, u32>,

    pub methods: Vec<MethodEntry>,
    method_index: HashMap<MethodKey, u32>,
}

#[derive(Debug)]
pub struct DexIdTables {
    pub strings: Vec<DexString>,
    pub type_ids: Vec<u32>,
    pub proto_ids: Vec<PrototypeItem>,
    pub field_ids: Vec<FieldItem>,
    pub method_ids: Vec<MethodItem>,
}

/// High-level plan describing how the various ID sections and data-area payloads should be
/// arranged inside a future DEX file.
#[derive(Debug)]
pub struct DexLayoutPlan {
    pub pools: DexIndexPools,
    pub ids: DexIdTables,
    pub sections: SectionOffsets,
    pub data_section: DataSectionPlan,
    pub class_defs: Vec<ClassDefPlan>,
    pub string_data_offsets: Vec<u32>,
    pub proto_parameter_offsets: Vec<Option<u32>>,
}

/// Build a binary DEX file from the provided classes, returning the serialized bytes.
pub fn build_dex_file_bytes(classes: &[SmaliClass]) -> Result<Vec<u8>, DexError> {
    let plan = DexLayoutPlan::from_classes(classes)?;
    emit_dex_file(plan)
}

#[derive(Debug, Clone, Copy)]
pub struct SectionInfo {
    pub count: u32,
    pub offset: u32,
}

#[derive(Debug)]
pub struct SectionOffsets {
    pub string_ids: SectionInfo,
    pub type_ids: SectionInfo,
    pub proto_ids: SectionInfo,
    pub field_ids: SectionInfo,
    pub method_ids: SectionInfo,
    pub class_defs: SectionInfo,
    pub data_off: u32,
    pub data_size: u32,
}

#[derive(Debug)]
pub struct ClassDefPlan {
    pub class_idx: u32,
    pub item: ClassDefItem,
    pub offsets: ClassDefOffsets,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct ClassDefOffsets {
    pub interfaces_off: u32,
    pub annotations_off: u32,
    pub class_data_off: u32,
    pub static_values_off: u32,
}

#[derive(Debug)]
pub struct DataSectionPlan {
    chunks: Vec<DataChunk>,
    pub size: u32,
}

#[derive(Debug, Clone)]
struct DataChunk {
    owner: DataChunkOwner,
    kind: DataChunkKind,
    align: u32,
    offset: u32,
    bytes: Vec<u8>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DataChunkKind {
    StringData,
    TypeList,
    EncodedArray,
}

#[derive(Debug, Clone, Copy)]
enum DataChunkOwner {
    StringData(usize),
    ProtoParameters(usize),
    ClassInterfaces(usize),
    ClassStaticValues(usize),
}

#[derive(Debug)]
struct MapItem {
    type_code: u16,
    size: u32,
    offset: u32,
}

impl MapItem {
    fn new(type_code: u16, size: u32, offset: u32) -> Self {
        MapItem {
            type_code,
            size,
            offset,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeEntry {
    pub descriptor: String,
    pub string_idx: u32,
}

#[derive(Debug, Clone)]
pub struct ProtoEntry {
    pub shorty_idx: u32,
    pub return_type_idx: u32,
    pub parameters: Vec<u32>,
}

#[derive(Debug, Clone)]
pub struct FieldEntry {
    pub class_idx: u32,
    pub type_idx: u32,
    pub name_idx: u32,
}

#[derive(Debug, Clone)]
pub struct MethodEntry {
    pub class_idx: u32,
    pub proto_idx: u32,
    pub name_idx: u32,
}

impl DexIndexPools {
    pub fn from_classes(classes: &[SmaliClass]) -> Result<Self, DexError> {
        let mut builder = DexPoolBuilder::default();
        for class in classes {
            builder.ingest_class(class);
        }
        builder.finalize()
    }

    pub fn dex_strings(&self) -> Vec<DexString> {
        self.strings
            .iter()
            .map(|s| DexString::from_string(s))
            .collect()
    }

    pub fn build_id_tables(&self) -> DexIdTables {
        let strings = self.dex_strings();
        let type_ids = self.types.iter().map(|entry| entry.string_idx).collect();
        let proto_ids = self
            .protos
            .iter()
            .map(|entry| PrototypeItem {
                shorty_idx: usize::try_from(entry.shorty_idx).expect("shorty idx fits usize"),
                return_type_idx: usize::try_from(entry.return_type_idx)
                    .expect("return type idx fits usize"),
                parameters: TypeList::from_type_ids(
                    entry
                        .parameters
                        .iter()
                        .map(|idx| usize::try_from(*idx).expect("type idx fits usize"))
                        .collect(),
                ),
            })
            .collect();
        let field_ids = self
            .fields
            .iter()
            .map(|entry| FieldItem {
                class_idx: usize::try_from(entry.class_idx).expect("class idx fits usize"),
                type_idx: usize::try_from(entry.type_idx).expect("type idx fits usize"),
                name_idx: usize::try_from(entry.name_idx).expect("name idx fits usize"),
            })
            .collect();
        let method_ids = self
            .methods
            .iter()
            .map(|entry| MethodItem {
                class_idx: usize::try_from(entry.class_idx).expect("class idx fits usize"),
                proto_idx: usize::try_from(entry.proto_idx).expect("proto idx fits usize"),
                name_idx: usize::try_from(entry.name_idx).expect("name idx fits usize"),
            })
            .collect();

        DexIdTables {
            strings,
            type_ids,
            proto_ids,
            field_ids,
            method_ids,
        }
    }

    pub fn string_index(&self, value: &str) -> Option<u32> {
        self.string_index.get(value).copied()
    }

    pub fn type_index(&self, descriptor: &str) -> Option<u32> {
        self.type_index.get(descriptor).copied()
    }

    pub fn proto_index(&self, signature: &MethodSignature) -> Option<u32> {
        let key = PrototypeKey::from_signature(signature);
        self.proto_index.get(&key).copied()
    }

    pub fn field_index(&self, class_desc: &str, name: &str, type_desc: &str) -> Option<u32> {
        let key = FieldKey::new(
            class_desc.to_string(),
            name.to_string(),
            type_desc.to_string(),
        );
        self.field_index.get(&key).copied()
    }

    pub fn method_index(
        &self,
        class_desc: &str,
        name: &str,
        signature: &MethodSignature,
    ) -> Option<u32> {
        let proto = PrototypeKey::from_signature(signature);
        let key = MethodKey::new(class_desc.to_string(), name.to_string(), proto);
        self.method_index.get(&key).copied()
    }
}

impl SectionOffsets {
    const HEADER_SIZE: u32 = 0x70;
    const STRING_ID_ITEM_SIZE: u32 = 4;
    const TYPE_ID_ITEM_SIZE: u32 = 4;
    const PROTO_ID_ITEM_SIZE: u32 = 12;
    const FIELD_ID_ITEM_SIZE: u32 = 8;
    const METHOD_ID_ITEM_SIZE: u32 = 8;
    const CLASS_DEF_ITEM_SIZE: u32 = 32;

    fn new(ids: &DexIdTables, class_count: u32, data_size: u32) -> Self {
        let mut cursor = Self::HEADER_SIZE;

        let (string_ids, next) =
            Self::section(cursor, ids.strings.len() as u32, Self::STRING_ID_ITEM_SIZE);
        cursor = next;
        let (type_ids, next) =
            Self::section(cursor, ids.type_ids.len() as u32, Self::TYPE_ID_ITEM_SIZE);
        cursor = next;
        let (proto_ids, next) =
            Self::section(cursor, ids.proto_ids.len() as u32, Self::PROTO_ID_ITEM_SIZE);
        cursor = next;
        let (field_ids, next) =
            Self::section(cursor, ids.field_ids.len() as u32, Self::FIELD_ID_ITEM_SIZE);
        cursor = next;
        let (method_ids, next) = Self::section(
            cursor,
            ids.method_ids.len() as u32,
            Self::METHOD_ID_ITEM_SIZE,
        );
        cursor = next;
        let (class_defs, cursor) = Self::section(cursor, class_count, Self::CLASS_DEF_ITEM_SIZE);

        let data_off = cursor;
        let data_size_aligned = align_to(data_size, 4);

        SectionOffsets {
            string_ids,
            type_ids,
            proto_ids,
            field_ids,
            method_ids,
            class_defs,
            data_off,
            data_size: data_size_aligned,
        }
    }

    fn section(base: u32, count: u32, item_size: u32) -> (SectionInfo, u32) {
        if count == 0 {
            (
                SectionInfo {
                    count: 0,
                    offset: 0,
                },
                base,
            )
        } else {
            let offset = base;
            let next = base + count * item_size;
            (SectionInfo { count, offset }, next)
        }
    }
}

impl ClassDefPlan {
    fn new(class_idx: u32, item: ClassDefItem) -> Self {
        ClassDefPlan {
            class_idx,
            item,
            offsets: ClassDefOffsets::default(),
        }
    }
}

impl DataSectionPlan {
    fn collect_string_offsets(&self, count: usize, base: u32) -> Vec<u32> {
        let mut out = vec![0; count];
        for chunk in &self.chunks {
            if let DataChunkOwner::StringData(idx) = chunk.owner {
                out[idx] = base + chunk.offset;
            }
        }
        out
    }

    fn collect_proto_parameter_offsets(&self, count: usize, base: u32) -> Vec<Option<u32>> {
        let mut out = vec![None; count];
        for chunk in &self.chunks {
            if let DataChunkOwner::ProtoParameters(idx) = chunk.owner {
                out[idx] = Some(base + chunk.offset);
            }
        }
        out
    }

    fn collect_interface_offsets(&self, count: usize, base: u32) -> Vec<Option<u32>> {
        let mut out = vec![None; count];
        for chunk in &self.chunks {
            if let DataChunkOwner::ClassInterfaces(idx) = chunk.owner {
                out[idx] = Some(base + chunk.offset);
            }
        }
        out
    }

    fn collect_static_value_offsets(&self, count: usize, base: u32) -> Vec<Option<u32>> {
        let mut out = vec![None; count];
        for chunk in &self.chunks {
            if let DataChunkOwner::ClassStaticValues(idx) = chunk.owner {
                out[idx] = Some(base + chunk.offset);
            }
        }
        out
    }

    fn emit_bytes(&self) -> Vec<u8> {
        let mut chunks = self.chunks.clone();
        chunks.sort_by_key(|chunk| chunk.offset);
        let mut buf = Vec::with_capacity(self.size as usize);
        for chunk in chunks {
            ensure_len(&mut buf, chunk.offset as usize);
            buf.extend_from_slice(&chunk.bytes);
        }
        buf
    }

    fn chunks(&self) -> &[DataChunk] {
        &self.chunks
    }
}

struct DataSectionBuilder {
    chunks: Vec<DataChunk>,
}

impl DataSectionBuilder {
    fn new() -> Self {
        DataSectionBuilder { chunks: Vec::new() }
    }

    fn add_string_data(&mut self, strings: &[DexString]) {
        for (idx, string) in strings.iter().enumerate() {
            let mut bytes = Vec::new();
            string.write(&mut bytes);
            self.chunks.push(DataChunk {
                owner: DataChunkOwner::StringData(idx),
                kind: DataChunkKind::StringData,
                align: 1,
                offset: 0,
                bytes,
            });
        }
    }

    fn add_proto_parameter_lists(&mut self, protos: &[PrototypeItem]) {
        for (idx, proto) in protos.iter().enumerate() {
            if proto.parameters.items().is_empty() {
                continue;
            }
            let mut bytes = Vec::new();
            proto.parameters.write(&mut bytes);
            self.chunks.push(DataChunk {
                owner: DataChunkOwner::ProtoParameters(idx),
                kind: DataChunkKind::TypeList,
                align: 4,
                offset: 0,
                bytes,
            });
        }
    }

    fn add_class_interface_lists(&mut self, class_defs: &[ClassDefPlan]) {
        for (idx, plan) in class_defs.iter().enumerate() {
            if let Some(ifaces) = &plan.item.interfaces {
                if ifaces.items().is_empty() {
                    continue;
                }
                let mut bytes = Vec::new();
                ifaces.write(&mut bytes);
                self.chunks.push(DataChunk {
                    owner: DataChunkOwner::ClassInterfaces(idx),
                    kind: DataChunkKind::TypeList,
                    align: 4,
                    offset: 0,
                    bytes,
                });
            }
        }
    }

    fn add_static_values(&mut self, class_idx: usize, values: &[EncodedValue]) {
        let mut bytes = Vec::new();
        write_encoded_array(values, &mut bytes);
        self.chunks.push(DataChunk {
            owner: DataChunkOwner::ClassStaticValues(class_idx),
            kind: DataChunkKind::EncodedArray,
            align: 1,
            offset: 0,
            bytes,
        });
    }

    fn finish(mut self) -> DataSectionPlan {
        let mut cursor = 0u32;
        for chunk in &mut self.chunks {
            cursor = align_to(cursor, chunk.align);
            chunk.offset = cursor;
            cursor += chunk.bytes.len() as u32;
        }
        DataSectionPlan {
            chunks: self.chunks,
            size: cursor,
        }
    }
}

impl DexLayoutPlan {
    pub fn from_classes(classes: &[SmaliClass]) -> Result<Self, DexError> {
        let pools = DexIndexPools::from_classes(classes)?;
        let ids = pools.build_id_tables();

        let mut class_defs = build_class_def_plans(classes, &pools)?;
        let mut class_plan_by_idx = HashMap::new();
        for (i, plan) in class_defs.iter().enumerate() {
            class_plan_by_idx.insert(plan.class_idx, i);
        }

        let mut data_builder = DataSectionBuilder::new();
        data_builder.add_string_data(&ids.strings);
        data_builder.add_proto_parameter_lists(&ids.proto_ids);
        for class in classes {
            let class_idx = pools
                .type_index(&class.name.as_jni_type())
                .ok_or_else(|| DexError::new("missing type index for class"))?;
            let plan_idx = *class_plan_by_idx
                .get(&class_idx)
                .ok_or_else(|| DexError::new("missing class plan entry"))?;
            if let Some(values) = build_static_value_array(class, &pools)? {
                data_builder.add_static_values(plan_idx, &values);
                class_defs[plan_idx].item.static_values = Some(values);
            }
        }
        data_builder.add_class_interface_lists(&class_defs);
        let data_section = data_builder.finish();

        let sections = SectionOffsets::new(&ids, class_defs.len() as u32, data_section.size);
        let data_base = sections.data_off;

        let string_data_offsets = data_section.collect_string_offsets(ids.strings.len(), data_base);
        let proto_parameter_offsets =
            data_section.collect_proto_parameter_offsets(ids.proto_ids.len(), data_base);
        let interface_offsets = data_section.collect_interface_offsets(class_defs.len(), data_base);
        for (plan, offset) in class_defs.iter_mut().zip(interface_offsets.into_iter()) {
            if let Some(off) = offset {
                plan.offsets.interfaces_off = off;
            }
        }

        let static_value_offsets =
            data_section.collect_static_value_offsets(class_defs.len(), data_base);
        for (plan, offset) in class_defs.iter_mut().zip(static_value_offsets.into_iter()) {
            if let Some(off) = offset {
                plan.offsets.static_values_off = off;
            }
        }

        Ok(DexLayoutPlan {
            pools,
            ids,
            sections,
            data_section,
            class_defs,
            string_data_offsets,
            proto_parameter_offsets,
        })
    }
}

fn emit_dex_file(plan: DexLayoutPlan) -> Result<Vec<u8>, DexError> {
    let mut data_bytes = plan.data_section.emit_bytes();
    let aligned_len = align_to(data_bytes.len() as u32, 4) as usize;
    if data_bytes.len() < aligned_len {
        data_bytes.resize(aligned_len, 0);
    }
    let data_bytes_len = data_bytes.len() as u32;
    let map_off = plan.sections.data_off + data_bytes_len;
    let map_items = build_map_items(&plan, map_off);
    let map_bytes = write_map_list(&map_items);
    data_bytes.extend_from_slice(&map_bytes);
    let total_data_size = data_bytes.len() as u32;

    let mut file = vec![0u8; SectionOffsets::HEADER_SIZE as usize];

    write_string_ids(
        &mut file,
        &plan.sections,
        &plan.string_data_offsets,
        plan.ids.strings.len(),
    )?;
    write_type_ids(&mut file, &plan.sections, &plan.ids.type_ids)?;
    write_proto_ids(
        &mut file,
        &plan.sections,
        &plan.ids.proto_ids,
        &plan.proto_parameter_offsets,
    )?;
    write_field_ids(&mut file, &plan.sections, &plan.ids.field_ids)?;
    write_method_ids(&mut file, &plan.sections, &plan.ids.method_ids)?;
    write_class_defs(&mut file, &plan.sections, &plan.class_defs)?;

    ensure_len(&mut file, plan.sections.data_off as usize);
    file.extend_from_slice(&data_bytes);

    let mut header = Header {
        magic: DEX_FILE_MAGIC,
        checksum: 0,
        signature: [0; 20],
        file_size: file.len() as u32,
        header_size: SectionOffsets::HEADER_SIZE,
        endian_tag: ENDIAN_CONSTANT,
        link_size: 0,
        link_off: 0,
        map_off,
        string_ids_size: plan.sections.string_ids.count,
        string_ids_off: plan.sections.string_ids.offset,
        type_ids_size: plan.sections.type_ids.count,
        type_ids_off: plan.sections.type_ids.offset,
        proto_ids_size: plan.sections.proto_ids.count,
        proto_ids_off: plan.sections.proto_ids.offset,
        field_ids_size: plan.sections.field_ids.count,
        field_ids_off: plan.sections.field_ids.offset,
        method_ids_size: plan.sections.method_ids.count,
        method_ids_off: plan.sections.method_ids.offset,
        class_defs_size: plan.sections.class_defs.count,
        class_defs_off: plan.sections.class_defs.offset,
        data_size: total_data_size,
        data_off: plan.sections.data_off,
    };

    overwrite_header(&mut file, &header);
    let signature = compute_sha1(&file[32..]);
    header.signature.copy_from_slice(&signature);
    overwrite_header(&mut file, &header);
    let checksum = adler32_slice(&file[12..]);
    header.checksum = checksum;
    overwrite_header(&mut file, &header);

    Ok(file)
}

fn build_map_items(plan: &DexLayoutPlan, map_off: u32) -> Vec<MapItem> {
    let mut items = Vec::new();
    items.push(MapItem::new(TYPE_HEADER_ITEM, 1, 0));

    if plan.sections.string_ids.count > 0 {
        items.push(MapItem::new(
            TYPE_STRING_ID_ITEM,
            plan.sections.string_ids.count,
            plan.sections.string_ids.offset,
        ));
    }
    if plan.sections.type_ids.count > 0 {
        items.push(MapItem::new(
            TYPE_TYPE_ID_ITEM,
            plan.sections.type_ids.count,
            plan.sections.type_ids.offset,
        ));
    }
    if plan.sections.proto_ids.count > 0 {
        items.push(MapItem::new(
            TYPE_PROTO_ID_ITEM,
            plan.sections.proto_ids.count,
            plan.sections.proto_ids.offset,
        ));
    }
    if plan.sections.field_ids.count > 0 {
        items.push(MapItem::new(
            TYPE_FIELD_ID_ITEM,
            plan.sections.field_ids.count,
            plan.sections.field_ids.offset,
        ));
    }
    if plan.sections.method_ids.count > 0 {
        items.push(MapItem::new(
            TYPE_METHOD_ID_ITEM,
            plan.sections.method_ids.count,
            plan.sections.method_ids.offset,
        ));
    }
    if plan.sections.class_defs.count > 0 {
        items.push(MapItem::new(
            TYPE_CLASS_DEF_ITEM,
            plan.sections.class_defs.count,
            plan.sections.class_defs.offset,
        ));
    }

    let string_count = plan.string_data_offsets.len() as u32;
    if string_count > 0 {
        let first = plan
            .string_data_offsets
            .iter()
            .copied()
            .min()
            .expect("string offsets present");
        items.push(MapItem::new(TYPE_STRING_DATA_ITEM, string_count, first));
    }

    let (type_list_count, type_list_offset) = chunk_stats_by_kind(&plan, DataChunkKind::TypeList);
    if type_list_count > 0 {
        items.push(MapItem::new(
            TYPE_TYPE_LIST,
            type_list_count,
            type_list_offset.unwrap(),
        ));
    }

    let (encoded_count, encoded_offset) = chunk_stats_by_kind(&plan, DataChunkKind::EncodedArray);
    if encoded_count > 0 {
        items.push(MapItem::new(
            TYPE_ENCODED_ARRAY_ITEM,
            encoded_count,
            encoded_offset.unwrap(),
        ));
    }

    items.push(MapItem::new(TYPE_MAP_LIST, 1, map_off));
    items
}

fn chunk_stats_by_kind(plan: &DexLayoutPlan, kind: DataChunkKind) -> (u32, Option<u32>) {
    let mut count = 0;
    let mut offset: Option<u32> = None;
    for chunk in plan.data_section.chunks() {
        if chunk.kind == kind {
            count += 1;
            let absolute = plan.sections.data_off + chunk.offset;
            let new_offset = match offset {
                Some(current) => current.min(absolute),
                None => absolute,
            };
            offset = Some(new_offset);
        }
    }
    (count, offset)
}

fn write_map_list(entries: &[MapItem]) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(4 + entries.len() * 12);
    write_u4(&mut bytes, entries.len() as u32);
    for item in entries {
        write_u2(&mut bytes, item.type_code);
        write_u2(&mut bytes, 0);
        write_u4(&mut bytes, item.size);
        write_u4(&mut bytes, item.offset);
    }
    bytes
}

fn overwrite_header(buf: &mut Vec<u8>, header: &Header) {
    let mut header_bytes = Vec::with_capacity(SectionOffsets::HEADER_SIZE as usize);
    header.write(&mut header_bytes);
    buf[..SectionOffsets::HEADER_SIZE as usize].copy_from_slice(&header_bytes);
}

fn compute_sha1(data: &[u8]) -> [u8; 20] {
    let mut hasher = Sha1::new();
    hasher.update(data);
    let digest = hasher.finalize();
    let mut out = [0u8; 20];
    out.copy_from_slice(&digest);
    out
}

fn write_string_ids(
    buf: &mut Vec<u8>,
    sections: &SectionOffsets,
    offsets: &[u32],
    expected_strings: usize,
) -> Result<(), DexError> {
    if sections.string_ids.count == 0 {
        return Ok(());
    }
    if offsets.len() != expected_strings {
        return Err(DexError::new("string data offsets mismatch"));
    }
    if offsets.len() != sections.string_ids.count as usize {
        return Err(DexError::new("string ids size mismatch"));
    }
    ensure_len(buf, sections.string_ids.offset as usize);
    for offset in offsets {
        write_u4(buf, *offset);
    }
    Ok(())
}

fn write_type_ids(
    buf: &mut Vec<u8>,
    sections: &SectionOffsets,
    ids: &[u32],
) -> Result<(), DexError> {
    if sections.type_ids.count == 0 {
        return Ok(());
    }
    if ids.len() != sections.type_ids.count as usize {
        return Err(DexError::new("type ids size mismatch"));
    }
    ensure_len(buf, sections.type_ids.offset as usize);
    for idx in ids {
        write_u4(buf, *idx);
    }
    Ok(())
}

fn write_proto_ids(
    buf: &mut Vec<u8>,
    sections: &SectionOffsets,
    protos: &[PrototypeItem],
    param_offsets: &[Option<u32>],
) -> Result<(), DexError> {
    if sections.proto_ids.count == 0 {
        return Ok(());
    }
    if protos.len() != sections.proto_ids.count as usize || protos.len() != param_offsets.len() {
        return Err(DexError::new("proto ids size mismatch"));
    }
    ensure_len(buf, sections.proto_ids.offset as usize);
    for (proto, param) in protos.iter().zip(param_offsets.iter()) {
        proto.write(buf, param.unwrap_or(0));
    }
    Ok(())
}

fn write_field_ids(
    buf: &mut Vec<u8>,
    sections: &SectionOffsets,
    fields: &[FieldItem],
) -> Result<(), DexError> {
    if sections.field_ids.count == 0 {
        return Ok(());
    }
    if fields.len() != sections.field_ids.count as usize {
        return Err(DexError::new("field ids size mismatch"));
    }
    ensure_len(buf, sections.field_ids.offset as usize);
    for field in fields {
        field.write(buf);
    }
    Ok(())
}

fn write_method_ids(
    buf: &mut Vec<u8>,
    sections: &SectionOffsets,
    methods: &[MethodItem],
) -> Result<(), DexError> {
    if sections.method_ids.count == 0 {
        return Ok(());
    }
    if methods.len() != sections.method_ids.count as usize {
        return Err(DexError::new("method ids size mismatch"));
    }
    ensure_len(buf, sections.method_ids.offset as usize);
    for method in methods {
        method.write(buf);
    }
    Ok(())
}

fn write_class_defs(
    buf: &mut Vec<u8>,
    sections: &SectionOffsets,
    class_defs: &[ClassDefPlan],
) -> Result<(), DexError> {
    if sections.class_defs.count == 0 {
        return Ok(());
    }
    if class_defs.len() != sections.class_defs.count as usize {
        return Err(DexError::new("class defs size mismatch"));
    }
    ensure_len(buf, sections.class_defs.offset as usize);
    for plan in class_defs {
        plan.item.write_with_offsets(
            buf,
            plan.offsets.interfaces_off,
            plan.offsets.annotations_off,
            plan.offsets.class_data_off,
            plan.offsets.static_values_off,
        );
    }
    Ok(())
}

#[derive(Default)]
struct DexPoolBuilder {
    strings: StringCollector,
    type_descriptors: BTreeSet<String>,
    proto_keys: BTreeSet<PrototypeKey>,
    field_keys: BTreeSet<FieldKey>,
    method_keys: BTreeSet<MethodKey>,
}

impl DexPoolBuilder {
    fn ingest_class(&mut self, class: &SmaliClass) {
        let class_desc = class.name.as_jni_type();
        self.insert_type_descriptor(&class_desc);
        if let Some(source) = &class.source {
            self.strings.insert(source);
        }

        let super_desc = class.super_class.as_jni_type();
        self.insert_type_descriptor(&super_desc);

        for iface in &class.implements {
            let iface_desc = iface.as_jni_type();
            self.insert_type_descriptor(&iface_desc);
        }

        for ann in &class.annotations {
            self.collect_annotation(ann);
        }

        for field in &class.fields {
            self.collect_field(&class_desc, field);
        }

        for method in &class.methods {
            self.collect_method(&class_desc, method);
        }
    }

    fn finalize(self) -> Result<DexIndexPools, DexError> {
        let (strings, string_index) = self.strings.into_vec();

        let mut types = Vec::with_capacity(self.type_descriptors.len());
        let mut type_index = HashMap::with_capacity(self.type_descriptors.len());
        for desc in self.type_descriptors.into_iter() {
            let string_idx = *string_index.get(&desc).ok_or_else(|| {
                DexError::new(&format!("missing string for type descriptor {desc}"))
            })?;
            let idx = types.len() as u32;
            types.push(TypeEntry {
                descriptor: desc.clone(),
                string_idx,
            });
            type_index.insert(desc, idx);
        }

        let mut protos = Vec::with_capacity(self.proto_keys.len());
        let mut proto_index_map = HashMap::with_capacity(self.proto_keys.len());
        for key in self.proto_keys.into_iter() {
            let shorty_idx = *string_index
                .get(key.shorty())
                .ok_or_else(|| DexError::new("missing string for shorty"))?;
            let return_type_idx = *type_index
                .get(key.return_type())
                .ok_or_else(|| DexError::new("missing type for prototype return"))?;
            let mut params = Vec::with_capacity(key.parameters().len());
            for param in key.parameters() {
                let idx = *type_index
                    .get(param)
                    .ok_or_else(|| DexError::new("missing type for prototype param"))?;
                params.push(idx);
            }
            let idx = protos.len() as u32;
            protos.push(ProtoEntry {
                shorty_idx,
                return_type_idx,
                parameters: params,
            });
            proto_index_map.insert(key.clone(), idx);
        }

        let mut fields = Vec::with_capacity(self.field_keys.len());
        let mut field_index_map = HashMap::with_capacity(self.field_keys.len());
        for key in self.field_keys.into_iter() {
            let class_idx = *type_index
                .get(key.class_descriptor())
                .ok_or_else(|| DexError::new("missing type for field class"))?;
            let type_idx = *type_index
                .get(key.type_descriptor())
                .ok_or_else(|| DexError::new("missing type for field"))?;
            let name_idx = *string_index
                .get(key.name())
                .ok_or_else(|| DexError::new("missing string for field name"))?;
            let idx = fields.len() as u32;
            fields.push(FieldEntry {
                class_idx,
                type_idx,
                name_idx,
            });
            field_index_map.insert(key, idx);
        }

        let mut methods = Vec::with_capacity(self.method_keys.len());
        let mut method_index_map = HashMap::with_capacity(self.method_keys.len());
        for key in self.method_keys.into_iter() {
            let class_idx = *type_index
                .get(key.class_descriptor())
                .ok_or_else(|| DexError::new("missing type for method class"))?;
            let name_idx = *string_index
                .get(key.name())
                .ok_or_else(|| DexError::new("missing string for method name"))?;
            let proto_idx = *proto_index_map
                .get(key.prototype())
                .ok_or_else(|| DexError::new("missing prototype for method"))?;
            let idx = methods.len() as u32;
            methods.push(MethodEntry {
                class_idx,
                proto_idx,
                name_idx,
            });
            method_index_map.insert(key, idx);
        }

        Ok(DexIndexPools {
            strings,
            string_index,
            types,
            type_index,
            protos,
            proto_index: proto_index_map,
            fields,
            field_index: field_index_map,
            methods,
            method_index: method_index_map,
        })
    }

    fn collect_field(&mut self, class_desc: &str, field: &SmaliField) {
        self.strings.insert(&field.name);
        if let Some(init) = &field.initial_value {
            if let Some(parsed) = parse_smali_string_literal(init) {
                self.strings.insert(parsed);
            } else {
                self.strings.insert(init);
            }
        }
        self.collect_annotations(&field.annotations);

        let field_type = canonical_type_descriptor(&field.signature);
        self.insert_type_descriptor(&field_type);
        let key = FieldKey::new(class_desc.to_string(), field.name.clone(), field_type);
        self.field_keys.insert(key);
    }

    fn collect_method(&mut self, class_desc: &str, method: &SmaliMethod) {
        self.strings.insert(&method.name);
        self.collect_annotations(&method.annotations);
        for param in &method.params {
            if let Some(name) = &param.name {
                self.strings.insert(name);
            }
            self.collect_annotations(&param.annotations);
        }

        for op in &method.ops {
            match op {
                SmaliOp::Catch(catch) => {
                    if let CatchDirective::Catch { exception, .. } = catch {
                        self.insert_type_descriptor(exception);
                    }
                }
                SmaliOp::Op(dex_op) => self.collect_dex_op(dex_op),
                _ => {}
            }
        }

        let proto_key = PrototypeKey::from_signature(&method.signature);
        self.register_proto_components(&proto_key);
        self.proto_keys.insert(proto_key.clone());
        let key = MethodKey::new(class_desc.to_string(), method.name.clone(), proto_key);
        self.method_keys.insert(key);
    }

    fn collect_dex_op(&mut self, op: &DexOp) {
        match op {
            DexOp::ConstString { value, .. } | DexOp::ConstStringJumbo { value, .. } => {
                self.strings.insert(value);
            }
            DexOp::ConstClass { class, .. }
            | DexOp::CheckCast { class, .. }
            | DexOp::InstanceOf { class, .. }
            | DexOp::NewInstance { class, .. }
            | DexOp::NewArray { class, .. }
            | DexOp::FilledNewArray { class, .. }
            | DexOp::FilledNewArrayRange { class, .. } => {
                self.insert_type_descriptor(class);
            }
            DexOp::IGet { field, .. }
            | DexOp::IGetWide { field, .. }
            | DexOp::IGetObject { field, .. }
            | DexOp::IGetBoolean { field, .. }
            | DexOp::IGetByte { field, .. }
            | DexOp::IGetChar { field, .. }
            | DexOp::IGetShort { field, .. }
            | DexOp::IPut { field, .. }
            | DexOp::IPutWide { field, .. }
            | DexOp::IPutObject { field, .. }
            | DexOp::IPutBoolean { field, .. }
            | DexOp::IPutByte { field, .. }
            | DexOp::IPutChar { field, .. }
            | DexOp::IPutShort { field, .. }
            | DexOp::SGet { field, .. }
            | DexOp::SGetWide { field, .. }
            | DexOp::SGetObject { field, .. }
            | DexOp::SGetBoolean { field, .. }
            | DexOp::SGetByte { field, .. }
            | DexOp::SGetChar { field, .. }
            | DexOp::SGetShort { field, .. }
            | DexOp::SPut { field, .. }
            | DexOp::SPutWide { field, .. }
            | DexOp::SPutObject { field, .. }
            | DexOp::SPutBoolean { field, .. }
            | DexOp::SPutByte { field, .. }
            | DexOp::SPutChar { field, .. }
            | DexOp::SPutShort { field, .. } => {
                self.collect_field_ref(field);
            }
            DexOp::InvokeVirtual { method, .. }
            | DexOp::InvokeSuper { method, .. }
            | DexOp::InvokeInterface { method, .. }
            | DexOp::InvokeVirtualRange { method, .. }
            | DexOp::InvokeSuperRange { method, .. }
            | DexOp::InvokeDirectRange { method, .. }
            | DexOp::InvokeStaticRange { method, .. }
            | DexOp::InvokeInterfaceRange { method, .. }
            | DexOp::InvokeDirect { method, .. }
            | DexOp::InvokeStatic { method, .. } => {
                self.collect_method_ref(method);
            }
            DexOp::InvokePolymorphic { method, proto, .. }
            | DexOp::InvokePolymorphicRange { method, proto, .. } => {
                self.collect_method_ref(method);
                self.collect_proto_descriptor(proto);
            }
            DexOp::InvokeCustom { call_site, .. } | DexOp::InvokeCustomRange { call_site, .. } => {
                self.strings.insert(call_site);
            }
            DexOp::ConstMethodHandle { method_handle, .. } => {
                self.strings.insert(method_handle);
            }
            DexOp::ConstMethodType { proto, .. } => {
                self.collect_proto_descriptor(proto);
            }
            _ => {}
        }
    }

    fn collect_field_ref(&mut self, field: &FieldRef) {
        self.insert_type_descriptor(&field.class);
        self.insert_type_descriptor(&field.descriptor);
        self.strings.insert(&field.name);
        let key = FieldKey::new(
            field.class.clone(),
            field.name.clone(),
            field.descriptor.clone(),
        );
        self.field_keys.insert(key);
    }

    fn collect_method_ref(&mut self, method: &MethodRef) {
        self.insert_type_descriptor(&method.class);
        self.strings.insert(&method.name);
        let signature = MethodSignature::from_jni(&method.descriptor);
        let proto_key = PrototypeKey::from_signature(&signature);
        self.register_proto_components(&proto_key);
        self.proto_keys.insert(proto_key.clone());
        let key = MethodKey::new(method.class.clone(), method.name.clone(), proto_key);
        self.method_keys.insert(key);
    }

    fn collect_proto_descriptor(&mut self, descriptor: &str) {
        let signature = MethodSignature::from_jni(descriptor);
        let proto_key = PrototypeKey::from_signature(&signature);
        self.register_proto_components(&proto_key);
        self.proto_keys.insert(proto_key);
    }

    fn register_proto_components(&mut self, proto_key: &PrototypeKey) {
        self.strings.insert(proto_key.shorty());
        self.insert_type_descriptor(proto_key.return_type());
        for param in proto_key.parameters() {
            self.insert_type_descriptor(param);
        }
    }

    fn collect_annotation(&mut self, ann: &SmaliAnnotation) {
        let ty = canonical_type_descriptor(&ann.annotation_type);
        self.insert_type_descriptor(&ty);
        for element in &ann.elements {
            self.strings.insert(&element.name);
            self.collect_annotation_value(&element.value);
        }
    }

    fn collect_annotations(&mut self, annotations: &[SmaliAnnotation]) {
        for ann in annotations {
            self.collect_annotation(ann);
        }
    }

    fn collect_annotation_value(&mut self, value: &AnnotationValue) {
        match value {
            AnnotationValue::Single(v) => self.strings.insert(v),
            AnnotationValue::Array(values) => {
                for v in values {
                    self.strings.insert(v);
                }
            }
            AnnotationValue::SubAnnotation(sub) => self.collect_annotation(sub),
            AnnotationValue::Enum(obj, name) => {
                let desc = canonical_object_descriptor(obj);
                self.insert_type_descriptor(&desc);
                self.strings.insert(name);
            }
        }
    }

    fn insert_type_descriptor(&mut self, desc: &str) {
        if desc.is_empty() {
            return;
        }
        self.strings.insert(desc);
        self.type_descriptors.insert(desc.to_string());
    }
}

fn build_class_def_plans(
    classes: &[SmaliClass],
    pools: &DexIndexPools,
) -> Result<Vec<ClassDefPlan>, DexError> {
    let mut plans = Vec::with_capacity(classes.len());

    for class in classes {
        let class_desc = class.name.as_jni_type();
        let class_idx = pools
            .type_index(&class_desc)
            .ok_or_else(|| DexError::new(&format!("missing type for class {class_desc}")))?;
        let class_idx_usize = usize::try_from(class_idx)
            .map_err(|_| DexError::new("class index does not fit in usize"))?;

        let super_desc = class.super_class.as_jni_type();
        let superclass_idx = pools
            .type_index(&super_desc)
            .ok_or_else(|| DexError::new(&format!("missing type for superclass {super_desc}")))?;
        let superclass_idx_usize = usize::try_from(superclass_idx)
            .map_err(|_| DexError::new("superclass index does not fit in usize"))?;

        let access_flags = access_flags_from_modifiers(&class.modifiers);

        let source_file_idx = if let Some(source) = &class.source {
            pools
                .string_index(source)
                .map(|idx| usize::try_from(idx).expect("string idx fits usize"))
                .unwrap_or(NO_INDEX)
        } else {
            NO_INDEX
        };

        let interfaces: Vec<usize> = class
            .implements
            .iter()
            .map(|iface| type_idx_usize(pools, &iface.as_jni_type()))
            .collect::<Result<Vec<_>, _>>()?;

        let interfaces = if interfaces.is_empty() {
            None
        } else {
            Some(TypeList::from_type_ids(interfaces))
        };

        let item = ClassDefItem {
            class_idx: class_idx_usize,
            access_flags,
            superclass_idx: superclass_idx_usize,
            interfaces,
            source_file_idx,
            annotations: None,
            class_data: None,
            static_values: None,
        };

        plans.push(ClassDefPlan::new(class_idx, item));
    }

    plans.sort_by_key(|plan| plan.class_idx);
    Ok(plans)
}

fn type_idx_usize(pools: &DexIndexPools, desc: &str) -> Result<usize, DexError> {
    let idx = pools
        .type_index(desc)
        .ok_or_else(|| DexError::new(&format!("missing type for descriptor {desc}")))?;
    usize::try_from(idx).map_err(|_| DexError::new("type index does not fit in usize"))
}

fn access_flags_from_modifiers(modifiers: &[Modifier]) -> u32 {
    let mut flags = 0u32;
    for modifier in modifiers {
        match modifier {
            Modifier::Public => flags |= crate::dex::dex_file::ACC_PUBLIC,
            Modifier::Private => flags |= crate::dex::dex_file::ACC_PRIVATE,
            Modifier::Protected => flags |= crate::dex::dex_file::ACC_PROTECTED,
            Modifier::Static => flags |= crate::dex::dex_file::ACC_STATIC,
            Modifier::Final => flags |= crate::dex::dex_file::ACC_FINAL,
            Modifier::Synchronized => flags |= crate::dex::dex_file::ACC_SYNCHRONIZED,
            Modifier::Volatile => flags |= crate::dex::dex_file::ACC_VOLATILE,
            Modifier::Bridge => flags |= crate::dex::dex_file::ACC_BRIDGE,
            Modifier::Transient => flags |= crate::dex::dex_file::ACC_TRANSIENT,
            Modifier::Varargs => flags |= crate::dex::dex_file::ACC_VARARGS,
            Modifier::Native => flags |= crate::dex::dex_file::ACC_NATIVE,
            Modifier::Interface => flags |= crate::dex::dex_file::ACC_INTERFACE,
            Modifier::Abstract => flags |= crate::dex::dex_file::ACC_ABSTRACT,
            Modifier::Strict => flags |= crate::dex::dex_file::ACC_STRICT,
            Modifier::Synthetic => flags |= crate::dex::dex_file::ACC_SYNTHETIC,
            Modifier::Annotation => flags |= crate::dex::dex_file::ACC_ANNOTATION,
            Modifier::Enum => flags |= crate::dex::dex_file::ACC_ENUM,
            Modifier::Constructor => flags |= crate::dex::dex_file::ACC_CONSTRUCTOR,
            Modifier::DeclaredSynchronized => {
                flags |= crate::dex::dex_file::ACC_DECLARED_SYNCHRONIZED
            }
        }
    }
    flags
}

#[derive(Default)]
struct StringCollector(BTreeSet<String>);

impl StringCollector {
    fn insert<S: AsRef<str>>(&mut self, value: S) {
        let val = value.as_ref();
        if val.is_empty() {
            return;
        }
        self.0.insert(val.to_string());
    }

    fn into_vec(self) -> (Vec<String>, HashMap<String, u32>) {
        let mut map = HashMap::new();
        let mut vec = Vec::with_capacity(self.0.len());
        for (idx, value) in self.0.into_iter().enumerate() {
            map.insert(value.clone(), idx as u32);
            vec.push(value);
        }
        (vec, map)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
struct PrototypeKey {
    shorty: String,
    return_type: String,
    parameters: Vec<String>,
}

impl PrototypeKey {
    fn from_signature(sig: &crate::types::MethodSignature) -> Self {
        let return_type = canonical_type_descriptor(&sig.result);
        let mut parameters = Vec::with_capacity(sig.args.len());
        for arg in &sig.args {
            parameters.push(canonical_type_descriptor(arg));
        }
        let mut shorty = String::new();
        shorty.push(shorty_char(&sig.result));
        for arg in &sig.args {
            shorty.push(shorty_char(arg));
        }
        PrototypeKey {
            shorty,
            return_type,
            parameters,
        }
    }

    fn shorty(&self) -> &str {
        &self.shorty
    }

    fn return_type(&self) -> &str {
        &self.return_type
    }

    fn parameters(&self) -> &[String] {
        &self.parameters
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
struct FieldKey {
    class_descriptor: String,
    name: String,
    type_descriptor: String,
}

impl FieldKey {
    fn new(class_descriptor: String, name: String, type_descriptor: String) -> Self {
        FieldKey {
            class_descriptor,
            name,
            type_descriptor,
        }
    }

    fn class_descriptor(&self) -> &str {
        &self.class_descriptor
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn type_descriptor(&self) -> &str {
        &self.type_descriptor
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
struct MethodKey {
    class_descriptor: String,
    name: String,
    prototype: PrototypeKey,
}

impl MethodKey {
    fn new(class_descriptor: String, name: String, prototype: PrototypeKey) -> Self {
        MethodKey {
            class_descriptor,
            name,
            prototype,
        }
    }

    fn class_descriptor(&self) -> &str {
        &self.class_descriptor
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn prototype(&self) -> &PrototypeKey {
        &self.prototype
    }
}

fn shorty_char(sig: &TypeSignature) -> char {
    match sig {
        TypeSignature::Void => 'V',
        TypeSignature::Bool => 'Z',
        TypeSignature::Byte => 'B',
        TypeSignature::Char => 'C',
        TypeSignature::Short => 'S',
        TypeSignature::Int => 'I',
        TypeSignature::Long => 'J',
        TypeSignature::Float => 'F',
        TypeSignature::Double => 'D',
        _ => 'L',
    }
}

fn canonical_type_descriptor(sig: &TypeSignature) -> String {
    match sig {
        TypeSignature::Array(inner) => format!("[{}", canonical_type_descriptor(inner)),
        TypeSignature::Object(obj) => canonical_object_descriptor(obj),
        TypeSignature::Bool
        | TypeSignature::Byte
        | TypeSignature::Char
        | TypeSignature::Short
        | TypeSignature::Int
        | TypeSignature::Long
        | TypeSignature::Float
        | TypeSignature::Double
        | TypeSignature::Void => sig.to_jni(),
        TypeSignature::TypeParameters(_, rest) => canonical_type_descriptor(rest),
        TypeSignature::TypeParameter(_, inner) => canonical_type_descriptor(inner),
        TypeSignature::TypeVariableSignature(_)
        | TypeSignature::WildcardPlus
        | TypeSignature::WildcardMinus
        | TypeSignature::WildcardStar => "Ljava/lang/Object;".to_string(),
    }
}

fn canonical_object_descriptor(obj: &ObjectIdentifier) -> String {
    let mut s = String::from("L");
    s.push_str(&obj.class_name);
    if let Some(suffix) = &obj.suffix {
        s.push('.');
        s.push_str(suffix);
    }
    s.push(';');
    s
}

fn align_to(value: u32, alignment: u32) -> u32 {
    if alignment <= 1 {
        return value;
    }
    let mask = alignment - 1;
    (value + mask) & !mask
}

fn ensure_len(buf: &mut Vec<u8>, target: usize) {
    if buf.len() > target {
        panic!("buffer advanced past target offset");
    }
    if buf.len() < target {
        buf.resize(target, 0);
    }
}

fn build_static_value_array(
    class: &SmaliClass,
    pools: &DexIndexPools,
) -> Result<Option<Vec<EncodedValue>>, DexError> {
    let mut entries = Vec::new();
    let class_desc = class.name.as_jni_type();
    for field in &class.fields {
        if !field
            .modifiers
            .iter()
            .any(|m| matches!(m, Modifier::Static))
        {
            continue;
        }
        if let Some(init) = &field.initial_value {
            if let Some(value) = literal_to_encoded_value(init, &field.signature, pools)? {
                let field_type = canonical_type_descriptor(&field.signature);
                let field_idx = pools
                    .field_index(&class_desc, &field.name, &field_type)
                    .ok_or_else(|| DexError::new("missing field index for static value"))?;
                entries.push((field_idx, value));
            }
        }
    }
    if entries.is_empty() {
        return Ok(None);
    }
    entries.sort_by_key(|(idx, _)| *idx);
    Ok(Some(entries.into_iter().map(|(_, value)| value).collect()))
}

fn literal_to_encoded_value(
    literal: &str,
    signature: &TypeSignature,
    pools: &DexIndexPools,
) -> Result<Option<EncodedValue>, DexError> {
    let trimmed = literal.trim();
    match signature {
        TypeSignature::Bool => {
            if let Some(val) = parse_boolean_literal(trimmed) {
                return Ok(Some(EncodedValue::Boolean(val)));
            }
        }
        TypeSignature::Byte => {
            if let Ok((_, value)) = parse_literal_int::<i8>(trim_numeric_literal_suffix(trimmed)) {
                return Ok(Some(EncodedValue::Byte(value)));
            }
        }
        TypeSignature::Short => {
            if let Ok((_, value)) = parse_literal_int::<i16>(trim_numeric_literal_suffix(trimmed)) {
                return Ok(Some(EncodedValue::Short(value)));
            }
        }
        TypeSignature::Char => {
            if let Some(ch) = parse_char_literal(trimmed) {
                return Ok(Some(EncodedValue::Char(ch as u16)));
            }
            if let Ok((_, value)) = parse_literal_int::<i16>(trim_numeric_literal_suffix(trimmed)) {
                return Ok(Some(EncodedValue::Char(value as u16)));
            }
        }
        TypeSignature::Int => {
            if let Ok((_, value)) = parse_literal_int::<i32>(trim_numeric_literal_suffix(trimmed)) {
                return Ok(Some(EncodedValue::Int(value)));
            }
        }
        TypeSignature::Long => {
            if let Ok((_, value)) = parse_literal_int::<i64>(trim_numeric_literal_suffix(trimmed)) {
                return Ok(Some(EncodedValue::Long(value)));
            }
        }
        TypeSignature::Float => {
            if let Some(value) = parse_float_literal(trimmed) {
                return Ok(Some(EncodedValue::Float(value)));
            }
            if let Ok((_, bits)) = parse_literal_int::<i32>(trim_numeric_literal_suffix(trimmed)) {
                return Ok(Some(EncodedValue::Float(f32::from_bits(bits as u32))));
            }
        }
        TypeSignature::Double => {
            if let Some(value) = parse_double_literal(trimmed) {
                return Ok(Some(EncodedValue::Double(value)));
            }
            if let Ok((_, bits)) = parse_literal_int::<i64>(trim_numeric_literal_suffix(trimmed)) {
                return Ok(Some(EncodedValue::Double(f64::from_bits(bits as u64))));
            }
        }
        TypeSignature::Object(_) | TypeSignature::Array(_) => {
            if trimmed.eq_ignore_ascii_case("null") {
                return Ok(Some(EncodedValue::Null));
            }
            if let Some(parsed) = parse_smali_string_literal(trimmed) {
                let idx = pools
                    .string_index(&parsed)
                    .ok_or_else(|| DexError::new("missing string literal in pool"))?;
                return Ok(Some(EncodedValue::String(idx)));
            }
        }
        _ => {}
    }
    Ok(None)
}

fn parse_boolean_literal(literal: &str) -> Option<bool> {
    match literal {
        "true" | "TRUE" => Some(true),
        "false" | "FALSE" => Some(false),
        _ => None,
    }
}

fn parse_smali_string_literal(literal: &str) -> Option<String> {
    let trimmed = literal.trim();
    if trimmed.len() < 2 || !trimmed.starts_with('"') || !trimmed.ends_with('"') {
        return None;
    }
    let inner = &trimmed[1..trimmed.len() - 1];
    Some(unescape_smali_string(inner))
}

fn parse_char_literal(literal: &str) -> Option<char> {
    let trimmed = literal.trim();
    if trimmed.len() < 2 || !trimmed.starts_with('\'') || !trimmed.ends_with('\'') {
        return None;
    }
    let inner = &trimmed[1..trimmed.len() - 1];
    let decoded = unescape_smali_string(inner);
    let mut chars = decoded.chars();
    let ch = chars.next()?;
    if chars.next().is_some() {
        return None;
    }
    Some(ch)
}

fn parse_float_literal(literal: &str) -> Option<f32> {
    let trimmed = literal.trim().trim_end_matches(|c| matches!(c, 'f' | 'F'));
    trimmed.parse::<f32>().ok()
}

fn parse_double_literal(literal: &str) -> Option<f64> {
    let trimmed = literal.trim().trim_end_matches(|c| matches!(c, 'd' | 'D'));
    trimmed.parse::<f64>().ok()
}

fn unescape_smali_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    let mut chars = value.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(next) = chars.next() {
                match next {
                    'n' => out.push('\n'),
                    'r' => out.push('\r'),
                    't' => out.push('\t'),
                    '\\' => out.push('\\'),
                    '\"' => out.push('"'),
                    '\'' => out.push('\''),
                    '0' => out.push('\0'),
                    _ => {
                        out.push('\\');
                        out.push(next);
                    }
                }
            } else {
                out.push('\\');
            }
        } else {
            out.push(ch);
        }
    }
    out
}

fn trim_numeric_literal_suffix(literal: &str) -> &str {
    literal.trim_end_matches(|c: char| {
        matches!(c, 't' | 'T' | 's' | 'S' | 'l' | 'L' | 'f' | 'F' | 'd' | 'D')
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{MethodSignature, SmaliClass};

    #[test]
    fn builds_basic_indexes_from_class() {
        let smali = r#"
.class public Lcom/example/Foo;
.super Ljava/lang/Object;

.field public static THE_FIELD:Ljava/lang/String;

.method public foo(I)Ljava/lang/String;
    .locals 1
    return-object v0
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let pools = DexIndexPools::from_classes(&[class]).expect("build pools");

        assert!(pools.string_index.contains_key("Lcom/example/Foo;"));
        assert!(pools.type_index.contains_key("Lcom/example/Foo;"));
        assert_eq!(pools.fields.len(), 1);
        assert_eq!(pools.methods.len(), 1);

        let proto = &pools.protos[0];
        assert_eq!(proto.parameters.len(), 1);
    }

    #[test]
    fn collects_references_from_bytecode() {
        let smali = r#"
.class public Lcom/example/Foo;
.super Ljava/lang/Object;

.method public test()V
    .locals 2
    const-string v0, "hello"
    sget-object v1, Ljava/lang/System;->out:Ljava/io/PrintStream;
    invoke-static {v1, v0}, Lcom/example/Bar;->log(Ljava/io/PrintStream;Ljava/lang/String;)V
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let pools = DexIndexPools::from_classes(&[class]).expect("build pools");

        assert!(pools.string_index.contains_key("hello"));
        let print_stream_idx = pools
            .type_index
            .get("Ljava/io/PrintStream;")
            .expect("print stream type");
        assert!(pools.types[*print_stream_idx as usize].descriptor == "Ljava/io/PrintStream;");

        let field_entry = pools
            .fields
            .iter()
            .find(|field| pools.types[field.class_idx as usize].descriptor == "Ljava/lang/System;")
            .expect("system field");
        assert_eq!(pools.strings[field_entry.name_idx as usize], "out");
        assert_eq!(
            pools.types[field_entry.type_idx as usize].descriptor,
            "Ljava/io/PrintStream;"
        );

        let method_entry = pools
            .methods
            .iter()
            .find(|method| pools.types[method.class_idx as usize].descriptor == "Lcom/example/Bar;")
            .expect("bar method");
        assert_eq!(pools.strings[method_entry.name_idx as usize], "log");
        let proto = &pools.protos[method_entry.proto_idx as usize];
        let param_types: Vec<&str> = proto
            .parameters
            .iter()
            .map(|idx| pools.types[*idx as usize].descriptor.as_str())
            .collect();
        assert_eq!(
            param_types,
            vec!["Ljava/io/PrintStream;", "Ljava/lang/String;"]
        );
    }

    #[test]
    fn builds_id_tables_ready_for_layout() {
        let smali = r#"
.class public Lcom/example/Foo;
.super Ljava/lang/Object;

.field public static message:Ljava/lang/String; = "hi"

.method public test()V
    .locals 2
    const-string v0, "world"
    sput-object v0, Lcom/example/Foo;->message:Ljava/lang/String;
    invoke-static {v0}, Lcom/example/Bar;->log(Ljava/lang/String;)V
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let pools = DexIndexPools::from_classes(&[class]).expect("build pools");
        let tables = pools.build_id_tables();

        assert_eq!(tables.strings.len(), pools.strings.len());
        let string_idx = pools
            .string_index("Ljava/lang/String;")
            .expect("string idx");
        let type_idx = pools.type_index("Ljava/lang/String;").expect("type idx");
        assert_eq!(tables.type_ids[type_idx as usize], string_idx);

        let field_idx = pools
            .field_index("Lcom/example/Foo;", "message", "Ljava/lang/String;")
            .expect("field index");
        let field = &tables.field_ids[field_idx as usize];
        assert_eq!(
            field.class_idx as u32,
            pools.type_index("Lcom/example/Foo;").unwrap()
        );
        assert_eq!(field.type_idx as u32, type_idx);

        let method_sig = MethodSignature::from_jni("(Ljava/lang/String;)V");
        let method_idx = pools
            .method_index("Lcom/example/Bar;", "log", &method_sig)
            .expect("method index");
        let method = &tables.method_ids[method_idx as usize];
        assert_eq!(
            method.class_idx as u32,
            pools.type_index("Lcom/example/Bar;").unwrap()
        );
        let proto = &pools.protos[method.proto_idx];
        assert_eq!(proto.parameters.len(), 1);
        assert_eq!(proto.parameters[0], type_idx);
    }

    #[test]
    fn plans_section_layout_with_data_offsets() {
        let smali = r#"
.class public Lcom/example/Foo;
.super Ljava/lang/Object;
.implements Ljava/lang/Runnable;

.method public run()V
    .locals 1
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let plan = DexLayoutPlan::from_classes(&[class]).expect("layout plan");

        assert!(plan.sections.type_ids.offset > plan.sections.string_ids.offset);
        assert!(plan.sections.data_off > plan.sections.class_defs.offset);
        assert_eq!(plan.string_data_offsets.len(), plan.pools.strings.len());
        for off in &plan.string_data_offsets {
            assert!(*off >= plan.sections.data_off);
        }

        // Runnable interface should allocate a TypeList chunk and assign a non-zero offset.
        assert_eq!(plan.class_defs.len(), 1);
        assert!(plan.class_defs[0].offsets.interfaces_off >= plan.sections.data_off);
    }

    #[test]
    fn plans_static_value_offsets_for_string_fields() {
        let smali = r#"
.class public Lcom/example/HasStatic;
.super Ljava/lang/Object;

.field public static final GREETING:Ljava/lang/String; = "hello"

.method public constructor <init>()V
    .locals 0
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let plan = DexLayoutPlan::from_classes(&[class]).expect("layout plan");

        assert_eq!(plan.class_defs.len(), 1);
        let class_plan = &plan.class_defs[0];
        assert!(class_plan.item.static_values.is_some());
        assert!(class_plan.offsets.static_values_off >= plan.sections.data_off);
        assert!(plan.pools.strings.iter().any(|s| s == "hello"));
    }

    #[test]
    fn builds_binary_dex_file() {
        let smali = r#"
.class public Lcom/example/Hello;
.super Ljava/lang/Object;

.field public static final MESSAGE:Ljava/lang/String; = "hello"

.method public constructor <init>()V
    .locals 0
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let bytes = build_dex_file_bytes(&[class]).expect("build dex");

        assert!(bytes.starts_with(b"dex\n"));
        let mut ix = 0;
        let header = Header::read(&bytes, &mut ix).expect("parse header");
        assert_eq!(header.file_size as usize, bytes.len());
        assert!(header.map_off >= header.data_off);
        assert!(header.string_ids_size > 0);
    }
}
