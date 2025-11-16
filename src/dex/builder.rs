use crate::dex::annotations::AnnotationItem;
use crate::dex::dex_file::{
    ClassDataItem, ClassDefItem, CodeItem, DBG_ADVANCE_LINE, DBG_ADVANCE_PC, DBG_END_LOCAL,
    DBG_END_SEQUENCE, DBG_FIRST_SPECIAL, DBG_LINE_BASE, DBG_LINE_RANGE, DBG_RESTART_LOCAL,
    DBG_SET_EPILOGUE_BEGIN, DBG_SET_PROLOGUE_END, DBG_START_LOCAL, DBG_START_LOCAL_EXTENDED,
    DEX_FILE_MAGIC, DebugInfo, DexString, ENDIAN_CONSTANT, EncodedCatchHandler, EncodedField,
    EncodedMethod, EncodedTypeAddrPair, FieldItem, Header, MethodItem, NO_INDEX, PrototypeItem,
    TryItem, TypeList,
};
use crate::dex::encoded_values::{
    AnnotationElement as DexAnnotationElement, EncodedAnnotation, EncodedValue, write_encoded_array,
};
use crate::dex::error::DexError;
use crate::dex::opcode_format::assemble::{
    AssemblerIndexResolver, DebugDirective, DebugDirectiveEvent, LineEvent, MethodAssembler,
    MethodAssemblyResult,
};
use crate::dex::{write_sleb128, write_u2, write_u4, write_uleb128, write_uleb128p1};
use crate::smali_ops::{DexOp, FieldRef, MethodRef, parse_literal_int, unescape_smali_string};
use crate::smali_parse::parse_java_array;
use crate::types::{
    AnnotationValue, AnnotationVisibility, CatchDirective, MethodSignature, Modifier,
    ObjectIdentifier, SmaliAnnotation, SmaliClass, SmaliField, SmaliMethod, SmaliOp,
    TypeSignature, parse_methodsignature, parse_typesignature,
};

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap};
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
const TYPE_CLASS_DATA_ITEM: u16 = 0x2000;
const TYPE_MAP_LIST: u16 = 0x1000;
const TYPE_TYPE_LIST: u16 = 0x1001;
const TYPE_ANNOTATION_SET_REF_LIST: u16 = 0x1002;
const TYPE_ANNOTATION_SET_ITEM: u16 = 0x1003;
const TYPE_STRING_DATA_ITEM: u16 = 0x2002;
const TYPE_ANNOTATION_ITEM: u16 = 0x2004;
const TYPE_ANNOTATIONS_DIRECTORY_ITEM: u16 = 0x2006;
const TYPE_ENCODED_ARRAY_ITEM: u16 = 0x2005;
const TYPE_CODE_ITEM: u16 = 0x2001;

const DEFAULT_API_LEVEL: i32 = 33;
const DEFAULT_ART_VERSION: i32 = 200;

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

    annotation_elements: HashMap<String, HashMap<String, TypeSignature>>,
}

struct PoolIndexResolver<'a> {
    pools: &'a DexIndexPools,
}

impl<'a> PoolIndexResolver<'a> {
    fn new(pools: &'a DexIndexPools) -> Self {
        PoolIndexResolver { pools }
    }
}

impl<'a> AssemblerIndexResolver for PoolIndexResolver<'a> {
    fn string_index(&self, value: &str) -> Result<u32, DexError> {
        self.pools
            .string_index(value)
            .ok_or_else(|| DexError::new("missing string index"))
    }

    fn type_index(&self, descriptor: &str) -> Result<u32, DexError> {
        self.pools
            .type_index(descriptor)
            .ok_or_else(|| DexError::new("missing type index"))
    }

    fn field_index(&self, class_desc: &str, name: &str, type_desc: &str) -> Result<u32, DexError> {
        self.pools
            .field_index(class_desc, name, type_desc)
            .ok_or_else(|| DexError::new("missing field index"))
    }

    fn method_index(
        &self,
        class_desc: &str,
        name: &str,
        proto: &MethodSignature,
    ) -> Result<u32, DexError> {
        self.pools
            .method_index(class_desc, name, proto)
            .ok_or_else(|| DexError::new("missing method index"))
    }

    fn proto_index(&self, proto: &MethodSignature) -> Result<u32, DexError> {
        self.pools
            .proto_index(proto)
            .ok_or_else(|| DexError::new("missing proto index"))
    }

    fn call_site_index(&self, _name: &str) -> Result<u32, DexError> {
        Err(DexError::new(
            "call-site instructions are not supported in assembler yet",
        ))
    }

    fn method_handle_index(&self, _literal: &str) -> Result<u32, DexError> {
        Err(DexError::new(
            "method-handle instructions are not supported in assembler yet",
        ))
    }
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
    pub class_data: Option<ClassDataItem>,
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
    fixups: Vec<ChunkFixup>,
}

#[derive(Debug, Clone)]
struct ChunkFixup {
    position: usize,
    target_chunk: usize,
}

#[derive(Debug, Clone)]
struct PendingCodeAssignment {
    class_plan_idx: usize,
    method_idx: usize,
    is_direct: bool,
    chunk_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DataChunkKind {
    StringData,
    TypeList,
    EncodedArray,
    ClassData,
    AnnotationItem,
    AnnotationSet,
    AnnotationSetRefList,
    AnnotationsDirectory,
    CodeItem,
}

#[derive(Debug, Clone)]
enum DataChunkOwner {
    StringData(usize),
    ProtoParameters(usize),
    ClassInterfaces(usize),
    ClassStaticValues(usize),
    ClassData(usize),
    AnnotationItem,
    AnnotationSet,
    AnnotationSetRefList,
    ClassAnnotations {
        class_idx: usize,
        directory_rel_offset: u32,
    },
    MethodCode {
        class_idx: usize,
        method_idx: usize,
    },
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

fn cmp_proto_entries(a: &ProtoEntry, b: &ProtoEntry) -> Ordering {
    a.return_type_idx
        .cmp(&b.return_type_idx)
        .then_with(|| cmp_proto_parameters(&a.parameters, &b.parameters))
}

fn cmp_proto_parameters(a: &[u32], b: &[u32]) -> Ordering {
    for (lhs, rhs) in a.iter().zip(b.iter()) {
        match lhs.cmp(rhs) {
            Ordering::Equal => continue,
            other => return other,
        }
    }
    a.len().cmp(&b.len())
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

    fn annotation_element_type(
        &self,
        annotation_desc: &str,
        element: &str,
    ) -> Option<&TypeSignature> {
        self.annotation_elements
            .get(annotation_desc)
            .and_then(|m| m.get(element))
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
            class_data: None,
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

    fn collect_annotation_directory_offsets(&self, count: usize, base: u32) -> Vec<Option<u32>> {
        let mut out = vec![None; count];
        for chunk in &self.chunks {
            if let DataChunkOwner::ClassAnnotations {
                class_idx,
                directory_rel_offset,
            } = chunk.owner
            {
                out[class_idx] = Some(base + chunk.offset + directory_rel_offset);
            }
        }
        out
    }

    fn collect_class_data_offsets(&self, count: usize, base: u32) -> Vec<Option<u32>> {
        let mut out = vec![None; count];
        for chunk in &self.chunks {
            if let DataChunkOwner::ClassData(idx) = chunk.owner {
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
            if buf.len() > chunk.offset as usize {
                panic!(
                    "chunk {:?} (offset {}) overlaps previously written data",
                    chunk.owner, chunk.offset
                );
            }
            ensure_len(&mut buf, chunk.offset as usize);
            buf.extend_from_slice(&chunk.bytes);
        }
        buf
    }

    fn chunks(&self) -> &[DataChunk] {
        &self.chunks
    }

    fn chunks_mut(&mut self) -> &mut [DataChunk] {
        &mut self.chunks
    }

    fn apply_fixups(&mut self, base: u32) {
        let offsets: Vec<u32> = self.chunks.iter().map(|c| c.offset).collect();
        for (_idx, chunk) in self.chunks.iter_mut().enumerate() {
            for fixup in &chunk.fixups {
                let absolute = base + offsets[fixup.target_chunk];
                overwrite_u32(&mut chunk.bytes, fixup.position, absolute);
            }
            // Clear fixups after applying to avoid double application if called again.
            // (Not strictly necessary but keeps future calls idempotent.)
            if !chunk.fixups.is_empty() {
                chunk.fixups.clear();
            }
        }
    }
}

struct DataSectionBuilder {
    chunks: Vec<DataChunk>,
}

impl DataSectionBuilder {
    fn new() -> Self {
        DataSectionBuilder { chunks: Vec::new() }
    }

    fn chunk_priority(kind: DataChunkKind) -> u32 {
        match kind {
            DataChunkKind::StringData => 0,
            DataChunkKind::TypeList => 1,
            DataChunkKind::EncodedArray => 2,
            DataChunkKind::AnnotationItem => 3,
            DataChunkKind::AnnotationSetRefList => 4,
            DataChunkKind::AnnotationSet => 5,
            DataChunkKind::AnnotationsDirectory => 6,
            DataChunkKind::CodeItem => 7,
            DataChunkKind::ClassData => 8,
        }
    }

    fn reorder_chunks(&mut self) {
        let mut indexed: Vec<(usize, DataChunk)> = self.chunks.drain(..).enumerate().collect();
        indexed.sort_by_key(|(idx, chunk)| (Self::chunk_priority(chunk.kind), *idx));
        let mut remap = vec![0usize; indexed.len()];
        self.chunks.reserve(indexed.len());
        for (new_idx, (old_idx, chunk)) in indexed.into_iter().enumerate() {
            remap[old_idx] = new_idx;
            self.chunks.push(chunk);
        }
        for chunk in &mut self.chunks {
            for fixup in &mut chunk.fixups {
                fixup.target_chunk = remap[fixup.target_chunk];
            }
        }
    }

    fn recompute_offsets(&mut self) -> u32 {
        let mut cursor = 0u32;
        for chunk in &mut self.chunks {
            cursor = align_to(cursor, chunk.align);
            chunk.offset = cursor;
            cursor += chunk.bytes.len() as u32;
        }
        cursor
    }

    fn planned_offsets(&self) -> Vec<u32> {
        let mut indexed: Vec<(usize, &DataChunk)> = self.chunks.iter().enumerate().collect();
        indexed.sort_by_key(|(idx, chunk)| (Self::chunk_priority(chunk.kind), *idx));
        let mut offsets = vec![0u32; self.chunks.len()];
        let mut cursor = 0u32;
        for (original_idx, chunk) in indexed {
            cursor = align_to(cursor, chunk.align);
            offsets[original_idx] = cursor;
            cursor += chunk.bytes.len() as u32;
        }
        offsets
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
                fixups: Vec::new(),
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
                fixups: Vec::new(),
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
                    fixups: Vec::new(),
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
            fixups: Vec::new(),
        });
    }

    fn add_class_data(&mut self, class_idx: usize, class_data: &ClassDataItem) -> usize {
        let mut bytes = Vec::new();
        class_data.write(&mut bytes);
        self.push_chunk(DataChunk {
            owner: DataChunkOwner::ClassData(class_idx),
            kind: DataChunkKind::ClassData,
            align: 1,
            offset: 0,
            bytes,
            fixups: Vec::new(),
        })
    }

    fn add_code_item(
        &mut self,
        class_idx: usize,
        method_idx: usize,
        code_item: &CodeItem,
    ) -> Result<usize, DexError> {
        let mut bytes = Vec::new();
        code_item.write(&mut bytes, 0)?;
        let chunk_idx = self.push_chunk(DataChunk {
            owner: DataChunkOwner::MethodCode {
                class_idx,
                method_idx,
            },
            kind: DataChunkKind::CodeItem,
            align: 4,
            offset: 0,
            bytes,
            fixups: Vec::new(),
        });
        Ok(chunk_idx)
    }

    fn finish(mut self) -> DataSectionPlan {
        self.reorder_chunks();
        let cursor = self.recompute_offsets();
        DataSectionPlan {
            chunks: self.chunks,
            size: cursor,
        }
    }

    fn push_chunk(&mut self, chunk: DataChunk) -> usize {
        let idx = self.chunks.len();
        self.chunks.push(chunk);
        idx
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

        let preliminary_sections = SectionOffsets::new(&ids, class_defs.len() as u32, 0);
        let data_base = preliminary_sections.data_off;
        let resolver = PoolIndexResolver::new(&pools);
        let assembler = MethodAssembler::new(&resolver, DEFAULT_API_LEVEL, DEFAULT_ART_VERSION);
        let mut data_builder = DataSectionBuilder::new();
        data_builder.add_string_data(&ids.strings);
        data_builder.add_proto_parameter_lists(&ids.proto_ids);
        data_builder.add_class_interface_lists(&class_defs);
        let mut annotation_jobs = Vec::new();
        let mut pending_class_data = Vec::new();
        let mut pending_code_assignments = Vec::new();
        for (class_src_idx, class) in classes.iter().enumerate() {
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
            if let Some(class_data) = build_class_data_item(
                class,
                &pools,
                plan_idx,
                &assembler,
                &mut data_builder,
                &mut pending_code_assignments,
            )? {
                class_defs[plan_idx].class_data = Some(class_data);
                pending_class_data.push(plan_idx);
            }
            annotation_jobs.push((class_src_idx, plan_idx));
        }
        for (class_src_idx, plan_idx) in annotation_jobs {
            let class = &classes[class_src_idx];
            build_class_annotations(class, plan_idx, &pools, &mut data_builder)?;
        }

        assign_code_offsets(
            &mut data_builder,
            data_base,
            &mut class_defs,
            &pending_code_assignments,
        )?;

        for plan_idx in pending_class_data {
            if let Some(class_data) = &class_defs[plan_idx].class_data {
                data_builder.add_class_data(plan_idx, class_data);
            }
        }
        let mut data_section = data_builder.finish();

        let sections = SectionOffsets::new(&ids, class_defs.len() as u32, data_section.size);
        debug_assert_eq!(data_base, sections.data_off);

        data_section.apply_fixups(data_base);
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

        let class_data_offsets =
            data_section.collect_class_data_offsets(class_defs.len(), data_base);
        for (plan, offset) in class_defs.iter_mut().zip(class_data_offsets.into_iter()) {
            if let Some(off) = offset {
                plan.offsets.class_data_off = off;
            }
        }

        let annotation_offsets =
            data_section.collect_annotation_directory_offsets(class_defs.len(), data_base);
        for (plan, offset) in class_defs.iter_mut().zip(annotation_offsets.into_iter()) {
            if let Some(off) = offset {
                plan.offsets.annotations_off = off;
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
    let mut items = vec![MapItem::new(TYPE_HEADER_ITEM, 1, 0)];
    let mut rest = Vec::new();
    if plan.sections.string_ids.count > 0 {
        rest.push(MapItem::new(
            TYPE_STRING_ID_ITEM,
            plan.sections.string_ids.count,
            plan.sections.string_ids.offset,
        ));
    }
    if plan.sections.type_ids.count > 0 {
        rest.push(MapItem::new(
            TYPE_TYPE_ID_ITEM,
            plan.sections.type_ids.count,
            plan.sections.type_ids.offset,
        ));
    }
    if plan.sections.proto_ids.count > 0 {
        rest.push(MapItem::new(
            TYPE_PROTO_ID_ITEM,
            plan.sections.proto_ids.count,
            plan.sections.proto_ids.offset,
        ));
    }
    if plan.sections.field_ids.count > 0 {
        rest.push(MapItem::new(
            TYPE_FIELD_ID_ITEM,
            plan.sections.field_ids.count,
            plan.sections.field_ids.offset,
        ));
    }
    if plan.sections.method_ids.count > 0 {
        rest.push(MapItem::new(
            TYPE_METHOD_ID_ITEM,
            plan.sections.method_ids.count,
            plan.sections.method_ids.offset,
        ));
    }
    if plan.sections.class_defs.count > 0 {
        rest.push(MapItem::new(
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
        rest.push(MapItem::new(TYPE_STRING_DATA_ITEM, string_count, first));
    }

    let (type_list_count, type_list_offset) = chunk_stats_by_kind(&plan, DataChunkKind::TypeList);
    if type_list_count > 0 {
        rest.push(MapItem::new(
            TYPE_TYPE_LIST,
            type_list_count,
            type_list_offset.unwrap(),
        ));
    }

    let (encoded_count, encoded_offset) = chunk_stats_by_kind(&plan, DataChunkKind::EncodedArray);
    if encoded_count > 0 {
        rest.push(MapItem::new(
            TYPE_ENCODED_ARRAY_ITEM,
            encoded_count,
            encoded_offset.unwrap(),
        ));
    }

    let (code_item_count, code_item_offset) = chunk_stats_by_kind(&plan, DataChunkKind::CodeItem);
    if code_item_count > 0 {
        rest.push(MapItem::new(
            TYPE_CODE_ITEM,
            code_item_count,
            code_item_offset.unwrap(),
        ));
    }

    let (class_data_count, class_data_offset) =
        chunk_stats_by_kind(&plan, DataChunkKind::ClassData);
    if class_data_count > 0 {
        rest.push(MapItem::new(
            TYPE_CLASS_DATA_ITEM,
            class_data_count,
            class_data_offset.unwrap(),
        ));
    }

    let (set_ref_count, set_ref_offset) =
        chunk_stats_by_kind(&plan, DataChunkKind::AnnotationSetRefList);
    if set_ref_count > 0 {
        rest.push(MapItem::new(
            TYPE_ANNOTATION_SET_REF_LIST,
            set_ref_count,
            set_ref_offset.unwrap(),
        ));
    }

    let (set_count, set_offset) = chunk_stats_by_kind(&plan, DataChunkKind::AnnotationSet);
    if set_count > 0 {
        rest.push(MapItem::new(
            TYPE_ANNOTATION_SET_ITEM,
            set_count,
            set_offset.unwrap(),
        ));
    }

    let (annotation_item_count, annotation_item_offset) =
        chunk_stats_by_kind(&plan, DataChunkKind::AnnotationItem);
    if annotation_item_count > 0 {
        rest.push(MapItem::new(
            TYPE_ANNOTATION_ITEM,
            annotation_item_count,
            annotation_item_offset.unwrap(),
        ));
    }

    let (directory_count, directory_offset) =
        chunk_stats_by_kind(&plan, DataChunkKind::AnnotationsDirectory);
    if directory_count > 0 {
        rest.push(MapItem::new(
            TYPE_ANNOTATIONS_DIRECTORY_ITEM,
            directory_count,
            directory_offset.unwrap(),
        ));
    }

    rest.push(MapItem::new(TYPE_MAP_LIST, 1, map_off));
    rest.sort_by_key(|item| item.offset);
    items.extend(rest);
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
    method_keys: Vec<MethodKey>,
    method_key_set: BTreeSet<MethodKey>,
    annotation_elements: HashMap<String, HashMap<String, TypeSignature>>,
    string_aliases: HashMap<String, String>,
}

impl DexPoolBuilder {
    fn ingest_class(&mut self, class: &SmaliClass) {
        let class_desc = class.name.as_jni_type();
        self.insert_type_descriptor(&class_desc);
        if let Some(source) = &class.source {
            self.strings.insert(source);
        }

        if class
            .modifiers
            .iter()
            .any(|m| matches!(m, Modifier::Annotation))
        {
            let entry = self
                .annotation_elements
                .entry(class_desc.to_string())
                .or_default();
            for method in &class.methods {
                entry.insert(method.name.clone(), method.signature.result.clone());
            }
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
        let (strings, mut string_index) = self.strings.into_vec();
        for (raw, normalized) in self.string_aliases {
            if let Some(&idx) = string_index.get(&normalized) {
                string_index.insert(raw, idx);
            }
        }

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

        let mut proto_entries = Vec::with_capacity(self.proto_keys.len());
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
            proto_entries.push((key, ProtoEntry {
                shorty_idx,
                return_type_idx,
                parameters: params,
            }));
        }
        proto_entries.sort_by(|a, b| cmp_proto_entries(&a.1, &b.1));

        let mut protos = Vec::with_capacity(proto_entries.len());
        let mut proto_index_map = HashMap::with_capacity(proto_entries.len());
        for (idx, (key, entry)) in proto_entries.into_iter().enumerate() {
            protos.push(entry);
            proto_index_map.insert(key, idx as u32);
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
        let mut method_keys = self.method_keys;
        method_keys.sort();
        for key in method_keys.into_iter() {
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
            annotation_elements: self.annotation_elements,
        })
    }

    fn collect_field(&mut self, class_desc: &str, field: &SmaliField) {
        self.strings.insert(&field.name);
        if let Some(init) = &field.initial_value {
            self.collect_literal(init);
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
                SmaliOp::Local {
                    name,
                    descriptor,
                    signature,
                    ..
                } => {
                    if let Some(name) = name {
                        self.strings.insert(name);
                    }
                    if let Some(desc) = descriptor {
                        self.insert_type_descriptor(desc);
                    }
                    if let Some(sig) = signature {
                        self.strings.insert(sig);
                    }
                }
                _ => {}
            }
        }

        let proto_key = PrototypeKey::from_signature(&method.signature);
        self.register_proto_components(&proto_key);
        self.proto_keys.insert(proto_key.clone());
        let key = MethodKey::new(class_desc.to_string(), method.name.clone(), proto_key);
        self.insert_method_key(key);
    }

    fn insert_method_key(&mut self, key: MethodKey) {
        if self.method_key_set.insert(key.clone()) {
            self.method_keys.push(key);
        }
    }

    fn collect_dex_op(&mut self, op: &DexOp) {
        match op {
            DexOp::ConstString { value, .. } | DexOp::ConstStringJumbo { value, .. } => {
                let normalized = unescape_smali_string(value);
                if normalized != *value {
                    self.string_aliases
                        .entry(value.clone())
                        .or_insert_with(|| normalized.clone());
                }
                self.strings.insert(normalized);
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
                self.collect_call_site_literal(call_site);
            }
            DexOp::ConstMethodHandle { method_handle, .. } => {
                self.strings.insert(method_handle);
                self.collect_method_handle_literal(method_handle);
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
        self.insert_method_key(key);
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
            AnnotationValue::Single(v) => {
                if let Some(parsed) = parse_smali_string_literal(v) {
                    self.strings.insert(parsed);
                } else {
                    self.strings.insert(v);
                    if looks_like_type_descriptor(v.trim()) {
                        self.insert_type_descriptor(v.trim());
                    }
                }
            }
            AnnotationValue::Array(values) => {
                for v in values {
                    if let Some(parsed) = parse_smali_string_literal(v) {
                        self.strings.insert(parsed);
                    } else {
                        self.strings.insert(v);
                        if looks_like_type_descriptor(v.trim()) {
                            self.insert_type_descriptor(v.trim());
                        }
                    }
                }
            }
            AnnotationValue::SubAnnotation(sub) => self.collect_annotation(sub),
            AnnotationValue::Enum(obj, name) => {
                let desc = canonical_object_descriptor(obj);
                self.insert_type_descriptor(&desc);
                self.strings.insert(name);
                let key = FieldKey::new(desc.clone(), name.clone(), desc);
                self.field_keys.insert(key);
            }
        }
    }

    fn collect_method_handle_literal(&mut self, literal: &str) {
        if let Some(parsed) = parse_method_handle_descriptor(literal) {
            match parsed {
                ParsedMemberRef::Method(method) => self.collect_parsed_method(&method),
                ParsedMemberRef::Field(field) => self.collect_parsed_field(&field),
            }
        }
    }

    fn collect_call_site_literal(&mut self, literal: &str) {
        if let Some(descriptor) = parse_call_site_descriptor(literal) {
            self.collect_parsed_method(&descriptor.bootstrap);
            self.strings.insert(&descriptor.method_name);
            let proto_key = PrototypeKey::from_signature(&descriptor.method_proto);
            self.register_proto_components(&proto_key);
            self.proto_keys.insert(proto_key);
        }
    }

    fn collect_parsed_method(&mut self, method: &ParsedMethodRef) {
        self.insert_type_descriptor(&method.class_desc);
        self.strings.insert(&method.name);
        let proto_key = PrototypeKey::from_signature(&method.proto);
        self.register_proto_components(&proto_key);
        self.proto_keys.insert(proto_key.clone());
        let key = MethodKey::new(method.class_desc.clone(), method.name.clone(), proto_key);
        self.method_keys.push(key);
    }

    fn collect_parsed_field(&mut self, field: &ParsedFieldRef) {
        self.insert_type_descriptor(&field.class_desc);
        self.insert_type_descriptor(&field.type_desc);
        self.strings.insert(&field.name);
        let key = FieldKey::new(
            field.class_desc.clone(),
            field.name.clone(),
            field.type_desc.clone(),
        );
        self.field_keys.insert(key);
    }

    fn insert_type_descriptor(&mut self, desc: &str) {
        if desc.is_empty() {
            return;
        }
        self.strings.insert(desc);
        self.type_descriptors.insert(desc.to_string());
    }

    fn collect_literal(&mut self, literal: &str) {
        let trimmed = literal.trim();
        if trimmed.is_empty() {
            return;
        }
        if let Ok((_, entries)) = parse_java_array(trimmed) {
            for entry in entries {
                self.collect_literal(entry.trim());
            }
            return;
        }
        if let Some(parsed) = parse_smali_string_literal(trimmed) {
            self.strings.insert(parsed);
            return;
        }
        if looks_like_type_descriptor(trimmed) {
            self.insert_type_descriptor(trimmed);
        }
        self.strings.insert(trimmed);
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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

impl Ord for FieldKey {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.class_descriptor
            .cmp(&other.class_descriptor)
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.type_descriptor.cmp(&other.type_descriptor))
    }
}

impl PartialOrd for FieldKey {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
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

#[derive(Debug)]
struct ParsedMethodRef {
    class_desc: String,
    name: String,
    proto: MethodSignature,
}

#[derive(Debug)]
struct ParsedFieldRef {
    class_desc: String,
    name: String,
    type_desc: String,
}

#[derive(Debug)]
enum ParsedMemberRef {
    Method(ParsedMethodRef),
    Field(ParsedFieldRef),
}

#[derive(Debug)]
struct CallSiteDescriptor {
    bootstrap: ParsedMethodRef,
    method_name: String,
    method_proto: MethodSignature,
}

fn parse_method_handle_descriptor(literal: &str) -> Option<ParsedMemberRef> {
    let trimmed = literal.trim_matches('"').trim();
    if let Some(parsed) = parse_member_ref_literal(trimmed) {
        return Some(parsed);
    }
    for (idx, ch) in trimmed.char_indices() {
        if ch == 'L' {
            if let Some(parsed) = parse_member_ref_literal(&trimmed[idx..]) {
                return Some(parsed);
            }
        }
    }
    None
}

fn parse_call_site_descriptor(literal: &str) -> Option<CallSiteDescriptor> {
    let trimmed = literal.trim_matches('"').trim();
    let (bootstrap_part, target_part) = trimmed.split_once("::")?;
    let bootstrap = match parse_member_ref_literal(bootstrap_part.trim())? {
        ParsedMemberRef::Method(method) => method,
        ParsedMemberRef::Field(_) => return None,
    };
    let (method_name, method_proto) = parse_method_name_and_proto(target_part.trim())?;
    Some(CallSiteDescriptor {
        bootstrap,
        method_name,
        method_proto,
    })
}

fn parse_member_ref_literal(text: &str) -> Option<ParsedMemberRef> {
    let trimmed = text.trim();
    if !trimmed.starts_with('L') {
        return None;
    }
    let arrow_idx = trimmed.find("->")?;
    let class_desc = trimmed[..arrow_idx].trim().to_string();
    let remainder = trimmed[arrow_idx + 2..].trim();
    if let Some(paren_idx) = remainder.find('(') {
        let name = remainder[..paren_idx].trim().to_string();
        let descriptor_slice = &remainder[paren_idx..];
        let (_, signature) = parse_methodsignature(descriptor_slice).ok()?;
        return Some(ParsedMemberRef::Method(ParsedMethodRef {
            class_desc,
            name,
            proto: signature,
        }));
    }
    if let Some(colon_idx) = remainder.find(':') {
        let name = remainder[..colon_idx].trim().to_string();
        let descriptor_slice = &remainder[colon_idx + 1..];
        let (_, type_sig) = parse_typesignature(descriptor_slice).ok()?;
        let type_desc = canonical_type_descriptor(&type_sig);
        return Some(ParsedMemberRef::Field(ParsedFieldRef {
            class_desc,
            name,
            type_desc,
        }));
    }
    None
}

fn parse_method_name_and_proto(text: &str) -> Option<(String, MethodSignature)> {
    let trimmed = text.trim();
    let paren_idx = trimmed.find('(')?;
    let name = trimmed[..paren_idx].trim().to_string();
    let descriptor_slice = &trimmed[paren_idx..];
    let (_, signature) = parse_methodsignature(descriptor_slice).ok()?;
    Some((name, signature))
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

fn overwrite_u32(buf: &mut [u8], position: usize, value: u32) {
    let bytes = value.to_le_bytes();
    buf[position..position + 4].copy_from_slice(&bytes);
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

fn build_class_data_item(
    class: &SmaliClass,
    pools: &DexIndexPools,
    class_plan_idx: usize,
    assembler: &MethodAssembler<PoolIndexResolver<'_>>,
    data_builder: &mut DataSectionBuilder,
    pending_code: &mut Vec<PendingCodeAssignment>,
) -> Result<Option<ClassDataItem>, DexError> {
    let class_desc = class.name.as_jni_type();

    let mut static_fields = Vec::new();
    let mut instance_fields = Vec::new();
    for field in &class.fields {
        let field_type = canonical_type_descriptor(&field.signature);
        let field_idx = pools
            .field_index(&class_desc, &field.name, &field_type)
            .ok_or_else(|| DexError::new("missing field index for class data"))?;
        let field_idx = usize::try_from(field_idx)
            .map_err(|_| DexError::new("field index does not fit in usize"))?;
        let encoded = EncodedField {
            field_idx,
            access_flags: field_access_flags(&field.modifiers),
        };
        if field
            .modifiers
            .iter()
            .any(|m| matches!(m, Modifier::Static))
        {
            static_fields.push(encoded);
        } else {
            instance_fields.push(encoded);
        }
    }
    static_fields.sort_by_key(|f| f.field_idx);
    instance_fields.sort_by_key(|f| f.field_idx);

    let mut direct_methods = Vec::new();
    let mut virtual_methods = Vec::new();
    for method in &class.methods {
        let needs_code = method_requires_code(method);

        let method_idx = pools
            .method_index(&class_desc, &method.name, &method.signature)
            .ok_or_else(|| DexError::new("missing method index for class data"))?;
        let method_idx = usize::try_from(method_idx)
            .map_err(|_| DexError::new("method index does not fit in usize"))?;

        let mut encoded = EncodedMethod {
            method_idx,
            access_flags: method_access_flags(method),
            code_off: 0,
            code: None,
        };

        let is_direct = is_direct_method(method);
        if needs_code {
            let code_item = assemble_concrete_method(&class_desc, method, assembler, pools)?;
            let chunk_index = data_builder.add_code_item(class_plan_idx, method_idx, &code_item)?;
            pending_code.push(PendingCodeAssignment {
                class_plan_idx,
                method_idx,
                is_direct,
                chunk_index,
            });
        }

        if is_direct {
            direct_methods.push(encoded);
        } else {
            virtual_methods.push(encoded);
        }
    }
    direct_methods.sort_by_key(|m| m.method_idx);
    virtual_methods.sort_by_key(|m| m.method_idx);

    if static_fields.is_empty()
        && instance_fields.is_empty()
        && direct_methods.is_empty()
        && virtual_methods.is_empty()
    {
        return Ok(None);
    }

    Ok(Some(ClassDataItem {
        static_fields,
        instance_fields,
        direct_methods,
        virtual_methods,
    }))
}

fn build_class_annotations(
    class: &SmaliClass,
    class_plan_idx: usize,
    pools: &DexIndexPools,
    builder: &mut DataSectionBuilder,
) -> Result<(), DexError> {
    let class_desc = class.name.as_jni_type();
    let class_annotations = write_annotation_set_chunk(&class.annotations, pools, builder)?;

    let mut field_entries = Vec::new();
    for field in &class.fields {
        if field.annotations.is_empty() {
            continue;
        }
        let field_type = canonical_type_descriptor(&field.signature);
        let field_idx = pools
            .field_index(&class_desc, &field.name, &field_type)
            .ok_or_else(|| DexError::new("missing field index for annotated field"))?;
        let set_idx = write_annotation_set_chunk(&field.annotations, pools, builder)?
            .expect("annotation set created for non-empty annotations");
        field_entries.push((field_idx, set_idx));
    }

    field_entries.sort_by_key(|(idx, _)| *idx);

    let mut method_entries = Vec::new();
    let mut parameter_entries = Vec::new();

    for method in &class.methods {
        let method_idx = pools
            .method_index(&class_desc, &method.name, &method.signature)
            .ok_or_else(|| DexError::new("missing method index for annotated method"))?;

        if !method.annotations.is_empty() {
            let set_idx = write_annotation_set_chunk(&method.annotations, pools, builder)?
                .expect("annotation set created for non-empty method annotations");
            method_entries.push((method_idx, set_idx));
        }

        let mut param_set_indices = Vec::with_capacity(method.params.len());
        for param in &method.params {
            if param.annotations.is_empty() {
                param_set_indices.push(None);
            } else {
                let set_idx = write_annotation_set_chunk(&param.annotations, pools, builder)?
                    .expect("annotation set created for parameter annotations");
                param_set_indices.push(Some(set_idx));
            }
        }
        if param_set_indices.iter().any(|entry| entry.is_some()) {
            let list_idx = write_annotation_set_ref_list_chunk(&param_set_indices, builder);
            parameter_entries.push((method_idx, list_idx));
        }
    }

    method_entries.sort_by_key(|(idx, _)| *idx);
    parameter_entries.sort_by_key(|(idx, _)| *idx);

    if class_annotations.is_none()
        && field_entries.is_empty()
        && method_entries.is_empty()
        && parameter_entries.is_empty()
    {
        return Ok(());
    }

    write_annotations_directory_chunk(
        class_plan_idx,
        class_annotations,
        &field_entries,
        &method_entries,
        &parameter_entries,
        builder,
    );

    Ok(())
}

fn assign_code_offsets(
    builder: &mut DataSectionBuilder,
    data_base: u32,
    class_defs: &mut [ClassDefPlan],
    pending: &[PendingCodeAssignment],
) -> Result<(), DexError> {
    if pending.is_empty() {
        return Ok(());
    }
    let planned_offsets = builder.planned_offsets();
    for entry in pending {
        let chunk = builder
            .chunks
            .get_mut(entry.chunk_index)
            .ok_or_else(|| DexError::new("code chunk missing during offset assignment"))?;
        let relative = planned_offsets
            .get(entry.chunk_index)
            .copied()
            .ok_or_else(|| DexError::new("missing planned offset for code chunk"))?;
        let code_off = data_base
            .checked_add(relative)
            .ok_or_else(|| DexError::new("code offset overflow"))?;
        patch_debug_info_pointer(chunk, code_off)?;
        let class_data = class_defs[entry.class_plan_idx]
            .class_data
            .as_mut()
            .ok_or_else(|| DexError::new("class data missing during code offset assignment"))?;
        let methods = if entry.is_direct {
            &mut class_data.direct_methods
        } else {
            &mut class_data.virtual_methods
        };
        let encoded = methods
            .iter_mut()
            .find(|m| m.method_idx == entry.method_idx)
            .ok_or_else(|| DexError::new("encoded method missing during code offset assignment"))?;
        encoded.code_off = code_off;
    }
    Ok(())
}

fn patch_debug_info_pointer(chunk: &mut DataChunk, code_off: u32) -> Result<(), DexError> {
    if chunk.bytes.len() < 12 {
        return Ok(());
    }
    let mut buf = [0u8; 4];
    buf.copy_from_slice(&chunk.bytes[8..12]);
    let current = u32::from_le_bytes(buf);
    if current == 0 {
        return Ok(());
    }
    let absolute = code_off
        .checked_add(current)
        .ok_or_else(|| DexError::new("debug info offset overflow"))?;
    chunk.bytes[8..12].copy_from_slice(&absolute.to_le_bytes());
    Ok(())
}

const EMIT_DEBUG_INFO: bool = false;

fn assemble_concrete_method(
    class_desc: &str,
    method: &SmaliMethod,
    assembler: &MethodAssembler<PoolIndexResolver<'_>>,
    pools: &DexIndexPools,
) -> Result<CodeItem, DexError> {
    let assembly = assembler.assemble_method(class_desc, method)?;
    let (tries, handlers) = build_try_items(method, &assembly.label_offsets, pools)?;
    let debug_info = if EMIT_DEBUG_INFO {
        build_debug_info(method, pools, &assembly)?
    } else {
        None
    };
    Ok(CodeItem::new(
        assembly.registers_size,
        assembly.ins_size,
        assembly.outs_size,
        assembly.encoded_code_units,
        tries,
        handlers,
        debug_info,
    ))
}

#[derive(Default)]
struct TryHandlerAccumulator {
    typed: Vec<(String, String)>,
    catch_all: Option<String>,
}

fn build_try_items(
    method: &SmaliMethod,
    label_offsets: &HashMap<String, u32>,
    pools: &DexIndexPools,
) -> Result<(Vec<TryItem>, Vec<EncodedCatchHandler>), DexError> {
    let mut accumulators: BTreeMap<(String, String), TryHandlerAccumulator> = BTreeMap::new();

    for op in &method.ops {
        if let SmaliOp::Catch(directive) = op {
            match directive {
                CatchDirective::Catch {
                    exception,
                    try_range,
                    handler,
                } => {
                    let key = (try_range.start.0.clone(), try_range.end.0.clone());
                    let entry = accumulators.entry(key).or_default();
                    entry.typed.push((exception.clone(), handler.0.clone()));
                }
                CatchDirective::CatchAll { try_range, handler } => {
                    let key = (try_range.start.0.clone(), try_range.end.0.clone());
                    let entry = accumulators.entry(key).or_default();
                    if entry.catch_all.is_some() {
                        return Err(DexError::new("duplicate catchall for the same try range"));
                    }
                    entry.catch_all = Some(handler.0.clone());
                }
            }
        }
    }

    if accumulators.is_empty() {
        return Ok((Vec::new(), Vec::new()));
    }

    let mut handlers = Vec::with_capacity(accumulators.len());
    let mut tries = Vec::with_capacity(accumulators.len());

    for ((start_label, end_label), acc) in accumulators {
        if acc.typed.is_empty() && acc.catch_all.is_none() {
            continue;
        }
        let start = resolve_label_offset(&start_label, label_offsets, "try start")?;
        let end = resolve_label_offset(&end_label, label_offsets, "try end")?;
        if end <= start {
            return Err(DexError::new(&format!(
                "try range {:} .. {:} has non-positive length",
                start_label, end_label
            )));
        }
        let insn_span = end - start;
        if insn_span > u16::MAX as u32 {
            return Err(DexError::new(
                "try range exceeds maximum encodable instruction span",
            ));
        }

        let mut typed_pairs = Vec::with_capacity(acc.typed.len());
        for (exception_desc, handler_label) in acc.typed {
            let type_idx = pools
                .type_index(&exception_desc)
                .ok_or_else(|| DexError::new("missing type index for catch handler"))?;
            let type_idx = usize::try_from(type_idx)
                .map_err(|_| DexError::new("type index does not fit in usize"))?;
            let addr = resolve_label_offset(&handler_label, label_offsets, "catch handler")?;
            typed_pairs.push(EncodedTypeAddrPair { type_idx, addr });
        }
        typed_pairs.sort_by_key(|pair| pair.type_idx);

        let catch_all_addr = if let Some(label) = acc.catch_all {
            Some(resolve_label_offset(
                &label,
                label_offsets,
                "catchall handler",
            )?)
        } else {
            None
        };

        let handler_idx = handlers.len();
        handlers.push(EncodedCatchHandler {
            handlers: typed_pairs,
            catch_all_addr,
        });
        tries.push(TryItem {
            start_addr: start,
            insn_count: insn_span as u16,
            handler_off: 0,
            handler_idx: Some(handler_idx),
        });
    }

    Ok((tries, handlers))
}

fn resolve_label_offset(
    label: &str,
    label_offsets: &HashMap<String, u32>,
    context: &str,
) -> Result<u32, DexError> {
    label_offsets.get(label).copied().ok_or_else(|| {
        DexError::new(&format!(
            "label :{} referenced in {} not defined",
            label, context
        ))
    })
}

fn build_debug_info(
    method: &SmaliMethod,
    pools: &DexIndexPools,
    assembly: &MethodAssemblyResult,
) -> Result<Option<DebugInfo>, DexError> {
    let mut line_events = assembly.line_events.clone();
    line_events.sort_by_key(|event| event.offset_cu);
    let mut debug_events = assembly.debug_events.clone();
    debug_events.sort_by_key(|event| event.offset_cu);

    let line_start = line_events
        .first()
        .map(|event| event.line)
        .or_else(|| {
            method.ops.iter().find_map(|op| {
                if let SmaliOp::Line(line) = op {
                    Some(*line)
                } else {
                    None
                }
            })
        })
        .unwrap_or(0);

    let mut parameter_names = Vec::with_capacity(method.params.len());
    let mut has_names = false;
    for param in &method.params {
        if let Some(name) = &param.name {
            let idx = pools
                .string_index(name)
                .ok_or_else(|| DexError::new("missing string index for parameter"))?;
            parameter_names.push(Some(idx));
            has_names = true;
        } else {
            parameter_names.push(None);
        }
    }

    let has_lines = !line_events.is_empty();
    let has_debug_directives = !debug_events.is_empty();
    if !has_lines && !has_names && !has_debug_directives {
        return Ok(None);
    }

    let debug_opcodes = encode_debug_opcodes(line_start, &line_events, &debug_events, pools)?;

    Ok(Some(DebugInfo {
        line_start,
        parameter_names,
        debug_opcodes,
    }))
}

fn encode_debug_position_opcodes(events: &[LineEvent]) -> Vec<u8> {
    if events.is_empty() {
        return vec![DBG_END_SEQUENCE];
    }
    let mut ops = Vec::new();
    let mut last_pc = 0u32;
    let mut last_line = events[0].line;
    append_position_delta(
        &mut ops,
        events[0].offset_cu,
        events[0].line,
        last_pc,
        last_line,
    );
    last_pc = events[0].offset_cu;
    last_line = events[0].line;
    for event in events.iter().skip(1) {
        append_position_delta(&mut ops, event.offset_cu, event.line, last_pc, last_line);
        last_pc = event.offset_cu;
        last_line = event.line;
    }
    if ops.last().copied() != Some(DBG_END_SEQUENCE) {
        ops.push(DBG_END_SEQUENCE);
    }
    ops
}

fn append_position_delta(
    ops: &mut Vec<u8>,
    target_pc: u32,
    target_line: u32,
    last_pc: u32,
    last_line: u32,
) {
    let mut pc_delta = target_pc.saturating_sub(last_pc);
    let mut line_delta = target_line as i32 - last_line as i32;
    if pc_delta == 0 && line_delta == 0 {
        return;
    }

    while line_delta < DBG_LINE_BASE {
        ops.push(DBG_ADVANCE_LINE);
        write_sleb128(ops, -(DBG_LINE_RANGE as i32));
        line_delta += DBG_LINE_RANGE as i32;
    }
    while line_delta >= DBG_LINE_BASE + DBG_LINE_RANGE as i32 {
        ops.push(DBG_ADVANCE_LINE);
        write_sleb128(ops, DBG_LINE_RANGE as i32);
        line_delta -= DBG_LINE_RANGE as i32;
    }

    while pc_delta >= DBG_LINE_RANGE as u32 {
        let step = pc_delta - (DBG_LINE_RANGE as u32 - 1);
        ops.push(DBG_ADVANCE_PC);
        write_uleb128(ops, step);
        pc_delta -= step;
    }

    let special = DBG_FIRST_SPECIAL
        + ((line_delta - DBG_LINE_BASE) as u8)
        + (DBG_LINE_RANGE * pc_delta as u8);
    ops.push(special);
}

fn encode_debug_opcodes(
    line_start: u32,
    line_events: &[LineEvent],
    debug_events: &[DebugDirectiveEvent],
    pools: &DexIndexPools,
) -> Result<Vec<u8>, DexError> {
    let mut ops = Vec::new();
    let mut current_pc = 0u32;
    let mut current_line = line_start;
    let mut line_iter = line_events.iter().peekable();
    let mut directive_iter = debug_events.iter().peekable();

    while line_iter.peek().is_some() || directive_iter.peek().is_some() {
        let take_line = match (line_iter.peek(), directive_iter.peek()) {
            (Some(line), Some(dir)) => line.offset_cu <= dir.offset_cu,
            (Some(_), None) => true,
            (None, Some(_)) => false,
            (None, None) => false,
        };

        if take_line {
            let event = line_iter.next().unwrap();
            append_position_delta(
                &mut ops,
                event.offset_cu,
                event.line,
                current_pc,
                current_line,
            );
            current_pc = event.offset_cu;
            current_line = event.line;
        } else if let Some(event) = directive_iter.next() {
            advance_pc(&mut ops, &mut current_pc, event.offset_cu)?;
            encode_debug_directive(&mut ops, event, pools)?;
        }
    }

    if ops.last().copied() != Some(DBG_END_SEQUENCE) {
        ops.push(DBG_END_SEQUENCE);
    }
    Ok(ops)
}

fn advance_pc(ops: &mut Vec<u8>, current_pc: &mut u32, target: u32) -> Result<(), DexError> {
    if target < *current_pc {
        return Err(DexError::new(
            "debug directive cannot move backwards in code",
        ));
    }
    if target == *current_pc {
        return Ok(());
    }
    ops.push(DBG_ADVANCE_PC);
    write_uleb128(ops, target - *current_pc);
    *current_pc = target;
    Ok(())
}

fn encode_debug_directive(
    ops: &mut Vec<u8>,
    event: &DebugDirectiveEvent,
    pools: &DexIndexPools,
) -> Result<(), DexError> {
    match &event.directive {
        DebugDirective::PrologueEnd => ops.push(DBG_SET_PROLOGUE_END),
        DebugDirective::EpilogueBegin => ops.push(DBG_SET_EPILOGUE_BEGIN),
        DebugDirective::StartLocal {
            register,
            name,
            descriptor,
            signature,
        } => {
            let name_idx = lookup_string_index(pools, name.as_deref(), "local name")?;
            let type_idx = lookup_type_index(pools, descriptor.as_deref(), "local type")?;
            let reg = *register as u32;
            if let Some(sig) = signature {
                let sig_idx = lookup_string_index(pools, Some(sig), "local signature")?;
                ops.push(DBG_START_LOCAL_EXTENDED);
                write_uleb128(ops, reg);
                write_uleb128p1(ops, name_idx);
                write_uleb128p1(ops, type_idx);
                write_uleb128p1(ops, sig_idx);
            } else {
                ops.push(DBG_START_LOCAL);
                write_uleb128(ops, reg);
                write_uleb128p1(ops, name_idx);
                write_uleb128p1(ops, type_idx);
            }
        }
        DebugDirective::EndLocal { register } => {
            ops.push(DBG_END_LOCAL);
            write_uleb128(ops, *register as u32);
        }
        DebugDirective::RestartLocal { register } => {
            ops.push(DBG_RESTART_LOCAL);
            write_uleb128(ops, *register as u32);
        }
    }
    Ok(())
}

fn lookup_string_index(
    pools: &DexIndexPools,
    value: Option<&str>,
    context: &str,
) -> Result<i32, DexError> {
    if let Some(val) = value {
        pools
            .string_index(val)
            .map(|idx| idx as i32)
            .ok_or_else(|| DexError::new(&format!("missing string index for {context}")))
    } else {
        Ok(-1)
    }
}

fn lookup_type_index(
    pools: &DexIndexPools,
    value: Option<&str>,
    context: &str,
) -> Result<i32, DexError> {
    if let Some(desc) = value {
        pools
            .type_index(desc)
            .map(|idx| idx as i32)
            .ok_or_else(|| DexError::new(&format!("missing type index for {context}")))
    } else {
        Ok(-1)
    }
}

fn write_annotation_set_chunk(
    annotations: &[SmaliAnnotation],
    pools: &DexIndexPools,
    builder: &mut DataSectionBuilder,
) -> Result<Option<usize>, DexError> {
    if annotations.is_empty() {
        return Ok(None);
    }

    let mut item_entries = Vec::with_capacity(annotations.len());
    for ann in annotations {
        let item = encode_annotation_item(ann, pools)?;
        let type_idx = item.annotation.type_idx;
        let mut bytes = Vec::new();
        item.write(&mut bytes);
        let chunk_idx = builder.push_chunk(DataChunk {
            owner: DataChunkOwner::AnnotationItem,
            kind: DataChunkKind::AnnotationItem,
            align: 1,
            offset: 0,
            bytes,
            fixups: Vec::new(),
        });
        item_entries.push((type_idx, chunk_idx));
    }

    item_entries.sort_by_key(|(type_idx, _)| *type_idx);

    let mut set_bytes = Vec::new();
    write_u4(&mut set_bytes, item_entries.len() as u32);
    let mut fixups = Vec::new();
    for (_, idx) in item_entries {
        let pos = set_bytes.len();
        write_u4(&mut set_bytes, 0);
        fixups.push(ChunkFixup {
            position: pos,
            target_chunk: idx,
        });
    }

    let chunk_idx = builder.push_chunk(DataChunk {
        owner: DataChunkOwner::AnnotationSet,
        kind: DataChunkKind::AnnotationSet,
        align: 4,
        offset: 0,
        bytes: set_bytes,
        fixups,
    });

    Ok(Some(chunk_idx))
}

fn encode_annotation_item(
    annotation: &SmaliAnnotation,
    pools: &DexIndexPools,
) -> Result<AnnotationItem, DexError> {
    let annotation_payload = encode_annotation_payload(annotation, pools)?;
    let visibility = encode_annotation_visibility(&annotation.visibility);
    Ok(AnnotationItem {
        visibility,
        annotation: annotation_payload,
    })
}

fn encode_annotation_payload(
    annotation: &SmaliAnnotation,
    pools: &DexIndexPools,
) -> Result<EncodedAnnotation, DexError> {
    let type_desc = canonical_type_descriptor(&annotation.annotation_type);
    let type_idx = pools
        .type_index(&type_desc)
        .ok_or_else(|| DexError::new("missing type index for annotation"))?;
    let mut elements = Vec::with_capacity(annotation.elements.len());
    for element in &annotation.elements {
        let name_idx = pools
            .string_index(&element.name)
            .ok_or_else(|| DexError::new("missing string index for annotation element"))?;
        let expected_type = pools.annotation_element_type(&type_desc, &element.name);
        let value = encode_annotation_value(&element.value, pools, expected_type)?;
        elements.push(DexAnnotationElement { name_idx, value });
    }
    elements.sort_by_key(|el| el.name_idx);
    Ok(EncodedAnnotation { type_idx, elements })
}

fn encode_annotation_visibility(vis: &AnnotationVisibility) -> u8 {
    match vis {
        AnnotationVisibility::Build => 0x00,
        AnnotationVisibility::Runtime => 0x01,
        AnnotationVisibility::System => 0x02,
    }
}

fn encode_annotation_value(
    value: &AnnotationValue,
    pools: &DexIndexPools,
    expected_type: Option<&TypeSignature>,
) -> Result<EncodedValue, DexError> {
    match value {
        AnnotationValue::Single(literal) => {
            if let Some(sig) = expected_type {
                if let Some(encoded) = literal_to_encoded_value(literal, sig, pools)? {
                    return Ok(encoded);
                }
            }
            encode_annotation_literal(literal, pools)
        }
        AnnotationValue::Array(entries) => {
            let mut values = Vec::with_capacity(entries.len());
            let element_sig = match expected_type {
                Some(TypeSignature::Array(inner)) => Some(inner.as_ref()),
                _ => None,
            };
            for entry in entries {
                if let Some(sig) = element_sig {
                    if let Some(encoded) = literal_to_encoded_value(entry, sig, pools)? {
                        values.push(encoded);
                        continue;
                    }
                }
                values.push(encode_annotation_literal(entry, pools)?);
            }
            Ok(EncodedValue::Array(values))
        }
        AnnotationValue::SubAnnotation(sub) => {
            let payload = encode_annotation_payload(sub, pools)?;
            Ok(EncodedValue::Annotation(payload))
        }
        AnnotationValue::Enum(obj, name) => {
            let class_desc = canonical_object_descriptor(obj);
            let field_type = class_desc.clone();
            let field_idx = pools
                .field_index(&class_desc, name, &field_type)
                .ok_or_else(|| DexError::new("missing enum field index"))?;
            Ok(EncodedValue::Enum(field_idx))
        }
    }
}

fn encode_annotation_literal(
    literal: &str,
    pools: &DexIndexPools,
) -> Result<EncodedValue, DexError> {
    let trimmed = literal.trim();
    if trimmed.is_empty() {
        return Err(DexError::new("empty annotation literal"));
    }

    if let Some(ch) = parse_char_literal(trimmed) {
        return Ok(EncodedValue::Char(ch as u16));
    }

    if trimmed.eq_ignore_ascii_case("true") {
        return Ok(EncodedValue::Boolean(true));
    }
    if trimmed.eq_ignore_ascii_case("false") {
        return Ok(EncodedValue::Boolean(false));
    }
    if trimmed.eq_ignore_ascii_case("null") {
        return Ok(EncodedValue::Null);
    }

    if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
        if let Some(parsed) = parse_smali_string_literal(trimmed) {
            let idx = pools.string_index(&parsed).ok_or_else(|| {
                DexError::new(&format!("missing string literal '{}' in pool", parsed))
            })?;
            return Ok(EncodedValue::String(idx));
        }
    }

    if looks_like_type_descriptor(trimmed) {
        if let Some(idx) = pools.type_index(trimmed) {
            return Ok(EncodedValue::Type(idx));
        }
    }

    let has_float_suffix = trimmed.ends_with('f') || trimmed.ends_with('F');
    let has_double_suffix = trimmed.ends_with('d') || trimmed.ends_with('D');
    let looks_fractional = has_float_suffix
        || has_double_suffix
        || trimmed.contains('.')
        || trimmed.contains('e')
        || trimmed.contains('E')
        || trimmed.contains('p')
        || trimmed.contains('P');
    if looks_fractional {
        if has_float_suffix {
            if let Some(value) = parse_float_literal(trimmed) {
                return Ok(EncodedValue::Float(value));
            }
        }
        if let Some(value) = parse_double_literal(trimmed) {
            return Ok(EncodedValue::Double(value));
        }
        if !has_float_suffix {
            if let Some(value) = parse_float_literal(trimmed) {
                return Ok(EncodedValue::Float(value));
            }
        }
    }

    if let Ok((_, value)) = parse_literal_int::<i32>(trim_numeric_literal_suffix(trimmed)) {
        return Ok(EncodedValue::Int(value));
    }
    if let Ok((_, value)) = parse_literal_int::<i64>(trim_numeric_literal_suffix(trimmed)) {
        return Ok(EncodedValue::Long(value));
    }

    if let Some(parsed) = parse_smali_string_literal(trimmed) {
        let idx = pools.string_index(&parsed).ok_or_else(|| {
            DexError::new(&format!("missing string literal '{}' in pool", parsed))
        })?;
        return Ok(EncodedValue::String(idx));
    }

    Err(DexError::new(&format!(
        "unsupported annotation literal: {trimmed}"
    )))
}

fn looks_like_type_descriptor(value: &str) -> bool {
    if value.is_empty() {
        return false;
    }
    if value.starts_with('L') && value.ends_with(';') {
        return true;
    }
    if value.starts_with('[') {
        return true;
    }
    matches!(
        value.chars().next().unwrap(),
        'V' | 'Z' | 'B' | 'S' | 'C' | 'I' | 'J' | 'F' | 'D'
    )
}

fn write_annotation_set_ref_list_chunk(
    sets: &[Option<usize>],
    builder: &mut DataSectionBuilder,
) -> usize {
    let mut bytes = Vec::new();
    write_u4(&mut bytes, sets.len() as u32);
    let mut fixups = Vec::new();
    for set in sets {
        let pos = bytes.len();
        if let Some(idx) = set {
            write_u4(&mut bytes, 0);
            fixups.push(ChunkFixup {
                position: pos,
                target_chunk: *idx,
            });
        } else {
            write_u4(&mut bytes, 0);
        }
    }
    builder.push_chunk(DataChunk {
        owner: DataChunkOwner::AnnotationSetRefList,
        kind: DataChunkKind::AnnotationSetRefList,
        align: 4,
        offset: 0,
        bytes,
        fixups,
    })
}

fn write_annotations_directory_chunk(
    class_plan_idx: usize,
    class_annotations: Option<usize>,
    field_entries: &[(u32, usize)],
    method_entries: &[(u32, usize)],
    parameter_entries: &[(u32, usize)],
    builder: &mut DataSectionBuilder,
) {
    let mut bytes = Vec::new();
    let mut fixups = Vec::new();

    // class_annotations_off
    if let Some(idx) = class_annotations {
        let pos = bytes.len();
        write_u4(&mut bytes, 0);
        fixups.push(ChunkFixup {
            position: pos,
            target_chunk: idx,
        });
    } else {
        write_u4(&mut bytes, 0);
    }

    write_u4(&mut bytes, field_entries.len() as u32);
    write_u4(&mut bytes, method_entries.len() as u32);
    write_u4(&mut bytes, parameter_entries.len() as u32);

    for (field_idx, set_idx) in field_entries {
        write_u4(&mut bytes, *field_idx);
        let pos = bytes.len();
        write_u4(&mut bytes, 0);
        fixups.push(ChunkFixup {
            position: pos,
            target_chunk: *set_idx,
        });
    }

    for (method_idx, set_idx) in method_entries {
        write_u4(&mut bytes, *method_idx);
        let pos = bytes.len();
        write_u4(&mut bytes, 0);
        fixups.push(ChunkFixup {
            position: pos,
            target_chunk: *set_idx,
        });
    }

    for (method_idx, list_idx) in parameter_entries {
        write_u4(&mut bytes, *method_idx);
        let pos = bytes.len();
        write_u4(&mut bytes, 0);
        fixups.push(ChunkFixup {
            position: pos,
            target_chunk: *list_idx,
        });
    }

    builder.push_chunk(DataChunk {
        owner: DataChunkOwner::ClassAnnotations {
            class_idx: class_plan_idx,
            directory_rel_offset: 0,
        },
        kind: DataChunkKind::AnnotationsDirectory,
        align: 4,
        offset: 0,
        bytes,
        fixups,
    });
}

fn field_access_flags(modifiers: &[Modifier]) -> u32 {
    let mut flags = 0u32;
    for modifier in modifiers {
        match modifier {
            Modifier::Public => flags |= crate::dex::dex_file::ACC_PUBLIC,
            Modifier::Private => flags |= crate::dex::dex_file::ACC_PRIVATE,
            Modifier::Protected => flags |= crate::dex::dex_file::ACC_PROTECTED,
            Modifier::Static => flags |= crate::dex::dex_file::ACC_STATIC,
            Modifier::Final => flags |= crate::dex::dex_file::ACC_FINAL,
            Modifier::Volatile => flags |= crate::dex::dex_file::ACC_VOLATILE,
            Modifier::Transient => flags |= crate::dex::dex_file::ACC_TRANSIENT,
            Modifier::Synthetic => flags |= crate::dex::dex_file::ACC_SYNTHETIC,
            Modifier::Enum => flags |= crate::dex::dex_file::ACC_ENUM,
            _ => {}
        }
    }
    flags
}

fn method_access_flags(method: &SmaliMethod) -> u32 {
    let mut flags = 0u32;
    for modifier in &method.modifiers {
        match modifier {
            Modifier::Public => flags |= crate::dex::dex_file::ACC_PUBLIC,
            Modifier::Private => flags |= crate::dex::dex_file::ACC_PRIVATE,
            Modifier::Protected => flags |= crate::dex::dex_file::ACC_PROTECTED,
            Modifier::Static => flags |= crate::dex::dex_file::ACC_STATIC,
            Modifier::Final => flags |= crate::dex::dex_file::ACC_FINAL,
            Modifier::Synchronized => flags |= crate::dex::dex_file::ACC_SYNCHRONIZED,
            Modifier::Bridge => flags |= crate::dex::dex_file::ACC_BRIDGE,
            Modifier::Varargs => flags |= crate::dex::dex_file::ACC_VARARGS,
            Modifier::Native => flags |= crate::dex::dex_file::ACC_NATIVE,
            Modifier::Abstract => flags |= crate::dex::dex_file::ACC_ABSTRACT,
            Modifier::Strict => flags |= crate::dex::dex_file::ACC_STRICT,
            Modifier::Synthetic => flags |= crate::dex::dex_file::ACC_SYNTHETIC,
            Modifier::DeclaredSynchronized => {
                flags |= crate::dex::dex_file::ACC_DECLARED_SYNCHRONIZED
            }
            _ => {}
        }
    }
    if method.constructor {
        flags |= crate::dex::dex_file::ACC_CONSTRUCTOR;
    }
    flags
}

fn method_requires_code(method: &SmaliMethod) -> bool {
    let is_abstract = method
        .modifiers
        .iter()
        .any(|m| matches!(m, Modifier::Abstract));
    let is_native = method
        .modifiers
        .iter()
        .any(|m| matches!(m, Modifier::Native));
    !(is_abstract || is_native)
}

fn is_direct_method(method: &SmaliMethod) -> bool {
    let is_static = method
        .modifiers
        .iter()
        .any(|m| matches!(m, Modifier::Static));
    let is_private = method
        .modifiers
        .iter()
        .any(|m| matches!(m, Modifier::Private));
    is_static || is_private || method.constructor
}

fn literal_to_encoded_value(
    literal: &str,
    signature: &TypeSignature,
    pools: &DexIndexPools,
) -> Result<Option<EncodedValue>, DexError> {
    let trimmed = literal.trim();

    if let TypeSignature::Array(inner) = signature {
        if let Ok((_, entries)) = parse_java_array(trimmed) {
            let mut values = Vec::with_capacity(entries.len());
            for entry in entries {
                let value = literal_to_encoded_value(entry.trim(), inner, pools)?
                    .ok_or_else(|| DexError::new("array literal element missing value"))?;
                values.push(value);
            }
            return Ok(Some(EncodedValue::Array(values)));
        }
    }

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
            if let Ok((_, value)) = parse_literal_int::<i32>(trim_numeric_literal_suffix(trimmed)) {
                let clamped = value & 0xFFFF;
                return Ok(Some(EncodedValue::Char(clamped as u16)));
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
                let idx = pools.string_index(&parsed).ok_or_else(|| {
                    DexError::new(&format!("missing string literal '{}' in pool", parsed))
                })?;
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

fn trim_numeric_literal_suffix(literal: &str) -> &str {
    literal.trim_end_matches(|c: char| {
        matches!(c, 't' | 'T' | 's' | 'S' | 'l' | 'L' | 'f' | 'F' | 'd' | 'D')
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dex::dex_file::{
        DBG_END_LOCAL, DBG_RESTART_LOCAL, DBG_SET_EPILOGUE_BEGIN, DBG_SET_PROLOGUE_END,
        DBG_START_LOCAL, DBG_START_LOCAL_EXTENDED, DexFile,
    };
    use crate::types::{MethodSignature, SmaliClass};

    const ABSTRACT_CLASS: &str = r#"
.class public abstract Lcom/example/AbstractThing;
.super Ljava/lang/Object;
.implements Ljava/lang/Runnable;

.field public static final GREETING:Ljava/lang/String; = "hello"
.field public static final FLAG:Z = true
.field public value:I

.method public static native helper()V
.end method

.method public abstract compute()I
.end method
"#;

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
        let class = SmaliClass::from_smali(ABSTRACT_CLASS).expect("parse class");
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
        assert!(plan.class_defs[0].offsets.class_data_off >= plan.sections.data_off);
    }

    #[test]
    fn plans_static_value_offsets_for_string_fields() {
        let class = SmaliClass::from_smali(ABSTRACT_CLASS).expect("parse class");
        let plan = DexLayoutPlan::from_classes(&[class]).expect("layout plan");

        assert_eq!(plan.class_defs.len(), 1);
        let class_plan = &plan.class_defs[0];
        assert!(class_plan.item.static_values.is_some());
        assert!(class_plan.offsets.static_values_off >= plan.sections.data_off);
        assert!(plan.pools.strings.iter().any(|s| s == "hello"));
        assert!(class_plan.class_data.is_some());
        assert!(class_plan.offsets.class_data_off >= plan.sections.data_off);
        assert!(
            !class_plan
                .class_data
                .as_ref()
                .unwrap()
                .static_fields
                .is_empty()
        );
    }

    #[test]
    fn builds_binary_dex_file() {
        let class = SmaliClass::from_smali(ABSTRACT_CLASS).expect("parse class");
        let bytes = build_dex_file_bytes(&[class]).expect("build dex");

        assert!(bytes.starts_with(b"dex\n"));
        let mut ix = 0;
        let header = Header::read(&bytes, &mut ix).expect("parse header");
        assert_eq!(header.file_size as usize, bytes.len());
        assert!(header.map_off >= header.data_off);
        assert!(header.string_ids_size > 0);
    }

    #[test]
    fn roundtrips_static_numeric_and_array_literals() {
        let smali = r#"
.class public abstract Lcom/example/Literals;
.super Ljava/lang/Object;

.field public static final COUNT:I = 0x2a
.field public static final BIG:J = 0x100000000L
.field public static final FACTOR:F = 1.5f
.field public static final SCALE:D = 3.5
.field public static final LETTER:C = 'A'
.field public static final WORD:Ljava/lang/String; = "hello"
.field public static final INTS:[I = { 0x1, 0x2, 0x3 }
.field public static final WORDS:[Ljava/lang/String; = { "one", "two" }

.method public abstract noop()V
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let bytes = build_dex_file_bytes(&[class]).expect("build dex");

        let dex = DexFile::from_bytes(&bytes).expect("read dex");
        assert_eq!(dex.class_defs.len(), 1);
        let class_def = &dex.class_defs[0];
        let class_data = class_def
            .class_data
            .as_ref()
            .expect("class data present for literals");
        let static_values = class_def
            .static_values
            .as_ref()
            .expect("static values encoded");
        assert_eq!(class_data.static_fields.len(), static_values.len());

        for (field_entry, value) in class_data.static_fields.iter().zip(static_values.iter()) {
            let field = &dex.fields[field_entry.field_idx as usize];
            let name = dex.strings[field.name_idx as usize]
                .to_string()
                .expect("field name string");
            match (name.as_str(), value) {
                ("COUNT", EncodedValue::Int(v)) => assert_eq!(*v, 0x2a),
                ("BIG", EncodedValue::Long(v)) => assert_eq!(*v, 0x100000000),
                ("FACTOR", EncodedValue::Float(v)) => assert!((*v - 1.5).abs() < f32::EPSILON),
                ("SCALE", EncodedValue::Double(v)) => assert!((*v - 3.5).abs() < f64::EPSILON),
                ("LETTER", EncodedValue::Char(v)) => assert_eq!(*v, 'A' as u16),
                ("WORD", EncodedValue::String(idx)) => {
                    let s = dex.strings[*idx as usize].to_string().expect("word string");
                    assert_eq!(s, "hello");
                }
                ("INTS", EncodedValue::Array(values)) => {
                    let ints: Vec<i32> = values
                        .iter()
                        .map(|v| match v {
                            EncodedValue::Int(i) => *i,
                            _ => panic!("expected int in array"),
                        })
                        .collect();
                    assert_eq!(ints, vec![1, 2, 3]);
                }
                ("WORDS", EncodedValue::Array(values)) => {
                    let words: Vec<String> = values
                        .iter()
                        .map(|v| match v {
                            EncodedValue::String(idx) => {
                                dex.strings[*idx as usize].to_string().expect("word")
                            }
                            _ => panic!("expected string in array"),
                        })
                        .collect();
                    assert_eq!(words, vec!["one", "two"]);
                }
                other => panic!("unexpected literal {:?}", other),
            }
        }
    }

    const ANNOTATED_CLASS: &str = r#"
.class public abstract Lcom/example/Annotated;
.super Ljava/lang/Object;

.annotation runtime Lcom/example/ClassAnn;
    value = 0x2
.end annotation

.field public static final FLAG:Z = true
    .annotation system Lcom/example/FieldAnn;
        name = "field"
    .end annotation
.end field

.method public static native note(I)V
    .annotation runtime Lcom/example/MethodAnn;
        value = "method"
    .end annotation
    .param p0
        .annotation runtime Lcom/example/ParamAnn;
            name = "param"
        .end annotation
    .end param
.end method
"#;

    #[test]
    fn plans_annotation_offsets_and_chunks() {
        let class = SmaliClass::from_smali(ANNOTATED_CLASS).expect("parse class");
        let plan = DexLayoutPlan::from_classes(&[class]).expect("layout plan");

        assert_eq!(plan.class_defs.len(), 1);
        let class_plan = &plan.class_defs[0];
        assert!(class_plan.offsets.annotations_off >= plan.sections.data_off);

        let kinds: Vec<DataChunkKind> = plan
            .data_section
            .chunks()
            .iter()
            .map(|chunk| chunk.kind)
            .collect();

        assert!(
            kinds
                .iter()
                .any(|k| matches!(k, DataChunkKind::AnnotationItem))
        );
        assert!(
            kinds
                .iter()
                .any(|k| matches!(k, DataChunkKind::AnnotationSet))
        );
        assert!(
            kinds
                .iter()
                .any(|k| matches!(k, DataChunkKind::AnnotationsDirectory))
        );
    }

    #[test]
    fn plans_methods_with_code_items() {
        let smali = r#"
.class public Lcom/example/HasCode;
.super Ljava/lang/Object;

.method public constructor <init>()V
    .locals 1
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
"#;

        let plan_class = SmaliClass::from_smali(smali).expect("parse class");
        let plan = DexLayoutPlan::from_classes(&[plan_class]).expect("layout plan with code");

        assert!(
            plan.data_section
                .chunks()
                .iter()
                .any(|chunk| matches!(chunk.kind, DataChunkKind::CodeItem))
        );

        let class_data = plan.class_defs[0]
            .class_data
            .as_ref()
            .expect("class data present");
        assert!(plan.class_defs[0].offsets.class_data_off >= plan.sections.data_off);
        let has_method = class_data.direct_methods.iter().any(|m| m.code_off > 0)
            || class_data.virtual_methods.iter().any(|m| m.code_off > 0);
        assert!(has_method, "expected at least one method with code");

        let dex_class = SmaliClass::from_smali(smali).expect("parse class for dex");
        let bytes = build_dex_file_bytes(&[dex_class]).expect("build dex");
        let dex = DexFile::from_bytes(&bytes).expect("read dex");
        let class_def = &dex.class_defs[0];
        let cd = class_def.class_data.as_ref().expect("class data in dex");
        let method = cd
            .direct_methods
            .iter()
            .chain(cd.virtual_methods.iter())
            .find(|m| m.code.is_some())
            .expect("method with code in dex");
        assert!(method.code_off > 0);
    }

    #[test]
    fn assembles_try_catch_blocks() {
        let smali = r#"
.class public Lcom/example/Catcher;
.super Ljava/lang/Object;

.method public static demo()V
    .locals 1
    :try_start
    const/4 v0, 0x0
    goto :try_end
    :handler
    move-exception v0
    :try_end
    return-void
.catch Ljava/lang/Exception; {:try_start .. :try_end} :handler
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let bytes = build_dex_file_bytes(&[class]).expect("build dex");
        let dex = DexFile::from_bytes(&bytes).expect("read dex");
        let class_def = &dex.class_defs[0];
        let data = class_def.class_data.as_ref().expect("class data");
        let method = data
            .direct_methods
            .iter()
            .chain(data.virtual_methods.iter())
            .find(|m| m.code.is_some())
            .expect("method with code");
        assert!(method.code_off > 0);
    }

    #[test]
    fn encodes_debug_line_events() {
        let smali = r#"
.class public Lcom/example/Debuggable;
.super Ljava/lang/Object;

.method public static doWork()V
    .locals 1
    .line 10
    const/4 v0, 0x0
    .line 20
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let bytes = build_dex_file_bytes(&[class]).expect("build dex");
        let dex = DexFile::from_bytes(&bytes).expect("read dex");
        let class_def = &dex.class_defs[0];
        let data = class_def.class_data.as_ref().expect("class data");
        let method = data
            .direct_methods
            .iter()
            .find(|m| m.code.is_some())
            .expect("method with code");
        let code = method.code.as_ref().expect("code item");
        assert!(code.debug_info().is_some());
    }

    #[test]
    fn encodes_debug_local_directives() {
        let smali = r#"
.class public Lcom/example/DebugLocals;
.super Ljava/lang/Object;

.method public static trace()V
    .locals 2
    .prologue
    .line 1
    .local v0, "value":I, "Ljava/lang/Integer;"
    const/4 v0, 0x0
    .line 2
    .end local v0
    .line 3
    .restart local v0
    .epilogue
    return-void
.end method
"#;

        let class = SmaliClass::from_smali(smali).expect("parse class");
        let bytes = build_dex_file_bytes(&[class]).expect("build dex");
        let dex = DexFile::from_bytes(&bytes).expect("read dex");
        let class_def = &dex.class_defs[0];
        let data = class_def.class_data.as_ref().expect("class data");
        let method = data
            .direct_methods
            .iter()
            .find(|m| m.code.is_some())
            .expect("method with code");
        let debug = method
            .code
            .as_ref()
            .and_then(|code| code.debug_info())
            .expect("debug info");
        let opcodes = &debug.debug_opcodes;
        assert!(
            opcodes.contains(&DBG_START_LOCAL) || opcodes.contains(&DBG_START_LOCAL_EXTENDED),
            "missing start-local opcode"
        );
        assert!(opcodes.contains(&DBG_END_LOCAL), "missing end-local opcode");
        assert!(
            opcodes.contains(&DBG_RESTART_LOCAL),
            "missing restart-local opcode"
        );
        assert!(
            opcodes.contains(&DBG_SET_PROLOGUE_END),
            "missing prologue marker"
        );
        assert!(
            opcodes.contains(&DBG_SET_EPILOGUE_BEGIN),
            "missing epilogue marker"
        );
    }

    #[test]
    fn parses_method_handle_descriptor_variants() {
        let literal =
            "invoke-static {}, Lcom/example/Target;->doIt(Ljava/lang/String;)Ljava/lang/Object;";
        let parsed =
            parse_method_handle_descriptor(literal).expect("method handle descriptor parsed");
        match parsed {
            ParsedMemberRef::Method(method) => {
                assert_eq!(method.class_desc, "Lcom/example/Target;");
                assert_eq!(method.name, "doIt");
                assert_eq!(
                    method.proto.to_jni(),
                    "(Ljava/lang/String;)Ljava/lang/Object;"
                );
            }
            ParsedMemberRef::Field(_) => panic!("expected method"),
        }

        let field_literal = "static-get Lcom/example/Holder;->VALUE:Ljava/lang/String;";
        let parsed = parse_method_handle_descriptor(field_literal).expect("field descriptor");
        match parsed {
            ParsedMemberRef::Field(field) => {
                assert_eq!(field.class_desc, "Lcom/example/Holder;");
                assert_eq!(field.name, "VALUE");
                assert_eq!(field.type_desc, "Ljava/lang/String;");
            }
            ParsedMemberRef::Method(_) => panic!("expected field"),
        }
    }

    #[test]
    fn collects_method_handle_and_call_site_literals_into_pools() {
        let class = SmaliClass::from_smali(HANDLE_CLASS).expect("parse class");
        let pools = DexIndexPools::from_classes(&[class]).expect("build pools");

        let handle_sig = MethodSignature::from_jni("(Ljava/lang/String;)Ljava/lang/Object;");
        assert!(
            pools
                .method_index("Lcom/example/Target;", "doIt", &handle_sig)
                .is_some()
        );

        let bootstrap_sig = MethodSignature::from_jni(
            "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;",
        );
        assert!(
            pools
                .method_index("Lcom/example/Bootstrap;", "bootstrap", &bootstrap_sig)
                .is_some()
        );

        let call_site_proto =
            MethodSignature::from_jni("(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;");
        assert!(pools.proto_index(&call_site_proto).is_some());
        assert!(pools.string_index("makeConcat").is_some());
    }

    const HANDLE_CLASS: &str = r#"
.class public Lcom/example/Handles;
.super Ljava/lang/Object;

.method public static bridge()V
    .locals 1
    const-method-handle v0, "invoke-static {}, Lcom/example/Target;->doIt(Ljava/lang/String;)Ljava/lang/Object;"
    invoke-custom {v0}, "Lcom/example/Bootstrap;->bootstrap(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;::makeConcat(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"
    return-void
.end method
"#;
}
