//! DEX annotations-related structures and I/O
//!
//! Implements the core binary structures from the DEX spec:
//! - annotations_directory_item
//! - annotation_set_item
//! - annotation_set_ref_list
//! - annotation_item (wraps EncodedAnnotation)

use crate::dex::encoded_values::EncodedAnnotation;
use crate::dex::error::DexError;
use crate::dex::{read_u1, read_u4, write_u1, write_u4};

/// annotation_item
/// https://source.android.com/docs/core/runtime/dex-format#annotation-item
#[derive(Debug, Clone, PartialEq)]
pub struct AnnotationItem {
    /// Visibility: 0x00 = build, 0x01 = runtime, 0x02 = system
    pub visibility: u8,
    /// The encoded annotation payload
    pub annotation: EncodedAnnotation,
}

impl AnnotationItem {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<AnnotationItem, DexError> {
        let visibility = read_u1(bytes, ix)?;
        let annotation = EncodedAnnotation::read(bytes, ix)?;
        Ok(AnnotationItem {
            visibility,
            annotation,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_u1(bytes, self.visibility);
        c += self.annotation.write(bytes);
        c
    }
}

/// annotation_set_item
/// A list of offsets to `annotation_item`s
/// https://source.android.com/docs/core/runtime/dex-format#annotation-set-item
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnotationSetItem {
    /// Offsets (from start of the file) to `annotation_item`s
    pub entries: Vec<u32>,
}

impl AnnotationSetItem {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<AnnotationSetItem, DexError> {
        let size = read_u4(bytes, ix)? as usize;
        let mut entries = Vec::with_capacity(size);
        for _ in 0..size {
            entries.push(read_u4(bytes, ix)?);
        }
        Ok(AnnotationSetItem { entries })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_u4(bytes, self.entries.len() as u32);
        for off in &self.entries {
            c += write_u4(bytes, *off);
        }
        c
    }
}

/// annotation_set_ref_list
/// A list of offsets to `annotation_set_item`s
/// https://source.android.com/docs/core/runtime/dex-format#annotation-set-ref-list
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnotationSetRefList {
    pub list: Vec<u32>,
}

impl AnnotationSetRefList {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<AnnotationSetRefList, DexError> {
        let size = read_u4(bytes, ix)? as usize;
        let mut list = Vec::with_capacity(size);
        for _ in 0..size {
            list.push(read_u4(bytes, ix)?);
        }
        Ok(AnnotationSetRefList { list })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_u4(bytes, self.list.len() as u32);
        for off in &self.list {
            c += write_u4(bytes, *off);
        }
        c
    }
}

/// field_annotations_item
/// https://source.android.com/docs/core/runtime/dex-format#field-annotation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAnnotations {
    /// index into field_ids
    pub field_idx: u32,
    /// offset to an `annotation_set_item`
    pub annotations_off: u32,
}

impl FieldAnnotations {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<FieldAnnotations, DexError> {
        Ok(FieldAnnotations {
            field_idx: read_u4(bytes, ix)?,
            annotations_off: read_u4(bytes, ix)?,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        write_u4(bytes, self.field_idx) + write_u4(bytes, self.annotations_off)
    }
}

/// method_annotations_item
/// https://source.android.com/docs/core/runtime/dex-format#method-annotation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodAnnotations {
    /// index into method_ids
    pub method_idx: u32,
    /// offset to an `annotation_set_item`
    pub annotations_off: u32,
}

impl MethodAnnotations {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<MethodAnnotations, DexError> {
        Ok(MethodAnnotations {
            method_idx: read_u4(bytes, ix)?,
            annotations_off: read_u4(bytes, ix)?,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        write_u4(bytes, self.method_idx) + write_u4(bytes, self.annotations_off)
    }
}

/// parameter_annotations_item
/// https://source.android.com/docs/core/runtime/dex-format#parameter-annotation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterAnnotations {
    /// index into method_ids (method whose parameters are being annotated)
    pub method_idx: u32,
    /// offset to an `annotation_set_ref_list`
    pub annotations_off: u32,
}

impl ParameterAnnotations {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<ParameterAnnotations, DexError> {
        Ok(ParameterAnnotations {
            method_idx: read_u4(bytes, ix)?,
            annotations_off: read_u4(bytes, ix)?,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        write_u4(bytes, self.method_idx) + write_u4(bytes, self.annotations_off)
    }
}

/// annotations_directory_item
/// https://source.android.com/docs/core/runtime/dex-format#annotations-directory-item
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnotationsDirectoryItem {
    /// Offset to the class `annotation_set_item` (or 0 if none)
    pub class_annotations_off: u32,
    pub field_annotations: Vec<FieldAnnotations>,
    pub method_annotations: Vec<MethodAnnotations>,
    pub parameter_annotations: Vec<ParameterAnnotations>,
}

impl AnnotationsDirectoryItem {
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<AnnotationsDirectoryItem, DexError> {
        let class_annotations_off = read_u4(bytes, ix)?;
        let fields_size = read_u4(bytes, ix)? as usize;
        let annotated_methods_size = read_u4(bytes, ix)? as usize;
        let annotated_parameters_size = read_u4(bytes, ix)? as usize;

        let mut field_annotations = Vec::with_capacity(fields_size);
        for _ in 0..fields_size {
            field_annotations.push(FieldAnnotations::read(bytes, ix)?);
        }

        let mut method_annotations = Vec::with_capacity(annotated_methods_size);
        for _ in 0..annotated_methods_size {
            method_annotations.push(MethodAnnotations::read(bytes, ix)?);
        }

        let mut parameter_annotations = Vec::with_capacity(annotated_parameters_size);
        for _ in 0..annotated_parameters_size {
            parameter_annotations.push(ParameterAnnotations::read(bytes, ix)?);
        }

        Ok(AnnotationsDirectoryItem {
            class_annotations_off,
            field_annotations,
            method_annotations,
            parameter_annotations,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize {
        let mut c = 0;
        c += write_u4(bytes, self.class_annotations_off);
        c += write_u4(bytes, self.field_annotations.len() as u32);
        c += write_u4(bytes, self.method_annotations.len() as u32);
        c += write_u4(bytes, self.parameter_annotations.len() as u32);

        for fa in &self.field_annotations {
            c += fa.write(bytes);
        }
        for ma in &self.method_annotations {
            c += ma.write(bytes);
        }
        for pa in &self.parameter_annotations {
            c += pa.write(bytes);
        }
        c
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_annotation_item_roundtrip() {
        // A tiny encoded annotation: type_idx=3, size=0
        // EncodedAnnotation::read/write is tested elsewhere; we just exercise framing here.
        let enc = EncodedAnnotation {
            type_idx: 3,
            elements: vec![],
        };
        let item = AnnotationItem {
            visibility: 1,
            annotation: enc,
        };

        let mut buf = vec![];
        let _n = item.write(&mut buf);
        let mut ix = 0;
        let item2 = AnnotationItem::read(&buf, &mut ix).expect("read failed");
        assert_eq!(ix, buf.len());
        assert_eq!(item, item2);
    }

    #[test]
    fn test_annotation_set_item_roundtrip() {
        let set = AnnotationSetItem {
            entries: vec![0x10, 0x20, 0x30],
        };
        let mut buf = vec![];
        let _ = set.write(&mut buf);
        let mut ix = 0;
        let set2 = AnnotationSetItem::read(&buf, &mut ix).expect("read failed");
        assert_eq!(ix, buf.len());
        assert_eq!(set, set2);
    }

    #[test]
    fn test_annotations_directory_roundtrip() {
        let dir = AnnotationsDirectoryItem {
            class_annotations_off: 0x1000,
            field_annotations: vec![FieldAnnotations {
                field_idx: 1,
                annotations_off: 0x2000,
            }],
            method_annotations: vec![MethodAnnotations {
                method_idx: 2,
                annotations_off: 0x3000,
            }],
            parameter_annotations: vec![ParameterAnnotations {
                method_idx: 3,
                annotations_off: 0x4000,
            }],
        };
        let mut buf = vec![];
        let _ = dir.write(&mut buf);
        let mut ix = 0;
        let dir2 = AnnotationsDirectoryItem::read(&buf, &mut ix).expect("read failed");
        assert_eq!(ix, buf.len());
        assert_eq!(dir, dir2);
    }
}
