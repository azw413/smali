/* Dex file format structures */

use std::any::Any;
use std::error::Error;
use crate::dex::error::DexError;
use crate::dex::{read_u1, read_u2, read_u4, read_uleb128, read_uleb128p1, read_x, write_u1, write_u2, write_u4, write_uleb128, write_uleb128p1, write_x};
use cesu8::to_java_cesu8;
use log::error;
use crate::dex::encoded_values::{read_encoded_array, EncodedValue};
use crate::types::{Modifiers, ObjectIdentifier, SmaliClass, SmaliField, TypeSignature};


/* Constants */
pub const DEX_FILE_MAGIC: [u8; 8] = [ 0x64, 0x65, 0x78, 0x0a, 0x30, 0x33, 0x39, 0x00 ];
pub const ENDIAN_CONSTANT: u32 = 0x12345678;
pub const REVERSE_ENDIAN_CONSTANT: u32 = 0x78563412;
pub const NO_INDEX: usize = 0xffffffff;

/* Access flags */
pub const ACC_PUBLIC: u32 = 0x1;
pub const ACC_PRIVATE: u32 = 0x2;
pub const ACC_PROTECTED: u32 = 0x4;
pub const ACC_STATIC: u32 = 0x8;
pub const ACC_FINAL: u32 = 0x10;
pub const ACC_SYNCHRONIZED: u32 = 0x20;
pub const ACC_VOLATILE: u32 = 0x40;
pub const ACC_BRIDGE: u32 = 0x40;
pub const ACC_TRANSIENT: u32 = 0x80;
pub const ACC_VARARGS: u32 = 0x80;
pub const ACC_NATIVE: u32 = 0x100;
pub const ACC_INTERFACE: u32 = 0x200;
pub const ACC_ABSTRACT: u32 = 0x400;
pub const ACC_STRICT: u32 = 0x800;
pub const ACC_SYNTHETIC: u32 = 0x1000;
pub const ACC_ANNOTATION: u32 = 0x2000;
pub const ACC_ENUM: u32 = 0x4000;
pub const ACC_CONSTRUCTOR: u32 = 0x10000;
pub const ACC_DECLARED_SYNCHRONIZED: u32 = 0x20000;





type StringId = usize;
type TypeId = StringId;
type ProtoId = usize;
type FieldId = usize;
type MethodId = usize;

#[derive(Debug)]
struct TypeList(Vec<TypeId>);

impl TypeList
{
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<crate::dex::dex_file::TypeList, DexError>
    {
        let mut v = vec![];
        let size = read_u4(bytes, ix)?;
        for _ in 0..size { v.push(read_u2(bytes, ix)? as TypeId); }
        Ok(TypeList(v))
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize
    {
        let mut c = 0;
        c += write_u4(bytes, self.0.len() as u32);
        for i in &self.0 { c += write_u2(bytes, *i as u16); }
        c
    }

}


#[derive(Debug)]
pub struct PrototypeItem {
    // The proto_id_item struct
    pub shorty_idx: StringId,
    pub return_type_idx: TypeId,
    pub parameters: TypeList
}

impl PrototypeItem
{
    pub fn to_string(&self, dex_file: &DexFile) -> Result<String, DexError>
    {
        let mut s = dex_file.strings[self.shorty_idx].to_string()?;
        s.push_str(" (");
        for t in &self.parameters.0 { s.push_str(&dex_file.strings[dex_file.types[*t]].to_string()?) ; }
        s.push_str(")");
        s.push_str(&dex_file.strings[dex_file.types[self.return_type_idx]].to_string()?) ;
        Ok(s)
    }
}


#[derive(Debug, PartialEq, Eq)]
pub struct FieldItem {
    // The field_id_item struct
    pub class_idx: TypeId,
    pub type_idx: TypeId,
    pub name_idx: StringId
}

impl FieldItem
{
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<crate::dex::dex_file::FieldItem, DexError>
    {
        Ok(FieldItem {
            class_idx: read_u2(bytes, ix)? as TypeId,
            type_idx: read_u2(bytes, ix)? as TypeId,
            name_idx: read_u4(bytes, ix)? as StringId,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize
    {
        let mut c = 0;
        c += write_u2(bytes, self.class_idx as u16);
        c += write_u2(bytes, self.type_idx as u16);
        c += write_u4(bytes, self.name_idx as u32);
        c
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MethodItem {
    // The field_id_item struct
    pub class_idx: TypeId,
    pub proto_idx: ProtoId,
    pub name_idx: StringId
}

impl crate::dex::dex_file::MethodItem
{
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<crate::dex::dex_file::MethodItem, DexError>
    {
        Ok(crate::dex::dex_file::MethodItem {
            class_idx: read_u2(bytes, ix)? as TypeId,
            proto_idx: read_u2(bytes, ix)? as ProtoId,
            name_idx: read_u4(bytes, ix)? as StringId,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize
    {
        let mut c = 0;
        c += write_u2(bytes, self.class_idx as u16);
        c += write_u2(bytes, self.proto_idx as u16);
        c += write_u4(bytes, self.name_idx as u32);
        c
    }
}

#[derive(Debug)]
pub struct EncodedField
{
    pub field_idx: FieldId,
    pub access_flags: u32
}


#[derive(Debug)]
pub struct DebugInfo
{
    pub line_start: u32,
    pub parameter_names: Vec<u32>

    // todo: Byte code follows

}

impl crate::dex::dex_file::DebugInfo
{
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<crate::dex::dex_file::DebugInfo, DexError>
    {
        let line_start = read_uleb128(bytes, ix)?;
        let parameters_size = read_uleb128(bytes, ix)?;
        let mut parameter_names = vec![];
        let mut last = 0;
        for _ in 0..parameters_size
        {
            let diff = read_uleb128p1(bytes, ix)?; // Maybe it's not a diff
            last += diff;
            parameter_names.push(last as u32);
        }

        // todo: read byte code

        Ok(DebugInfo { line_start, parameter_names })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize
    {
        let mut c = 0;
        c += write_uleb128(bytes, self.line_start);
        c += write_uleb128(bytes, self.parameter_names.len() as u32);
        let mut last = 0;
        for p in &self.parameter_names
        {
            let diff = (*p as i32) - (last as i32);
            c += write_uleb128p1(bytes, diff);
            last = *p;
        }

        // todo: write the bytecode
        c += write_u1(bytes, 0); // DBG_END_SEQUENCE

        c
    }
}



#[derive(Debug)]
pub struct CodeItem
{
    registers_size: u16,
    args_in_size: u16,
    args_out_size: u16,
    tries_size: u16,
    debug_info: Option<DebugInfo>,
    instructions: Vec<u16>,
    padding: u16,
    //todo: tries: Vec<TryItem>,
    //todo: handlers: Vec<EncodedCatchHandler>
}

impl crate::dex::dex_file::CodeItem
{
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<crate::dex::dex_file::CodeItem, DexError>
    {
        let registers_size = read_u2(bytes, ix)?;
        let args_in_size = read_u2(bytes, ix)?;
        let args_out_size = read_u2(bytes, ix)?;
        let tries_size = read_u2(bytes, ix)?;
        let mut debug_offset = read_u4(bytes, ix)? as usize;
        let debug_info = if debug_offset > 0 { Some(DebugInfo::read(bytes, &mut debug_offset)?) }
            else { None };
        let instructions_size = read_u4(bytes, ix)?;
        let mut instructions = vec![];
        for _ in 0..instructions_size { instructions.push( read_u2(bytes, ix)? ); }
        let padding = read_u2(bytes, ix)?;
        Ok(CodeItem { registers_size, args_in_size, args_out_size, tries_size, debug_info, instructions, padding } )
    }

    // Written to the data section
    pub fn write(&self, bytes: &mut Vec<u8>) -> usize
    {
        let mut c = 0;
        c += write_u2(bytes, self.registers_size);
        c += write_u2(bytes, self.args_in_size);
        c += write_u2(bytes, self.args_out_size);
        c += write_u2(bytes, 0); // todo: Write tries length later

        let debug_offset = bytes.len(); // Placeholder to patch later
        c += write_u4(bytes, 0);

        c += write_u4(bytes, self.instructions.len() as u32);
        for i in &self.instructions { c += write_u2(bytes, *i); }

        c += write_u2(bytes, self.padding);
        c += write_uleb128(bytes, 0); // todo: handler list

        // Debug info needs to be written to data and the offset updated.
        if self.debug_info.is_some()
        {
            let mut temp = vec![];
            write_u4(&mut temp, bytes.len() as u32);
            bytes[debug_offset..].clone_from_slice(temp.as_slice());
            c += self.debug_info.as_ref().unwrap().write(bytes);
        }
        else { c += write_u4(bytes, 0); }

        c
    }
}

#[derive(Debug)]
pub struct EncodedMethod
{
    pub method_idx: FieldId,
    pub access_flags: u32,
    pub code: Option<CodeItem>
}


#[derive(Debug)]
pub struct ClassDataItem {
    // The class_def_item struct
    pub static_fields: Vec<EncodedField>,
    pub instance_fields: Vec<EncodedField>,
    pub direct_methods: Vec<EncodedMethod>,
    pub virtual_methods: Vec<EncodedMethod>,
}

impl ClassDataItem
{
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<crate::dex::dex_file::ClassDataItem, DexError>
    {
        let static_field_size = read_uleb128(bytes, ix)?;
        let instance_field_size = read_uleb128(bytes, ix)?;
        let direct_method_size = read_uleb128(bytes, ix)?;
        let virtual_method_size = read_uleb128(bytes, ix)?;

        let mut static_fields = vec![];
        let mut instance_fields = vec![];
        let mut direct_methods = vec![];
        let mut virtual_methods = vec![];

        let mut offset = 0;
        for _ in 0..static_field_size {
            offset += read_uleb128(bytes, ix)?;
            static_fields.push( EncodedField { field_idx: offset as FieldId, access_flags: read_uleb128(bytes, ix)? } )
        }

        offset = 0;
        for _ in 0..instance_field_size {
            offset += read_uleb128(bytes, ix)?;
            instance_fields.push( EncodedField { field_idx: offset as FieldId, access_flags: read_uleb128(bytes, ix)? } )
        }

        offset = 0;
        for _ in 0..direct_method_size {
            offset += read_uleb128(bytes, ix)?;
            let access_flags = read_uleb128(bytes, ix)?;
            let mut code_offset = read_uleb128(bytes, ix)? as usize;
            let code = if code_offset > 0 { Some(CodeItem::read(bytes, &mut code_offset)?) }
                else { None };
            direct_methods.push( EncodedMethod { method_idx: offset as MethodId, access_flags, code } );
        }

        offset = 0;
        for _ in 0..virtual_method_size {
            offset += read_uleb128(bytes, ix)?;
            let access_flags = read_uleb128(bytes, ix)?;
            let mut code_offset = read_uleb128(bytes, ix)? as usize;
            let code = if code_offset > 0 { Some(CodeItem::read(bytes, &mut code_offset)?) }
                else { None };
            direct_methods.push( EncodedMethod { method_idx: offset as MethodId, access_flags, code } );
        }

        Ok(ClassDataItem { static_fields, instance_fields, direct_methods, virtual_methods })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize
    {
        let mut c = 0;
        c += write_uleb128(bytes, self.static_fields.len() as u32);
        c += write_uleb128(bytes, self.instance_fields.len() as u32);
        c += write_uleb128(bytes, self.direct_methods.len() as u32);
        c += write_uleb128(bytes, self.virtual_methods.len() as u32);

        let mut last = 0;
        for i in &self.static_fields {
            c += write_uleb128(bytes, (i.field_idx - last) as u32);
            last = i.field_idx;
            c += write_uleb128(bytes, i.access_flags);
        }

        let mut last = 0;
        for i in &self.instance_fields {
            c += write_uleb128(bytes, (i.field_idx - last) as u32);
            last = i.field_idx;
            c += write_uleb128(bytes, i.access_flags);
        }

        let mut last = 0;
        for i in &self.direct_methods {
            c += write_uleb128(bytes, (i.method_idx - last) as u32);
            last = i.method_idx;
            c += write_uleb128(bytes, i.access_flags);

        }

        c
    }
}



#[derive(Debug)]
pub struct ClassDefItem {
    // The class_def_item struct
    pub class_idx: TypeId,
    pub access_flags: u32,
    pub superclass_idx: TypeId,
    pub interfaces: Option<TypeList>,
    pub source_file_idx: StringId,
    //pub annotations: Vec<AnnotationItem>,
    pub class_data: Option<ClassDataItem>,
    pub static_values: Option<Vec<EncodedValue>>
}

impl ClassDefItem
{
    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<ClassDefItem, DexError>
    {
        let class_idx = read_u4(bytes, ix)? as TypeId;
        let access_flags = read_u4(bytes, ix)?;
        let superclass_idx = read_u4(bytes, ix)? as TypeId;
        let mut interface_offset = read_u4(bytes, ix)? as usize;
        let interfaces = if interface_offset > 0  { Some(TypeList::read(bytes, &mut interface_offset)?) }
            else { None };
        let source_file_idx = read_u4(bytes, ix)? as StringId;
        let mut annotations_offset = read_u4(bytes, ix)? as usize;
        // Todo: load annotations
        let mut class_data_offset = read_u4(bytes, ix)? as usize;
        let class_data = if class_data_offset > 0 { Some(ClassDataItem::read(bytes, &mut class_data_offset)?) }
            else { None };
        let mut static_values_offset = read_u4(bytes, ix)? as usize;
        let static_values = if static_values_offset > 0 { Some(read_encoded_array(bytes, &mut static_values_offset)?) }
            else { None };


        Ok(ClassDefItem {
            class_idx, access_flags,
            superclass_idx, interfaces,
            source_file_idx, class_data,
            static_values
        })

    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize
    {
        let mut c = 0;
        //c += write_x(bytes, &self.magic);

        c
    }
}


#[derive(Debug, PartialEq, Eq)]
pub struct CallSiteId {
    // The call_site_id_item struct
}

#[derive(Debug, PartialEq, Eq)]
pub struct MethodHandle {
    // The method_handle_item struct
}

#[derive(Debug)]
pub struct DexFile {
    pub header: Header,
    pub strings: Vec<DexString>,
    pub types: Vec<TypeId>,
    pub prototypes: Vec<PrototypeItem>,
    pub fields: Vec<FieldItem>,
    pub methods: Vec<MethodItem>,
    pub class_defs: Vec<ClassDefItem>,
    pub call_site_ids: Vec<CallSiteId>,
    pub method_handles: Vec<MethodHandle>,
    pub data: Vec<u8>,
    pub link_data: Vec<u8>,
}

impl DexFile {

    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<DexFile, DexError> {

        let header = Header::read(bytes, ix)?;

        let mut dex = DexFile {
            header,
            strings: vec![],
            types: vec![],
            prototypes: vec![],
            fields: vec![],
            methods: vec![],
            class_defs: vec![],
            call_site_ids: vec![],
            method_handles: vec![],
            data: vec![],
            link_data: vec![],
        };

        // Read the strings
        *ix = dex.header.string_ids_off as usize;
        for _ in 0..dex.header.string_ids_size
        {
            let mut string_id = read_u4(bytes, ix)? as usize;
            let ds = DexString::read(bytes, &mut string_id)?;
            dex.strings.push(ds);
        }

        // Read the type_ids
        *ix = dex.header.type_ids_off as usize;
        for _ in 0..dex.header.type_ids_size
        {
            let mut type_id: TypeId = read_u4(bytes, ix)? as usize;
            if let DexString::Decoded(s) = &dex.strings[type_id]
            {
                dex.types.push(type_id);
            }
            else { fail!("Invalid type description: {:?}", &dex.strings[type_id]); }
        }

        // Read the prototypes
        *ix = dex.header.proto_ids_off as usize;
        for _ in 0..dex.header.proto_ids_size
        {
            let shorty_idx = read_u4(bytes, ix)? as crate::dex::dex_file::StringId;
            let return_type_idx = read_u4(bytes, ix)? as crate::dex::dex_file::TypeId;
            let mut parameter_offset = read_u4(bytes, ix)? as usize;
            let p = PrototypeItem {
                shorty_idx, return_type_idx,
                parameters: if parameter_offset == 0 { TypeList(vec![]) }
                else { TypeList::read(bytes, &mut parameter_offset)? },
            };
            dex.prototypes.push(p);
        }

        // Read the Field ids
        *ix = dex.header.field_ids_off as usize;
        for _ in 0..dex.header.field_ids_size
        {
            dex.fields.push(FieldItem::read(bytes, ix)?);
        }

        // Read the Methods ids
        *ix = dex.header.method_ids_off as usize;
        for _ in 0..dex.header.method_ids_size
        {
            dex.methods.push(MethodItem::read(bytes, ix)?);
        }

        // Read the Class Defs
        *ix = dex.header.class_defs_off as usize;
        for _ in 0..dex.header.class_defs_size
        {
            dex.class_defs.push(ClassDefItem::read(bytes, ix)?);
        }


        Ok(dex)
    }

    fn get_string(&self, id: StringId) -> Result<String, DexError>
    {
        let name_string = &self.strings[id];
        let name = match name_string
        {
            DexString::Decoded(s) => s.to_string(),
            DexString::Raw(_, _) => return Err(DexError::new("Invalid string in class name."))
        };

        Ok(name)
    }

    fn get_type(&self, id: TypeId) -> Result<ObjectIdentifier, DexError>
    {
        let type_str = self.get_string(self.types[id])?;
        Ok(ObjectIdentifier::from_jni_type(&type_str))
    }

    pub fn to_smali(&self) -> Result<Vec<SmaliClass>, DexError>
    {
        let mut smali_classes = vec![];

        for c in &self.class_defs
        {


            let mut smali = SmaliClass {
                name: self.get_type(c.class_idx)?,
                modifiers: Modifiers::from_u32(c.access_flags),
                source: if c.source_file_idx != NO_INDEX { Some(self.get_string(c.source_file_idx)?) }
                        else { None },
                super_class: if c.superclass_idx != NO_INDEX { self.get_type(c.superclass_idx)? }
                             else { ObjectIdentifier::from_jni_type("Ljava/lang/Object;") },
                implements: vec![],
                annotations: vec![],
                fields: vec![],
                methods: vec![],
                file_path: None,
            };

            // Any interfaces?
            if let Some(tl) = &c.interfaces
            {
                for t in &tl.0 { smali.implements.push(self.get_type(*t)?); }
            }

            // Class annotations go here



            if let Some(class_data) = &c.class_data
            {
                // Fields
                for (i, f) in class_data.static_fields.iter().enumerate()
                {
                    let dex_field = &self.fields[f.field_idx];

                    smali.fields.push(SmaliField {
                        name: self.get_string(dex_field.name_idx)?,
                        modifiers: Modifiers::from_u32(f.access_flags),
                        signature: TypeSignature::from_jni(&self.get_string(self.types[dex_field.type_idx])?),
                        initial_value: if let Some(s) = &c.static_values {
                            if i < s.len() {
                                match s[i]
                                {
                                    EncodedValue::Null => None,
                                    _ => Some(s[i].to_string())
                                }
                            }
                            else { None }
                        }
                        else { None },
                        annotations: vec![],
                    });
                }
            }

            smali_classes.push(smali);
        }

        Ok(smali_classes)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Header {
    pub magic: [u8; 8],
    pub checksum: u32,
    pub signature: [u8; 20],
    pub file_size: u32,
    pub header_size: u32,
    pub endian_tag: u32,
    pub link_size: u32,
    pub link_off: u32,
    pub map_off: u32,
    pub string_ids_size: u32,
    pub string_ids_off: u32,
    pub type_ids_size: u32,
    pub type_ids_off: u32,
    pub proto_ids_size: u32,
    pub proto_ids_off: u32,
    pub field_ids_size: u32,
    pub field_ids_off: u32,
    pub method_ids_size: u32,
    pub method_ids_off: u32,
    pub class_defs_size: u32,
    pub class_defs_off: u32,
    pub data_size: u32,
    pub data_off: u32,
}

impl Header
{

    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<Header, DexError>
    {
        if bytes.len() < 0x70 {
            return Err(DexError::new("Not enough bytes for header"));
        }

        let magic = <[u8; 8]>::try_from(read_x(bytes, ix, 8)?).unwrap();
        if magic[0] != 0x64 || magic[1] != 0x65 || magic[2] != 0x78 { return Err(DexError::new("Invalid magic value")); }

        Ok(Header {
            magic,
            checksum: read_u4(bytes, ix)?,
            signature: <[u8; 20]>::try_from(read_x(bytes, ix, 20)?).unwrap(),
            file_size: read_u4(bytes, ix)?,
            header_size: read_u4(bytes, ix)?,
            endian_tag: read_u4(bytes, ix)?,
            link_size: read_u4(bytes, ix)?,
            link_off: read_u4(bytes, ix)?,
            map_off: read_u4(bytes, ix)?,
            string_ids_size: read_u4(bytes, ix)?,
            string_ids_off: read_u4(bytes, ix)?,
            type_ids_size: read_u4(bytes, ix)?,
            type_ids_off: read_u4(bytes, ix)?,
            proto_ids_size: read_u4(bytes, ix)?,
            proto_ids_off: read_u4(bytes, ix)?,
            field_ids_size: read_u4(bytes, ix)?,
            field_ids_off: read_u4(bytes, ix)?,
            method_ids_size: read_u4(bytes, ix)?,
            method_ids_off: read_u4(bytes, ix)?,
            class_defs_size: read_u4(bytes, ix)?,
            class_defs_off: read_u4(bytes, ix)?,
            data_size: read_u4(bytes, ix)?,
            data_off: read_u4(bytes, ix)?,
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize
    {
        let mut c = 0;
        c += write_x(bytes, &self.magic);
        c += write_u4(bytes, self.checksum);
        c += write_x(bytes, &self.signature);
        c += write_u4(bytes, self.file_size);
        c += write_u4(bytes, self.header_size);
        c += write_u4(bytes, self.endian_tag);
        c += write_u4(bytes, self.link_size);
        c += write_u4(bytes, self.link_off);
        c += write_u4(bytes, self.map_off);
        c += write_u4(bytes, self.file_size);
        c += write_u4(bytes, self.string_ids_size);
        c += write_u4(bytes, self.string_ids_off);
        c += write_u4(bytes, self.type_ids_size);
        c += write_u4(bytes, self.type_ids_off);
        c += write_u4(bytes, self.file_size);
        c += write_u4(bytes, self.proto_ids_size);
        c += write_u4(bytes, self.proto_ids_off);
        c += write_u4(bytes, self.field_ids_size);
        c += write_u4(bytes, self.field_ids_off);
        c += write_u4(bytes, self.method_ids_size);
        c += write_u4(bytes, self.method_ids_off);
        c += write_u4(bytes, self.class_defs_size);
        c += write_u4(bytes, self.class_defs_off);
        c += write_u4(bytes, self.data_size);
        c += write_u4(bytes, self.data_off);
        c
    }
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum DexString
{
    Decoded(String),
    Raw(u32, Vec<u8>),
}

impl DexString
{
    pub fn from_string(s: &str) -> DexString
    {
        DexString::Decoded(s.to_string())
    }

    pub fn to_string(&self) -> Result<String, DexError>
    {
        match &self
        {
            DexString::Decoded(s) => Ok(s.to_string()),
            DexString::Raw(_,_) => Err(DexError::new(
                "DexString failed conversion",
            )),
        }
    }

    pub fn read(bytes: &[u8], ix: &mut usize) -> Result<DexString, DexError>
    {
        let utf16_size = read_uleb128(bytes, ix)?;
        let mut v = vec![];

        loop
        {
            let u = read_u1(bytes, ix)?;
            if u != 0 { v.push(u); }
            else { break; }
        }

        Ok(match cesu8::from_java_cesu8(v.as_slice())
        {
            Ok(converted_str) => DexString::Decoded(converted_str.to_string()),
            _ => DexString::Raw(utf16_size, v)
        })
    }

    pub fn write(&self, bytes: &mut Vec<u8>) -> usize
    {
        let mut c = 0;

        match self
        {
            DexString::Raw(utf16_size, v) => {
                c += write_uleb128(bytes, *utf16_size);
                c += write_x(bytes, v);
                c += write_u1(bytes, 0);
            },

            DexString::Decoded(s) => {
                let encoded = to_java_cesu8(s).to_vec();
                c += write_uleb128(bytes, s.chars().count() as u32);
                c += write_x(bytes, encoded.as_slice());
                c += write_u1(bytes, 0);
            }
        }
        c
    }

    pub fn is_decoded(&self) -> bool
    {
        matches!(self, DexString::Decoded(_))
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::read;

    #[test]
    fn test_header_from_bytes()
    {
        let dex_path = "tests/classes.dex";
        let dex_bytes = read(dex_path).expect("Failed to read DEX file");
        let mut ix = 0;
        let header = Header::read(&dex_bytes, &mut ix).expect("Failed to parse DEX header");
        let mut encoded_bytes = vec![];
        let encoded_size = header.write(&mut encoded_bytes);
        ix = 0;
        let decoded = Header::read(encoded_bytes.as_slice(), &mut ix).unwrap();

        println!("{:x?}", header);

        assert_eq!(encoded_bytes.len(), 0x70);
        assert_eq!(header, decoded);
    }

    #[test]
    fn test_decode_dexfile()
    {
        let dex_path = "tests/classes.dex";
        let dex_bytes = read(dex_path).expect("Failed to read DEX file");
        let mut ix = 0;
        let dex = DexFile::read(dex_bytes.as_slice(), &mut ix).expect("Failed read");
        println!("Strings: {:} [header: {:}]", dex.strings.len(), dex.header.string_ids_size);
        println!("Types: {:} [header: {:}]", dex.types.len(), dex.header.type_ids_size);
        println!("Prototypes: {:} [header: {:}]", dex.prototypes.len(), dex.header.proto_ids_size);
        println!("Fields: {:} [header: {:}]", dex.fields.len(), dex.header.field_ids_size);
        println!("Methods: {:} [header: {:}]", dex.methods.len(), dex.header.method_ids_size);
        println!("Classes: {:} [header: {:}]", dex.class_defs.len(), dex.header.class_defs_size);

        let smali = dex.to_smali().expect("Failed to generate smali");
        println!("\n{}", &smali[3100].to_smali());

    }
}