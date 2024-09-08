//! Representation of the Dalvik bytecodes

use std::{
    fmt::Debug,
};
use crate::types::{ObjectIdentifier, TypeSignature};

#[derive(Debug)]
#[allow(missing_docs)]
pub enum DexInstruction {
    Nop,
    Move(u8, u8),
    MoveFrom16(u8, u16),
    Move16(u16, u16),
    MoveWide(u8, u8),
    MoveWideFrom16(u8, u16),
    MoveWide16(u16, u16),
    MoveObject(u8, u8),
    MoveObjectFrom16(u8, u16),
    MoveObject16(u16, u16),
    MoveResult(u8),
    MoveResultWide(u8),
    MoveResultObject(u8),
    MoveException(u8),
    ReturnVoid,
    Return(u8),
    ReturnWide(u8),
    ReturnObject(u8),
    Const4(u8, i32),
    Const16(u8, i32),
    Const(u8, i32),
    ConstHigh16(u8, i32),
    ConstWide16(u8, i64),
    ConstWide32(u8, i64),
    ConstWide(u8, i64),
    ConstWideHigh16(u8, i64),
    ConstString(u8, String),
    ConstStringJumbo(u8, String),
    ConstClass(u8, ObjectIdentifier),
    MonitorEnter(u8),
    MonitorExit(u8),
    CheckCast(u8, TypeSignature),
    InstanceOf(u8, u8, TypeSignature),
    ArrayLength(u8, u8),
    NewInstance(u8, TypeSignature),
    NewArray(u8, u8, TypeSignature),
    FilledNewArray(Vec<u8>, TypeSignature),
    FilledNewArrayRange(u16, u8, TypeSignature),
    FillArrayData(u8, i32),
    Throw(u8),
    Goto(Label),
    Goto16(Label),
    Goto32(Label),
    PackedSwitch(u8, i32),
    SparseSwitch(u8, i32),
    Compare(CompareType, u8, u8, u8),
    If(TestType, u8, u8, i16),
    If0(TestType, u8, i16),
    Array(ArrayOperation, u8, u8, u8),
    Instance(ArrayOperation, u8, u8, FieldReference),
    Static(ArrayOperation, u8, FieldReference),
    Invoke(InvokeKind, Vec<u8>, MethodReference),
    InvokeRange(InvokeKind, u16, u8, MethodReference),
    Unary(UnaryOperation, u8, u8),
    Binary(BinaryOperation, u8, u8, u8),
    Binary2Addr(BinaryOperation, u8, u8),
    BinaryLit16(BinaryOperation, u8, u8, i16),
    BinaryLit8(BinaryOperation, u8, u8, i8),
    InvokePolymorphic(Vec<u8>, MethodReference, PrototypeReference),
    InvokePolymorphicRange(u16, u8, MethodReference, PrototypeReference),
    InvokeCustom(Vec<u8>, CallSiteReference),
    InvokeCustomRange(u16, u8, CallSiteReference),
}

#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub enum CompareType {
    LessThanFloat,
    GreaterThanFloat,
    LessThanDouble,
    GreaterThanDouble,
    Long,
    Unknown,
}

impl From<u8> for CompareType {
    fn from(opcode: u8) -> Self {
        match opcode {
            0x2D => Self::LessThanFloat,
            0x2E => Self::GreaterThanFloat,
            0x2F => Self::LessThanDouble,
            0x30 => Self::GreaterThanDouble,
            0x31 => Self::Long,
            _ => Self::Unknown,
        }
    }
}

impl ToString for CompareType {
    fn to_string(&self) -> String {
        match self {
            Self::LessThanFloat => "cmpl-float".to_string(),
            Self::GreaterThanFloat => "cmpg-float".to_string(),
            Self::LessThanDouble => "cmpl-double".to_string(),
            Self::GreaterThanDouble => "cmpg-double".to_string(),
            Self::Long => "cmp-long".to_string(),
            Self::Unknown => "unknown".to_string(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub enum TestType {
    Equal,
    NotEqual,
    LessThan,
    GreaterThanOrEqual,
    GreaterThan,
    LessThanOrEqual,
    Unknown,
}

impl ToString for TestType {
    fn to_string(&self) -> String {
        match self {
            Self::Equal => "if-eq".to_string(),
            Self::NotEqual => "if-ne".to_string(),
            Self::LessThan => "if-lt".to_string(),
            Self::GreaterThanOrEqual => "if-ge".to_string(),
            Self::GreaterThan => "if-gt".to_string(),
            Self::LessThanOrEqual => "if-le".to_string(),
            Self::Unknown => "unknown".to_string(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub enum ArrayOperation {
    Get,
    GetWide,
    GetObject,
    GetBoolean,
    GetByte,
    GetChar,
    GetShort,
    Put,
    PutWide,
    PutObject,
    PutBoolean,
    PutByte,
    PutChar,
    PutShort,
    Unknown,
}

impl ToString for ArrayOperation {
    fn to_string(&self) -> String {
        match self {
            Self::Get => "get".to_string(),
            Self::GetWide => "get-wide".to_string(),
            Self::GetObject => "get-object".to_string(),
            Self::GetBoolean => "get-boolean".to_string(),
            Self::GetByte => "get-byte".to_string(),
            Self::GetChar => "get-char".to_string(),
            Self::GetShort => "get-short".to_string(),
            Self::Put => "put".to_string(),
            Self::PutWide => "put-wide".to_string(),
            Self::PutObject => "put-object".to_string(),
            Self::PutBoolean => "put-boolean".to_string(),
            Self::PutByte => "put-byte".to_string(),
            Self::PutChar => "put-char".to_string(),
            Self::PutShort => "put-short".to_string(),
            Self::Unknown => "unknown".to_string(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub enum InvokeKind {
    Virtual,
    Super,
    Direct,
    Static,
    Interface,
    Unknown,
}

impl ToString for InvokeKind {
    fn to_string(&self) -> String {
        match self {
            Self::Virtual => "invoke-virtual".to_string(),
            Self::Super => "invoke-super".to_string(),
            Self::Direct => "invoke-direct".to_string(),
            Self::Static => "invoke-static".to_string(),
            Self::Interface => "invoke-interface".to_string(),
            Self::Unknown => "unknown".to_string(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub enum UnaryOperation {
    NegateInt,
    NotInt,
    NegateLong,
    NotLong,
    NegateFloat,
    NegateDouble,
    IntToLong,
    IntToFloat,
    IntToDouble,
    LongToInt,
    LongToFloat,
    LongToDouble,
    FloatToInt,
    FloatToLong,
    FloatToDouble,
    DoubleToInt,
    DoubleToLong,
    DoubleToFloat,
    IntToByte,
    IntToChar,
    IntToShort,
    Unknown,
}

impl ToString for UnaryOperation {
    fn to_string(&self) -> String {
        match self {
            Self::NegateInt => "neg-int".to_string(),
            Self::NotInt => "not-int".to_string(),
            Self::NegateLong => "neg-long".to_string(),
            Self::NotLong => "not-long".to_string(),
            Self::NegateFloat => "neg-float".to_string(),
            Self::NegateDouble => "neg-double".to_string(),
            Self::IntToLong => "int-to-long".to_string(),
            Self::IntToFloat => "int-to-float".to_string(),
            Self::IntToDouble => "int-to-double".to_string(),
            Self::LongToInt => "long-to-int".to_string(),
            Self::LongToFloat => "long-to-float".to_string(),
            Self::LongToDouble => "long-to-double".to_string(),
            Self::FloatToInt => "float-to-int".to_string(),
            Self::FloatToLong => "float-to-long".to_string(),
            Self::FloatToDouble => "float-to-double".to_string(),
            Self::DoubleToInt => "double-to-int".to_string(),
            Self::DoubleToLong => "double-to-long".to_string(),
            Self::DoubleToFloat => "double-to-float".to_string(),
            Self::IntToByte => "int-to-byte".to_string(),
            Self::IntToChar => "int-to-char".to_string(),
            Self::IntToShort => "int-to-short".to_string(),
            Self::Unknown => "unknown".to_string(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub enum BinaryOperation {
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    RemInt,
    AndInt,
    OrInt,
    XorInt,
    ShlInt,
    ShrInt,
    UshrInt,
    AddLong,
    SubLong,
    MulLong,
    DivLong,
    RemLong,
    AndLong,
    OrLong,
    XorLong,
    ShlLong,
    ShrLong,
    UshrLong,
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,
    RemFloat,
    AddDouble,
    SubDouble,
    MulDouble,
    DivDouble,
    RemDouble,
    Unknown,
}

impl ToString for BinaryOperation {
    fn to_string(&self) -> String {
        match self {
            Self::AddInt => "add-int".to_string(),
            Self::SubInt => "sub-int".to_string(),
            Self::MulInt => "mul-int".to_string(),
            Self::DivInt => "div-int".to_string(),
            Self::RemInt => "rem-int".to_string(),
            Self::AndInt => "and-int".to_string(),
            Self::OrInt => "or-int".to_string(),
            Self::XorInt => "xor-int".to_string(),
            Self::ShlInt => "shl-int".to_string(),
            Self::ShrInt => "shr-int".to_string(),
            Self::UshrInt => "ushr-int".to_string(),
            Self::AddLong => "add-long".to_string(),
            Self::SubLong => "sub-long".to_string(),
            Self::MulLong => "mul-long".to_string(),
            Self::DivLong => "div-long".to_string(),
            Self::RemLong => "rem-long".to_string(),
            Self::AndLong => "and-long".to_string(),
            Self::OrLong => "or-long".to_string(),
            Self::XorLong => "xor-long".to_string(),
            Self::ShlLong => "shl-long".to_string(),
            Self::ShrLong => "shr-long".to_string(),
            Self::UshrLong => "ushr-long".to_string(),
            Self::AddFloat => "add-float".to_string(),
            Self::SubFloat => "sub-float".to_string(),
            Self::MulFloat => "mul-float".to_string(),
            Self::DivFloat => "div-float".to_string(),
            Self::RemFloat => "rem-float".to_string(),
            Self::AddDouble => "add-double".to_string(),
            Self::SubDouble => "sub-double".to_string(),
            Self::MulDouble => "mul-double".to_string(),
            Self::DivDouble => "div-double".to_string(),
            Self::RemDouble => "rem-double".to_string(),
            Self::Unknown => "unknown".to_string(),
        }
    }
}


pub type Label = String;
/// Field index on the Dex field table
pub type FieldReference = String;
/// Method index on the Dex method table
pub type MethodReference = String;
/// Prototype index on the Dex prototype table
pub type PrototypeReference = String;
/// Call site index on the Dex call site table
pub type CallSiteReference = String;

impl ToString for DexInstruction {
    fn to_string(&self) -> String {
        match self {
            Self::Nop => "nop".to_string(),
            Self::Move(dest, source) => format!("move v{}, v{}", dest, source),
            Self::MoveFrom16(dest, source) => format!("move/from16 v{}, v{}", dest, source),
            Self::Move16(dest, source) => format!("move/16 v{}, v{}", dest, source),
            Self::MoveWide(dest, source) => format!("move-wide v{}, v{}", dest, source),
            Self::MoveWideFrom16(dest, source) => {
                format!("move-wide/from16 v{}, v{}", dest, source)
            }
            Self::MoveWide16(dest, source) => format!("move-wide/16 v{}, v{}", dest, source),
            Self::MoveObject(dest, source) => format!("move-object v{}, v{}", dest, source),
            Self::MoveObjectFrom16(dest, source) => {
                format!("move-object/from16 v{}, v{}", dest, source)
            }
            Self::MoveObject16(dest, source) => format!("move-object/16 v{}, v{}", dest, source),
            Self::MoveResult(dest) => format!("move-result v{}", dest),
            Self::MoveResultWide(dest) => format!("move-result-wide v{}", dest),
            Self::MoveResultObject(dest) => format!("move-result-object v{}", dest),
            Self::MoveException(dest) => format!("move-exception v{}", dest),
            Self::ReturnVoid => "return-void".to_string(),
            Self::Return(dest) => format!("return v{}", dest),
            Self::ReturnWide(dest) => format!("return-wide v{}", dest),
            Self::ReturnObject(dest) => format!("return-object v{}", dest),
            Self::Const4(dest, literal) => format!("const/4 v{}, #{}", dest, literal),
            Self::Const16(dest, literal) => format!("const/16 v{}, #{}", dest, literal),
            Self::Const(dest, literal) => format!("const v{}, #{}", dest, literal),
            Self::ConstHigh16(dest, literal) => format!("const/high16 v{}, #{}", dest, literal),
            Self::ConstWide16(dest, literal) => format!("const-wide/16 v{}, #{}", dest, literal),
            Self::ConstWide32(dest, literal) => format!("const-wide/32 v{}, #{}", dest, literal),
            Self::ConstWide(dest, literal) => format!("const-wide v{}, #{}", dest, literal),
            Self::ConstWideHigh16(dest, literal) => {
                format!("const-wide/high16 v{}, #{}", dest, literal)
            }
            Self::ConstString(dest, reference) => {
                format!("const-string v{}, string@{}", dest, reference)
            }
            Self::ConstStringJumbo(dest, reference) => {
                format!("const-string/jumbo v{}, string@{}", dest, reference)
            }
            Self::ConstClass(dest, reference) => {
                format!("const-class v{}, class@{}", dest, reference)
            }
            Self::MonitorEnter(reg) => format!("monitor-enter v{}", reg),
            Self::MonitorExit(reg) => format!("monitor-exit v{}", reg),
            Self::CheckCast(reg, reference) => format!("check-cast v{}, type@{}", reg, reference),
            Self::InstanceOf(dest, src, reference) => {
                format!("instance-of v{}, v{}, type@{}", dest, src, reference)
            }
            Self::ArrayLength(dest, src) => format!("array-length v{}, v{}", dest, src),
            Self::NewInstance(dest, reference) => {
                format!("new-instance v{}, type@{}", dest, reference)
            }
            Self::NewArray(dest, src, reference) => {
                format!("new-array v{}, v{}, type@{}", dest, src, reference)
            }
            Self::FilledNewArray(ref registers, reference) => {
                let str_register: Vec<String> =
                    registers.iter().map(|r| format!("v{}", r)).collect();
                format!(
                    "filled-new-array {{{}}}, type@{}",
                    str_register.join(", "),
                    reference
                )
            }
            Self::FilledNewArrayRange(first_reg, amount, reference) => {
                let str_register: Vec<String> = (*first_reg..=(*first_reg + u16::from(*amount)))
                    .map(|r| format!("v{}", r))
                    .collect();
                format!(
                    "filled-new-array/range {{{}}}, type@{}",
                    str_register.join(", "),
                    reference
                )
            }
            Self::FillArrayData(reg, offset) => format!("fill-array-data v{}, {}", reg, offset),
            Self::Throw(reg) => format!("throw v{}", reg),
            Self::Goto(offset) => format!("goto {}", offset),
            Self::Goto16(offset) => format!("goto/16 {}", offset),
            Self::Goto32(offset) => format!("goto/32 {}", offset),
            Self::PackedSwitch(reg, offset) => format!("packed-switch v{}, {}", reg, offset),
            Self::SparseSwitch(reg, offset) => format!("sparse-switch v{}, {}", reg, offset),
            Self::Compare(ref ct, dest, op1, op2) => {
                format!("{} v{}, v{}, v{}", ct.to_string(), dest, op1, op2)
            }
            Self::If(ref tt, dest, src, offset) => {
                format!("{} v{}, v{}, {}", tt.to_string(), dest, src, offset)
            }
            Self::If0(ref tt, dest, offset) => format!("{}z v{}, {}", tt.to_string(), dest, offset),
            Self::Array(ref array_op, dest, op1, op2) => {
                format!("a{} v{}, v{}, v{}", array_op.to_string(), dest, op1, op2)
            }
            Self::Instance(ref array_op, dest, op1, field) => format!(
                "i{} v{}, v{}, field@{}",
                array_op.to_string(),
                dest,
                op1,
                field
            ),
            Self::Static(ref array_op, dest, field) => {
                format!("s{} v{}, field@{}", array_op.to_string(), dest, field)
            }
            Self::Invoke(ref invoke_kind, ref registers, method) => {
                let str_register: Vec<String> =
                    registers.iter().map(|r| format!("v{}", r)).collect();
                format!(
                    "{} {{{}}}, method@{}",
                    invoke_kind.to_string(),
                    str_register.join(", "),
                    method
                )
            }
            Self::InvokeRange(ref invoke_kind, first_reg, amount, reference) => {
                let str_register: Vec<String> = (*first_reg..(*first_reg + u16::from(*amount)))
                    .map(|r| format!("v{}", r))
                    .collect();
                format!(
                    "{}/range {{{}}}, method@{}",
                    invoke_kind.to_string(),
                    str_register.join(", "),
                    reference
                )
            }
            Self::Unary(ref operation, dest, src) => {
                format!("{} v{}, v{}", operation.to_string(), dest, src)
            }
            Self::Binary(ref operation, dest, op1, op2) => {
                format!("{} v{}, v{}, v{}", operation.to_string(), dest, op1, op2)
            }
            Self::Binary2Addr(ref operation, src1, src2) => {
                format!("{}/2addr v{}, v{}", operation.to_string(), src1, src2)
            }
            Self::BinaryLit16(ref operation, dest, src, literal) => {
                if let BinaryOperation::SubInt = operation {
                    format!("rsub-int v{}, v{}, #{}", dest, src, literal)
                } else {
                    format!(
                        "{}/lit16 v{}, v{}, #{}",
                        operation.to_string(),
                        dest,
                        src,
                        literal
                    )
                }
            }
            Self::BinaryLit8(ref operation, dest, src, literal) => format!(
                "{}/lit8 v{}, v{}, #{}",
                operation.to_string(),
                dest,
                src,
                literal
            ),
            Self::InvokePolymorphic(ref registers, method, proto) => {
                let str_register: Vec<String> =
                    registers.iter().map(|r| format!("v{}", r)).collect();
                format!(
                    "invoke-polymorphic {{{}}}, method@{} proto@{}",
                    str_register.join(", "),
                    method,
                    proto
                )
            }
            Self::InvokePolymorphicRange(first_reg, amount, method, proto) => {
                let str_register: Vec<String> = (*first_reg..(*first_reg + u16::from(*amount)))
                    .map(|r| format!("v{}", r))
                    .collect();
                format!(
                    "invoke-polymorphic/range {{{}}}, method@{} proto@{}",
                    str_register.join(", "),
                    method,
                    proto
                )
            }
            Self::InvokeCustom(ref registers, call_site) => {
                let str_register: Vec<String> =
                    registers.iter().map(|r| format!("v{}", r)).collect();
                format!(
                    "invoke-custom {{{}}}, call_site@{}",
                    str_register.join(", "),
                    call_site
                )
            }
            Self::InvokeCustomRange(first_reg, amount, call_site) => {
                let str_register: Vec<String> = (*first_reg..(*first_reg + u16::from(*amount)))
                    .map(|r| format!("v{}", r))
                    .collect();
                format!(
                    "invoke-custom/range {{{}}}, call_site@{}",
                    str_register.join(", "),
                    call_site
                )
            }
        }
    }
}

