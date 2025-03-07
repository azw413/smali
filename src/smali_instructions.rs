use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until, take_while1},
    character::complete::{alphanumeric1, char, digit1, multispace0, space0, space1},
    combinator::map,
    multi::separated_list1,
    sequence::{delimited, preceded, separated_pair, tuple},
    IResult,
};
use std::fmt;
use std::fmt::Debug;
use nom::bytes::complete::{escaped, take_while};
use nom::character::complete::{none_of, one_of};
use nom::combinator::{fail, opt};
use nom::error::{convert_error, Error, ErrorKind, make_error, ParseError, VerboseError};
use nom::multi::separated_list0;
use nom::sequence::pair;
use crate::types::{parse_methodsignature, parse_typesignature};


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label(pub String);

// A helper function to determine valid characters for a label.
fn is_label_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '$'
}

/// Parse a label in smali syntax, e.g. ":cond_0"
pub fn parse_label(input: &str) -> IResult<&str, Label> {
    // Expect a colon first, then one or more valid characters.
    let (input, _) = tag(":")(input)?;
    let (input, label_body) = take_while1(is_label_char)(input)?;
    Ok((input, Label(label_body.to_string())))
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Prepend a colon when printing
        write!(f, ":{}", self.0)
    }
}

/// A symbolic reference to a method.
#[derive(Debug, Clone, PartialEq)]
pub struct MethodRef
{
    /// The fully qualified class name, e.g. "Lcom/example/MyClass;".
    pub class: String,
    /// The method name.
    pub name: String,
    /// The method descriptor (signature), e.g. "(I)V".
    pub descriptor: String,
}

impl fmt::Display for MethodRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Example: Lkotlin/jvm/internal/Intrinsics;->checkNotNullParameter(Ljava/lang/Object;Ljava/lang/String;)V
        write!(f, "{}->{}{}", self.class, self.name, self.descriptor)
    }
}

/// A symbolic reference to a field.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldRef
{
    /// The fully qualified class name, e.g. "Lcom/example/MyClass;".
    pub class: String,
    /// The field name.
    pub name: String,
    /// The field descriptor (type), e.g. "I" for int.
    pub descriptor: String,
}

impl fmt::Display for FieldRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Example: Lcom/example/MyClass;->myField:I
        write!(f, "{}->{}:{}", self.class, self.name, self.descriptor)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SmaliRegister {
    Parameter(u16),
    Local(u16),
}

pub fn p(u: u16) -> SmaliRegister { SmaliRegister::Parameter(u) }
pub fn v(u: u16) -> SmaliRegister { SmaliRegister::Local(u) }


impl fmt::Display for SmaliRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Here we don't know the method context so we just print the raw value.
        // In a full implementation you would convert using the method context.
        match self {
            SmaliRegister::Parameter(n) => write!(f, "p{}", n),
            SmaliRegister::Local(n) => write!(f, "v{}", n),
        }
    }
}

/// A symbolic range of registers as written in smali, e.g. "{v0 .. v6}"
#[derive(Debug, Clone, PartialEq)]
pub struct RegisterRange {
    pub start: SmaliRegister,
    pub end: SmaliRegister,
}

impl fmt::Display for RegisterRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print in smali style: "{v0 .. v6}"
        write!(f, "{{ {} .. {} }}", self.start, self.end)
    }
}

/// A high-level representation of a DEX instruction.
///
/// This enum “lifts” many opcodes so that literal values and symbolic references
/// (e.g. for strings, classes, methods, fields, call sites, prototypes) are stored
/// directly rather than as indices.
#[derive(Debug, Clone, PartialEq)]
pub enum DexInstruction {
    // Group A: constants, moves, returns, etc.
    ConstString { dest: SmaliRegister, value: String },
    ConstStringJumbo { dest: SmaliRegister, value: String },
    Nop,
    Move { dest: SmaliRegister, src: SmaliRegister },
    MoveFrom16 { dest: SmaliRegister, src: SmaliRegister },
    Move16 { dest: SmaliRegister, src: SmaliRegister },
    MoveWide { dest: SmaliRegister, src: SmaliRegister },
    MoveWideFrom16 { dest: SmaliRegister, src: SmaliRegister },
    MoveWide16 { dest: SmaliRegister, src: SmaliRegister },
    MoveObject { dest: SmaliRegister, src: SmaliRegister },
    MoveObjectFrom16 { dest: SmaliRegister, src: SmaliRegister },
    MoveObject16 { dest: SmaliRegister, src: SmaliRegister },
    MoveResult { dest: SmaliRegister },
    MoveResultWide { dest: SmaliRegister },
    MoveResultObject { dest: SmaliRegister },
    MoveException { dest: SmaliRegister },
    ReturnVoid,
    Return { src: SmaliRegister },
    ReturnWide { src: SmaliRegister },
    ReturnObject { src: SmaliRegister },
    Const4 { dest: SmaliRegister, value: i8 },
    Const16 { dest: SmaliRegister, value: i16 },
    Const { dest: SmaliRegister, value: i32 },
    ConstHigh16 { dest: SmaliRegister, value: i16 },
    ConstWide16 { dest: SmaliRegister, value: i16 },
    ConstWide32 { dest: SmaliRegister, value: i32 },
    ConstWide { dest: SmaliRegister, value: i64 },
    ConstWideHigh16 { dest: SmaliRegister, value: i16 },
    ConstClass { dest: SmaliRegister, class: String },
    MonitorEnter { src: SmaliRegister },
    MonitorExit { src: SmaliRegister },
    CheckCast { dest: SmaliRegister, class: String },
    InstanceOf { dest: SmaliRegister, src: SmaliRegister, class: String },
    ArrayLength { dest: SmaliRegister, array: SmaliRegister },
    NewInstance { dest: SmaliRegister, class: String },
    NewArray { dest: SmaliRegister, size_reg: SmaliRegister, class: String },
    FilledNewArray { registers: Vec<SmaliRegister>, class: String },
    FilledNewArrayRange { registers: RegisterRange, class: String },
    FillArrayData { reg: SmaliRegister, offset: Label },
    Throw { src: SmaliRegister },
    Goto { offset: Label },
    Goto16 { offset: Label },
    Goto32 { offset: Label },
    PackedSwitch { reg: SmaliRegister, offset: Label },
    SparseSwitch { reg: SmaliRegister, offset: Label },
    CmplFloat { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    CmpgFloat { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    CmplDouble { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    CmpgDouble { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    CmpLong { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },

    // Group B: Array, field, and invocation instructions.
    AGet { dest: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    AGetWide { dest: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    AGetObject { dest: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    AGetBoolean { dest: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    AGetByte { dest: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    AGetChar { dest: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    AGetShort { dest: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    APut { src: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    APutWide { src: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    APutObject { src: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    APutBoolean { src: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    APutByte { src: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    APutChar { src: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    APutShort { src: SmaliRegister, array: SmaliRegister, index: SmaliRegister },
    IGet { dest: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IGetWide { dest: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IGetObject { dest: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IGetBoolean { dest: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IGetByte { dest: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IGetChar { dest: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IGetShort { dest: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IPut { src: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IPutWide { src: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IPutObject { src: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IPutBoolean { src: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IPutByte { src: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IPutChar { src: SmaliRegister, object: SmaliRegister, field: FieldRef },
    IPutShort { src: SmaliRegister, object: SmaliRegister, field: FieldRef },
    SGet { dest: SmaliRegister, field: FieldRef },
    SGetWide { dest: SmaliRegister, field: FieldRef },
    SGetObject { dest: SmaliRegister, field: FieldRef },
    SGetBoolean { dest: SmaliRegister, field: FieldRef },
    SGetByte { dest: SmaliRegister, field: FieldRef },
    SGetChar { dest: SmaliRegister, field: FieldRef },
    SGetShort { dest: SmaliRegister, field: FieldRef },
    SPut { src: SmaliRegister, field: FieldRef },
    SPutWide { src: SmaliRegister, field: FieldRef },
    SPutObject { src: SmaliRegister, field: FieldRef },
    SPutBoolean { src: SmaliRegister, field: FieldRef },
    SPutByte { src: SmaliRegister, field: FieldRef },
    SPutChar { src: SmaliRegister, field: FieldRef },
    SPutShort { src: SmaliRegister, field: FieldRef },
    InvokeVirtual { registers: Vec<SmaliRegister>, method: MethodRef },
    InvokeSuper { registers: Vec<SmaliRegister>, method: MethodRef },
    InvokeInterface { registers: Vec<SmaliRegister>, method: MethodRef },
    InvokeVirtualRange { range: RegisterRange, method: MethodRef },
    InvokeSuperRange { range: RegisterRange, method: MethodRef },
    InvokeDirectRange { range: RegisterRange, method: MethodRef },
    InvokeStaticRange { range: RegisterRange, method: MethodRef },
    InvokeInterfaceRange { range: RegisterRange, method: MethodRef },
    InvokeDirect { registers: Vec<SmaliRegister>, method: MethodRef },
    InvokeStatic { registers: Vec<SmaliRegister>, method: MethodRef },

    // Group C: Arithmetic instructions (non-2addr).
    AddInt { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    SubInt { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    MulInt { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    DivInt { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    RemInt { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    AndInt { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    OrInt  { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    XorInt { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    ShlInt { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    ShrInt { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    UshrInt { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    AddLong { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    SubLong { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    MulLong { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    DivLong { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    RemLong { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    AndLong { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    OrLong  { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    XorLong { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    ShlLong { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    ShrLong { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    UshrLong { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    AddFloat { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    SubFloat { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    MulFloat { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    DivFloat { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    RemFloat { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    AddDouble { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    SubDouble { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    MulDouble { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    DivDouble { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },
    RemDouble { dest: SmaliRegister, src1: SmaliRegister, src2: SmaliRegister },

    // Group D: Arithmetic instructions (2addr variants).
    AddInt2Addr { reg: SmaliRegister, src: SmaliRegister },
    SubInt2Addr { reg: SmaliRegister, src: SmaliRegister },
    MulInt2Addr { reg: SmaliRegister, src: SmaliRegister },
    DivInt2Addr { reg: SmaliRegister, src: SmaliRegister },
    RemInt2Addr { reg: SmaliRegister, src: SmaliRegister },
    AndInt2Addr { reg: SmaliRegister, src: SmaliRegister },
    OrInt2Addr { reg: SmaliRegister, src: SmaliRegister },
    XorInt2Addr { reg: SmaliRegister, src: SmaliRegister },
    ShlInt2Addr { reg: SmaliRegister, src: SmaliRegister },
    ShrInt2Addr { reg: SmaliRegister, src: SmaliRegister },
    UshrInt2Addr { reg: SmaliRegister, src: SmaliRegister },
    AddLong2Addr { reg: SmaliRegister, src: SmaliRegister },
    SubLong2Addr { reg: SmaliRegister, src: SmaliRegister },
    MulLong2Addr { reg: SmaliRegister, src: SmaliRegister },
    DivLong2Addr { reg: SmaliRegister, src: SmaliRegister },
    RemLong2Addr { reg: SmaliRegister, src: SmaliRegister },
    AndLong2Addr { reg: SmaliRegister, src: SmaliRegister },
    OrLong2Addr { reg: SmaliRegister, src: SmaliRegister },
    XorLong2Addr { reg: SmaliRegister, src: SmaliRegister },
    ShlLong2Addr { reg: SmaliRegister, src: SmaliRegister },
    ShrLong2Addr { reg: SmaliRegister, src: SmaliRegister },
    UshrLong2Addr { reg: SmaliRegister, src: SmaliRegister },
    AddFloat2Addr { reg: SmaliRegister, src: SmaliRegister },
    SubFloat2Addr { reg: SmaliRegister, src: SmaliRegister },
    MulFloat2Addr { reg: SmaliRegister, src: SmaliRegister },
    DivFloat2Addr { reg: SmaliRegister, src: SmaliRegister },
    RemFloat2Addr { reg: SmaliRegister, src: SmaliRegister },
    AddDouble2Addr { reg: SmaliRegister, src: SmaliRegister },
    SubDouble2Addr { reg: SmaliRegister, src: SmaliRegister },
    MulDouble2Addr { reg: SmaliRegister, src: SmaliRegister },
    DivDouble2Addr { reg: SmaliRegister, src: SmaliRegister },
    RemDouble2Addr { reg: SmaliRegister, src: SmaliRegister },

    // Additional conversion instructions:
    IntToByte { dest: SmaliRegister, src: SmaliRegister },
    IntToChar { dest: SmaliRegister, src: SmaliRegister },
    IntToShort { dest: SmaliRegister, src: SmaliRegister },

    // Literal arithmetic instructions using lit8 encoding:
    AddIntLit8 { dest: SmaliRegister, src: SmaliRegister, literal: i8 },
    RSubIntLit8 { dest: SmaliRegister, src: SmaliRegister, literal: i8 },
    MulIntLit8 { dest: SmaliRegister, src: SmaliRegister, literal: i8 },
    DivIntLit8 { dest: SmaliRegister, src: SmaliRegister, literal: i8 },
    RemIntLit8 { dest: SmaliRegister, src: SmaliRegister, literal: i8 },
    AndIntLit8 { dest: SmaliRegister, src: SmaliRegister, literal: i8 },
    OrIntLit8  { dest: SmaliRegister, src: SmaliRegister, literal: i8 },
    XorIntLit8 { dest: SmaliRegister, src: SmaliRegister, literal: i8 },
    ShlIntLit8 { dest: SmaliRegister, src: SmaliRegister, literal: i8 },
    ShrIntLit8 { dest: SmaliRegister, src: SmaliRegister, literal: i8 },
    UshrIntLit8 { dest: SmaliRegister, src: SmaliRegister, literal: i8 },

    // Conditional branch instructions now using SmaliRegister:
    IfEq  { reg1: SmaliRegister, reg2: SmaliRegister, offset: Label },
    IfNe  { reg1: SmaliRegister, reg2: SmaliRegister, offset: Label },
    IfLt  { reg1: SmaliRegister, reg2: SmaliRegister, offset: Label },
    IfGe  { reg1: SmaliRegister, reg2: SmaliRegister, offset: Label },
    IfGt  { reg1: SmaliRegister, reg2: SmaliRegister, offset: Label },
    IfLe  { reg1: SmaliRegister, reg2: SmaliRegister, offset: Label },
    IfEqz { reg: SmaliRegister, offset: Label },
    IfNez { reg: SmaliRegister, offset: Label },
    IfLtz { reg: SmaliRegister, offset: Label },
    IfGez { reg: SmaliRegister, offset: Label },
    IfGtz { reg: SmaliRegister, offset: Label },
    IfLez { reg: SmaliRegister, offset: Label },

    // Arithmetic instructions:
    NegInt { dest: SmaliRegister, src: SmaliRegister },
    NotInt { dest: SmaliRegister, src: SmaliRegister },
    NegLong { dest: SmaliRegister, src: SmaliRegister },
    NotLong { dest: SmaliRegister, src: SmaliRegister },
    NegFloat { dest: SmaliRegister, src: SmaliRegister },
    NegDouble { dest: SmaliRegister, src: SmaliRegister },

    // Conversion instructions added to the DexInstruction enum:
    IntToLong   { dest: SmaliRegister, src: SmaliRegister },
    IntToFloat  { dest: SmaliRegister, src: SmaliRegister },
    IntToDouble { dest: SmaliRegister, src: SmaliRegister },
    LongToInt   { dest: SmaliRegister, src: SmaliRegister },
    LongToFloat { dest: SmaliRegister, src: SmaliRegister },
    LongToDouble{ dest: SmaliRegister, src: SmaliRegister },
    FloatToInt  { dest: SmaliRegister, src: SmaliRegister },
    FloatToLong { dest: SmaliRegister, src: SmaliRegister },
    FloatToDouble{ dest: SmaliRegister, src: SmaliRegister },
    DoubleToInt { dest: SmaliRegister, src: SmaliRegister },
    DoubleToLong{ dest: SmaliRegister, src: SmaliRegister },
    DoubleToFloat{ dest: SmaliRegister, src: SmaliRegister },

    AddIntLit16 { dest: SmaliRegister, src: SmaliRegister, literal: i16 },
    RSubIntLit16 { dest: SmaliRegister, src: SmaliRegister, literal: i16 },
    MulIntLit16 { dest: SmaliRegister, src: SmaliRegister, literal: i16 },
    DivIntLit16 { dest: SmaliRegister, src: SmaliRegister, literal: i16 },
    RemIntLit16 { dest: SmaliRegister, src: SmaliRegister, literal: i16 },
    AndIntLit16 { dest: SmaliRegister, src: SmaliRegister, literal: i16 },
    OrIntLit16  { dest: SmaliRegister, src: SmaliRegister, literal: i16 },
    XorIntLit16 { dest: SmaliRegister, src: SmaliRegister, literal: i16 },

    // Group E: Polymorphic, custom and method handle/type constants.
    InvokePolymorphic { registers: Vec<SmaliRegister>, method: MethodRef, proto: String },
    InvokePolymorphicRange { range: RegisterRange, method: MethodRef, proto: String },
    InvokeCustom { registers: Vec<SmaliRegister>, call_site: String },
    InvokeCustomRange { range: RegisterRange, call_site: String },
    ConstMethodHandle { dest: SmaliRegister, method_handle: String },
    ConstMethodType { dest: SmaliRegister, proto: String },
    Unused { opcode: u8 },
}


impl fmt::Display for DexInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Group A
            DexInstruction::ConstString { dest, value } =>
                write!(f, "const-string {}, \"{}\"", dest, value),
            DexInstruction::ConstStringJumbo { dest, value } =>
                write!(f, "const-string/jumbo {}, \"{}\"", dest, value),
            DexInstruction::Nop => write!(f, "nop"),
            DexInstruction::Move { dest, src } => write!(f, "move {}, {}", dest, src),
            DexInstruction::MoveFrom16 { dest, src } => write!(f, "move/from16 {}, {}", dest, src),
            DexInstruction::Move16 { dest, src } => write!(f, "move/16 {} , {}", dest, src),
            DexInstruction::MoveWide { dest, src } => write!(f, "move-wide {}, {}", dest, src),
            DexInstruction::MoveWideFrom16 { dest, src } =>
                write!(f, "move-wide/from16 {}, {}", dest, src),
            DexInstruction::MoveWide16 { dest, src } => write!(f, "move-wide/16 {} , {}", dest, src),
            DexInstruction::MoveObject { dest, src } =>
                write!(f, "move-object {}, {}", dest, src),
            DexInstruction::MoveObjectFrom16 { dest, src } =>
                write!(f, "move-object/from16 {}, {}", dest, src),
            DexInstruction::MoveObject16 { dest, src } =>
                write!(f, "move-object/16 {} , {}", dest, src),
            DexInstruction::MoveResult { dest } => write!(f, "move-result {}", dest),
            DexInstruction::MoveResultWide { dest } => write!(f, "move-result-wide {}", dest),
            DexInstruction::MoveResultObject { dest } => write!(f, "move-result-object {}", dest),
            DexInstruction::MoveException { dest } => write!(f, "move-exception {}", dest),
            DexInstruction::ReturnVoid => write!(f, "return-void"),
            DexInstruction::Return { src } => write!(f, "return {}", src),
            DexInstruction::ReturnWide { src } => write!(f, "return-wide {}", src),
            DexInstruction::ReturnObject { src } => write!(f, "return-object {}", src),
            DexInstruction::Const4 { dest, value } => write!(f, "const/4 {}, {}", dest, value),
            DexInstruction::Const16 { dest, value } => write!(f, "const/16 {}, {}", dest, value),
            DexInstruction::Const { dest, value } => write!(f, "const {}, {}", dest, value),
            DexInstruction::ConstHigh16 { dest, value } =>
                write!(f, "const/high16 {}, 0x{:0x}0000", dest, value),
            DexInstruction::ConstWide16 { dest, value } =>
                write!(f, "const-wide/16 {}, {}", dest, value),
            DexInstruction::ConstWide32 { dest, value } =>
                write!(f, "const-wide/32 {}, {}", dest, value),
            DexInstruction::ConstWide { dest, value } =>
                write!(f, "const-wide {}, 0x{:0x}L", dest, value),
            DexInstruction::ConstWideHigh16 { dest, value } =>
                write!(f, "const-wide/high16 {}, 0x{:0x}000000000000L", dest, value),
            DexInstruction::ConstClass { dest, class } =>
                write!(f, "const-class {}, {}", dest, class),
            DexInstruction::MonitorEnter { src } => write!(f, "monitor-enter {}", src),
            DexInstruction::MonitorExit { src } => write!(f, "monitor-exit {}", src),
            DexInstruction::CheckCast { dest, class } =>
                write!(f, "check-cast {}, {}", dest, class),
            DexInstruction::InstanceOf { dest, src, class } =>
                write!(f, "instance-of {}, {}, {}", dest, src, class),
            DexInstruction::ArrayLength { dest, array } =>
                write!(f, "array-length {}, {}", dest, array),
            DexInstruction::NewInstance { dest, class } =>
                write!(f, "new-instance {}, {}", dest, class),
            DexInstruction::NewArray { dest, size_reg, class } =>
                write!(f, "new-array {}, {}, {}", dest, size_reg, class),
            DexInstruction::FilledNewArray { registers, class } => {
                let regs: Vec<String> = registers.iter().map(|r| format!("{}", r)).collect();
                write!(f, "filled-new-array {{{}}}, {}", regs.join(", "), class)
            },
            DexInstruction::FilledNewArrayRange { registers, class } => {
                write!(f, "filled-new-array/range {}, {}", registers, class)
            },
            DexInstruction::FillArrayData { reg, offset } =>
                write!(f, "fill-array-data {}, {}", reg, offset),
            DexInstruction::Throw { src } => write!(f, "throw {}", src),
            DexInstruction::Goto { offset } => write!(f, "goto {}", offset),
            DexInstruction::Goto16 { offset } => write!(f, "goto/16 {}", offset),
            DexInstruction::Goto32 { offset } => write!(f, "goto/32 {}", offset),
            DexInstruction::PackedSwitch { reg, offset } =>
                write!(f, "packed-switch {}, {}", reg, offset),
            DexInstruction::SparseSwitch { reg, offset } =>
                write!(f, "sparse-switch {}, {}", reg, offset),
            DexInstruction::CmplFloat { dest, src1, src2 } =>
                write!(f, "cmpl-float {}, {}, {}", dest, src1, src2),
            DexInstruction::CmpgFloat { dest, src1, src2 } =>
                write!(f, "cmpg-float {}, {}, {}", dest, src1, src2),
            DexInstruction::CmplDouble { dest, src1, src2 } =>
                write!(f, "cmpl-double {}, {}, {}", dest, src1, src2),
            DexInstruction::CmpgDouble { dest, src1, src2 } =>
                write!(f, "cmpg-double {}, {}, {}", dest, src1, src2),
            DexInstruction::CmpLong { dest, src1, src2 } =>
                write!(f, "cmp-long {}, {}, {}", dest, src1, src2),
            // Group B: Array, field and invocation instructions.
            DexInstruction::AGet { dest, array, index } =>
                write!(f, "aget {}, {}, {}", dest, array, index),
            DexInstruction::AGetWide { dest, array, index } =>
                write!(f, "aget-wide {}, {}, {}", dest, array, index),
            DexInstruction::AGetObject { dest, array, index } =>
                write!(f, "aget-object {}, {}, {}", dest, array, index),
            DexInstruction::AGetBoolean { dest, array, index } =>
                write!(f, "aget-boolean {}, {}, {}", dest, array, index),
            DexInstruction::AGetByte { dest, array, index } =>
                write!(f, "aget-byte {}, {}, {}", dest, array, index),
            DexInstruction::AGetChar { dest, array, index } =>
                write!(f, "aget-char {}, {}, {}", dest, array, index),
            DexInstruction::AGetShort { dest, array, index } =>
                write!(f, "aget-short {}, {}, {}", dest, array, index),
            DexInstruction::APut { src, array, index } =>
                write!(f, "aput {}, {}, {}", src, array, index),
            DexInstruction::APutWide { src, array, index } =>
                write!(f, "aput-wide {}, {}, {}", src, array, index),
            DexInstruction::APutObject { src, array, index } =>
                write!(f, "aput-object {}, {}, {}", src, array, index),
            DexInstruction::APutBoolean { src, array, index } =>
                write!(f, "aput-boolean {}, {}, {}", src, array, index),
            DexInstruction::APutByte { src, array, index } =>
                write!(f, "aput-byte {}, {}, {}", src, array, index),
            DexInstruction::APutChar { src, array, index } =>
                write!(f, "aput-char {}, {}, {}", src, array, index),
            DexInstruction::APutShort { src, array, index } =>
                write!(f, "aput-short {}, {}, {}", src, array, index),
            DexInstruction::IGet { dest, object, field } =>
                write!(f, "iget {}, {}, {}", dest, object, field),
            DexInstruction::IGetWide { dest, object, field } =>
                write!(f, "iget-wide {}, {}, {}", dest, object, field),
            DexInstruction::IGetObject { dest, object, field } =>
                write!(f, "iget-object {}, {}, {}", dest, object, field),
            DexInstruction::IGetBoolean { dest, object, field } =>
                write!(f, "iget-boolean {}, {}, {}", dest, object, field),
            DexInstruction::IGetByte { dest, object, field } =>
                write!(f, "iget-byte {}, {}, {}", dest, object, field),
            DexInstruction::IGetChar { dest, object, field } =>
                write!(f, "iget-char {}, {}, {}", dest, object, field),
            DexInstruction::IGetShort { dest, object, field } =>
                write!(f, "iget-short {}, {}, {}", dest, object, field),
            DexInstruction::IPut { src, object, field } =>
                write!(f, "iput {}, {}, {}", src, object, field),
            DexInstruction::IPutWide { src, object, field } =>
                write!(f, "iput-wide {}, {}, {}", src, object, field),
            DexInstruction::IPutObject { src, object, field } =>
                write!(f, "iput-object {}, {}, {}", src, object, field),
            DexInstruction::IPutBoolean { src, object, field } =>
                write!(f, "iput-boolean {}, {}, {}", src, object, field),
            DexInstruction::IPutByte { src, object, field } =>
                write!(f, "iput-byte {}, {}, {}", src, object, field),
            DexInstruction::IPutChar { src, object, field } =>
                write!(f, "iput-char {}, {}, {}", src, object, field),
            DexInstruction::IPutShort { src, object, field } =>
                write!(f, "iput-short {}, {}, {}", src, object, field),
            DexInstruction::SGet { dest, field } =>
                write!(f, "sget {}, {}", dest, field),
            DexInstruction::SGetWide { dest, field } =>
                write!(f, "sget-wide {}, {}", dest, field),
            DexInstruction::SGetObject { dest, field } =>
                write!(f, "sget-object {}, {}", dest, field),
            DexInstruction::SGetBoolean { dest, field } =>
                write!(f, "sget-boolean {}, {}", dest, field),
            DexInstruction::SGetByte { dest, field } =>
                write!(f, "sget-byte {}, {}", dest, field),
            DexInstruction::SGetChar { dest, field } =>
                write!(f, "sget-char {}, {}", dest, field),
            DexInstruction::SGetShort { dest, field } =>
                write!(f, "sget-short {}, {}", dest, field),
            DexInstruction::SPut { src, field } =>
                write!(f, "sput {}, {}", src, field),
            DexInstruction::SPutWide { src, field } =>
                write!(f, "sput-wide {}, {}", src, field),
            DexInstruction::SPutObject { src, field } =>
                write!(f, "sput-object {}, {}", src, field),
            DexInstruction::SPutBoolean { src, field } =>
                write!(f, "sput-boolean {}, {}", src, field),
            DexInstruction::SPutByte { src, field } =>
                write!(f, "sput-byte {}, {}", src, field),
            DexInstruction::SPutChar { src, field } =>
                write!(f, "sput-char {}, {}", src, field),
            DexInstruction::SPutShort { src, field } =>
                write!(f, "sput-short {}, {}", src, field),
            DexInstruction::InvokeVirtual { registers, method } => {
                let regs = registers.iter().map(|r| format!("{}", r)).collect::<Vec<_>>().join(", ");
                write!(f, "invoke-virtual {{{}}}, {}", regs, method)
            }
            DexInstruction::InvokeSuper { registers, method } => {
                let regs = registers.iter().map(|r| format!("{}", r)).collect::<Vec<_>>().join(", ");
                write!(f, "invoke-super {{{}}}, {}", regs, method)
            }
            DexInstruction::InvokeInterface { registers, method } => {
                let regs = registers.iter().map(|r| format!("{}", r)).collect::<Vec<_>>().join(", ");
                write!(f, "invoke-interface {{{}}}, {}", regs, method)
            }
            DexInstruction::InvokeVirtualRange { range, method } =>
                write!(f, "invoke-virtual/range {}, {}", range, method),
            DexInstruction::InvokeSuperRange { range, method } =>
                write!(f, "invoke-super/range {}, {}", range, method),
            DexInstruction::InvokeDirectRange { range, method } =>
                write!(f, "invoke-direct/range {}, {}", range, method),
            DexInstruction::InvokeStaticRange { range, method } =>
                write!(f, "invoke-static/range {}, {}", range, method),
            DexInstruction::InvokeInterfaceRange { range, method } =>
                write!(f, "invoke-interface/range {}, {}", range, method),

            // Group C: Arithmetic (non-2addr)
            DexInstruction::AddInt { dest, src1, src2 } =>
                write!(f, "add-int {}, {}, {}", dest, src1, src2),
            DexInstruction::SubInt { dest, src1, src2 } =>
                write!(f, "sub-int {}, {}, {}", dest, src1, src2),
            DexInstruction::MulInt { dest, src1, src2 } =>
                write!(f, "mul-int {}, {}, {}", dest, src1, src2),
            DexInstruction::DivInt { dest, src1, src2 } =>
                write!(f, "div-int {}, {}, {}", dest, src1, src2),
            DexInstruction::RemInt { dest, src1, src2 } =>
                write!(f, "rem-int {}, {}, {}", dest, src1, src2),
            DexInstruction::AndInt { dest, src1, src2 } =>
                write!(f, "and-int {}, {}, {}", dest, src1, src2),
            DexInstruction::OrInt { dest, src1, src2 } =>
                write!(f, "or-int {}, {}, {}", dest, src1, src2),
            DexInstruction::XorInt { dest, src1, src2 } =>
                write!(f, "xor-int {}, {}, {}", dest, src1, src2),
            DexInstruction::ShlInt { dest, src1, src2 } =>
                write!(f, "shl-int {}, {}, {}", dest, src1, src2),
            DexInstruction::ShrInt { dest, src1, src2 } =>
                write!(f, "shr-int {}, {}, {}", dest, src1, src2),
            DexInstruction::UshrInt { dest, src1, src2 } =>
                write!(f, "ushr-int {}, {}, {}", dest, src1, src2),
            DexInstruction::AddLong { dest, src1, src2 } =>
                write!(f, "add-long {}, {}, {}", dest, src1, src2),
            DexInstruction::SubLong { dest, src1, src2 } =>
                write!(f, "sub-long {}, {}, {}", dest, src1, src2),
            DexInstruction::MulLong { dest, src1, src2 } =>
                write!(f, "mul-long {}, {}, {}", dest, src1, src2),
            DexInstruction::DivLong { dest, src1, src2 } =>
                write!(f, "div-long {}, {}, {}", dest, src1, src2),
            DexInstruction::RemLong { dest, src1, src2 } =>
                write!(f, "rem-long {}, {}, {}", dest, src1, src2),
            DexInstruction::AndLong { dest, src1, src2 } =>
                write!(f, "and-long {}, {}, {}", dest, src1, src2),
            DexInstruction::OrLong { dest, src1, src2 } =>
                write!(f, "or-long {}, {}, {}", dest, src1, src2),
            DexInstruction::XorLong { dest, src1, src2 } =>
                write!(f, "xor-long {}, {}, {}", dest, src1, src2),
            DexInstruction::ShlLong { dest, src1, src2 } =>
                write!(f, "shl-long {}, {}, {}", dest, src1, src2),
            DexInstruction::ShrLong { dest, src1, src2 } =>
                write!(f, "shr-long {}, {}, {}", dest, src1, src2),
            DexInstruction::UshrLong { dest, src1, src2 } =>
                write!(f, "ushr-long {}, {}, {}", dest, src1, src2),
            DexInstruction::AddFloat { dest, src1, src2 } =>
                write!(f, "add-float {}, {}, {}", dest, src1, src2),
            DexInstruction::SubFloat { dest, src1, src2 } =>
                write!(f, "sub-float {}, {}, {}", dest, src1, src2),
            DexInstruction::MulFloat { dest, src1, src2 } =>
                write!(f, "mul-float {}, {}, {}", dest, src1, src2),
            DexInstruction::DivFloat { dest, src1, src2 } =>
                write!(f, "div-float {}, {}, {}", dest, src1, src2),
            DexInstruction::RemFloat { dest, src1, src2 } =>
                write!(f, "rem-float {}, {}, {}", dest, src1, src2),
            DexInstruction::AddDouble { dest, src1, src2 } =>
                write!(f, "add-double {}, {}, {}", dest, src1, src2),
            DexInstruction::SubDouble { dest, src1, src2 } =>
                write!(f, "sub-double {}, {}, {}", dest, src1, src2),
            DexInstruction::MulDouble { dest, src1, src2 } =>
                write!(f, "mul-double {}, {}, {}", dest, src1, src2),
            DexInstruction::DivDouble { dest, src1, src2 } =>
                write!(f, "div-double {}, {}, {}", dest, src1, src2),
            DexInstruction::RemDouble { dest, src1, src2 } =>
                write!(f, "rem-double {}, {}, {}", dest, src1, src2),

            // Group D: 2addr arithmetic instructions.
            DexInstruction::AddInt2Addr { reg, src } =>
                write!(f, "add-int/2addr {}, {}", reg, src),
            DexInstruction::SubInt2Addr { reg, src } =>
                write!(f, "sub-int/2addr {}, {}", reg, src),
            DexInstruction::MulInt2Addr { reg, src } =>
                write!(f, "mul-int/2addr {}, {}", reg, src),
            DexInstruction::DivInt2Addr { reg, src } =>
                write!(f, "div-int/2addr {}, {}", reg, src),
            DexInstruction::RemInt2Addr { reg, src } =>
                write!(f, "rem-int/2addr {}, {}", reg, src),
            DexInstruction::AndInt2Addr { reg, src } =>
                write!(f, "and-int/2addr {}, {}", reg, src),
            DexInstruction::OrInt2Addr { reg, src } =>
                write!(f, "or-int/2addr {}, {}", reg, src),
            DexInstruction::XorInt2Addr { reg, src } =>
                write!(f, "xor-int/2addr {}, {}", reg, src),
            DexInstruction::ShlInt2Addr { reg, src } =>
                write!(f, "shl-int/2addr {}, {}", reg, src),
            DexInstruction::ShrInt2Addr { reg, src } =>
                write!(f, "shr-int/2addr {}, {}", reg, src),
            DexInstruction::UshrInt2Addr { reg, src } =>
                write!(f, "ushr-int/2addr {}, {}", reg, src),
            DexInstruction::AddLong2Addr { reg, src } =>
                write!(f, "add-long/2addr {}, {}", reg, src),
            DexInstruction::SubLong2Addr { reg, src } =>
                write!(f, "sub-long/2addr {}, {}", reg, src),
            DexInstruction::MulLong2Addr { reg, src } =>
                write!(f, "mul-long/2addr {}, {}", reg, src),
            DexInstruction::DivLong2Addr { reg, src } =>
                write!(f, "div-long/2addr {}, {}", reg, src),
            DexInstruction::RemLong2Addr { reg, src } =>
                write!(f, "rem-long/2addr {}, {}", reg, src),
            DexInstruction::AndLong2Addr { reg, src } =>
                write!(f, "and-long/2addr {}, {}", reg, src),
            DexInstruction::OrLong2Addr { reg, src } =>
                write!(f, "or-long/2addr {}, {}", reg, src),
            DexInstruction::XorLong2Addr { reg, src } =>
                write!(f, "xor-long/2addr {}, {}", reg, src),
            DexInstruction::ShlLong2Addr { reg, src } =>
                write!(f, "shl-long/2addr {}, {}", reg, src),
            DexInstruction::ShrLong2Addr { reg, src } =>
                write!(f, "shr-long/2addr {}, {}", reg, src),
            DexInstruction::UshrLong2Addr { reg, src } =>
                write!(f, "ushr-long/2addr {}, {}", reg, src),
            DexInstruction::AddFloat2Addr { reg, src } =>
                write!(f, "add-float/2addr {}, {}", reg, src),
            DexInstruction::SubFloat2Addr { reg, src } =>
                write!(f, "sub-float/2addr {}, {}", reg, src),
            DexInstruction::MulFloat2Addr { reg, src } =>
                write!(f, "mul-float/2addr {}, {}", reg, src),
            DexInstruction::DivFloat2Addr { reg, src } =>
                write!(f, "div-float/2addr {}, {}", reg, src),
            DexInstruction::RemFloat2Addr { reg, src } =>
                write!(f, "rem-float/2addr {}, {}", reg, src),
            DexInstruction::AddDouble2Addr { reg, src } =>
                write!(f, "add-double/2addr {}, {}", reg, src),
            DexInstruction::SubDouble2Addr { reg, src } =>
                write!(f, "sub-double/2addr {}, {}", reg, src),
            DexInstruction::MulDouble2Addr { reg, src } =>
                write!(f, "mul-double/2addr {}, {}", reg, src),
            DexInstruction::DivDouble2Addr { reg, src } =>
                write!(f, "div-double/2addr {}, {}", reg, src),
            DexInstruction::RemDouble2Addr { reg, src } =>
                write!(f, "rem-double/2addr {}, {}", reg, src),
            // Group E: Polymorphic, custom and method handle/type constants.
            DexInstruction::InvokePolymorphic { registers, method, proto } => {
                let regs = registers.iter().map(|r| format!("{}", r)).collect::<Vec<_>>().join(", ");
                write!(f, "invoke-polymorphic {{{}}}, {}, {}", regs, method, proto)
            }
            DexInstruction::InvokePolymorphicRange { range , method, proto } => {
                write!(f, "invoke-polymorphic/range {}, {}, {}", range, method, proto)
            }
            DexInstruction::InvokeCustom { registers, call_site } => {
                let regs = registers.iter().map(|r| format!("{}", r)).collect::<Vec<_>>().join(", ");
                write!(f, "invoke-custom {{{}}}, {}", regs, call_site)
            }
            DexInstruction::InvokeCustomRange { range, call_site } => {
                write!(f, "invoke-custom/range {}, {}", range, call_site)
            }
            DexInstruction::ConstMethodHandle { dest, method_handle } =>
                write!(f, "const-method-handle {}, {}", dest, method_handle),
            DexInstruction::ConstMethodType { dest, proto } =>
                write!(f, "const-method-type {}, {}", dest, proto),

            // Conditional branches:
            DexInstruction::IfEq { reg1, reg2, offset } =>
                write!(f, "if-eq {}, {}, {}", reg1, reg2, offset),
            DexInstruction::IfNe { reg1, reg2, offset } =>
                write!(f, "if-ne {}, {}, {}", reg1, reg2, offset),
            DexInstruction::IfLt { reg1, reg2, offset } =>
                write!(f, "if-lt {}, {}, {}", reg1, reg2, offset),
            DexInstruction::IfGe { reg1, reg2, offset } =>
                write!(f, "if-ge {}, {}, {}", reg1, reg2, offset),
            DexInstruction::IfGt { reg1, reg2, offset } =>
                write!(f, "if-gt {}, {}, {}", reg1, reg2, offset),
            DexInstruction::IfLe { reg1, reg2, offset } =>
                write!(f, "if-le {}, {}, {}", reg1, reg2, offset),

            // Conditional branch instructions with a single register:
            DexInstruction::IfEqz { reg, offset } =>
                write!(f, "if-eqz {}, {}", reg, offset),
            DexInstruction::IfNez { reg, offset } =>
                write!(f, "if-nez {}, {}", reg, offset),
            DexInstruction::IfLtz { reg, offset } =>
                write!(f, "if-ltz {}, {}", reg, offset),
            DexInstruction::IfGez { reg, offset } =>
                write!(f, "if-gez {}, {}", reg, offset),
            DexInstruction::IfGtz { reg, offset } =>
                write!(f, "if-gtz {}, {}", reg, offset),
            DexInstruction::IfLez { reg, offset } =>
                write!(f, "if-lez {}, {}", reg, offset),

            // Invocation instructions
            DexInstruction::InvokeDirect { registers, method } => {
                let regs = registers.iter().map(|r| format!("{}", r))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "invoke-direct {{{}}}, {}", regs, method)
            }
            DexInstruction::InvokeStatic { registers, method } => {
                let regs = registers.iter().map(|r| format!("{}", r))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "invoke-static {{{}}}, {}", regs, method)
            }

            // Arithmetic instructions
            DexInstruction::NegInt { dest, src } =>
                write!(f, "neg-int {}, {}", dest, src),
            DexInstruction::NotInt { dest, src } =>
                write!(f, "not-int {}, {}", dest, src),
            DexInstruction::NegLong { dest, src } =>
                write!(f, "neg-long {}, {}", dest, src),
            DexInstruction::NotLong { dest, src } =>
                write!(f, "not-long {}, {}", dest, src),
            DexInstruction::NegFloat { dest, src } =>
                write!(f, "neg-float {}, {}", dest, src),
            DexInstruction::NegDouble { dest, src } =>
                write!(f, "neg-double {}, {}", dest, src),

            // Conversion instructions:
            DexInstruction::IntToLong { dest, src } =>
                write!(f, "int-to-long {}, {}", dest, src),
            DexInstruction::IntToFloat { dest, src } =>
                write!(f, "int-to-float {}, {}", dest, src),
            DexInstruction::IntToDouble { dest, src } =>
                write!(f, "int-to-double {}, {}", dest, src),
            DexInstruction::LongToInt { dest, src } =>
                write!(f, "long-to-int {}, {}", dest, src),
            DexInstruction::LongToFloat { dest, src } =>
                write!(f, "long-to-float {}, {}", dest, src),
            DexInstruction::LongToDouble { dest, src } =>
                write!(f, "long-to-double {}, {}", dest, src),
            DexInstruction::FloatToInt { dest, src } =>
                write!(f, "float-to-int {}, {}", dest, src),
            DexInstruction::FloatToLong { dest, src } =>
                write!(f, "float-to-long {}, {}", dest, src),
            DexInstruction::FloatToDouble { dest, src } =>
                write!(f, "float-to-double {}, {}", dest, src),
            DexInstruction::DoubleToInt { dest, src } =>
                write!(f, "double-to-int {}, {}", dest, src),
            DexInstruction::DoubleToLong { dest, src } =>
                write!(f, "double-to-long {}, {}", dest, src),
            DexInstruction::DoubleToFloat { dest, src } =>
                write!(f, "double-to-float {}, {}", dest, src),

            // Additional conversion variants:
            DexInstruction::IntToByte { dest, src } =>
                write!(f, "int-to-byte {}, {}", dest, src),
            DexInstruction::IntToChar { dest, src } =>
                write!(f, "int-to-char {}, {}", dest, src),
            DexInstruction::IntToShort { dest, src } =>
                write!(f, "int-to-short {}, {}", dest, src),

            // Arithmetic literal instructions (example for int):
            DexInstruction::AddIntLit16 { dest, src, literal } =>
                write!(f, "add-int/lit16 {}, {}, {}", dest, src, literal),
            DexInstruction::RSubIntLit16 { dest, src, literal } =>
                write!(f, "rsub-int {}, {}, {}", dest, src, literal),
            DexInstruction::MulIntLit16 { dest, src, literal } =>
                write!(f, "mul-int/lit16 {}, {}, {}", dest, src, literal),
            DexInstruction::DivIntLit16 { dest, src, literal } =>
                write!(f, "div-int/lit16 {}, {}, {}", dest, src, literal),
            DexInstruction::RemIntLit16 { dest, src, literal } =>
                write!(f, "rem-int/lit16 {}, {}, {}", dest, src, literal),
            DexInstruction::AndIntLit16 { dest, src, literal } =>
                write!(f, "and-int/lit16 {}, {}, {}", dest, src, literal),
            DexInstruction::OrIntLit16 { dest, src, literal } =>
                write!(f, "or-int/lit16 {}, {}, {}", dest, src, literal),
            DexInstruction::XorIntLit16 { dest, src, literal } =>
                write!(f, "xor-int/lit16 {}, {}, {}", dest, src, literal),

            // Literal arithmetic instructions (lit8 variants):
            DexInstruction::AddIntLit8 { dest, src, literal } =>
                write!(f, "add-int/lit8 {}, {}, {}", dest, src, literal),
            DexInstruction::RSubIntLit8 { dest, src, literal } =>
                write!(f, "rsub-int/lit8 {}, {}, {}", dest, src, literal),
            DexInstruction::MulIntLit8 { dest, src, literal } =>
                write!(f, "mul-int/lit8 {}, {}, {}", dest, src, literal),
            DexInstruction::DivIntLit8 { dest, src, literal } =>
                write!(f, "div-int/lit8 {}, {}, {}", dest, src, literal),
            DexInstruction::RemIntLit8 { dest, src, literal } =>
                write!(f, "rem-int/lit8 {}, {}, {}", dest, src, literal),
            DexInstruction::AndIntLit8 { dest, src, literal } =>
                write!(f, "and-int/lit8 {}, {}, {}", dest, src, literal),
            DexInstruction::OrIntLit8  { dest, src, literal } =>
                write!(f, "or-int/lit8 {}, {}, {}", dest, src, literal),
            DexInstruction::XorIntLit8 { dest, src, literal } =>
                write!(f, "xor-int/lit8 {}, {}, {}", dest, src, literal),
            DexInstruction::ShlIntLit8 { dest, src, literal } =>
                write!(f, "shl-int/lit8 {}, {}, {}", dest, src, literal),
            DexInstruction::ShrIntLit8 { dest, src, literal } =>
                write!(f, "shr-int/lit8 {}, {}, {}", dest, src, literal),
            DexInstruction::UshrIntLit8 { dest, src, literal } =>
                write!(f, "ushr-int/lit8 {}, {}, {}", dest, src, literal),

            // Unused - shouldn't come across this
            DexInstruction::Unused { .. } => { panic!("Attempted fmt display on Unused instruction") }
        }
    }
}

/// Parse a register reference like "v0" or "p1", returning its number.
fn parse_register(input: &str) -> IResult<&str, SmaliRegister> {
    // We accept either 'v' or 'p' followed by one or more digits.
    let (input, t) = alt((char('v'), char('p')))(input)?;
    let (input, num_str) = digit1(input)?;
    let num = num_str.parse::<u16>().unwrap();
    Ok((input, match t { 'v' => v(num), _ => p(num),
    }))
}

/// Parse a comma-separated list of registers inside curly braces.
fn parse_register_list(input: &str) -> IResult<&str, Vec<SmaliRegister>> {
    delimited(
        char('{'),
        separated_list0(delimited(space0, char(','), space0), parse_register),
        char('}')
    )(input)
}

/// Parses a string literal that may be empty.
/// For example, it can parse `""` as well as `"builder"`.
fn parse_string_literal(input: &str) -> IResult<&str, String> {
    let esc = escaped(none_of("\\\""), '\\', one_of("'\"tbnrfu\\"));
    let esc_or_empty = alt((esc, tag("")));

    let (i, s) = delimited(
        pair(multispace0, char('"')),
        esc_or_empty,
        pair(char('"'), multispace0)
    )(input)?;

    IResult::Ok((i, s.to_string()))

    /*delimited(
        char('"'),
        // Use take_while instead of take_while1 so that the inner content may be empty.
        map(take_while(|c| c != '"'), |s: &str| s.to_string()),
        char('"')
    )(input)*/
}

pub(crate) fn parse_literal_int<T>(input: &str) -> IResult<&str, T>
    where
        T: num_traits::Num
        + std::ops::Neg<Output = T>
        + std::str::FromStr
        + TryFrom<i64>,
        <T as TryFrom<i64>>::Error: Debug,
{
    // Consume an optional sign.
    let (input, sign) = opt(char('-'))(input)?;

    if input.starts_with("0x") || input.starts_with("0X") {
        let (input, _) = alt((tag("0x"), tag("0X")))(input)?;
        let (input, hex_digits) = take_while1(|c: char| c.is_digit(16))(input)?;
        let (input, _) = opt(char('L'))(input)?;
        if sign.is_some() {
            // Parse the digits as a u64, then handle the negative value.
            let value_u64 = u64::from_str_radix(hex_digits, 16)
                .map_err(|_| nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Digit)))?;
            let value_i64 = if value_u64 == 0x8000000000000000 {
                i64::MIN
            } else if value_u64 <= i64::MAX as u64 {
                -(value_u64 as i64)
            } else {
                return Err(nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Digit)));
            };
            let value = T::try_from(value_i64).expect("Conversion failed");
            Ok((input, value))
        } else {
            let value_u64 = u64::from_str_radix(hex_digits, 16)
                .map_err(|_| nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Digit)))?;
            if value_u64 > i64::MAX as u64 {
                return Err(nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Digit)));
            }
            let value_i64 = value_u64 as i64;
            let value = T::try_from(value_i64).expect("Conversion failed");
            Ok((input, value))
        }
    } else {
        let (input, num_str) = digit1(input)?;
        let (input, _) = opt(char('L'))(input)?;
        let mut value_i64 = num_str.parse::<i64>()
            .map_err(|_| nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Digit)))?;
        if sign.is_some() {
            value_i64 = -value_i64;
        }
        let value = T::try_from(value_i64).expect("Conversion failed");
        Ok((input, value))
    }
}

/*
pub(crate) fn parse_literal_int<T>(input: &str) -> IResult<&str, T>
    where
        T: num_traits::Num
        + std::ops::Neg<Output = T>
        + std::str::FromStr
        + TryFrom<i64>,
        <T as TryFrom<i64>>::Error: std::fmt::Debug,
{
    // Consume an optional sign.
    let (input, sign) = opt(char('-'))(input)?;

    if input.starts_with("0x") || input.starts_with("0X") {
        let (input, _) = alt((tag("0x"), tag("0X")))(input)?;
        let (input, hex_digits) = take_while1(|c: char| c.is_digit(16))(input)?;
        let (input, _) = opt(char('L'))(input)?;
        let mut value_i64 = i64::from_str_radix(hex_digits, 16)
            .map_err(|_| nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Digit)))?;
        if sign.is_some() {
            value_i64 = -value_i64;
        }
        let value = T::try_from(value_i64).expect("Conversion failed");
        Ok((input, value))
    } else {
        let (input, num_str) = digit1(input)?;
        let (input, _) = opt(char('L'))(input)?;
        let mut value_i64 = num_str.parse::<i64>()
            .map_err(|_| nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Digit)))?;
        if sign.is_some() {
            value_i64 = -value_i64;
        }
        let value = T::try_from(value_i64).expect("Conversion failed");
        Ok((input, value))
    }
}
 */

/// Parse a method reference of the form:
///    L<class>;-><method>(<args>)<ret>
/// For example:
///    Lkotlin/jvm/internal/Intrinsics;->checkNotNullParameter(Ljava/lang/Object;Ljava/lang/String;)V
fn parse_method_ref(input: &str) -> IResult<&str, MethodRef> {
    // Parse until the "->"
    let (input, class) = take_until("->")(input)?;
    let (input, _) = tag("->")(input)?;
    // Parse the method name (up to the opening parenthesis)
    let (input, name) = take_until("(")(input)?;
    let (input, descriptor) = parse_methodsignature(input)?;

    Ok((input, MethodRef {
        class: class.trim().to_owned(),
        name: name.trim().to_owned(),
        descriptor: descriptor.to_jni(),
    }))
}

fn parse_field_ref(input: &str) -> IResult<&str, FieldRef> {
    // Parse until the "->"
    let (input, class) = take_until("->")(input)?;
    let (input, _) = tag("->")(input)?;
    // Parse the method name (up to the opening parenthesis)
    let (input, name) = take_until(":")(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, descriptor) = parse_typesignature(input)?;

    Ok((input, FieldRef {
        class: class.trim().to_owned(),
        name: name.trim().to_owned(),
        descriptor: descriptor.to_jni(),
    }))
}


fn parse_const_high16(input: &str) -> IResult<&str, DexInstruction> {
    let (input, _) = space1(input)?;
    let (input, dest) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, value32): (&str, i32) = parse_literal_int(input)?;
    let value = (value32 >> 16) as i16;
    Ok((input, DexInstruction::ConstHigh16 { dest, value }))
}

fn parse_const_wide_high16(input: &str) -> IResult<&str, DexInstruction> {
    let (input, _) = space1(input)?;
    let (input, dest) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, value64): (&str, i64) = parse_literal_int(input)?;
    let value = (value64 >> 48) as i16;
    Ok((input, DexInstruction::ConstWideHigh16 { dest, value }))
}

fn parse_unused(input: &str) -> IResult<&str, DexInstruction> {
    let (input, opcode_str) = alphanumeric1(input)?;
    let opcode = u8::from_str_radix(opcode_str, 16).unwrap_or(0);
    Ok((input, DexInstruction::Unused { opcode }))
}

/// Parses a register range enclosed in braces, e.g. "{v0 .. v6}".
/// Returns a tuple (first_reg, last_reg)
fn parse_register_range(input: &str) -> IResult<&str, RegisterRange> {
    let (input, _) = delimited(space0, char('{'), space0)(input)?;
    let (input, start) = parse_register(input)?;
    let (input, _) = delimited(space0, tag(".."), space0)(input)?;
    let (input, end) = parse_register(input)?;
    let (input, _) = delimited(space0, char('}'), space0)(input)?;
    Ok((input, RegisterRange { start, end }))
}

fn parse_invoke_polymorphic(input: &str) -> IResult<&str, DexInstruction> {
    let (input, _) = space1(input)?;
    let (input, registers) = parse_register_list(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, method) = parse_method_ref(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, proto) = alphanumeric1(input)?;
    Ok((input, DexInstruction::InvokePolymorphic {
        registers,
        method,
        proto: proto.to_owned(),
    }))
}

fn parse_invoke_polymorphic_range(input: &str) -> IResult<&str, DexInstruction> {
    let (input, _) = space1(input)?;
    let (input, range) = parse_register_range(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, method) = parse_method_ref(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, proto) = alphanumeric1(input)?;
    Ok((input, DexInstruction::InvokePolymorphicRange { range, method, proto: proto.to_owned() }))
}

fn parse_invoke_custom(input: &str) -> IResult<&str, DexInstruction> {
    let (input, _) = space1(input)?;
    let (input, registers) = parse_register_list(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, call_site) = alphanumeric1(input)?;
    Ok((input, DexInstruction::InvokeCustom {
        registers,
        call_site: call_site.to_owned(),
    }))
}

fn parse_invoke_custom_range(input: &str) -> IResult<&str, DexInstruction> {
    let (input, _) = space1(input)?;
    let (input, range) = parse_register_range(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, call_site) = alphanumeric1(input)?;
    Ok((input, DexInstruction::InvokeCustomRange {
        range,
        call_site: call_site.to_owned(),
    }))
}


fn parse_invoke<F>(
    constructor: F,
    input: &str,
) -> IResult<&str, DexInstruction>
    where
        F: Fn(Vec<SmaliRegister>, MethodRef) -> DexInstruction,
{
    let (input, _) = space1(input)?;
    let (input, registers) = parse_register_list(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, method) = parse_method_ref(input)?;
    Ok((input, constructor(registers, method)))
}

macro_rules! invoke_case {
    ($variant:ident, $input: expr) => {
        parse_invoke(|regs, method| DexInstruction::$variant { registers: regs, method }, $input)
    };
}

fn parse_one_reg_instruction<F>(input: &str, constructor: F) -> IResult<&str, DexInstruction>
    where
        F: Fn(SmaliRegister) -> DexInstruction,
{
    let (input, _) = space1(input)?;
    let (input, reg) = parse_register(input)?;
    Ok((input, constructor(reg)))
}

macro_rules! one_reg_case {
    ($variant:ident, $field:ident, $input:expr) => {
        parse_one_reg_instruction($input, |r| DexInstruction::$variant { $field: r })
    };
}

/// Helper function: it consumes a space, then a register, then a comma (with optional spaces), then another register.
fn parse_two_reg_instruction<F>(input: &str, constructor: F) -> IResult<&str, DexInstruction>
    where
        F: Fn(SmaliRegister, SmaliRegister) -> DexInstruction,
{
    let (input, _) = space1(input)?;
    let (input, r1) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, r2) = parse_register(input)?;
    Ok((input, constructor(r1, r2)))
}

/// Macro for two-register instructions. You specify the variant name and the names of the fields.
macro_rules! two_reg_case {
    ($variant:ident, $field1:ident, $field2:ident, $input:expr) => {
        parse_two_reg_instruction($input, |r1, r2| DexInstruction::$variant {
            $field1: r1,
            $field2: r2,
        })
    };
}

/// Helper function: parses three registers from the input.
/// It expects at least one space, then a register, a comma, another register,
/// a comma, and a third register.
fn parse_three_reg_instruction<F>(input: &str, constructor: F) -> IResult<&str, DexInstruction>
    where
        F: Fn(SmaliRegister, SmaliRegister, SmaliRegister) -> DexInstruction,
{
    let (input, _) = space1(input)?;
    let (input, r1) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, r2) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, r3) = parse_register(input)?;
    Ok((input, constructor(r1, r2, r3)))
}

/// Macro for three-register instructions.
/// You supply the enum variant and the field names for each register,
/// along with the input.
macro_rules! three_reg_case {
    ($variant:ident, $field1:ident, $field2:ident, $field3:ident, $input:expr) => {
        parse_three_reg_instruction($input, |r1, r2, r3| {
            DexInstruction::$variant {
                $field1: r1,
                $field2: r2,
                $field3: r3,
            }
        })
    };
}

/// Helper for one-reg + literal instructions.
/// It assumes the opcode has already been consumed.
fn parse_one_reg_and_literal<T, F>(
    input: &str,
    constructor: F,
) -> IResult<&str, DexInstruction>
    where
        T: num_traits::Num + std::ops::Neg<Output = T> + std::str::FromStr + TryFrom<i64>,
        F: Fn(SmaliRegister, T) -> DexInstruction, <T as TryFrom<i64>>::Error: std::fmt::Debug
{
    let (input, _) = space1(input)?;
    let (input, reg) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, literal) = parse_literal_int::<T>(input)?;
    Ok((input, constructor(reg, literal)))
}

macro_rules! one_reg_lit_case {
    ($variant:ident, $field:ident, $lit_ty:ty, $input:expr) => {
        parse_one_reg_and_literal::<$lit_ty, _>($input, |r, lit| {
            DexInstruction::$variant { $field: r, value: lit }
        })
    };
}

/// Helper for two-reg + literal instructions.
fn parse_two_reg_and_literal<T, F>(
    input: &str,
    constructor: F,
) -> IResult<&str, DexInstruction>
    where
        T: num_traits::Num + std::ops::Neg<Output = T> + std::str::FromStr + TryFrom<i64>,
        F: Fn(SmaliRegister, SmaliRegister, T) -> DexInstruction, <T as TryFrom<i64>>::Error: std::fmt::Debug
{
    let (input, _) = space1(input)?;
    let (input, r1) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, r2) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, literal) = parse_literal_int::<T>(input)?;
    Ok((input, constructor(r1, r2, literal)))
}

macro_rules! two_reg_lit_case {
    ($variant:ident, $field1:ident, $field2:ident, $lit_ty:ty, $input:expr) => {
        parse_two_reg_and_literal::<$lit_ty, _>($input, |r1, r2, lit| {
            DexInstruction::$variant { $field1: r1, $field2: r2, literal: lit }
        })
    };
}

fn parse_one_reg_and_fieldref<F>(
    input: &str,
    constructor: F,
) -> IResult<&str, DexInstruction>
    where
        F: Fn(SmaliRegister, FieldRef) -> DexInstruction,
{
    let (input, _) = space1(input)?;
    let (input, dest) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, field) = parse_field_ref(input)?;
    Ok((input, constructor(dest, field)))
}

macro_rules! one_reg_fieldref_case {
    ($variant:ident, $reg:ident, $input:expr) => {
        parse_one_reg_and_fieldref($input, |reg, field| {
            DexInstruction::$variant { $reg: reg, field }
        })
    };
}

fn parse_two_reg_and_fieldref<F>(
    input: &str,
    constructor: F,
) -> IResult<&str, DexInstruction>
    where
        F: Fn(SmaliRegister, SmaliRegister, FieldRef) -> DexInstruction,
{
    let (input, _) = space1(input)?;
    let (input, reg1) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, reg2) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, field) = parse_field_ref(input)?;
    Ok((input, constructor(reg1, reg2, field)))
}

macro_rules! two_reg_fieldref_case {
    ($variant:ident, $reg1:ident, $input:expr) => {
        parse_two_reg_and_fieldref($input, |reg1, object, field| {
            DexInstruction::$variant { $reg1: reg1, object, field }
        })
    };
}

/// Helper for one-reg + literal instructions.
/// It assumes the opcode has already been consumed.
fn parse_one_reg_and_string<F>(
    input: &str,
    constructor: F,
) -> IResult<&str, DexInstruction>
    where
        F: Fn(SmaliRegister, String) -> DexInstruction,
{
    let (input, _) = space1(input)?;
    let (input, reg) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;

    // A string literal will have quotes
    let r: IResult<&str, &str> = tag("\"")(input);
    let (input, literal) = match r
    {
        IResult::Ok(_) => parse_string_literal(input)?,
        IResult::Err(_) => { let (i, ts) = parse_typesignature(input)?; (i, ts.to_jni()) }
    };
    Ok((input, constructor(reg, literal)))
}

macro_rules! one_reg_string_case {
    ($variant:ident, $field:ident, $string:ident, $input:expr) => {
        parse_one_reg_and_string($input, |r, lit| {
            DexInstruction::$variant { $field: r, $string: lit }
        })
    };
}

fn parse_two_reg_and_string<F>(
    input: &str,
    constructor: F,
) -> IResult<&str, DexInstruction>
    where
        F: Fn(SmaliRegister, SmaliRegister, String) -> DexInstruction,
{
    let (input, _) = space1(input)?;
    let (input, reg1) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, reg2) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;

    // A string literal will have quotes
    let r: IResult<&str, &str> = tag("\"")(input);
    let (input, literal) = match r
    {
        IResult::Ok(_) => parse_string_literal(input)?,
        IResult::Err(_) => { let (i, ts) = parse_typesignature(input)?; (i, ts.to_jni()) }
    };
    Ok((input, constructor(reg1, reg2, literal)))
}

macro_rules! two_reg_string_case {
    ($variant:ident, $reg:ident, $string:ident, $input:expr) => {
        parse_two_reg_and_string($input, |dest, reg, lit| {
            DexInstruction::$variant { dest, $reg: reg, $string: lit }
        })
    };
}


fn parse_one_reg_and_label<F>(
    input: &str,
    constructor: F,
) -> IResult<&str, DexInstruction>
    where
        F: Fn(SmaliRegister, Label) -> DexInstruction,
{
    let (input, _) = space1(input)?;
    let (input, reg) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, label) = parse_label(input)?;
    Ok((input, constructor(reg, label)))
}

macro_rules! one_reg_label_case {
    ($variant:ident, $input:expr) => {
        parse_one_reg_and_label($input, |reg, offset| {
            DexInstruction::$variant { reg, offset }
        })
    };
}

fn parse_two_reg_and_label<F>(
    input: &str,
    constructor: F,
) -> IResult<&str, DexInstruction>
    where
        F: Fn(SmaliRegister, SmaliRegister, Label) -> DexInstruction,
{
    let (input, _) = space1(input)?;
    let (input, reg1) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, reg2) = parse_register(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, label) = parse_label(input)?;
    Ok((input, constructor(reg1, reg2, label)))
}

macro_rules! two_reg_label_case {
    ($variant:ident, $input:expr) => {
        parse_two_reg_and_label($input, |reg1, reg2, offset| {
            DexInstruction::$variant { reg1, reg2, offset }
        })
    };
}

pub fn parse_range_and_method<F>(input: &str, constructor: F,) -> IResult<&str, DexInstruction>
    where
        F: Fn(RegisterRange, MethodRef) -> DexInstruction,
{
    let (input, _) = space1(input)?;
    let (input, range) = parse_register_range(input)?;
    let (input, _) = delimited(space0, char(','), space0)(input)?;
    let (input, method) = parse_method_ref(input)?;
    Ok((input, constructor(range, method )))
}

macro_rules! range_method_case {
    ($variant:ident, $input:expr) => {
        parse_range_and_method($input, |range, method| {
            DexInstruction::$variant { range, method }
        })
    };
}


// Higher level parser for all operations
pub fn parse_instruction(input: &str) -> IResult<&str, DexInstruction>
{
    let (input, op) = take_while1(|c: char| c.is_alphanumeric() || c == '-' || c == '/')(input)?;
    let r = match op
    {
        // Invoke instructions
        "invoke-static"    => invoke_case!(InvokeStatic, input),
        "invoke-virtual"   => invoke_case!(InvokeVirtual, input),
        "invoke-super"     => invoke_case!(InvokeSuper, input),
        "invoke-interface" => invoke_case!(InvokeInterface, input),
        "invoke-direct"    => invoke_case!(InvokeDirect, input),

        // One-register instructions.
        "move-result"       => one_reg_case!(MoveResult,       dest, input),
        "move-result-wide"  => one_reg_case!(MoveResultWide,   dest, input),
        "move-result-object"=> one_reg_case!(MoveResultObject, dest, input),
        "move-exception"    => one_reg_case!(MoveException,    dest, input),
        "return"            => one_reg_case!(Return,           src,  input),
        "return-wide"       => one_reg_case!(ReturnWide,       src,  input),
        "return-object"     => one_reg_case!(ReturnObject,     src,  input),
        "monitor-enter"     => one_reg_case!(MonitorEnter,     src,  input),
        "monitor-exit"      => one_reg_case!(MonitorExit,      src,  input),
        "throw"             => one_reg_case!(Throw,            src,  input),

        // Two register instructions
        // Group A: Move instructions.
        "move"               => two_reg_case!(Move, dest, src, input),
        "move/from16"        => two_reg_case!(MoveFrom16, dest, src, input),
        "move/16"            => two_reg_case!(Move16, dest, src, input),
        "move-wide"          => two_reg_case!(MoveWide, dest, src, input),
        "move-wide/from16"   => two_reg_case!(MoveWideFrom16, dest, src, input),
        "move-wide/16"       => two_reg_case!(MoveWide16, dest, src, input),
        "move-object"        => two_reg_case!(MoveObject, dest, src, input),
        "move-object/from16" => two_reg_case!(MoveObjectFrom16, dest, src, input),
        "move-object/16"     => two_reg_case!(MoveObject16, dest, src, input),
        // Group A: Array length.
        "array-length"       => two_reg_case!(ArrayLength, dest, array, input),
        // Group A: Conversion instructions.
        "int-to-byte"        => two_reg_case!(IntToByte, dest, src, input),
        "int-to-char"        => two_reg_case!(IntToChar, dest, src, input),
        "int-to-short"       => two_reg_case!(IntToShort, dest, src, input),
        // Group A: Unary arithmetic instructions.
        "neg-int"            => two_reg_case!(NegInt, dest, src, input),
        "not-int"            => two_reg_case!(NotInt, dest, src, input),
        "neg-long"           => two_reg_case!(NegLong, dest, src, input),
        "not-long"           => two_reg_case!(NotLong, dest, src, input),
        "neg-float"          => two_reg_case!(NegFloat, dest, src, input),
        "neg-double"         => two_reg_case!(NegDouble, dest, src, input),
        // Group C: Conversion arithmetic instructions.
        "int-to-long"        => two_reg_case!(IntToLong, dest, src, input),
        "int-to-float"       => two_reg_case!(IntToFloat, dest, src, input),
        "int-to-double"      => two_reg_case!(IntToDouble, dest, src, input),
        "long-to-int"        => two_reg_case!(LongToInt, dest, src, input),
        "long-to-float"      => two_reg_case!(LongToFloat, dest, src, input),
        "long-to-double"     => two_reg_case!(LongToDouble, dest, src, input),
        "float-to-int"       => two_reg_case!(FloatToInt, dest, src, input),
        "float-to-long"      => two_reg_case!(FloatToLong, dest, src, input),
        "float-to-double"    => two_reg_case!(FloatToDouble, dest, src, input),
        "double-to-int"      => two_reg_case!(DoubleToInt, dest, src, input),
        "double-to-long"     => two_reg_case!(DoubleToLong, dest, src, input),
        "double-to-float"    => two_reg_case!(DoubleToFloat, dest, src, input),
        // Group D: 2addr arithmetic instructions (using reg and src).
        "add-int/2addr"      => two_reg_case!(AddInt2Addr, reg, src, input),
        "sub-int/2addr"      => two_reg_case!(SubInt2Addr, reg, src, input),
        "mul-int/2addr"      => two_reg_case!(MulInt2Addr, reg, src, input),
        "div-int/2addr"      => two_reg_case!(DivInt2Addr, reg, src, input),
        "rem-int/2addr"      => two_reg_case!(RemInt2Addr, reg, src, input),
        "and-int/2addr"      => two_reg_case!(AndInt2Addr, reg, src, input),
        "or-int/2addr"       => two_reg_case!(OrInt2Addr, reg, src, input),
        "xor-int/2addr"      => two_reg_case!(XorInt2Addr, reg, src, input),
        "shl-int/2addr"      => two_reg_case!(ShlInt2Addr, reg, src, input),
        "shr-int/2addr"      => two_reg_case!(ShrInt2Addr, reg, src, input),
        "ushr-int/2addr"     => two_reg_case!(UshrInt2Addr, reg, src, input),
        "add-long/2addr"     => two_reg_case!(AddLong2Addr, reg, src, input),
        "sub-long/2addr"     => two_reg_case!(SubLong2Addr, reg, src, input),
        "mul-long/2addr"     => two_reg_case!(MulLong2Addr, reg, src, input),
        "div-long/2addr"     => two_reg_case!(DivLong2Addr, reg, src, input),
        "rem-long/2addr"     => two_reg_case!(RemLong2Addr, reg, src, input),
        "and-long/2addr"     => two_reg_case!(AndLong2Addr, reg, src, input),
        "or-long/2addr"      => two_reg_case!(OrLong2Addr, reg, src, input),
        "xor-long/2addr"     => two_reg_case!(XorLong2Addr, reg, src, input),
        "shl-long/2addr"     => two_reg_case!(ShlLong2Addr, reg, src, input),
        "shr-long/2addr"     => two_reg_case!(ShrLong2Addr, reg, src, input),
        "ushr-long/2addr"    => two_reg_case!(UshrLong2Addr, reg, src, input),
        "add-float/2addr"    => two_reg_case!(AddFloat2Addr, reg, src, input),
        "sub-float/2addr"    => two_reg_case!(SubFloat2Addr, reg, src, input),
        "mul-float/2addr"    => two_reg_case!(MulFloat2Addr, reg, src, input),
        "div-float/2addr"    => two_reg_case!(DivFloat2Addr, reg, src, input),
        "rem-float/2addr"    => two_reg_case!(RemFloat2Addr, reg, src, input),
        "add-double/2addr"   => two_reg_case!(AddDouble2Addr, reg, src, input),
        "sub-double/2addr"   => two_reg_case!(SubDouble2Addr, reg, src, input),
        "mul-double/2addr"   => two_reg_case!(MulDouble2Addr, reg, src, input),
        "div-double/2addr"   => two_reg_case!(DivDouble2Addr, reg, src, input),
        "rem-double/2addr"   => two_reg_case!(RemDouble2Addr, reg, src, input),

        // Three register instructions
        // Group B: Array get/put instructions.
        "aget"         => three_reg_case!(AGet, dest, array, index, input),
        "aget-wide"    => three_reg_case!(AGetWide, dest, array, index, input),
        "aget-object"  => three_reg_case!(AGetObject, dest, array, index, input),
        "aget-boolean" => three_reg_case!(AGetBoolean, dest, array, index, input),
        "aget-byte"    => three_reg_case!(AGetByte, dest, array, index, input),
        "aget-char"    => three_reg_case!(AGetChar, dest, array, index, input),
        "aget-short"   => three_reg_case!(AGetShort, dest, array, index, input),
        "aput"         => three_reg_case!(APut, src, array, index, input),
        "aput-wide"    => three_reg_case!(APutWide, src, array, index, input),
        "aput-object"  => three_reg_case!(APutObject, src, array, index, input),
        "aput-boolean" => three_reg_case!(APutBoolean, src, array, index, input),
        "aput-byte"    => three_reg_case!(APutByte, src, array, index, input),
        "aput-char"    => three_reg_case!(APutChar, src, array, index, input),
        "aput-short"   => three_reg_case!(APutShort, src, array, index, input),
        // Group C: Arithmetic instructions (non-2addr and comparisons).
        "add-int"   => three_reg_case!(AddInt, dest, src1, src2, input),
        "sub-int"   => three_reg_case!(SubInt, dest, src1, src2, input),
        "mul-int"   => three_reg_case!(MulInt, dest, src1, src2, input),
        "div-int"   => three_reg_case!(DivInt, dest, src1, src2, input),
        "rem-int"   => three_reg_case!(RemInt, dest, src1, src2, input),
        "and-int"   => three_reg_case!(AndInt, dest, src1, src2, input),
        "or-int"    => three_reg_case!(OrInt, dest, src1, src2, input),
        "xor-int"   => three_reg_case!(XorInt, dest, src1, src2, input),
        "shl-int"   => three_reg_case!(ShlInt, dest, src1, src2, input),
        "shr-int"   => three_reg_case!(ShrInt, dest, src1, src2, input),
        "ushr-int"  => three_reg_case!(UshrInt, dest, src1, src2, input),
        "add-long"  => three_reg_case!(AddLong, dest, src1, src2, input),
        "sub-long"  => three_reg_case!(SubLong, dest, src1, src2, input),
        "mul-long"  => three_reg_case!(MulLong, dest, src1, src2, input),
        "div-long"  => three_reg_case!(DivLong, dest, src1, src2, input),
        "rem-long"  => three_reg_case!(RemLong, dest, src1, src2, input),
        "and-long"  => three_reg_case!(AndLong, dest, src1, src2, input),
        "or-long"   => three_reg_case!(OrLong, dest, src1, src2, input),
        "xor-long"  => three_reg_case!(XorLong, dest, src1, src2, input),
        "shl-long"  => three_reg_case!(ShlLong, dest, src1, src2, input),
        "shr-long"  => three_reg_case!(ShrLong, dest, src1, src2, input),
        "ushr-long" => three_reg_case!(UshrLong, dest, src1, src2, input),
        "add-float"  => three_reg_case!(AddFloat, dest, src1, src2, input),
        "sub-float"  => three_reg_case!(SubFloat, dest, src1, src2, input),
        "mul-float"  => three_reg_case!(MulFloat, dest, src1, src2, input),
        "div-float"  => three_reg_case!(DivFloat, dest, src1, src2, input),
        "rem-float"  => three_reg_case!(RemFloat, dest, src1, src2, input),
        "add-double" => three_reg_case!(AddDouble, dest, src1, src2, input),
        "sub-double" => three_reg_case!(SubDouble, dest, src1, src2, input),
        "mul-double" => three_reg_case!(MulDouble, dest, src1, src2, input),
        "div-double" => three_reg_case!(DivDouble, dest, src1, src2, input),
        "rem-double" => three_reg_case!(RemDouble, dest, src1, src2, input),
        // Comparison instructions.
        "cmpl-float" => three_reg_case!(CmplFloat, dest, src1, src2, input),
        "cmpg-float" => three_reg_case!(CmpgFloat, dest, src1, src2, input),
        "cmpl-double" => three_reg_case!(CmplDouble, dest, src1, src2, input),
        "cmpg-double" => three_reg_case!(CmpgDouble, dest, src1, src2, input),
        "cmp-long"   => three_reg_case!(CmpLong, dest, src1, src2, input),

        // One-register literal instructions (constants):
        "const"         => one_reg_lit_case!(Const, dest, i32, input),
        "const/4"       => one_reg_lit_case!(Const4, dest, i8, input),
        "const/16"      => one_reg_lit_case!(Const16, dest, i16, input),
        "const-wide"    => one_reg_lit_case!(ConstWide, dest, i64, input),
        "const-wide/16" => one_reg_lit_case!(ConstWide16, dest, i16, input),
        "const-wide/32" => one_reg_lit_case!(ConstWide32, dest, i32, input),

        // Two-register literal instructions (lit8):
        "add-int/lit8"  => two_reg_lit_case!(AddIntLit8, dest, src, i8, input),
        "rsub-int/lit8" => two_reg_lit_case!(RSubIntLit8, dest, src, i8, input),
        "mul-int/lit8"  => two_reg_lit_case!(MulIntLit8, dest, src, i8, input),
        "div-int/lit8"  => two_reg_lit_case!(DivIntLit8, dest, src, i8, input),
        "rem-int/lit8"  => two_reg_lit_case!(RemIntLit8, dest, src, i8, input),
        "and-int/lit8"  => two_reg_lit_case!(AndIntLit8, dest, src, i8, input),
        "or-int/lit8"   => two_reg_lit_case!(OrIntLit8, dest, src, i8, input),
        "xor-int/lit8"  => two_reg_lit_case!(XorIntLit8, dest, src, i8, input),
        "shl-int/lit8"  => two_reg_lit_case!(ShlIntLit8, dest, src, i8, input),
        "shr-int/lit8"  => two_reg_lit_case!(ShrIntLit8, dest, src, i8, input),
        "ushr-int/lit8" => two_reg_lit_case!(UshrIntLit8, dest, src, i8, input),

        // Two-register literal instructions (lit16):
        "add-int/lit16" => two_reg_lit_case!(AddIntLit16, dest, src, i16, input),
        "rsub-int"      => two_reg_lit_case!(RSubIntLit16, dest, src, i16, input),
        "mul-int/lit16" => two_reg_lit_case!(MulIntLit16, dest, src, i16, input),
        "div-int/lit16" => two_reg_lit_case!(DivIntLit16, dest, src, i16, input),
        "rem-int/lit16" => two_reg_lit_case!(RemIntLit16, dest, src, i16, input),
        "and-int/lit16" => two_reg_lit_case!(AndIntLit16, dest, src, i16, input),
        "or-int/lit16"  => two_reg_lit_case!(OrIntLit16, dest, src, i16, input),
        "xor-int/lit16" => two_reg_lit_case!(XorIntLit16, dest, src, i16, input),

        // One reg and field
        "sget"         => one_reg_fieldref_case!(SGet,       dest, input),
        "sget-wide"    => one_reg_fieldref_case!(SGetWide,   dest, input),
        "sget-object"  => one_reg_fieldref_case!(SGetObject, dest, input),
        "sget-boolean" => one_reg_fieldref_case!(SGetBoolean,dest, input),
        "sget-byte"    => one_reg_fieldref_case!(SGetByte,   dest, input),
        "sget-char"    => one_reg_fieldref_case!(SGetChar,   dest, input),
        "sget-short"   => one_reg_fieldref_case!(SGetShort,  dest, input),
        "sput"         => one_reg_fieldref_case!(SPut,       src,  input),
        "sput-wide"    => one_reg_fieldref_case!(SPutWide,   src,  input),
        "sput-object"  => one_reg_fieldref_case!(SPutObject, src,  input),
        "sput-boolean" => one_reg_fieldref_case!(SPutBoolean,src,  input),
        "sput-byte"    => one_reg_fieldref_case!(SPutByte,   src,  input),
        "sput-char"    => one_reg_fieldref_case!(SPutChar,   src,  input),
        "sput-short"   => one_reg_fieldref_case!(SPutShort,  src,  input),

        // Two reg and field
        "iget"         => two_reg_fieldref_case!(IGet, dest, input),
        "iget-wide"    => two_reg_fieldref_case!(IGetWide, dest, input),
        "iget-object"  => two_reg_fieldref_case!(IGetObject, dest, input),
        "iget-boolean"  => two_reg_fieldref_case!(IGetBoolean, dest, input),
        "iget-byte"     => two_reg_fieldref_case!(IGetByte, dest, input),
        "iget-char"     => two_reg_fieldref_case!(IGetChar, dest, input),
        "iget-short"    => two_reg_fieldref_case!(IGetShort, dest, input),
        "iput"          => two_reg_fieldref_case!(IPut, src, input),
        "iput-wide"     => two_reg_fieldref_case!(IPutWide, src, input),
        "iput-object"   => two_reg_fieldref_case!(IPutObject, src, input),
        "iput-boolean"  => two_reg_fieldref_case!(IPutBoolean, src, input),
        "iput-byte"     => two_reg_fieldref_case!(IPutByte, src, input),
        "iput-char"     => two_reg_fieldref_case!(IPutChar, src, input),
        "iput-short"    => two_reg_fieldref_case!(IPutShort, src, input),

        // One reg & string
        "const-string" => one_reg_string_case!(ConstString, dest, value, input),
        "const-string/jumbo" => one_reg_string_case!(ConstStringJumbo, dest, value, input),
        "const-class" => one_reg_string_case!(ConstClass, dest, class, input),
        "check-cast" => one_reg_string_case!(CheckCast, dest, class, input),
        "new-instance" => one_reg_string_case!(NewInstance, dest, class, input),
        "const-method-handle" => one_reg_string_case!(ConstMethodHandle, dest, method_handle, input),
        "const-method-type" => one_reg_string_case!(ConstMethodType, dest, proto, input),

        // Two regs & string
        "instance-of"  => two_reg_string_case!(InstanceOf, src, class, input),
        "new-array"  => two_reg_string_case!(NewArray, size_reg, class, input),

        // Gotos = 1 label
        "goto" => { let (_, offset) = preceded(space1, parse_label)(input)?; IResult::Ok((input, DexInstruction::Goto { offset })) }
        "goto/16" => { let (_, offset) = preceded(space1, parse_label)(input)?; IResult::Ok((input, DexInstruction::Goto16 { offset })) }
        "goto/32" => { let (_, offset) = preceded(space1, parse_label)(input)?; IResult::Ok((input, DexInstruction::Goto32 { offset })) }

        // One reg & label
        "if-eqz" => one_reg_label_case!(IfEqz, input),
        "if-nez" => one_reg_label_case!(IfNez, input),
        "if-ltz" => one_reg_label_case!(IfLtz, input),
        "if-gez" => one_reg_label_case!(IfGez, input),
        "if-gtz" => one_reg_label_case!(IfGtz, input),
        "if-lez" => one_reg_label_case!(IfLez, input),
        "packed-switch" => one_reg_label_case!(PackedSwitch, input),
        "sparse-switch" => one_reg_label_case!(SparseSwitch, input),
        "fill-array-data"  => one_reg_label_case!(FillArrayData, input),

        // Two regs & label
        "if-eq" => two_reg_label_case!(IfEq, input),
        "if-ne" => two_reg_label_case!(IfNe, input),
        "if-lt" => two_reg_label_case!(IfLt, input),
        "if-ge" => two_reg_label_case!(IfGe, input),
        "if-gt" => two_reg_label_case!(IfGt, input),
        "if-le" => two_reg_label_case!(IfLe, input),

        // Range and method
        "invoke-virtual/range" => range_method_case!(InvokeVirtualRange, input),
        "invoke-super/range" => range_method_case!(InvokeSuperRange, input),
        "invoke-direct/range" => range_method_case!(InvokeDirectRange, input),
        "invoke-static/range" => range_method_case!(InvokeStaticRange, input),
        "invoke-interface/range" => range_method_case!(InvokeInterfaceRange, input),

        // Oddities
        "invoke-polymorphic" => parse_invoke_polymorphic(input),
        "invoke-polymorphic/range" => parse_invoke_polymorphic_range(input),
        "invoke-custom" => parse_invoke_custom(input),
        "invoke-custom/range" => parse_invoke_custom_range(input),
        "const/high16" => parse_const_high16(input),
        "const-wide/high16" => parse_const_wide_high16(input),
        "nop" => IResult::Ok((input, DexInstruction::Nop)),
        "return-void" => IResult::Ok((input, DexInstruction::ReturnVoid)),

        _ => { panic!("Unhandled instruction {} {}", op, input) }
    };
    match r
    {
        Ok(_) => r,
        Err(e) => { panic!("Error parsing instruction {} {}: {}", op, input, e) }
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_const_string() {
        let input = r#"const-string v0, "builder""#;
        let (rest, instr) = parse_instruction(input).unwrap();
        assert!(rest.trim().is_empty());
        assert_eq!(instr, DexInstruction::ConstString {
            dest: v(0),
            value: "builder".to_owned()
        });
    }

    #[test]
    fn test_parse_method_ref() {
        let input = r#"Landroidx/core/content/res/TypedArrayUtils;->getNamedString(Landroid/content/res/TypedArray;Lorg/xmlpull/v1/XmlPullParser;Ljava/lang/String;I)Ljava/lang/String;"#;
        let (rest, mr) = parse_method_ref(input).unwrap();
        assert!(rest.trim().is_empty());
        assert_eq!(mr.to_string(), input);
    }

    #[test]
    fn test_invoke_static() {
        let input = r#"invoke-static {p1, p2, v0, v1}, Landroidx/core/content/res/TypedArrayUtils;->getNamedString(Landroid/content/res/TypedArray;Lorg/xmlpull/v1/XmlPullParser;Ljava/lang/String;I)Ljava/lang/String;"#;
        let (rest, instr) = parse_instruction(input).unwrap();
        assert!(rest.trim().is_empty());
        let expected_method = MethodRef {
            class: "Landroidx/core/content/res/TypedArrayUtils;".to_owned(),
            name: "getNamedString".to_owned(),
            descriptor: "(Landroid/content/res/TypedArray;Lorg/xmlpull/v1/XmlPullParser;Ljava/lang/String;I)Ljava/lang/String;".to_owned(),
        };
        assert_eq!(instr, DexInstruction::InvokeStatic {
            registers: vec![p(1), p(2), v(0), v(1)],
            method: expected_method,
        });
    }

    #[test]
    fn test_invoke_direct() {
        let input = r#"invoke-direct {p0}, Ljava/lang/Object;-><init>()V"#;
        let (rest, instr) = parse_instruction(input).unwrap();
        assert!(rest.trim().is_empty());
        let expected_method = MethodRef {
            class: "Ljava/lang/Object;".to_owned(),
            name: "<init>".to_owned(),
            descriptor: "()V".to_owned(),
        };
        assert_eq!(instr, DexInstruction::InvokeDirect {
            registers: vec![p(0)],
            method: expected_method,
        });
    }

    #[test]
    fn test_parse_literal_int()
    {
        let (_, i): (_, i8)  = parse_literal_int("-0x5").unwrap();
        assert_eq!(i, -5);

        let (_, i): (_, i8)  = parse_literal_int("50").unwrap();
        assert_eq!(i, 50);

        let (_, i): (_, i16)  = parse_literal_int("-0x7c05").unwrap();
        assert_eq!(i, -0x7c05);

        let (_, i): (_, i32)  = parse_literal_int("0x7fffffff").unwrap();
        assert_eq!(i, 0x7fffffff);

        let (_, i): (_, i32)  = parse_literal_int("-0x80000000").unwrap();
        assert_eq!(i, -0x80000000);

        let (_, i): (_, i32)  = parse_literal_int("-0x80000000").unwrap();
        let sixteen: i16 = (i >> 16) as i16;
        assert_eq!(sixteen, -0x8000);
    }
}