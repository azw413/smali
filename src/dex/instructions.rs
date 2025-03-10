//! Representation of the Dalvik bytecodes


/*
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum FormatID {
    Format00x,   // "00x"
    Format10x,   // "10x"
    Format12x,   // "12x"
    Format11n,   // "11n"
    Format11x,   // "11x"
    Format10t,   // "10t"
    Format20t,   // "20t"
    Format20bc,  // "20bc"
    Format22x,   // "22x"
    Format21t,   // "21t"
    Format21s,   // "21s"
    Format21h,   // "21h"
    Format21c,   // "21c"
    Format23x,   // "23x"
    Format22b,   // "22b"
    Format22t,   // "22t"
    Format22s,   // "22s"
    Format22c,   // "22c"
    Format22cs,  // "22cs"
    Format30t,   // "30t"
    Format32x,   // "32x"
    Format31i,   // "31i"
    Format31t,   // "31t"
    Format31c,   // "31c"
    Format35c,   // "35c"
    Format35ms,  // "35ms"
    Format35mi,  // "35mi"
    Format3rc,   // "3rc"
    Format3rms,  // "3rms"
    Format3rmi,  // "3rmi"
    Format45cc,  // "45cc"
    Format4rcc,  // "4rcc"
    Format51l,   // "51l"
    FormatNAX,   // "N/A" - pseudo-format
}

use std::collections::HashMap;

/// Represents the opcode and its corresponding instruction format.
struct OpcodeInfo {
    opcode: u8,
    format: FormatID,
}

/// Returns a HashMap mapping opcodes to their OpcodeInfo.
/// This map should include all Dalvik opcodes used by DexInstruction.
fn get_opcode_map() -> HashMap<u8, OpcodeInfo> {
    let mut map = HashMap::new();

    // Example Opcode Mappings
    // Note: Update these mappings based on the official Dalvik Opcode Documentation.

    // NOP: opcode 0x00, format "00x"
    map.insert(0x00, OpcodeInfo { opcode: 0x00, format: FormatID::Format00x });

    // MOVE: opcode 0x01, format "12x"
    map.insert(0x01, OpcodeInfo { opcode: 0x01, format: FormatID::Format12x });

    // MOVE_FROM16: opcode 0x02, format "22x"
    map.insert(0x02, OpcodeInfo { opcode: 0x02, format: FormatID::Format22x });

    // MOVE_16: opcode 0x03, format "32x"
    map.insert(0x03, OpcodeInfo { opcode: 0x03, format: FormatID::Format32x });

    // MOVE_WIDE: opcode 0x04, format "12x"
    map.insert(0x04, OpcodeInfo { opcode: 0x04, format: FormatID::Format12x });

    // MOVE_WIDE_FROM16: opcode 0x05, format "22x"
    map.insert(0x05, OpcodeInfo { opcode: 0x05, format: FormatID::Format22x });

    // MOVE_WIDE16: opcode 0x06, format "32x"
    map.insert(0x06, OpcodeInfo { opcode: 0x06, format: FormatID::Format32x });

    // MOVE_OBJECT: opcode 0x07, format "12x"
    map.insert(0x07, OpcodeInfo { opcode: 0x07, format: FormatID::Format12x });

    // MOVE_OBJECT_FROM16: opcode 0x08, format "22x"
    map.insert(0x08, OpcodeInfo { opcode: 0x08, format: FormatID::Format22x });

    // MOVE_OBJECT16: opcode 0x09, format "32x"
    map.insert(0x09, OpcodeInfo { opcode: 0x09, format: FormatID::Format32x });

    // MOVE_RESULT: opcode 0x0A, format "11x"
    map.insert(0x0A, OpcodeInfo { opcode: 0x0A, format: FormatID::Format11x });

    // MOVE_RESULT_WIDE: opcode 0x0B, format "11x"
    map.insert(0x0B, OpcodeInfo { opcode: 0x0B, format: FormatID::Format11x });

    // MOVE_RESULT_OBJECT: opcode 0x0C, format "11x"
    map.insert(0x0C, OpcodeInfo { opcode: 0x0C, format: FormatID::Format11x });

    // MOVE_EXCEPTION: opcode 0x0D, format "11x"
    map.insert(0x0D, OpcodeInfo { opcode: 0x0D, format: FormatID::Format11x });

    // RETURN_VOID: opcode 0x0E, format "10x"
    map.insert(0x0E, OpcodeInfo { opcode: 0x0E, format: FormatID::Format10x });

    // RETURN: opcode 0x0F, format "11x"
    map.insert(0x0F, OpcodeInfo { opcode: 0x0F, format: FormatID::Format11x });

    // RETURN_WIDE: opcode 0x10, format "11x"
    map.insert(0x10, OpcodeInfo { opcode: 0x10, format: FormatID::Format11x });

    // RETURN_OBJECT: opcode 0x11, format "11x"
    map.insert(0x11, OpcodeInfo { opcode: 0x11, format: FormatID::Format11x });

    // CONST_4: opcode 0x12, format "11n"
    map.insert(0x12, OpcodeInfo { opcode: 0x12, format: FormatID::Format11n });

    // CONST_16: opcode 0x13, format "21s"
    map.insert(0x13, OpcodeInfo { opcode: 0x13, format: FormatID::Format21s });

    // CONST: opcode 0x14, format "21c"
    map.insert(0x14, OpcodeInfo { opcode: 0x14, format: FormatID::Format21c });

    // CONST_HIGH16: opcode 0x15, format "21h"
    map.insert(0x15, OpcodeInfo { opcode: 0x15, format: FormatID::Format21h });

    // CONST_WIDE_16: opcode 0x16, format "31i"
    map.insert(0x16, OpcodeInfo { opcode: 0x16, format: FormatID::Format31i });

    // CONST_WIDE_32: opcode 0x17, format "31i"
    map.insert(0x17, OpcodeInfo { opcode: 0x17, format: FormatID::Format31i });

    // CONST_WIDE: opcode 0x18, format "31i"
    map.insert(0x18, OpcodeInfo { opcode: 0x18, format: FormatID::Format31i });

    // CONST_WIDE_HIGH16: opcode 0x19, format "21h"
    map.insert(0x19, OpcodeInfo { opcode: 0x19, format: FormatID::Format21h });

    // CONST_STRING: opcode 0x1A, format "21c"
    map.insert(0x1A, OpcodeInfo { opcode: 0x1A, format: FormatID::Format21c });

    // CONST_STRING_JUMBO: opcode 0x1B, format "31c"
    map.insert(0x1B, OpcodeInfo { opcode: 0x1B, format: FormatID::Format31c });

    // CONST_CLASS: opcode 0x1C, format "21c"
    map.insert(0x1C, OpcodeInfo { opcode: 0x1C, format: FormatID::Format21c });

    // MONITOR_ENTER: opcode 0x1D, format "11x"
    map.insert(0x1D, OpcodeInfo { opcode: 0x1D, format: FormatID::Format11x });

    // MONITOR_EXIT: opcode 0x1E, format "11x"
    map.insert(0x1E, OpcodeInfo { opcode: 0x1E, format: FormatID::Format11x });

    // CHECK_CAST: opcode 0x1F, format "21c"
    map.insert(0x1F, OpcodeInfo { opcode: 0x1F, format: FormatID::Format21c });

    // INSTANCE_OF: opcode 0x20, format "22c"
    map.insert(0x20, OpcodeInfo { opcode: 0x20, format: FormatID::Format22c });

    // ARRAY_LENGTH: opcode 0x21, format "22x"
    map.insert(0x21, OpcodeInfo { opcode: 0x21, format: FormatID::Format22x });

    // NEW_INSTANCE: opcode 0x22, format "21c"
    map.insert(0x22, OpcodeInfo { opcode: 0x22, format: FormatID::Format21c });

    // NEW_ARRAY: opcode 0x23, format "22c"
    map.insert(0x23, OpcodeInfo { opcode: 0x23, format: FormatID::Format22c });

    // FILLED_NEW_ARRAY: opcode 0x24, format "35c"
    map.insert(0x24, OpcodeInfo { opcode: 0x24, format: FormatID::Format35c });

    // FILLED_NEW_ARRAY_RANGE: opcode 0x25, format "35c"
    map.insert(0x25, OpcodeInfo { opcode: 0x25, format: FormatID::Format35c });

    // FILL_ARRAY_DATA: opcode 0x26, format "31i"
    map.insert(0x26, OpcodeInfo { opcode: 0x26, format: FormatID::Format31i });

    // THROW: opcode 0x27, format "11x"
    map.insert(0x27, OpcodeInfo { opcode: 0x27, format: FormatID::Format11x });

    // GOTO: opcode 0x28, format "10t"
    map.insert(0x28, OpcodeInfo { opcode: 0x28, format: FormatID::Format10t });

    // GOTO_16: opcode 0x29, format "20t"
    map.insert(0x29, OpcodeInfo { opcode: 0x29, format: FormatID::Format20t });

    // GOTO_32: opcode 0x2A, format "30t"
    map.insert(0x2A, OpcodeInfo { opcode: 0x2A, format: FormatID::Format30t });

    // PACKED_SWITCH: opcode 0x2B, format "22t"
    map.insert(0x2B, OpcodeInfo { opcode: 0x2B, format: FormatID::Format22t });

    // SPARSE_SWITCH: opcode 0x2C, format "22t"
    map.insert(0x2C, OpcodeInfo { opcode: 0x2C, format: FormatID::Format22t });

    // CMP_LTZ: opcode 0x2D, format "12x"
    map.insert(0x2D, OpcodeInfo { opcode: 0x2D, format: FormatID::Format12x });

    // CMP_GTZ: opcode 0x2E, format "12x"
    map.insert(0x2E, OpcodeInfo { opcode: 0x2E, format: FormatID::Format12x });

    // CMP_LEZ: opcode 0x2F, format "12x"
    map.insert(0x2F, OpcodeInfo { opcode: 0x2F, format: FormatID::Format12x });

    // CMP_GEZ: opcode 0x30, format "12x"
    map.insert(0x30, OpcodeInfo { opcode: 0x30, format: FormatID::Format12x });

    // IF_EQ: opcode 0x31, format "22t"
    map.insert(0x31, OpcodeInfo { opcode: 0x31, format: FormatID::Format22t });

    // IF_NE: opcode 0x32, format "22t"
    map.insert(0x32, OpcodeInfo { opcode: 0x32, format: FormatID::Format22t });

    // IF_LT: opcode 0x33, format "22t"
    map.insert(0x33, OpcodeInfo { opcode: 0x33, format: FormatID::Format22t });

    // IF_GE: opcode 0x34, format "22t"
    map.insert(0x34, OpcodeInfo { opcode: 0x34, format: FormatID::Format22t });

    // IF_GT: opcode 0x35, format "22t"
    map.insert(0x35, OpcodeInfo { opcode: 0x35, format: FormatID::Format22t });

    // IF_LE: opcode 0x36, format "22t"
    map.insert(0x36, OpcodeInfo { opcode: 0x36, format: FormatID::Format22t });

    // IF_EQZ: opcode 0x37, format "21t"
    map.insert(0x37, OpcodeInfo { opcode: 0x37, format: FormatID::Format21t });

    // IF_NEZ: opcode 0x38, format "21t"
    map.insert(0x38, OpcodeInfo { opcode: 0x38, format: FormatID::Format21t });

    // IF_LTZ: opcode 0x39, format "21t"
    map.insert(0x39, OpcodeInfo { opcode: 0x39, format: FormatID::Format21t });

    // IF_GEZ: opcode 0x3A, format "21t"
    map.insert(0x3A, OpcodeInfo { opcode: 0x3A, format: FormatID::Format21t });

    // IF_GTZ: opcode 0x3B, format "21t"
    map.insert(0x3B, OpcodeInfo { opcode: 0x3B, format: FormatID::Format21t });

    // IF_LEZ: opcode 0x3C, format "21t"
    map.insert(0x3C, OpcodeInfo { opcode: 0x3C, format: FormatID::Format21t });

    // AGET: opcode 0x3D, format "22x"
    map.insert(0x3D, OpcodeInfo { opcode: 0x3D, format: FormatID::Format22x });

    // AGET_WIDE: opcode 0x3E, format "22x"
    map.insert(0x3E, OpcodeInfo { opcode: 0x3E, format: FormatID::Format22x });

    // AGET_OBJECT: opcode 0x3F, format "22x"
    map.insert(0x3F, OpcodeInfo { opcode: 0x3F, format: FormatID::Format22x });

    // AGET_BOOLEAN: opcode 0x40, format "22x"
    map.insert(0x40, OpcodeInfo { opcode: 0x40, format: FormatID::Format22x });

    // AGET_BYTE: opcode 0x41, format "22x"
    map.insert(0x41, OpcodeInfo { opcode: 0x41, format: FormatID::Format22x });

    // AGET_CHAR: opcode 0x42, format "22x"
    map.insert(0x42, OpcodeInfo { opcode: 0x42, format: FormatID::Format22x });

    // AGET_SHORT: opcode 0x43, format "22x"
    map.insert(0x43, OpcodeInfo { opcode: 0x43, format: FormatID::Format22x });

    // APUT: opcode 0x44, format "22x"
    map.insert(0x44, OpcodeInfo { opcode: 0x44, format: FormatID::Format22x });

    // APUT_WIDE: opcode 0x45, format "22x"
    map.insert(0x45, OpcodeInfo { opcode: 0x45, format: FormatID::Format22x });

    // APUT_OBJECT: opcode 0x46, format "22x"
    map.insert(0x46, OpcodeInfo { opcode: 0x46, format: FormatID::Format22x });

    // APUT_BOOLEAN: opcode 0x47, format "22x"
    map.insert(0x47, OpcodeInfo { opcode: 0x47, format: FormatID::Format22x });

    // APUT_BYTE: opcode 0x48, format "22x"
    map.insert(0x48, OpcodeInfo { opcode: 0x48, format: FormatID::Format22x });

    // APUT_CHAR: opcode 0x49, format "22x"
    map.insert(0x49, OpcodeInfo { opcode: 0x49, format: FormatID::Format22x });

    // APUT_SHORT: opcode 0x4A, format "22x"
    map.insert(0x4A, OpcodeInfo { opcode: 0x4A, format: FormatID::Format22x });

    // IGET: opcode 0x4B, format "22c"
    map.insert(0x4B, OpcodeInfo { opcode: 0x4B, format: FormatID::Format22c });

    // IGET_WIDE: opcode 0x4C, format "22c"
    map.insert(0x4C, OpcodeInfo { opcode: 0x4C, format: FormatID::Format22c });

    // IGET_OBJECT: opcode 0x4D, format "22c"
    map.insert(0x4D, OpcodeInfo { opcode: 0x4D, format: FormatID::Format22c });

    // IGET_BOOLEAN: opcode 0x4E, format "22c"
    map.insert(0x4E, OpcodeInfo { opcode: 0x4E, format: FormatID::Format22c });

    // IGET_BYTE: opcode 0x4F, format "22c"
    map.insert(0x4F, OpcodeInfo { opcode: 0x4F, format: FormatID::Format22c });

    // IGET_CHAR: opcode 0x50, format "22c"
    map.insert(0x50, OpcodeInfo { opcode: 0x50, format: FormatID::Format22c });

    // IGET_SHORT: opcode 0x51, format "22c"
    map.insert(0x51, OpcodeInfo { opcode: 0x51, format: FormatID::Format22c });

    // IPUT: opcode 0x52, format "22c"
    map.insert(0x52, OpcodeInfo { opcode: 0x52, format: FormatID::Format22c });

    // IPUT_WIDE: opcode 0x53, format "22c"
    map.insert(0x53, OpcodeInfo { opcode: 0x53, format: FormatID::Format22c });

    // IPUT_OBJECT: opcode 0x54, format "22c"
    map.insert(0x54, OpcodeInfo { opcode: 0x54, format: FormatID::Format22c });

    // IPUT_BOOLEAN: opcode 0x55, format "22c"
    map.insert(0x55, OpcodeInfo { opcode: 0x55, format: FormatID::Format22c });

    // IPUT_BYTE: opcode 0x56, format "22c"
    map.insert(0x56, OpcodeInfo { opcode: 0x56, format: FormatID::Format22c });

    // IPUT_CHAR: opcode 0x57, format "22c"
    map.insert(0x57, OpcodeInfo { opcode: 0x57, format: FormatID::Format22c });

    // IPUT_SHORT: opcode 0x58, format "22c"
    map.insert(0x58, OpcodeInfo { opcode: 0x58, format: FormatID::Format22c });

    // INVOKE_VIRTUAL: opcode 0x59, format "35c"
    map.insert(0x59, OpcodeInfo { opcode: 0x59, format: FormatID::Format35c });

    // INVOKE_SUPER: opcode 0x5A, format "35c"
    map.insert(0x5A, OpcodeInfo { opcode: 0x5A, format: FormatID::Format35c });

    // INVOKE_DIRECT: opcode 0x5B, format "35c"
    map.insert(0x5B, OpcodeInfo { opcode: 0x5B, format: FormatID::Format35c });

    // INVOKE_STATIC: opcode 0x5C, format "35c"
    map.insert(0x5C, OpcodeInfo { opcode: 0x5C, format: FormatID::Format35c });

    // INVOKE_INTERFACE: opcode 0x5D, format "35c"
    map.insert(0x5D, OpcodeInfo { opcode: 0x5D, format: FormatID::Format35c });

    // INVOKE_POLYMORPHIC: opcode 0x5E, format "4rcc"
    map.insert(0x5E, OpcodeInfo { opcode: 0x5E, format: FormatID::Format4rcc });

    // INVOKE_CUSTOM: opcode 0x5F, format "35c"
    map.insert(0x5F, OpcodeInfo { opcode: 0x5F, format: FormatID::Format35c });

    // ... Continue mapping all opcodes similarly ...

    // For brevity, not all opcodes are mapped here. You should complete this map based on the specification.

    map
}


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

impl DexInstruction
{
    /// Encodes the DexInstruction into a vector of 16-bit code units.
    pub fn encode(&self) -> Result<Vec<u16>, String> {
        let opcode_map = get_opcode_map();
        let mut code_units = Vec::new();

        match self {
            DexInstruction::Nop => {
                // Format00x: Opcode only
                let opcode_info = opcode_map.get(&0x00).ok_or("Unknown opcode for Nop")?;
                if opcode_info.format != FormatID::Format00x {
                    return Err("Format mismatch for Nop".to_string());
                }
                Ok(vec![opcode_info.opcode as u16])
            },

            DexInstruction::Move(dest, source) => {
                // Format12x: opcode | A (4 bits) | B (4 bits)
                let opcode_info = opcode_map.get(&0x01).ok_or("Unknown opcode for Move")?;
                if opcode_info.format != FormatID::Format12x {
                    return Err("Format mismatch for Move".to_string());
                }
                if *dest > 0xF || *source > 0xF {
                    return Err("Register out of range for Format12x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8) | ((*source as u16) << 4);
                Ok(vec![code_unit])
            },

            DexInstruction::MoveFrom16(dest, source) => {
                // Format22x: opcode | A (8 bits) | B (16 bits)
                let opcode_info = opcode_map.get(&0x02).ok_or("Unknown opcode for MoveFrom16")?;
                if opcode_info.format != FormatID::Format22x {
                    return Err("Format mismatch for MoveFrom16".to_string());
                }
                if *dest > 0xFF || *source > 0xFFFF {
                    return Err("Register or source out of range for Format22x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = *source as u16;
                Ok(vec![code_unit, code_unit_b])
            },

            DexInstruction::Move16(dest, source) => {
                // Format32x: opcode | AAAAAAAA | BBBBBBBB
                let opcode_info = opcode_map.get(&0x03).ok_or("Unknown opcode for Move16")?;
                if opcode_info.format != FormatID::Format32x {
                    return Err("Format mismatch for Move16".to_string());
                }
                if *dest > 0xFFFF || *source > 0xFFFF {
                    return Err("Register out of range for Format32x".to_string());
                }
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = *source as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::MoveWide(dest, source) => {
                // Format12x: similar to Move
                let opcode_info = get_opcode_map().get(&0x04).ok_or("Unknown opcode for MoveWide")?;
                if opcode_info.format != FormatID::Format12x {
                    return Err("Format mismatch for MoveWide".to_string());
                }
                if *dest > 0xF || *source > 0xF {
                    return Err("Register out of range for Format12x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8) | ((*source as u16) << 4);
                Ok(vec![code_unit])
            },

            DexInstruction::MoveWideFrom16(dest, source) => {
                // Format22x
                let opcode_info = get_opcode_map().get(&0x05).ok_or("Unknown opcode for MoveWideFrom16")?;
                if opcode_info.format != FormatID::Format22x {
                    return Err("Format mismatch for MoveWideFrom16".to_string());
                }
                if *dest > 0xFF || *source > 0xFFFF {
                    return Err("Register or source out of range for Format22x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = *source as u16;
                Ok(vec![code_unit, code_unit_b])
            },

            DexInstruction::MoveWide16(dest, source) => {
                // Format32x
                let opcode_info = get_opcode_map().get(&0x06).ok_or("Unknown opcode for MoveWide16")?;
                if opcode_info.format != FormatID::Format32x {
                    return Err("Format mismatch for MoveWide16".to_string());
                }
                if *dest > 0xFFFF || *source > 0xFFFF {
                    return Err("Register out of range for Format32x".to_string());
                }
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = *source as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::MoveObject(dest, source) => {
                // Format12x
                let opcode_info = get_opcode_map().get(&0x07).ok_or("Unknown opcode for MoveObject")?;
                if opcode_info.format != FormatID::Format12x {
                    return Err("Format mismatch for MoveObject".to_string());
                }
                if *dest > 0xF || *source > 0xF {
                    return Err("Register out of range for Format12x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8) | ((*source as u16) << 4);
                Ok(vec![code_unit])
            },

            DexInstruction::MoveObjectFrom16(dest, source) => {
                // Format22x
                let opcode_info = get_opcode_map().get(&0x08).ok_or("Unknown opcode for MoveObjectFrom16")?;
                if opcode_info.format != FormatID::Format22x {
                    return Err("Format mismatch for MoveObjectFrom16".to_string());
                }
                if *dest > 0xFF || *source > 0xFFFF {
                    return Err("Register or source out of range for Format22x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = *source as u16;
                Ok(vec![code_unit, code_unit_b])
            },

            DexInstruction::MoveObject16(dest, source) => {
                // Format32x
                let opcode_info = get_opcode_map().get(&0x09).ok_or("Unknown opcode for MoveObject16")?;
                if opcode_info.format != FormatID::Format32x {
                    return Err("Format mismatch for MoveObject16".to_string());
                }
                if *dest > 0xFFFF || *source > 0xFFFF {
                    return Err("Register out of range for Format32x".to_string());
                }
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = *source as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::MoveResult(dest) => {
                // Format11x
                let opcode_info = get_opcode_map().get(&0x0A).ok_or("Unknown opcode for MoveResult")?;
                if opcode_info.format != FormatID::Format11x {
                    return Err("Format mismatch for MoveResult".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format11x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                Ok(vec![code_unit])
            },

            DexInstruction::MoveResultWide(dest) => {
                // Format11x
                let opcode_info = get_opcode_map().get(&0x0B).ok_or("Unknown opcode for MoveResultWide")?;
                if opcode_info.format != FormatID::Format11x {
                    return Err("Format mismatch for MoveResultWide".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format11x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                Ok(vec![code_unit])
            },

            DexInstruction::MoveResultObject(dest) => {
                // Format11x
                let opcode_info = get_opcode_map().get(&0x0C).ok_or("Unknown opcode for MoveResultObject")?;
                if opcode_info.format != FormatID::Format11x {
                    return Err("Format mismatch for MoveResultObject".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format11x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                Ok(vec![code_unit])
            },

            DexInstruction::MoveException(dest) => {
                // Format11x
                let opcode_info = get_opcode_map().get(&0x0D).ok_or("Unknown opcode for MoveException")?;
                if opcode_info.format != FormatID::Format11x {
                    return Err("Format mismatch for MoveException".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format11x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                Ok(vec![code_unit])
            },

            DexInstruction::ReturnVoid => {
                // Format10x
                let opcode_info = get_opcode_map().get(&0x0E).ok_or("Unknown opcode for ReturnVoid")?;
                if opcode_info.format != FormatID::Format10x {
                    return Err("Format mismatch for ReturnVoid".to_string());
                }
                Ok(vec![opcode_info.opcode as u16])
            },

            DexInstruction::Return(dest) => {
                // Format11x
                let opcode_info = get_opcode_map().get(&0x0F).ok_or("Unknown opcode for Return")?;
                if opcode_info.format != FormatID::Format11x {
                    return Err("Format mismatch for Return".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format11x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                Ok(vec![code_unit])
            },

            DexInstruction::ReturnWide(dest) => {
                // Format11x
                let opcode_info = get_opcode_map().get(&0x10).ok_or("Unknown opcode for ReturnWide")?;
                if opcode_info.format != FormatID::Format11x {
                    return Err("Format mismatch for ReturnWide".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format11x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                Ok(vec![code_unit])
            },

            DexInstruction::ReturnObject(dest) => {
                // Format11x
                let opcode_info = get_opcode_map().get(&0x11).ok_or("Unknown opcode for ReturnObject")?;
                if opcode_info.format != FormatID::Format11x {
                    return Err("Format mismatch for ReturnObject".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format11x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                Ok(vec![code_unit])
            },

            DexInstruction::Const4(dest, literal) => {
                // Format11n: opcode | A (4 bits) | B (4 bits)
                let opcode_info = get_opcode_map().get(&0x12).ok_or("Unknown opcode for Const4")?;
                if opcode_info.format != FormatID::Format11n {
                    return Err("Format mismatch for Const4".to_string());
                }
                if *dest > 0xF {
                    return Err("Register out of range for Format11n".to_string());
                }
                if *literal < -8 || *literal > 7 {
                    return Err("Literal out of range for Const4 (-8 to +7)".to_string());
                }
                let literal_u4 = (*literal & 0xF) as u16;
                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8) | literal_u4;
                Ok(vec![code_unit])
            },

            DexInstruction::Const16(dest, literal) => {
                // Format21s: opcode | AA (8 bits) | BBBB (16 bits)
                let opcode_info = get_opcode_map().get(&0x13).ok_or("Unknown opcode for Const16")?;
                if opcode_info.format != FormatID::Format21s {
                    return Err("Format mismatch for Const16".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format21s".to_string());
                }
                if *literal < i16::MIN as i32 || *literal > i16::MAX as i32 {
                    return Err("Literal out of range for 16-bit immediate".to_string());
                }
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = *literal as i16 as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::Const(dest, literal) => {
                // Format21c: opcode | AA (8 bits) | BBBB (16 bits, const pool index)
                let opcode_info = get_opcode_map().get(&0x14).ok_or("Unknown opcode for Const")?;
                if opcode_info.format != FormatID::Format21c {
                    return Err("Format mismatch for Const".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format21c".to_string());
                }
                if *literal < 0 || *literal > 0xFFFF {
                    return Err("Constant pool index out of range for Format21c".to_string());
                }
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = *literal as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::ConstHigh16(dest, literal) => {
                // Format21h: opcode | AA (8 bits) | BBBB (16 bits, high 16 bits of literal)
                let opcode_info = get_opcode_map().get(&0x15).ok_or("Unknown opcode for ConstHigh16")?;
                if opcode_info.format != FormatID::Format21h {
                    return Err("Format mismatch for ConstHigh16".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format21h".to_string());
                }
                if *literal < (i32::MIN >> 16) || *literal > (i32::MAX >> 16) {
                    return Err("Literal out of range for high 16 bits".to_string());
                }
                let high_16 = ((*literal as i32) >> 16) as u16;
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = high_16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::ConstWide16(dest, literal) => {
                // Format31i: opcode | AA (8 bits) | BBBB (16 bits)
                let opcode_info = get_opcode_map().get(&0x16).ok_or("Unknown opcode for ConstWide16")?;
                if opcode_info.format != FormatID::Format31i {
                    return Err("Format mismatch for ConstWide16".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format31i".to_string());
                }
                if *literal < i32::MIN as i64 || *literal > i32::MAX as i64 {
                    return Err("Literal out of range for 32-bit immediate".to_string());
                }
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = (*literal as i32) as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::ConstWide32(dest, literal) => {
                // Format31i: similar to ConstWide16
                let opcode_info = get_opcode_map().get(&0x17).ok_or("Unknown opcode for ConstWide32")?;
                if opcode_info.format != FormatID::Format31i {
                    return Err("Format mismatch for ConstWide32".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format31i".to_string());
                }
                if *literal < i32::MIN as i64 || *literal > i32::MAX as i64 {
                    return Err("Literal out of range for 32-bit immediate".to_string());
                }
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = (*literal as i32) as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::ConstWide(dest, literal) => {
                // Format31i: similar to ConstWide16
                let opcode_info = get_opcode_map().get(&0x18).ok_or("Unknown opcode for ConstWide")?;
                if opcode_info.format != FormatID::Format31i {
                    return Err("Format mismatch for ConstWide".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format31i".to_string());
                }
                if *literal < i32::MIN as i64 || *literal > i32::MAX as i64 {
                    return Err("Literal out of range for 32-bit immediate".to_string());
                }
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = (*literal as i32) as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::ConstWideHigh16(dest, literal) => {
                // Format21h
                let opcode_info = get_opcode_map().get(&0x19).ok_or("Unknown opcode for ConstWideHigh16")?;
                if opcode_info.format != FormatID::Format21h {
                    return Err("Format mismatch for ConstWideHigh16".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format21h".to_string());
                }
                let high_16 = ((*literal as i64) >> 16) as u16;
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = high_16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::ConstString(dest, reference) => {
                // Format21c: opcode | AA (8 bits) | BBBB (16 bits, string@index)
                let opcode_info = get_opcode_map().get(&0x1A).ok_or("Unknown opcode for ConstString")?;
                if opcode_info.format != FormatID::Format21c {
                    return Err("Format mismatch for ConstString".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format21c".to_string());
                }
                // Assume reference is the string pool index as stringified u16
                let index: u16 = reference.parse().map_err(|_| "Invalid string pool index".to_string())?;
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = index;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::ConstStringJumbo(dest, reference) => {
                // Format31c: opcode | AA (8 bits) | BBBB (32 bits, string@index)
                let opcode_info = get_opcode_map().get(&0x1B).ok_or("Unknown opcode for ConstStringJumbo")?;
                if opcode_info.format != FormatID::Format31c {
                    return Err("Format mismatch for ConstStringJumbo".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format31c".to_string());
                }
                // Assume reference is the string pool index as stringified u32
                let index: u32 = reference.parse().map_err(|_| "Invalid string pool index".to_string())?;
                if index > u16::MAX as u32 {
                    return Err("String pool index out of range for 16-bit".to_string());
                }
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = index as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::ConstClass(dest, reference) => {
                // Format21c: opcode | AA (8 bits) | BBBB (16 bits, class@index)
                let opcode_info = get_opcode_map().get(&0x1C).ok_or("Unknown opcode for ConstClass")?;
                if opcode_info.format != FormatID::Format21c {
                    return Err("Format mismatch for ConstClass".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format21c".to_string());
                }
                // Assume reference is the class pool index as stringified u16
                let index: u16 = reference.parse().map_err(|_| "Invalid class pool index".to_string())?;
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = index;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::MonitorEnter(reg) => {
                // Format11x: opcode | AA (8 bits)
                let opcode_info = get_opcode_map().get(&0x1D).ok_or("Unknown opcode for MonitorEnter")?;
                if opcode_info.format != FormatID::Format11x {
                    return Err("Format mismatch for MonitorEnter".to_string());
                }
                if *reg > 0xFF {
                    return Err("Register out of range for Format11x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*reg as u16) << 8);
                Ok(vec![code_unit])
            },

            DexInstruction::MonitorExit(reg) => {
                // Format11x: opcode | AA (8 bits)
                let opcode_info = get_opcode_map().get(&0x1E).ok_or("Unknown opcode for MonitorExit")?;
                if opcode_info.format != FormatID::Format11x {
                    return Err("Format mismatch for MonitorExit".to_string());
                }
                if *reg > 0xFF {
                    return Err("Register out of range for Format11x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*reg as u16) << 8);
                Ok(vec![code_unit])
            },

            DexInstruction::CheckCast(reg, reference) => {
                // Format21c: opcode | AA (8 bits) | BBBB (16 bits, type@index)
                let opcode_info = get_opcode_map().get(&0x1F).ok_or("Unknown opcode for CheckCast")?;
                if opcode_info.format != FormatID::Format21c {
                    return Err("Format mismatch for CheckCast".to_string());
                }
                if *reg > 0xFF {
                    return Err("Register out of range for Format21c".to_string());
                }
                // Assume reference is the type pool index as stringified u16
                let index: u16 = reference.parse().map_err(|_| "Invalid type pool index".to_string())?;
                let code_unit_a = (opcode_info.opcode as u16) | ((*reg as u16) << 8);
                let code_unit_b = index;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::InstanceOf(dest, src, reference) => {
                // Format22c: opcode | AA (8 bits) | BBBB (16 bits, type@index)
                let opcode_info = get_opcode_map().get(&0x20).ok_or("Unknown opcode for InstanceOf")?;
                if opcode_info.format != FormatID::Format22c {
                    return Err("Format mismatch for InstanceOf".to_string());
                }
                if *dest > 0xFF || *src > 0xFFFF {
                    return Err("Register out of range for Format22c".to_string());
                }
                // Assume reference is the type pool index as stringified u16
                let index: u16 = reference.parse().map_err(|_| "Invalid type pool index".to_string())?;
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = index;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::ArrayLength(dest, src) => {
                // Format22x: opcode | AA (8 bits) | BB (8 bits)
                let opcode_info = get_opcode_map().get(&0x21).ok_or("Unknown opcode for ArrayLength")?;
                if opcode_info.format != FormatID::Format22x {
                    return Err("Format mismatch for ArrayLength".to_string());
                }
                if *dest > 0xFF || *src > 0xFF {
                    return Err("Register out of range for Format22x".to_string());
                }
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = *src as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::NewInstance(dest, reference) => {
                // Format21c: opcode | AA (8 bits) | BBBB (16 bits, type@index)
                let opcode_info = get_opcode_map().get(&0x22).ok_or("Unknown opcode for NewInstance")?;
                if opcode_info.format != FormatID::Format21c {
                    return Err("Format mismatch for NewInstance".to_string());
                }
                if *dest > 0xFF {
                    return Err("Register out of range for Format21c".to_string());
                }
                // Assume reference is the type pool index as stringified u16
                let index: u16 = reference.parse().map_err(|_| "Invalid type pool index".to_string())?;
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = index;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::NewArray(dest, src, reference) => {
                // Format22c: opcode | AA (8 bits) | BBBB (16 bits, type@index)
                let opcode_info = get_opcode_map().get(&0x23).ok_or("Unknown opcode for NewArray")?;
                if opcode_info.format != FormatID::Format22c {
                    return Err("Format mismatch for NewArray".to_string());
                }
                if *dest > 0xFF || *src > 0xFFFF {
                    return Err("Register out of range for Format22c".to_string());
                }
                // Assume reference is the type pool index as stringified u16
                let index: u16 = reference.parse().map_err(|_| "Invalid type pool index".to_string())?;
                let code_unit_a = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit_b = index;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::FilledNewArray(registers, reference) => {
                // Format35c: opcode | AA (5 registers) | BBBB (16 bits, method@index)
                let opcode_info = get_opcode_map().get(&0x24).ok_or("Unknown opcode for FilledNewArray")?;
                if opcode_info.format != FormatID::Format35c {
                    return Err("Format mismatch for FilledNewArray".to_string());
                }
                if registers.len() > 5 {
                    return Err("Too many registers for Format35c".to_string());
                }
                let a = registers.len() as u8;
                let b = registers.get(0).copied().unwrap_or(0);
                let c = registers.get(1).copied().unwrap_or(0);
                let d = registers.get(2).copied().unwrap_or(0);
                let e = registers.get(3).copied().unwrap_or(0);
                let f = registers.get(4).copied().unwrap_or(0);
                // Assume reference is the method pool index as stringified u16
                let index: u16 = reference.parse().map_err(|_| "Invalid method pool index".to_string())?;
                let code_unit_a = (opcode_info.opcode as u16) | ((a as u16) << 8);
                let code_unit_b = b as u16;
                let code_unit_c = c as u16;
                let code_unit_d = d as u16;
                let code_unit_e = e as u16;
                let code_unit_f = f as u16;
                let code_unit_g = index;
                Ok(vec![code_unit_a, code_unit_b, code_unit_c, code_unit_d, code_unit_e, code_unit_f, code_unit_g])
            },

            DexInstruction::FilledNewArrayRange(first_reg, amount, reference) => {
                // Format35c: similar to FilledNewArray but with range
                let opcode_info = get_opcode_map().get(&0x25).ok_or("Unknown opcode for FilledNewArrayRange")?;
                if opcode_info.format != FormatID::Format35c {
                    return Err("Format mismatch for FilledNewArrayRange".to_string());
                }
                if *amount > 5 {
                    return Err("Amount exceeds maximum registers for Format35c".to_string());
                }
                let a = *amount as u8;
                let c = *first_reg as u16;
                // Assume reference is the method pool index as stringified u16
                let index: u16 = reference.parse().map_err(|_| "Invalid method pool index".to_string())?;
                let code_unit_a = (opcode_info.opcode as u16) | ((a as u16) << 8);
                let code_unit_b = c;
                let code_unit_g = index;
                Ok(vec![code_unit_a, code_unit_b, code_unit_g])
            },

            DexInstruction::FillArrayData(reg, offset) => {
                // Format31i: opcode | AA (8 bits) | BBBB (16 bits, offset)
                let opcode_info = get_opcode_map().get(&0x26).ok_or("Unknown opcode for FillArrayData")?;
                if opcode_info.format != FormatID::Format31i {
                    return Err("Format mismatch for FillArrayData".to_string());
                }
                if *reg > 0xFF {
                    return Err("Register out of range for Format31i".to_string());
                }
                if *offset < i16::MIN as i32 || *offset > i16::MAX as i32 {
                    return Err("Offset out of range for 16-bit".to_string());
                }
                let code_unit_a = (opcode_info.opcode as u16) | ((*reg as u16) << 8);
                let code_unit_b = *offset as i16 as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::Throw(reg) => {
                // Format11x
                let opcode_info = get_opcode_map().get(&0x27).ok_or("Unknown opcode for Throw")?;
                if opcode_info.format != FormatID::Format11x {
                    return Err("Format mismatch for Throw".to_string());
                }
                if *reg > 0xFF {
                    return Err("Register out of range for Format11x".to_string());
                }
                let code_unit = (opcode_info.opcode as u16) | ((*reg as u16) << 8);
                Ok(vec![code_unit])
            },

            DexInstruction::Goto(label) => {
                // Format10t: opcode | AA (8 bits, branch target)
                let opcode_info = get_opcode_map().get(&0x28).ok_or("Unknown opcode for Goto")?;
                if opcode_info.format != FormatID::Format10t {
                    return Err("Format mismatch for Goto".to_string());
                }
                // Assume label is the relative branch target as stringified i8
                let target: i8 = label.parse().map_err(|_| "Invalid branch target for Goto".to_string())?;
                let code_unit_a = opcode_info.opcode as u16;
                let code_unit_b = target as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::Goto16(label) => {
                // Format20t: opcode | AAAA (16 bits, branch target)
                let opcode_info = get_opcode_map().get(&0x29).ok_or("Unknown opcode for Goto16")?;
                if opcode_info.format != FormatID::Format20t {
                    return Err("Format mismatch for Goto16".to_string());
                }
                // Assume label is the relative branch target as stringified i16
                let target: i16 = label.parse().map_err(|_| "Invalid branch target for Goto16".to_string())?;
                let code_unit_a = opcode_info.opcode as u16;
                let code_unit_b = target as u16;
                Ok(vec![code_unit_a, code_unit_b])
            },

            DexInstruction::Goto32(label) => {
                // Format30t: opcode | AAAA (32 bits, branch target)
                let opcode_info = get_opcode_map().get(&0x2A).ok_or("Unknown opcode for Goto32")?;
                if opcode_info.format != FormatID::Format30t {
                    return Err("Format mismatch for Goto32".to_string());
                }
                // Assume label is the relative branch target as stringified i32
                let target: i32 = label.parse().map_err(|_| "Invalid branch target for Goto32".to_string())?;
                let code_unit_a = opcode_info.opcode as u16;
                let code_unit_b = (target & 0xFFFF) as u16;
                let code_unit_c = ((target >> 16) & 0xFFFF) as u16;
                Ok(vec![code_unit_a, code_unit_b, code_unit_c])
            },

            // PackedSwitch instruction
            DexInstruction::PackedSwitch(reg, offset) => {
                // Opcode: 0x03 (example), Format: "packed-switch"
                let opcode_info = opcode_map.get(&0x03)
                    .ok_or("Opcode for PackedSwitch not found")?;

                // Assuming Format22x: opcode (8 bits) | reg (8 bits), followed by offset (16 bits)
                let reg_num = *reg;
                if reg_num > 255 {
                    return Err("Register number out of range for PackedSwitch".to_string());
                }
                let code_unit1 = (opcode_info.opcode as u16) | ((reg_num as u16) << 8);
                let code_unit2 = *offset as u16;
                code_units.push(code_unit1);
                code_units.push(code_unit2);
                Ok(code_units)
            },

            // SparseSwitch instruction
            DexInstruction::SparseSwitch(reg, offset) => {
                // Opcode: 0x04 (example), Format: "sparse-switch"
                let opcode_info = opcode_map.get(&0x04)
                    .ok_or("Opcode for SparseSwitch not found")?;

                // Assuming Format22x: opcode (8 bits) | reg (8 bits), followed by offset (16 bits)
                let reg_num = *reg;
                if reg_num > 255 {
                    return Err("Register number out of range for SparseSwitch".to_string());
                }
                let code_unit1 = (opcode_info.opcode as u16) | ((reg_num as u16) << 8);
                let code_unit2 = *offset as u16;
                code_units.push(code_unit1);
                code_units.push(code_unit2);
                Ok(code_units)
            },

            // Compare instruction
            DexInstruction::Compare(ct, dest, op1, op2) => {
                // Opcodes 0x2D-0x31 map to different CompareTypes
                let opcode = match ct {
                    CompareType::LessThanFloat => 0x2D,
                    CompareType::GreaterThanFloat => 0x2E,
                    CompareType::LessThanDouble => 0x2F,
                    CompareType::GreaterThanDouble => 0x30,
                    CompareType::Long => 0x31,
                    CompareType::Unknown => return Err("Unknown CompareType".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for CompareType {:?} not found", ct))?;

                // Assuming Format22x: opcode (8 bits) | dest (8 bits), followed by op1 and op2 (4 bits each)
                // Total: 2 code units
                if *dest > 255 || *op1 > 15 || *op2 > 15 {
                    return Err("Register or operand out of range for Compare".to_string());
                }

                let code_unit1 = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit2 = ((*op1 as u16) << 12) | ((*op2 as u16) << 8); // Example bit placement
                code_units.push(code_unit1);
                code_units.push(code_unit2);
                Ok(code_units)
            },

            // If instruction
            DexInstruction::If(tt, dest, src, offset) => {
                // Opcodes 0x40-0x45 for if-eq, if-ne, if-lt, if-ge, if-gt, if-le
                let opcode = match tt {
                    TestType::Equal => 0x40,
                    TestType::NotEqual => 0x41,
                    TestType::LessThan => 0x42,
                    TestType::GreaterThanOrEqual => 0x43,
                    TestType::GreaterThan => 0x44,
                    TestType::LessThanOrEqual => 0x45,
                    TestType::Unknown => return Err("Unknown TestType".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for TestType {:?} not found", tt))?;

                // Assuming Format22t: opcode (8 bits) | dest (8 bits) | src (8 bits) | offset (16 bits)
                if *dest > 255 || *src > 255 {
                    return Err("Register out of range for If instruction".to_string());
                }
                let code_unit1 = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit2 = (*src as u16) | ((*offset as u16) << 8);
                code_units.push(code_unit1);
                code_units.push(code_unit2);
                Ok(code_units)
            },

            // If0 instruction
            DexInstruction::If0(tt, dest, offset) => {
                // Opcodes for If0 variants (example)
                // Let's assume separate opcodes for If0 instructions
                let opcode = match tt {
                    TestType::Equal => 0x46,       // if-eqz
                    TestType::NotEqual => 0x47,    // if-nez
                    TestType::LessThan => 0x48,    // if-ltz
                    TestType::GreaterThanOrEqual => 0x49, // if-gez
                    TestType::GreaterThan => 0x4A, // if-gtz
                    TestType::LessThanOrEqual => 0x4B, // if-lez
                    TestType::Unknown => return Err("Unknown TestType for If0".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for If0 TestType {:?} not found", tt))?;

                // Assuming Format21t: opcode (8 bits) | dest (8 bits) | offset (16 bits)
                if *dest > 255 {
                    return Err("Register out of range for If0 instruction".to_string());
                }
                let code_unit1 = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit2 = *offset as u16;
                code_units.push(code_unit1);
                code_units.push(code_unit2);
                Ok(code_units)
            },

            // Array operation
            DexInstruction::Array(array_op, dest, op1, op2) => {
                // Assuming opcodes based on ArrayOperation
                let opcode = match array_op {
                    ArrayOperation::Get => 0x60,       // Example opcode
                    ArrayOperation::GetWide => 0x61,
                    ArrayOperation::GetObject => 0x62,
                    ArrayOperation::GetBoolean => 0x63,
                    ArrayOperation::GetByte => 0x64,
                    ArrayOperation::GetChar => 0x65,
                    ArrayOperation::GetShort => 0x66,
                    ArrayOperation::Put => 0x67,
                    ArrayOperation::PutWide => 0x68,
                    ArrayOperation::PutObject => 0x69,
                    ArrayOperation::PutBoolean => 0x6A,
                    ArrayOperation::PutByte => 0x6B,
                    ArrayOperation::PutChar => 0x6C,
                    ArrayOperation::PutShort => 0x6D,
                    ArrayOperation::Unknown => return Err("Unknown ArrayOperation".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for ArrayOperation {:?} not found", array_op))?;

                // Assuming Format22x: opcode (8 bits) | dest (8 bits) | op1 (8 bits) | op2 (8 bits)
                if *dest > 255 || *op1 > 255 || *op2 > 255 {
                    return Err("Register out of range for Array operation".to_string());
                }

                let code_unit1 = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit2 = ((*op1 as u16) << 8) | (*op2 as u16);
                code_units.push(code_unit1);
                code_units.push(code_unit2);
                Ok(code_units)
            },

            // Instance operation
            DexInstruction::Instance(array_op, dest, op1, field) => {
                // Assuming separate opcodes for instance operations
                let opcode = match array_op {
                    ArrayOperation::Get => 0x70,       // Example opcode
                    ArrayOperation::GetWide => 0x71,
                    ArrayOperation::GetObject => 0x72,
                    ArrayOperation::Put => 0x73,
                    ArrayOperation::PutWide => 0x74,
                    ArrayOperation::PutObject => 0x75,
                    _ => return Err("Unsupported ArrayOperation for Instance".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for Instance ArrayOperation {:?} not found", array_op))?;

                // Assuming Format22c: opcode (8 bits) | dest (8 bits) | op1 (8 bits) | field reference (16 bits)
                if *dest > 255 || *op1 > 255 {
                    return Err("Register out of range for Instance operation".to_string());
                }

                let code_unit1 = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit2 = ((*op1 as u16) << 8) | (field.parse::<u16>().map_err(|_| "Invalid FieldReference".to_string())?);
                code_units.push(code_unit1);
                code_units.push(code_unit2);
                Ok(code_units)
            },

            // Static operation
            DexInstruction::Static(array_op, dest, field) => {
                // Assuming separate opcodes for static operations
                let opcode = match array_op {
                    ArrayOperation::Get => 0x80,       // Example opcode
                    ArrayOperation::GetWide => 0x81,
                    ArrayOperation::GetObject => 0x82,
                    ArrayOperation::Put => 0x83,
                    ArrayOperation::PutWide => 0x84,
                    ArrayOperation::PutObject => 0x85,
                    _ => return Err("Unsupported ArrayOperation for Static".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for Static ArrayOperation {:?} not found", array_op))?;

                // Assuming Format22c: opcode (8 bits) | dest (8 bits) | field reference (16 bits)
                if *dest > 255 {
                    return Err("Register out of range for Static operation".to_string());
                }

                let code_unit1 = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let field_ref = field.parse::<u16>().map_err(|_| "Invalid FieldReference".to_string())?;
                let code_unit2 = field_ref;
                code_units.push(code_unit1);
                code_units.push(code_unit2);
                Ok(code_units)
            },

            // Invoke instruction
            DexInstruction::Invoke(invoke_kind, registers, method) => {
                // Determine opcode based on InvokeKind and number of registers
                let opcode = match invoke_kind {
                    InvokeKind::Virtual => 0x90,    // Example opcode
                    InvokeKind::Super => 0x91,
                    InvokeKind::Direct => 0x92,
                    InvokeKind::Static => 0x93,
                    InvokeKind::Interface => 0x94,
                    InvokeKind::Unknown => return Err("Unknown InvokeKind".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for InvokeKind {:?} not found", invoke_kind))?;

                // Assuming Format35c: opcode (8 bits) | A (4 bits) | {vC, vD, vE, vF, vG} (20 bits) | method@BBBB (16 bits)
                // For simplicity, assuming fixed number of registers (e.g., 5)
                if registers.len() > 5 {
                    return Err("Too many registers for Invoke instruction".to_string());
                }

                let a = registers.len() as u8; // Number of registers
                let mut vg_bits: u32 = 0;
                for (i, reg) in registers.iter().enumerate() {
                    if *reg > 31 {
                        return Err("Register number out of range for Invoke registers".to_string());
                    }
                    vg_bits |= (*reg as u32) << (5 * i); // Each register takes 5 bits
                }

                let code_unit1 = (opcode_info.opcode as u16) | ((a as u16) << 8);
                let code_unit2 = (vg_bits as u16) | (0x0000); // Placeholder for additional bits if needed
                let method_ref = method.parse::<u16>().map_err(|_| "Invalid MethodReference".to_string())?;
                let code_unit3 = method_ref;

                code_units.push(code_unit1);
                code_units.push(code_unit2);
                code_units.push(code_unit3);
                Ok(code_units)
            },

            // InvokeRange instruction
            DexInstruction::InvokeRange(invoke_kind, first_reg, amount, method) => {
                // Determine opcode based on InvokeKind and range
                let opcode = match invoke_kind {
                    InvokeKind::Virtual => 0xA0,    // Example opcode
                    InvokeKind::Super => 0xA1,
                    InvokeKind::Direct => 0xA2,
                    InvokeKind::Static => 0xA3,
                    InvokeKind::Interface => 0xA4,
                    InvokeKind::Unknown => return Err("Unknown InvokeKind".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for InvokeKind {:?} not found", invoke_kind))?;

                // Assuming Format3rc: opcode (8 bits) | A (8 bits) | first_reg (16 bits) | method@BBBB (16 bits)
                if *first_reg > 65535 || *amount > 255 {
                    return Err("Register number or amount out of range for InvokeRange".to_string());
                }

                let a = *amount;
                let code_unit1 = (opcode_info.opcode as u16) | ((a as u16) << 8);
                let code_unit2 = *first_reg as u16;
                let method_ref = method.parse::<u16>().map_err(|_| "Invalid MethodReference".to_string())?;
                let code_unit3 = method_ref;

                code_units.push(code_unit1);
                code_units.push(code_unit2);
                code_units.push(code_unit3);
                Ok(code_units)
            },

            // Unary operation
            DexInstruction::Unary(operation, dest, src) => {
                // Determine opcode based on UnaryOperation
                let opcode = match operation {
                    UnaryOperation::NegateInt => 0xB0,      // Example opcode
                    UnaryOperation::NotInt => 0xB1,
                    UnaryOperation::NegateLong => 0xB2,
                    UnaryOperation::NotLong => 0xB3,
                    UnaryOperation::NegateFloat => 0xB4,
                    UnaryOperation::NegateDouble => 0xB5,
                    UnaryOperation::IntToLong => 0xB6,
                    UnaryOperation::IntToFloat => 0xB7,
                    UnaryOperation::IntToDouble => 0xB8,
                    UnaryOperation::LongToInt => 0xB9,
                    UnaryOperation::LongToFloat => 0xBA,
                    UnaryOperation::LongToDouble => 0xBB,
                    UnaryOperation::FloatToInt => 0xBC,
                    UnaryOperation::FloatToLong => 0xBD,
                    UnaryOperation::FloatToDouble => 0xBE,
                    UnaryOperation::DoubleToInt => 0xBF,
                    UnaryOperation::DoubleToLong => 0xC0,
                    UnaryOperation::DoubleToFloat => 0xC1,
                    UnaryOperation::IntToByte => 0xC2,
                    UnaryOperation::IntToChar => 0xC3,
                    UnaryOperation::IntToShort => 0xC4,
                    UnaryOperation::Unknown => return Err("Unknown UnaryOperation".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for UnaryOperation {:?} not found", operation))?;

                // Assuming Format12x: opcode (8 bits) | dest (4 bits) | src (4 bits)
                if *dest > 15 || *src > 15 {
                    return Err("Register out of range for Unary operation".to_string());
                }

                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8) | ((*src as u16) << 4);
                code_units.push(code_unit);
                Ok(code_units)
            },

            // Binary operation
            DexInstruction::Binary(operation, dest, op1, op2) => {
                // Determine opcode based on BinaryOperation
                let opcode = match operation {
                    BinaryOperation::AddInt => 0xD0,      // Example opcode
                    BinaryOperation::SubInt => 0xD1,
                    BinaryOperation::MulInt => 0xD2,
                    BinaryOperation::DivInt => 0xD3,
                    BinaryOperation::RemInt => 0xD4,
                    BinaryOperation::AndInt => 0xD5,
                    BinaryOperation::OrInt => 0xD6,
                    BinaryOperation::XorInt => 0xD7,
                    BinaryOperation::ShlInt => 0xD8,
                    BinaryOperation::ShrInt => 0xD9,
                    BinaryOperation::UshrInt => 0xDA,
                    BinaryOperation::AddLong => 0xDB,
                    BinaryOperation::SubLong => 0xDC,
                    BinaryOperation::MulLong => 0xDD,
                    BinaryOperation::DivLong => 0xDE,
                    BinaryOperation::RemLong => 0xDF,
                    BinaryOperation::AndLong => 0xE0,
                    BinaryOperation::OrLong => 0xE1,
                    BinaryOperation::XorLong => 0xE2,
                    BinaryOperation::ShlLong => 0xE3,
                    BinaryOperation::ShrLong => 0xE4,
                    BinaryOperation::UshrLong => 0xE5,
                    BinaryOperation::AddFloat => 0xE6,
                    BinaryOperation::SubFloat => 0xE7,
                    BinaryOperation::MulFloat => 0xE8,
                    BinaryOperation::DivFloat => 0xE9,
                    BinaryOperation::RemFloat => 0xEA,
                    BinaryOperation::AddDouble => 0xEB,
                    BinaryOperation::SubDouble => 0xEC,
                    BinaryOperation::MulDouble => 0xED,
                    BinaryOperation::DivDouble => 0xEE,
                    BinaryOperation::RemDouble => 0xEF,
                    BinaryOperation::Unknown => return Err("Unknown BinaryOperation".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for BinaryOperation {:?} not found", operation))?;

                // Assuming Format22x: opcode (8 bits) | dest (4 bits) | op1 (4 bits) | op2 (4 bits)
                if *dest > 15 || *op1 > 15 || *op2 > 15 {
                    return Err("Register out of range for Binary operation".to_string());
                }

                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8) | ((*op1 as u16) << 4) | (*op2 as u16);
                code_units.push(code_unit);
                Ok(code_units)
            },

            // Binary2Addr operation
            DexInstruction::Binary2Addr(operation, src1, src2) => {
                // Determine opcode based on BinaryOperation
                let opcode = match operation {
                    BinaryOperation::AddInt => 0xF0,      // Example opcode for add-int/2addr
                    BinaryOperation::SubInt => 0xF1,
                    BinaryOperation::MulInt => 0xF2,
                    BinaryOperation::DivInt => 0xF3,
                    BinaryOperation::RemInt => 0xF4,
                    BinaryOperation::AndInt => 0xF5,
                    BinaryOperation::OrInt => 0xF6,
                    BinaryOperation::XorInt => 0xF7,
                    BinaryOperation::ShlInt => 0xF8,
                    BinaryOperation::ShrInt => 0xF9,
                    BinaryOperation::UshrInt => 0xFA,
                    BinaryOperation::AddLong => 0xFB,
                    BinaryOperation::SubLong => 0xFC,
                    BinaryOperation::MulLong => 0xFD,
                    BinaryOperation::DivLong => 0xFE,
                    BinaryOperation::RemLong => 0xFF,
                    BinaryOperation::Unknown => return Err("Unknown BinaryOperation for Binary2Addr".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for Binary2Addr Operation {:?} not found", operation))?;

                // Assuming Format12x: opcode (8 bits) | src1 (4 bits) | src2 (4 bits)
                if *src1 > 15 || *src2 > 15 {
                    return Err("Register out of range for Binary2Addr operation".to_string());
                }

                let code_unit = (opcode_info.opcode as u16) | ((*src1 as u16) << 8) | ((*src2 as u16) << 4);
                code_units.push(code_unit);
                Ok(code_units)
            },

            // BinaryLit16 operation
            DexInstruction::BinaryLit16(operation, dest, src, literal) => {
                // Determine opcode based on BinaryOperation
                let opcode = match operation {
                    BinaryOperation::AddInt => 0x100,      // Example opcode for add-int/lit16
                    BinaryOperation::SubInt => 0x101,
                    // ... other binary operations
                    _ => return Err("Unsupported BinaryOperation for BinaryLit16".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for BinaryLit16 Operation {:?} not found", operation))?;

                // Assuming Format22s: opcode (8 bits) | dest (8 bits) | src (8 bits) | literal (16 bits)
                if *dest > 255 || *src > 255 {
                    return Err("Register out of range for BinaryLit16 operation".to_string());
                }

                if *literal < i16::MIN as i32 || *literal > i16::MAX as i32 {
                    return Err("Literal out of range for BinaryLit16 operation".to_string());
                }

                let code_unit1 = (opcode_info.opcode as u16) | ((*dest as u16) << 8);
                let code_unit2 = (*src as u16) | ((*literal as i16) as u16);
                code_units.push(code_unit1);
                code_units.push(code_unit2);
                Ok(code_units)
            },

            // BinaryLit8 operation
            DexInstruction::BinaryLit8(operation, dest, src, literal) => {
                // Determine opcode based on BinaryOperation
                let opcode = match operation {
                    BinaryOperation::AddInt => 0x110,      // Example opcode for add-int/lit8
                    BinaryOperation::SubInt => 0x111,
                    // ... other binary operations
                    _ => return Err("Unsupported BinaryOperation for BinaryLit8".to_string()),
                };

                let opcode_info = opcode_map.get(&opcode)
                    .ok_or(format!("Opcode for BinaryLit8 Operation {:?} not found", operation))?;

                // Assuming Format22b: opcode (8 bits) | dest (4 bits) | src (4 bits) | literal (8 bits)
                if *dest > 15 || *src > 15 {
                    return Err("Register out of range for BinaryLit8 operation".to_string());
                }

                if *literal < i8::MIN as i32 || *literal > i8::MAX as i32 {
                    return Err("Literal out of range for BinaryLit8 operation".to_string());
                }

                let code_unit = (opcode_info.opcode as u16) | ((*dest as u16) << 8) | ((*src as u16) << 4) | (*literal as u8 as u16);
                code_units.push(code_unit);
                Ok(code_units)
            },

            // InvokePolymorphic instruction
            DexInstruction::InvokePolymorphic(registers, method, proto) => {
                // Opcode: 0x120 (example), Format: "invoke-polymorphic"
                let opcode_info = opcode_map.get(&0x120)
                    .ok_or("Opcode for InvokePolymorphic not found")?;

                // Assuming Format35c: opcode (8 bits) | A (4 bits) | {vC, vD, vE, vF, vG} (20 bits) | method@BBBB (16 bits) | proto@HHHH (16 bits)
                if registers.len() > 5 {
                    return Err("Too many registers for InvokePolymorphic".to_string());
                }

                let a = registers.len() as u8; // Number of registers
                let mut vg_bits: u32 = 0;
                for (i, reg) in registers.iter().enumerate() {
                    if *reg > 31 {
                        return Err("Register number out of range for InvokePolymorphic registers".to_string());
                    }
                    vg_bits |= (*reg as u32) << (5 * i); // Each register takes 5 bits
                }

                let code_unit1 = (opcode_info.opcode as u16) | ((a as u16) << 8);
                let code_unit2 = vg_bits as u16;
                let method_ref = method.parse::<u16>().map_err(|_| "Invalid MethodReference".to_string())?;
                let proto_ref = proto.parse::<u16>().map_err(|_| "Invalid PrototypeReference".to_string())?;
                let code_unit3 = method_ref;
                let code_unit4 = proto_ref;

                code_units.push(code_unit1);
                code_units.push(code_unit2);
                code_units.push(code_unit3);
                code_units.push(code_unit4);
                Ok(code_units)
            },

            // InvokePolymorphicRange instruction
            DexInstruction::InvokePolymorphicRange(first_reg, amount, method, proto) => {
                // Opcode: 0x130 (example), Format: "invoke-polymorphic/range"
                let opcode_info = opcode_map.get(&0x130)
                    .ok_or("Opcode for InvokePolymorphicRange not found")?;

                // Assuming Format3rc: opcode (8 bits) | A (8 bits) | first_reg (16 bits) | method@BBBB (16 bits) | proto@HHHH (16 bits)
                if *first_reg > 65535 || *amount > 255 {
                    return Err("Register number or amount out of range for InvokePolymorphicRange".to_string());
                }

                let a = *amount;
                let code_unit1 = (opcode_info.opcode as u16) | ((a as u16) << 8);
                let code_unit2 = *first_reg as u16;
                let method_ref = method.parse::<u16>().map_err(|_| "Invalid MethodReference".to_string())?;
                let proto_ref = proto.parse::<u16>().map_err(|_| "Invalid PrototypeReference".to_string())?;
                let code_unit3 = method_ref;
                let code_unit4 = proto_ref;

                code_units.push(code_unit1);
                code_units.push(code_unit2);
                code_units.push(code_unit3);
                code_units.push(code_unit4);
                Ok(code_units)
            },

            // InvokeCustom instruction
            DexInstruction::InvokeCustom(registers, call_site) => {
                // Opcode: 0x140 (example), Format: "invoke-custom"
                let opcode_info = opcode_map.get(&0x140)
                    .ok_or("Opcode for InvokeCustom not found")?;

                // Assuming Format35c: opcode (8 bits) | A (4 bits) | {vC, vD, vE, vF, vG} (20 bits) | call_site@BBBB (16 bits)
                if registers.len() > 5 {
                    return Err("Too many registers for InvokeCustom".to_string());
                }

                let a = registers.len() as u8; // Number of registers
                let mut vg_bits: u32 = 0;
                for (i, reg) in registers.iter().enumerate() {
                    if *reg > 31 {
                        return Err("Register number out of range for InvokeCustom registers".to_string());
                    }
                    vg_bits |= (*reg as u32) << (5 * i); // Each register takes 5 bits
                }

                let code_unit1 = (opcode_info.opcode as u16) | ((a as u16) << 8);
                let code_unit2 = vg_bits as u16;
                let call_site_ref = call_site.parse::<u16>().map_err(|_| "Invalid CallSiteReference".to_string())?;
                let code_unit3 = call_site_ref;

                code_units.push(code_unit1);
                code_units.push(code_unit2);
                code_units.push(code_unit3);
                Ok(code_units)
            },

            // InvokeCustomRange instruction
            DexInstruction::InvokeCustomRange(first_reg, amount, call_site) => {
                // Opcode: 0x150 (example), Format: "invoke-custom/range"
                let opcode_info = opcode_map.get(&0x150)
                    .ok_or("Opcode for InvokeCustomRange not found")?;

                // Assuming Format3rc: opcode (8 bits) | A (8 bits) | first_reg (16 bits) | call_site@BBBB (16 bits)
                if *first_reg > 65535 || *amount > 255 {
                    return Err("Register number or amount out of range for InvokeCustomRange".to_string());
                }

                let a = *amount;
                let code_unit1 = (opcode_info.opcode as u16) | ((a as u16) << 8);
                let code_unit2 = *first_reg as u16;
                let call_site_ref = call_site.parse::<u16>().map_err(|_| "Invalid CallSiteReference".to_string())?;
                let code_unit3 = call_site_ref;

                code_units.push(code_unit1);
                code_units.push(code_unit2);
                code_units.push(code_unit3);
                Ok(code_units)
            },

            // Add implementations for other DexInstruction variants similarly...
            // For brevity, not all instructions are implemented here.

            // Default case for unimplemented instructions
            _ => Err(format!("Encode not implemented for {:?}", self)),
        }
    }

    pub fn decode(bytes: &[u16]) -> Result<(Self, usize), String> {
        let opcode_map = get_opcode_map();

        if bytes.is_empty() {
            return Err("No bytes to decode".to_string());
        }

        let first_word = bytes[0];
        let opcode = (first_word & 0xFF) as u8;

        let opcode_info = opcode_map.get(&opcode)
            .ok_or_else(|| format!("Unknown opcode: 0x{:02X}", opcode))?;

        match opcode_info.format {
            FormatID::Format00x => {
                // Example: NOP
                if opcode == 0x00 {
                    Ok((DexInstruction::Nop, 1))
                } else {
                    Err(format!("Unhandled opcode for Format00x: 0x{:02X}", opcode))
                }
            },

            FormatID::Format10x => {
                // Format "10x" - single code unit with opcode in low 8 bits
                // Placeholder for other instructions
                Err("Decode not implemented for Format10x".to_string())
            },

            FormatID::Format12x => {
                // Example: MOVE
                let dest = ((first_word >> 8) & 0x0F) as u8;
                let source = ((first_word >> 4) & 0x0F) as u8;
                match opcode {
                    0x01 => Ok((DexInstruction::Move(dest, source), 1)),
                    // Add other Format12x instructions here...
                    _ => Err(format!("Unhandled opcode for Format12x: 0x{:02X}", opcode)),
                }
            },

            FormatID::Format21t => {
                // Example: GOTO
                if bytes.len() < 2 {
                    return Err("Insufficient bytes for Format21t".to_string());
                }
                let offset = bytes[1] as i16;
                match opcode {
                    0x02 => Ok((DexInstruction::Goto(offset.to_string()), 2)),
                    // Add other Format21t instructions here...
                    _ => Err(format!("Unhandled opcode for Format21t: 0x{:02X}", opcode)),
                }
            },

            FormatID::Format22x => {
                // Example: PackedSwitch and SparseSwitch
                if bytes.len() < 2 {
                    return Err("Insufficient bytes for Format22x".to_string());
                }
                let reg = ((first_word >> 8) & 0xFF) as u8;
                let offset = bytes[1] as i16;

                match opcode {
                    0x03 => Ok((DexInstruction::PackedSwitch(reg, offset.to_string()), 2)),
                    0x04 => Ok((DexInstruction::SparseSwitch(reg, offset.to_string()), 2)),
                    // Compare instructions
                    0x2D => Ok((DexInstruction::Compare(CompareType::LessThanFloat, reg, 0, 0), 2)), // Placeholder
                    0x2E => Ok((DexInstruction::Compare(CompareType::GreaterThanFloat, reg, 0, 0), 2)), // Placeholder
                    // ... other Compare opcodes
                    _ => Err(format!("Unhandled opcode for Format22x: 0x{:02X}", opcode)),
                }
            },

            FormatID::Format21s => {
                // Example: Const (vAA, #+BBBB)
                if bytes.len() < 2 {
                    return Err("Insufficient bytes for Format21s".to_string());
                }
                let dest = ((first_word >> 8) & 0xFF) as u8;
                let literal = bytes[1] as i16 as i32;

                match opcode {
                    0x03 => Ok((DexInstruction::Const(dest, literal), 2)),
                    // Add other Format21s instructions here...
                    _ => Err(format!("Unhandled opcode for Format21s: 0x{:02X}", opcode)),
                }
            },

            FormatID::Format22t => {
                // Example: If instructions
                if bytes.len() < 2 {
                    return Err("Insufficient bytes for Format22t".to_string());
                }
                let dest = ((first_word >> 8) & 0xFF) as u8;
                let src = (bytes[1] & 0xFF) as u8;
                let offset = ((bytes[1] & 0xFF00) >> 8) as i16;

                match opcode {
                    0x40 => Ok((DexInstruction::If(TestType::Equal, dest, src, offset), 2)),
                    0x41 => Ok((DexInstruction::If(TestType::NotEqual, dest, src, offset), 2)),
                    0x42 => Ok((DexInstruction::If(TestType::LessThan, dest, src, offset), 2)),
                    0x43 => Ok((DexInstruction::If(TestType::GreaterThanOrEqual, dest, src, offset), 2)),
                    0x44 => Ok((DexInstruction::If(TestType::GreaterThan, dest, src, offset), 2)),
                    0x45 => Ok((DexInstruction::If(TestType::LessThanOrEqual, dest, src, offset), 2)),
                    // Add other Format22t instructions here...
                    _ => Err(format!("Unhandled opcode for Format22t: 0x{:02X}", opcode)),
                }
            },

            FormatID::Format22c => {
                // Example: CheckCast, InstanceOf
                if bytes.len() < 2 {
                    return Err("Insufficient bytes for Format22c".to_string());
                }
                let dest = ((first_word >> 8) & 0xFF) as u8;
                let src = (bytes[1] & 0xFF) as u8;
                let reference = (bytes[1] >> 8) as u16;

                match opcode {
                    0x50 => Ok((DexInstruction::CheckCast(src, reference.to_string()), 2)),
                    0x51 => Ok((DexInstruction::InstanceOf(dest, src, reference.to_string()), 2)),
                    // Add other Format22c instructions here...
                    _ => Err(format!("Unhandled opcode for Format22c: 0x{:02X}", opcode)),
                }
            },

            FormatID::Format35c => {
                // Example: InvokePolymorphic, InvokeCustom
                if bytes.len() < 4 {
                    return Err("Insufficient bytes for Format35c".to_string());
                }
                let a = ((first_word >> 8) & 0x0F) as usize;
                let vg_bits = bytes[1] as u32;
                let mut registers = Vec::new();
                for i in 0..a {
                    let reg = ((vg_bits >> (5 * i)) & 0x1F) as u8;
                    registers.push(reg);
                }
                let reference = bytes[2] as u16;
                let proto_or_method = bytes[3] as u16;

                match opcode {
                    0x120 => Ok((DexInstruction::InvokePolymorphic(registers, reference.to_string(), proto_or_method.to_string()), 4)),
                    0x140 => Ok((DexInstruction::InvokeCustom(registers, proto_or_method.to_string()), 3)),
                    // Add other Format35c instructions here...
                    _ => Err(format!("Unhandled opcode for Format35c: 0x{:02X}", opcode)),
                }
            },

            FormatID::Format35ms => {
                // Example: InvokeStatic linked with vtaboff
                // Placeholder implementation
                Err("Decode not implemented for Format35ms".to_string())
            },

            FormatID::Format35mi => {
                // Example: InvokeStatic linked with inline
                // Placeholder implementation
                Err("Decode not implemented for Format35mi".to_string())
            },

            FormatID::Format3rc => {
                // Example: InvokeRange
                if bytes.len() < 3 {
                    return Err("Insufficient bytes for Format3rc".to_string());
                }
                let a = ((first_word >> 8) & 0xFF) as u8;
                let first_reg = bytes[1];
                let method_ref = bytes[2] as u16;

                match opcode {
                    0x130 => {
                        // Assuming proto is at a fixed position or needs additional bytes
                        // Placeholder for proto reference
                        let proto_ref = 0x0000; // Placeholder
                        Ok((DexInstruction::InvokePolymorphicRange(first_reg, a, method_ref.to_string(), proto_ref.to_string()), 3))
                    },
                    0x150 => {
                        // InvokeCustomRange
                        Ok((DexInstruction::InvokeCustomRange(first_reg, a, method_ref.to_string()), 3))
                    },
                    _ => Err(format!("Unhandled opcode for Format3rc: 0x{:02X}", opcode)),
                }
            },

            FormatID::Format4rcc => {
                // Example: InvokePolymorphic with proto
                if bytes.len() < 4 {
                    return Err("Insufficient bytes for Format4rcc".to_string());
                }
                let a = ((first_word >> 8) & 0xFF) as u8;
                let first_reg = bytes[1];
                let method_ref = bytes[2] as u16;
                let proto_ref = bytes[3] as u16;

                match opcode {
                    0x160 => Ok((DexInstruction::InvokePolymorphic(registers, method_ref.to_string(), proto_ref.to_string()), 4)),
                    // Add other Format4rcc instructions here...
                    _ => Err(format!("Unhandled opcode for Format4rcc: 0x{:02X}", opcode)),
                }
            },

            FormatID::Format51l => {
                // Example: Some long literal instruction
                if bytes.len() < 4 {
                    return Err("Insufficient bytes for Format51l".to_string());
                }
                let dest = ((first_word >> 8) & 0xFF) as u8;
                let high = bytes[1] as i16 as i32;
                let low = bytes[2] as i16 as i32;
                let literal = ((high as i64) << 32) | (low as i64);

                match opcode {
                    0x170 => Ok((DexInstruction::ConstWide(dest, literal), 4)),
                    // Add other Format51l instructions here...
                    _ => Err(format!("Unhandled opcode for Format51l: 0x{:02X}", opcode)),
                }
            },

            // Add decoding for other formats as needed...

            _ => Err(format!("Decode not implemented for format {:?}", opcode_info.format)),
        }
    }

    /// Encodes a single register into its bit representation.
    fn encode_register(reg: u8, bits: u8) -> Result<u16, String> {
        if reg >= (1 << bits) {
            return Err(format!("Register {} does not fit in {} bits", reg, bits));
        }
        Ok(reg as u16)
    }

    /// Encodes a literal into its bit representation.
    fn encode_literal(literal: i32, bits: u8) -> Result<u16, String> {
        // Check if literal fits in the specified number of bits
        let max = (1 << (bits - 1)) - 1;
        let min = -(1 << (bits - 1));
        if literal < min || literal > max {
            return Err(format!("Literal {} does not fit in {} bits", literal, bits));
        }
        Ok(literal as u16)
    }

    /// Encodes a reference (e.g., method, field) into its bit representation.
    fn encode_reference(ref_str: &str) -> Result<u16, String> {
        ref_str.parse::<u16>().map_err(|_| "Invalid reference format".to_string())
    }


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
*/
