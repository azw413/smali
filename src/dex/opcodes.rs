use once_cell;
use once_cell::sync::Lazy;
use crate::dex::opcode_format::{Format, Opcode, OpcodeFlags, ReferenceType};


static OPCODES: Lazy<Vec<Opcode>> = Lazy::new(|| {
    vec![
        // NOP
        Opcode::new(
            Opcode::all_versions(0x00),
            "nop",
            ReferenceType::None,
            None,
            Format::Format10x,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // MOVE
        Opcode::new(
            Opcode::all_versions(0x01),
            "move",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MOVE_FROM16
        Opcode::new(
            Opcode::all_versions(0x02),
            "move/from16",
            ReferenceType::None,
            None,
            Format::Format22x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MOVE_16
        Opcode::new(
            Opcode::all_versions(0x03),
            "move/16",
            ReferenceType::None,
            None,
            Format::Format32x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MOVE_WIDE
        Opcode::new(
            Opcode::all_versions(0x04),
            "move-wide",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // MOVE_WIDE_FROM16
        Opcode::new(
            Opcode::all_versions(0x05),
            "move-wide/from16",
            ReferenceType::None,
            None,
            Format::Format22x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // MOVE_WIDE_16
        Opcode::new(
            Opcode::all_versions(0x06),
            "move-wide/16",
            ReferenceType::None,
            None,
            Format::Format32x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // MOVE_OBJECT
        Opcode::new(
            Opcode::all_versions(0x07),
            "move-object",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MOVE_OBJECT_FROM16
        Opcode::new(
            Opcode::all_versions(0x08),
            "move-object/from16",
            ReferenceType::None,
            None,
            Format::Format22x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MOVE_OBJECT_16
        Opcode::new(
            Opcode::all_versions(0x09),
            "move-object/16",
            ReferenceType::None,
            None,
            Format::Format32x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MOVE_RESULT
        Opcode::new(
            Opcode::all_versions(0x0a),
            "move-result",
            ReferenceType::None,
            None,
            Format::Format11x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MOVE_RESULT_WIDE
        Opcode::new(
            Opcode::all_versions(0x0b),
            "move-result-wide",
            ReferenceType::None,
            None,
            Format::Format11x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // MOVE_RESULT_OBJECT
        Opcode::new(
            Opcode::all_versions(0x0c),
            "move-result-object",
            ReferenceType::None,
            None,
            Format::Format11x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MOVE_EXCEPTION
        Opcode::new(
            Opcode::all_versions(0x0d),
            "move-exception",
            ReferenceType::None,
            None,
            Format::Format11x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // RETURN_VOID
        Opcode::new_no_flags(
            Opcode::all_versions(0x0e),
            "return-void",
            ReferenceType::None,
            Format::Format10x,
        ),
        // RETURN
        Opcode::new_no_flags(
            Opcode::all_versions(0x0f),
            "return",
            ReferenceType::None,
            Format::Format11x,
        ),
        // RETURN_WIDE
        Opcode::new_no_flags(
            Opcode::all_versions(0x10),
            "return-wide",
            ReferenceType::None,
            Format::Format11x,
        ),
        // RETURN_OBJECT
        Opcode::new_no_flags(
            Opcode::all_versions(0x11),
            "return-object",
            ReferenceType::None,
            Format::Format11x,
        ),
        // CONST_4
        Opcode::new(
            Opcode::all_versions(0x12),
            "const/4",
            ReferenceType::None,
            None,
            Format::Format11n,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // CONST_16
        Opcode::new(
            Opcode::all_versions(0x13),
            "const/16",
            ReferenceType::None,
            None,
            Format::Format21s,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // CONST
        Opcode::new(
            Opcode::all_versions(0x14),
            "const",
            ReferenceType::None,
            None,
            Format::Format31i,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // CONST_HIGH16
        Opcode::new(
            Opcode::all_versions(0x15),
            "const/high16",
            ReferenceType::None,
            None,
            Format::Format21ih,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // CONST_WIDE_16
        Opcode::new(
            Opcode::all_versions(0x16),
            "const-wide/16",
            ReferenceType::None,
            None,
            Format::Format21s,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // CONST_WIDE_32
        Opcode::new(
            Opcode::all_versions(0x17),
            "const-wide/32",
            ReferenceType::None,
            None,
            Format::Format31i,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // CONST_WIDE
        Opcode::new(
            Opcode::all_versions(0x18),
            "const-wide",
            ReferenceType::None,
            None,
            Format::Format51l,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // CONST_WIDE_HIGH16
        Opcode::new(
            Opcode::all_versions(0x19),
            "const-wide/high16",
            ReferenceType::None,
            None,
            Format::Format21lh,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // CONST_STRING
        Opcode::new(
            Opcode::all_versions(0x1a),
            "const-string",
            ReferenceType::String,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // CONST_STRING_JUMBO
        Opcode::new(
            Opcode::all_versions(0x1b),
            "const-string/jumbo",
            ReferenceType::String,
            None,
            Format::Format31c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // CONST_CLASS
        Opcode::new(
            Opcode::all_versions(0x1c),
            "const-class",
            ReferenceType::Type,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MONITOR_ENTER
        Opcode::new(
            Opcode::all_versions(0x1d),
            "monitor-enter",
            ReferenceType::None,
            None,
            Format::Format11x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // MONITOR_EXIT
        Opcode::new(
            Opcode::all_versions(0x1e),
            "monitor-exit",
            ReferenceType::None,
            None,
            Format::Format11x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // CHECK_CAST
        Opcode::new(
            Opcode::all_versions(0x1f),
            "check-cast",
            ReferenceType::Type,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // INSTANCE_OF
        Opcode::new(
            Opcode::all_versions(0x20),
            "instance-of",
            ReferenceType::Type,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // ARRAY_LENGTH
        Opcode::new(
            Opcode::all_versions(0x21),
            "array-length",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // NEW_INSTANCE
        Opcode::new(
            Opcode::all_versions(0x22),
            "new-instance",
            ReferenceType::Type,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // NEW_ARRAY
        Opcode::new(
            Opcode::all_versions(0x23),
            "new-array",
            ReferenceType::Type,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // FILLED_NEW_ARRAY
        Opcode::new(
            Opcode::all_versions(0x24),
            "filled-new-array",
            ReferenceType::Type,
            None,
            Format::Format35c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // FILLED_NEW_ARRAY_RANGE
        Opcode::new(
            Opcode::all_versions(0x25),
            "filled-new-array/range",
            ReferenceType::Type,
            None,
            Format::Format3rc,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // FILL_ARRAY_DATA
        Opcode::new(
            Opcode::all_versions(0x26),
            "fill-array-data",
            ReferenceType::None,
            None,
            Format::Format31t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // THROW
        Opcode::new(
            Opcode::all_versions(0x27),
            "throw",
            ReferenceType::None,
            None,
            Format::Format11x,
            OpcodeFlags::CAN_THROW,
        ),
        // GOTO
        Opcode::new_no_flags(
            Opcode::all_versions(0x28),
            "goto",
            ReferenceType::None,
            Format::Format10t,
        ),
        // GOTO_16
        Opcode::new_no_flags(
            Opcode::all_versions(0x29),
            "goto/16",
            ReferenceType::None,
            Format::Format20t,
        ),
        // GOTO_32
        Opcode::new_no_flags(
            Opcode::all_versions(0x2a),
            "goto/32",
            ReferenceType::None,
            Format::Format30t,
        ),
        // PACKED_SWITCH
        Opcode::new(
            Opcode::all_versions(0x2b),
            "packed-switch",
            ReferenceType::None,
            None,
            Format::Format31t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // SPARSE_SWITCH
        Opcode::new(
            Opcode::all_versions(0x2c),
            "sparse-switch",
            ReferenceType::None,
            None,
            Format::Format31t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // CMPL_FLOAT
        Opcode::new(
            Opcode::all_versions(0x2d),
            "cmpl-float",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // CMPG_FLOAT
        Opcode::new(
            Opcode::all_versions(0x2e),
            "cmpg-float",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // CMPL_DOUBLE
        Opcode::new(
            Opcode::all_versions(0x2f),
            "cmpl-double",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),

        // CMPG_DOUBLE
        Opcode::new(
            Opcode::all_versions(0x30),
            "cmpg-double",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // CMP_LONG
        Opcode::new(
            Opcode::all_versions(0x31),
            "cmp-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IF_EQ
        Opcode::new(
            Opcode::all_versions(0x32),
            "if-eq",
            ReferenceType::None,
            None,
            Format::Format22t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // IF_NE
        Opcode::new(
            Opcode::all_versions(0x33),
            "if-ne",
            ReferenceType::None,
            None,
            Format::Format22t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // IF_LT
        Opcode::new(
            Opcode::all_versions(0x34),
            "if-lt",
            ReferenceType::None,
            None,
            Format::Format22t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // IF_GE
        Opcode::new(
            Opcode::all_versions(0x35),
            "if-ge",
            ReferenceType::None,
            None,
            Format::Format22t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // IF_GT
        Opcode::new(
            Opcode::all_versions(0x36),
            "if-gt",
            ReferenceType::None,
            None,
            Format::Format22t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // IF_LE
        Opcode::new(
            Opcode::all_versions(0x37),
            "if-le",
            ReferenceType::None,
            None,
            Format::Format22t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // IF_EQZ
        Opcode::new(
            Opcode::all_versions(0x38),
            "if-eqz",
            ReferenceType::None,
            None,
            Format::Format21t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // IF_NEZ
        Opcode::new(
            Opcode::all_versions(0x39),
            "if-nez",
            ReferenceType::None,
            None,
            Format::Format21t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // IF_LTZ
        Opcode::new(
            Opcode::all_versions(0x3a),
            "if-ltz",
            ReferenceType::None,
            None,
            Format::Format21t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // IF_GEZ
        Opcode::new(
            Opcode::all_versions(0x3b),
            "if-gez",
            ReferenceType::None,
            None,
            Format::Format21t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // IF_GTZ
        Opcode::new(
            Opcode::all_versions(0x3c),
            "if-gtz",
            ReferenceType::None,
            None,
            Format::Format21t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // IF_LEZ
        Opcode::new(
            Opcode::all_versions(0x3d),
            "if-lez",
            ReferenceType::None,
            None,
            Format::Format21t,
            OpcodeFlags::CAN_CONTINUE,
        ),
        // AGET
        Opcode::new(
            Opcode::all_versions(0x44),
            "aget",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // AGET_WIDE
        Opcode::new(
            Opcode::all_versions(0x45),
            "aget-wide",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // AGET_OBJECT
        Opcode::new(
            Opcode::all_versions(0x46),
            "aget-object",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // AGET_BOOLEAN
        Opcode::new(
            Opcode::all_versions(0x47),
            "aget-boolean",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // AGET_BYTE
        Opcode::new(
            Opcode::all_versions(0x48),
            "aget-byte",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // AGET_CHAR
        Opcode::new(
            Opcode::all_versions(0x49),
            "aget-char",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // AGET_SHORT
        Opcode::new(
            Opcode::all_versions(0x4a),
            "aget-short",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // APUT
        Opcode::new(
            Opcode::all_versions(0x4b),
            "aput",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // APUT_WIDE
        Opcode::new(
            Opcode::all_versions(0x4c),
            "aput-wide",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // APUT_OBJECT
        Opcode::new(
            Opcode::all_versions(0x4d),
            "aput-object",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // APUT_BOOLEAN
        Opcode::new(
            Opcode::all_versions(0x4e),
            "aput-boolean",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // APUT_BYTE
        Opcode::new(
            Opcode::all_versions(0x4f),
            "aput-byte",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // APUT_CHAR
        Opcode::new(
            Opcode::all_versions(0x50),
            "aput-char",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // APUT_SHORT
        Opcode::new(
            Opcode::all_versions(0x51),
            "aput-short",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // IGET
        Opcode::new(
            Opcode::all_versions(0x52),
            "iget",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IGET_WIDE
        Opcode::new(
            Opcode::all_versions(0x53),
            "iget-wide",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // IGET_OBJECT
        Opcode::new(
            Opcode::all_versions(0x54),
            "iget-object",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IGET_BOOLEAN
        Opcode::new(
            Opcode::all_versions(0x55),
            "iget-boolean",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IGET_BYTE
        Opcode::new(
            Opcode::all_versions(0x56),
            "iget-byte",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IGET_CHAR
        Opcode::new(
            Opcode::all_versions(0x57),
            "iget-char",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IGET_SHORT
        Opcode::new(
            Opcode::all_versions(0x58),
            "iget-short",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IPUT
        Opcode::new(
            Opcode::all_versions(0x59),
            "iput",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // IPUT_WIDE
        Opcode::new(
            Opcode::all_versions(0x5a),
            "iput-wide",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // IPUT_OBJECT
        Opcode::new(
            Opcode::all_versions(0x5b),
            "iput-object",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // IPUT_BOOLEAN
        Opcode::new(
            Opcode::all_versions(0x5c),
            "iput-boolean",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // IPUT_BYTE
        Opcode::new(
            Opcode::all_versions(0x5d),
            "iput-byte",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // IPUT_CHAR
        Opcode::new(
            Opcode::all_versions(0x5e),
            "iput-char",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // IPUT_SHORT
        Opcode::new(
            Opcode::all_versions(0x5f),
            "iput-short",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // SGET
        Opcode::new(
            Opcode::all_versions(0x60),
            "sget",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SGET_WIDE
        Opcode::new(
            Opcode::all_versions(0x61),
            "sget-wide",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SGET_OBJECT
        Opcode::new(
            Opcode::all_versions(0x62),
            "sget-object",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SGET_BOOLEAN
        Opcode::new(
            Opcode::all_versions(0x63),
            "sget-boolean",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SGET_BYTE
        Opcode::new(
            Opcode::all_versions(0x64),
            "sget-byte",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SGET_CHAR
        Opcode::new(
            Opcode::all_versions(0x65),
            "sget-char",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SGET_SHORT
        Opcode::new(
            Opcode::all_versions(0x66),
            "sget-short",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SPUT
        Opcode::new(
            Opcode::all_versions(0x67),
            "sput",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SPUT_WIDE
        Opcode::new(
            Opcode::all_versions(0x68),
            "sput-wide",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SPUT_OBJECT
        Opcode::new(
            Opcode::all_versions(0x69),
            "sput-object",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SPUT_BOOLEAN
        Opcode::new(
            Opcode::all_versions(0x6a),
            "sput-boolean",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SPUT_BYTE
        Opcode::new(
            Opcode::all_versions(0x6b),
            "sput-byte",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SPUT_CHAR
        Opcode::new(
            Opcode::all_versions(0x6c),
            "sput-char",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SPUT_SHORT
        Opcode::new(
            Opcode::all_versions(0x6d),
            "sput-short",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // INVOKE_VIRTUAL
        Opcode::new(
            Opcode::all_versions(0x6e),
            "invoke-virtual",
            ReferenceType::Method,
            None,
            Format::Format35c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_SUPER
        Opcode::new(
            Opcode::all_versions(0x6f),
            "invoke-super",
            ReferenceType::Method,
            None,
            Format::Format35c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_DIRECT
        Opcode::new(
            Opcode::all_versions(0x70),
            "invoke-direct",
            ReferenceType::Method,
            None,
            Format::Format35c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT | OpcodeFlags::CAN_INITIALIZE_REFERENCE,
        ),
        // INVOKE_STATIC
        Opcode::new(
            Opcode::all_versions(0x71),
            "invoke-static",
            ReferenceType::Method,
            None,
            Format::Format35c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_INTERFACE
        Opcode::new(
            Opcode::all_versions(0x72),
            "invoke-interface",
            ReferenceType::Method,
            None,
            Format::Format35c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_VIRTUAL_RANGE
        Opcode::new(
            Opcode::all_versions(0x74),
            "invoke-virtual/range",
            ReferenceType::Method,
            None,
            Format::Format3rc,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_SUPER_RANGE
        Opcode::new(
            Opcode::all_versions(0x75),
            "invoke-super/range",
            ReferenceType::Method,
            None,
            Format::Format3rc,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_DIRECT_RANGE
        Opcode::new(
            Opcode::all_versions(0x76),
            "invoke-direct/range",
            ReferenceType::Method,
            None,
            Format::Format3rc,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT | OpcodeFlags::CAN_INITIALIZE_REFERENCE,
        ),
        // INVOKE_STATIC_RANGE
        Opcode::new(
            Opcode::all_versions(0x77),
            "invoke-static/range",
            ReferenceType::Method,
            None,
            Format::Format3rc,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_INTERFACE_RANGE
        Opcode::new(
            Opcode::all_versions(0x78),
            "invoke-interface/range",
            ReferenceType::Method,
            None,
            Format::Format3rc,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // NEG_INT
        Opcode::new(
            Opcode::all_versions(0x7b),
            "neg-int",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // NOT_INT
        Opcode::new(
            Opcode::all_versions(0x7c),
            "not-int",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // NEG_LONG
        Opcode::new(
            Opcode::all_versions(0x7d),
            "neg-long",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // NOT_LONG
        Opcode::new(
            Opcode::all_versions(0x7e),
            "not-long",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // NEG_FLOAT
        Opcode::new(
            Opcode::all_versions(0x7f),
            "neg-float",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // NEG_DOUBLE
        Opcode::new(
            Opcode::all_versions(0x80),
            "neg-double",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // INT_TO_LONG
        Opcode::new(
            Opcode::all_versions(0x81),
            "int-to-long",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // INT_TO_FLOAT
        Opcode::new(
            Opcode::all_versions(0x82),
            "int-to-float",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // INT_TO_DOUBLE
        Opcode::new(
            Opcode::all_versions(0x83),
            "int-to-double",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // LONG_TO_INT
        Opcode::new(
            Opcode::all_versions(0x84),
            "long-to-int",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // LONG_TO_FLOAT
        Opcode::new(
            Opcode::all_versions(0x85),
            "long-to-float",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // LONG_TO_DOUBLE
        Opcode::new(
            Opcode::all_versions(0x86),
            "long-to-double",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // FLOAT_TO_INT
        Opcode::new(
            Opcode::all_versions(0x87),
            "float-to-int",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // FLOAT_TO_LONG
        Opcode::new(
            Opcode::all_versions(0x88),
            "float-to-long",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // FLOAT_TO_DOUBLE
        Opcode::new(
            Opcode::all_versions(0x89),
            "float-to-double",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // DOUBLE_TO_INT
        Opcode::new(
            Opcode::all_versions(0x8a),
            "double-to-int",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // DOUBLE_TO_LONG
        Opcode::new(
            Opcode::all_versions(0x8b),
            "double-to-long",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // DOUBLE_TO_FLOAT
        Opcode::new(
            Opcode::all_versions(0x8c),
            "double-to-float",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // INT_TO_BYTE
        Opcode::new(
            Opcode::all_versions(0x8d),
            "int-to-byte",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // INT_TO_CHAR
        Opcode::new(
            Opcode::all_versions(0x8e),
            "int-to-char",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // INT_TO_SHORT
        Opcode::new(
            Opcode::all_versions(0x8f),
            "int-to-short",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // ADD_INT
        Opcode::new(
            Opcode::all_versions(0x90),
            "add-int",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // SUB_INT
        Opcode::new(
            Opcode::all_versions(0x91),
            "sub-int",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MUL_INT
        Opcode::new(
            Opcode::all_versions(0x92),
            "mul-int",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // DIV_INT
        Opcode::new(
            Opcode::all_versions(0x93),
            "div-int",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // REM_INT
        Opcode::new(
            Opcode::all_versions(0x94),
            "rem-int",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // AND_INT
        Opcode::new(
            Opcode::all_versions(0x95),
            "and-int",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // OR_INT
        Opcode::new(
            Opcode::all_versions(0x96),
            "or-int",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // XOR_INT
        Opcode::new(
            Opcode::all_versions(0x97),
            "xor-int",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // SHL_INT
        Opcode::new(
            Opcode::all_versions(0x98),
            "shl-int",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // SHR_INT
        Opcode::new(
            Opcode::all_versions(0x99),
            "shr-int",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // USHR_INT
        Opcode::new(
            Opcode::all_versions(0x9a),
            "ushr-int",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // ADD_LONG
        Opcode::new(
            Opcode::all_versions(0x9b),
            "add-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // SUB_LONG
        Opcode::new(
            Opcode::all_versions(0x9c),
            "sub-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // MUL_LONG
        Opcode::new(
            Opcode::all_versions(0x9d),
            "mul-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // DIV_LONG
        Opcode::new(
            Opcode::all_versions(0x9e),
            "div-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // REM_LONG
        Opcode::new(
            Opcode::all_versions(0x9f),
            "rem-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // AND_LONG
        Opcode::new(
            Opcode::all_versions(0xa0),
            "and-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // OR_LONG
        Opcode::new(
            Opcode::all_versions(0xa1),
            "or-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // XOR_LONG
        Opcode::new(
            Opcode::all_versions(0xa2),
            "xor-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // SHL_LONG
        Opcode::new(
            Opcode::all_versions(0xa3),
            "shl-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // SHR_LONG
        Opcode::new(
            Opcode::all_versions(0xa4),
            "shr-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // USHR_LONG
        Opcode::new(
            Opcode::all_versions(0xa5),
            "ushr-long",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // ADD_FLOAT
        Opcode::new(
            Opcode::all_versions(0xa6),
            "add-float",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // SUB_FLOAT
        Opcode::new(
            Opcode::all_versions(0xa7),
            "sub-float",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MUL_FLOAT
        Opcode::new(
            Opcode::all_versions(0xa8),
            "mul-float",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // DIV_FLOAT
        Opcode::new(
            Opcode::all_versions(0xa9),
            "div-float",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // REM_FLOAT
        Opcode::new(
            Opcode::all_versions(0xaa),
            "rem-float",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // ADD_DOUBLE
        Opcode::new(
            Opcode::all_versions(0xab),
            "add-double",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // SUB_DOUBLE
        Opcode::new(
            Opcode::all_versions(0xac),
            "sub-double",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // MUL_DOUBLE
        Opcode::new(
            Opcode::all_versions(0xad),
            "mul-double",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // DIV_DOUBLE
        Opcode::new(
            Opcode::all_versions(0xae),
            "div-double",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // REM_DOUBLE
        Opcode::new(
            Opcode::all_versions(0xaf),
            "rem-double",
            ReferenceType::None,
            None,
            Format::Format23x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // ADD_INT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xb0),
            "add-int/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // SUB_INT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xb1),
            "sub-int/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MUL_INT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xb2),
            "mul-int/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // DIV_INT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xb3),
            "div-int/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // REM_INT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xb4),
            "rem-int/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // AND_INT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xb5),
            "and-int/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // OR_INT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xb6),
            "or-int/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // XOR_INT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xb7),
            "xor-int/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // SHL_INT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xb8),
            "shl-int/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // SHR_INT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xb9),
            "shr-int/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // USHR_INT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xba),
            "ushr-int/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // ADD_LONG_2ADDR
        Opcode::new(
            Opcode::all_versions(0xbb),
            "add-long/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // SUB_LONG_2ADDR
        Opcode::new(
            Opcode::all_versions(0xbc),
            "sub-long/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // MUL_LONG_2ADDR
        Opcode::new(
            Opcode::all_versions(0xbd),
            "mul-long/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // DIV_LONG_2ADDR
        Opcode::new(
            Opcode::all_versions(0xbe),
            "div-long/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // REM_LONG_2ADDR
        Opcode::new(
            Opcode::all_versions(0xbf),
            "rem-long/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // AND_LONG_2ADDR
        Opcode::new(
            Opcode::all_versions(0xc0),
            "and-long/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // OR_LONG_2ADDR
        Opcode::new(
            Opcode::all_versions(0xc1),
            "or-long/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // XOR_LONG_2ADDR
        Opcode::new(
            Opcode::all_versions(0xc2),
            "xor-long/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // SHL_LONG_2ADDR
        Opcode::new(
            Opcode::all_versions(0xc3),
            "shl-long/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // SHR_LONG_2ADDR
        Opcode::new(
            Opcode::all_versions(0xc4),
            "shr-long/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // USHR_LONG_2ADDR
        Opcode::new(
            Opcode::all_versions(0xc5),
            "ushr-long/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // ADD_FLOAT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xc6),
            "add-float/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // SUB_FLOAT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xc7),
            "sub-float/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MUL_FLOAT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xc8),
            "mul-float/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // DIV_FLOAT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xc9),
            "div-float/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // REM_FLOAT_2ADDR
        Opcode::new(
            Opcode::all_versions(0xca),
            "rem-float/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // ADD_DOUBLE_2ADDR
        Opcode::new(
            Opcode::all_versions(0xcb),
            "add-double/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // SUB_DOUBLE_2ADDR
        Opcode::new(
            Opcode::all_versions(0xcc),
            "sub-double/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // MUL_DOUBLE_2ADDR
        Opcode::new(
            Opcode::all_versions(0xcd),
            "mul-double/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // DIV_DOUBLE_2ADDR
        Opcode::new(
            Opcode::all_versions(0xce),
            "div-double/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // REM_DOUBLE_2ADDR
        Opcode::new(
            Opcode::all_versions(0xcf),
            "rem-double/2addr",
            ReferenceType::None,
            None,
            Format::Format12x,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // ADD_INT_LIT16
        Opcode::new(
            Opcode::all_versions(0xd0),
            "add-int/lit16",
            ReferenceType::None,
            None,
            Format::Format22s,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // RSUB_INT
        Opcode::new(
            Opcode::all_versions(0xd1),
            "rsub-int",
            ReferenceType::None,
            None,
            Format::Format22s,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MUL_INT_LIT16
        Opcode::new(
            Opcode::all_versions(0xd2),
            "mul-int/lit16",
            ReferenceType::None,
            None,
            Format::Format22s,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // DIV_INT_LIT16
        Opcode::new(
            Opcode::all_versions(0xd3),
            "div-int/lit16",
            ReferenceType::None,
            None,
            Format::Format22s,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // REM_INT_LIT16
        Opcode::new(
            Opcode::all_versions(0xd4),
            "rem-int/lit16",
            ReferenceType::None,
            None,
            Format::Format22s,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // AND_INT_LIT16
        Opcode::new(
            Opcode::all_versions(0xd5),
            "and-int/lit16",
            ReferenceType::None,
            None,
            Format::Format22s,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // OR_INT_LIT16
        Opcode::new(
            Opcode::all_versions(0xd6),
            "or-int/lit16",
            ReferenceType::None,
            None,
            Format::Format22s,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // XOR_INT_LIT16
        Opcode::new(
            Opcode::all_versions(0xd7),
            "xor-int/lit16",
            ReferenceType::None,
            None,
            Format::Format22s,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // ADD_INT_LIT8
        Opcode::new(
            Opcode::all_versions(0xd8),
            "add-int/lit8",
            ReferenceType::None,
            None,
            Format::Format22b,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // RSUB_INT_LIT8
        Opcode::new(
            Opcode::all_versions(0xd9),
            "rsub-int/lit8",
            ReferenceType::None,
            None,
            Format::Format22b,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // MUL_INT_LIT8
        Opcode::new(
            Opcode::all_versions(0xda),
            "mul-int/lit8",
            ReferenceType::None,
            None,
            Format::Format22b,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // DIV_INT_LIT8
        Opcode::new(
            Opcode::all_versions(0xdb),
            "div-int/lit8",
            ReferenceType::None,
            None,
            Format::Format22b,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // REM_INT_LIT8
        Opcode::new(
            Opcode::all_versions(0xdc),
            "rem-int/lit8",
            ReferenceType::None,
            None,
            Format::Format22b,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // AND_INT_LIT8
        Opcode::new(
            Opcode::all_versions(0xdd),
            "and-int/lit8",
            ReferenceType::None,
            None,
            Format::Format22b,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // OR_INT_LIT8
        Opcode::new(
            Opcode::all_versions(0xde),
            "or-int/lit8",
            ReferenceType::None,
            None,
            Format::Format22b,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // XOR_INT_LIT8
        Opcode::new(
            Opcode::all_versions(0xdf),
            "xor-int/lit8",
            ReferenceType::None,
            None,
            Format::Format22b,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // SHL_INT_LIT8
        Opcode::new(
            Opcode::all_versions(0xe0),
            "shl-int/lit8",
            ReferenceType::None,
            None,
            Format::Format22b,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // SHR_INT_LIT8
        Opcode::new(
            Opcode::all_versions(0xe1),
            "shr-int/lit8",
            ReferenceType::None,
            None,
            Format::Format22b,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // USHR_INT_LIT8
        Opcode::new(
            Opcode::all_versions(0xe2),
            "ushr-int/lit8",
            ReferenceType::None,
            None,
            Format::Format22b,
            OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IGET_VOLATILE
        Opcode::new(
            Opcode::first_api(0xe3, 9),
            "iget-volatile",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IPUT_VOLATILE
        Opcode::new(
            Opcode::first_api(0xe4, 9),
            "iput-volatile",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // SGET_VOLATILE
        Opcode::new(
            Opcode::first_api(0xe5, 9),
            "sget-volatile",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SPUT_VOLATILE
        Opcode::new(
            Opcode::first_api(0xe6, 9),
            "sput-volatile",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // IGET_OBJECT_VOLATILE
        Opcode::new(
            Opcode::first_api(0xe7, 9),
            "iget-object-volatile",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IGET_WIDE_VOLATILE
        Opcode::new(
            Opcode::first_api(0xe8, 9),
            "iget-wide-volatile",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // IPUT_WIDE_VOLATILE
        Opcode::new(
            Opcode::first_api(0xe9, 9),
            "iput-wide-volatile",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // SGET_WIDE_VOLATILE
        Opcode::new(
            Opcode::first_api(0xea, 9),
            "sget-wide-volatile",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SPUT_WIDE_VOLATILE
        Opcode::new(
            Opcode::first_api(0xeb, 9),
            "sput-wide-volatile",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // THROW_VERIFICATION_ERROR
        Opcode::new(
            Opcode::first_api(0xed, 5),
            "throw-verification-error",
            ReferenceType::None,
            None,
            Format::Format20bc,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW,
        ),
        // EXECUTE_INLINE
        Opcode::new(
            Opcode::all_apis(0xee),
            "execute-inline",
            ReferenceType::None,
            None,
            Format::Format35mi,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // EXECUTE_INLINE_RANGE
        Opcode::new(
            Opcode::first_api(0xef, 8),
            "execute-inline/range",
            ReferenceType::None,
            None,
            Format::Format3rmi,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_DIRECT_EMPTY
        Opcode::new(
            Opcode::last_api(0xf0, 13),
            "invoke-direct-empty",
            ReferenceType::Method,
            None,
            Format::Format35c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT | OpcodeFlags::CAN_INITIALIZE_REFERENCE,
        ),

        // INVOKE_OBJECT_INIT_RANGE
        Opcode::new(
            Opcode::first_api(0xf0, 14),
            "invoke-object-init/range",
            ReferenceType::Method,
            None,
            Format::Format3rc,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT | OpcodeFlags::CAN_INITIALIZE_REFERENCE,
        ),
        // RETURN_VOID_BARRIER
        Opcode::new(
            Opcode::combine(Opcode::first_api(0xf1, 11), Opcode::last_art_version(0x73, 59)),
            "return-void-barrier",
            ReferenceType::None,
            None,
            Format::Format10x,
            OpcodeFlags::ODEX_ONLY,
        ),
        // RETURN_VOID_NO_BARRIER
        Opcode::new(
            Opcode::first_art_version(0x73, 60),
            "return-void-no-barrier",
            ReferenceType::None,
            None,
            Format::Format10x,
            OpcodeFlags::ODEX_ONLY,
        ),
        // IGET_QUICK
        Opcode::new(
            Opcode::combine(Opcode::all_apis(0xf2), Opcode::all_art_versions(0xe3)),
            "iget-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::QUICK_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IGET_WIDE_QUICK
        Opcode::new(
            Opcode::combine(Opcode::all_apis(0xf3), Opcode::all_art_versions(0xe4)),
            "iget-wide-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::QUICK_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::SETS_WIDE_REGISTER,
        ),
        // IGET_OBJECT_QUICK
        Opcode::new(
            Opcode::combine(Opcode::all_apis(0xf4), Opcode::all_art_versions(0xe5)),
            "iget-object-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::QUICK_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IPUT_QUICK
        Opcode::new(
            Opcode::combine(Opcode::all_apis(0xf5), Opcode::all_art_versions(0xe6)),
            "iput-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::QUICK_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // IPUT_WIDE_QUICK
        Opcode::new(
            Opcode::combine(Opcode::all_apis(0xf6), Opcode::all_art_versions(0xe7)),
            "iput-wide-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::QUICK_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // IPUT_OBJECT_QUICK
        Opcode::new(
            Opcode::combine(Opcode::all_apis(0xf7), Opcode::all_art_versions(0xe8)),
            "iput-object-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::QUICK_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // IPUT_BOOLEAN_QUICK
        Opcode::new(
            Opcode::all_art_versions(0xeb),
            "iput-boolean-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::QUICK_FIELD_ACCESSOR,
        ),
        // IPUT_BYTE_QUICK
        Opcode::new(
            Opcode::all_art_versions(0xec),
            "iput-byte-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::QUICK_FIELD_ACCESSOR,
        ),
        // IPUT_CHAR_QUICK
        Opcode::new(
            Opcode::all_art_versions(0xed),
            "iput-char-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::QUICK_FIELD_ACCESSOR,
        ),
        // IPUT_SHORT_QUICK
        Opcode::new(
            Opcode::all_art_versions(0xee),
            "iput-short-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::QUICK_FIELD_ACCESSOR,
        ),
        // IGET_BOOLEAN_QUICK
        Opcode::new(
            Opcode::all_art_versions(0xef),
            "iget-boolean-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::QUICK_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IGET_BYTE_QUICK
        Opcode::new(
            Opcode::all_art_versions(0xf0),
            "iget-byte-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::QUICK_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IGET_CHAR_QUICK
        Opcode::new(
            Opcode::all_art_versions(0xf1),
            "iget-char-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::QUICK_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // IGET_SHORT_QUICK
        Opcode::new(
            Opcode::all_art_versions(0xf2),
            "iget-short-quick",
            ReferenceType::None,
            None,
            Format::Format22cs,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::QUICK_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // INVOKE_VIRTUAL_QUICK
        Opcode::new(
            Opcode::combine(Opcode::all_apis(0xf8), Opcode::all_art_versions(0xe9)),
            "invoke-virtual-quick",
            ReferenceType::None,
            None,
            Format::Format35ms,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_VIRTUAL_QUICK_RANGE
        Opcode::new(
            Opcode::combine(Opcode::all_apis(0xf9), Opcode::all_art_versions(0xea)),
            "invoke-virtual-quick/range",
            ReferenceType::None,
            None,
            Format::Format3rms,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_SUPER_QUICK
        Opcode::new(
            Opcode::last_api(0xfa, 25),
            "invoke-super-quick",
            ReferenceType::None,
            None,
            Format::Format35ms,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_SUPER_QUICK_RANGE
        Opcode::new(
            Opcode::last_api(0xfb, 25),
            "invoke-super-quick/range",
            ReferenceType::None,
            None,
            Format::Format3rms,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // IPUT_OBJECT_VOLATILE
        Opcode::new(
            Opcode::first_api(0xfc, 9),
            "iput-object-volatile",
            ReferenceType::Field,
            None,
            Format::Format22c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE,
        ),
        // SGET_OBJECT_VOLATILE
        Opcode::new(
            Opcode::first_api(0xfd, 9),
            "sget-object-volatile",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // SPUT_OBJECT_VOLATILE
        Opcode::new(
            Opcode::between_api(0xfe, 9, 19),
            "sput-object-volatile",
            ReferenceType::Field,
            None,
            Format::Format21c,
            OpcodeFlags::ODEX_ONLY | OpcodeFlags::VOLATILE_FIELD_ACCESSOR | OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::STATIC_FIELD_ACCESSOR,
        ),
        // PACKED_SWITCH_PAYLOAD
        Opcode::new_no_flags(
            Opcode::all_versions(0x100),
            "packed-switch-payload",
            ReferenceType::None,
            Format::PackedSwitchPayload,
        ),
        // SPARSE_SWITCH_PAYLOAD
        Opcode::new_no_flags(
            Opcode::all_versions(0x200),
            "sparse-switch-payload",
            ReferenceType::None,
            Format::SparseSwitchPayload,
        ),
        // ARRAY_PAYLOAD
        Opcode::new_no_flags(
            Opcode::all_versions(0x300),
            "array-payload",
            ReferenceType::None,
            Format::ArrayPayload,
        ),
        // INVOKE_POLYMORPHIC
        Opcode::new(
            Opcode::first_art_version(0xfa, 87),
            "invoke-polymorphic",
            ReferenceType::Method,
            Some(ReferenceType::MethodProto),
            Format::Format45cc,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_POLYMORPHIC_RANGE
        Opcode::new(
            Opcode::first_art_version(0xfb, 87),
            "invoke-polymorphic/range",
            ReferenceType::Method,
            Some(ReferenceType::MethodProto),
            Format::Format4rcc,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_CUSTOM
        Opcode::new(
            Opcode::first_art_version(0xfc, 111),
            "invoke-custom",
            ReferenceType::CallSite,
            None,
            Format::Format35c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // INVOKE_CUSTOM_RANGE
        Opcode::new(
            Opcode::first_art_version(0xfd, 111),
            "invoke-custom/range",
            ReferenceType::CallSite,
            None,
            Format::Format3rc,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_RESULT,
        ),
        // CONST_METHOD_HANDLE
        Opcode::new(
            Opcode::first_art_version(0xfe, 134),
            "const-method-handle",
            ReferenceType::MethodHandle,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        ),
        // CONST_METHOD_TYPE
        Opcode::new(
            Opcode::first_art_version(0xff, 134),
            "const-method-type",
            ReferenceType::MethodProto,
            None,
            Format::Format21c,
            OpcodeFlags::CAN_THROW | OpcodeFlags::CAN_CONTINUE | OpcodeFlags::SETS_REGISTER,
        )
    ]
});