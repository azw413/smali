use bitflags::bitflags;
use rangemap::{RangeInclusiveMap};
use std::ops::{ RangeBounds, RangeInclusive };

/// Represents different types of references used by opcodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceType {
    None,
    String,
    Type,
    Field,
    Method,
    CallSite,
    MethodProto,
    MethodHandle,
}

/// Defines various flags that can be associated with an opcode.
bitflags! {
    pub struct OpcodeFlags: u32 {
        const CAN_THROW = 0x1;
        const ODEX_ONLY = 0x2;
        const CAN_CONTINUE = 0x4;
        const SETS_RESULT = 0x8;
        const SETS_REGISTER = 0x10;
        const SETS_WIDE_REGISTER = 0x20;
        const QUICK_FIELD_ACCESSOR = 0x40;
        const VOLATILE_FIELD_ACCESSOR = 0x80;
        const STATIC_FIELD_ACCESSOR = 0x100;
        const JUMBO_OPCODE = 0x200;
        const CAN_INITIALIZE_REFERENCE = 0x400;
    }
}

/// Represents an opcode with its associated properties and mappings.
pub struct Opcode {
    pub name: &'static str,
    pub reference_type: ReferenceType,
    pub reference_type2: Option<ReferenceType>,
    pub format: Format,
    pub flags: OpcodeFlags,
    pub api_to_value_map: RangeInclusiveMap<i32, u16>,
    pub art_version_to_value_map: RangeInclusiveMap<i32, u16>,
}

/// Encapsulates API and ART version ranges along with opcode values.
pub struct VersionConstraint {
    pub api_range: Option<RangeInclusive<i32>>,
    pub art_version_range: Option<RangeInclusive<i32>>,
    pub opcode_value: u16,
}

impl Opcode {

    /// Creates a new Opcode instance.
    pub(crate) fn new(
        version_constraints: Vec<VersionConstraint>,
        name: &'static str,
        reference_type: ReferenceType,
        reference_type2: Option<ReferenceType>,
        format: Format,
        flags: OpcodeFlags,
    ) -> Self {
        let mut api_to_value_map = RangeInclusiveMap::new();
        let mut art_version_to_value_map = RangeInclusiveMap::new();

        for vc in version_constraints.iter() {
            if let Some(api_range) = &vc.api_range {
                if !api_range.is_empty() {
                    api_to_value_map.insert(api_range.clone(), vc.opcode_value);
                }
            }
            if let Some(art_range) = &vc.art_version_range {
                if !art_range.is_empty() {
                    art_version_to_value_map.insert(art_range.clone(), vc.opcode_value);
                }
            }
        }

        Opcode {
            name,
            reference_type,
            reference_type2,
            format,
            flags,
            api_to_value_map,
            art_version_to_value_map,
        }
    }

    pub(crate) fn new_no_flags(
        version_constraints: Vec<VersionConstraint>,
        name: &'static str,
        reference_type: ReferenceType,
        format: Format
    ) -> Self
    {
        Opcode::new(
            version_constraints,
            name,
            reference_type,
            None,
            format,
            OpcodeFlags::empty()
        )
    }

    /// Helper function similar to Java's `firstApi`.
    pub(crate) fn first_api(opcode_value: u16, api: i32) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: Some(api..=i32::MAX),
            art_version_range: None,
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `lastApi`.
    pub(crate) fn last_api(opcode_value: u16, api: i32) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: Some(i32::MIN..=api),
            art_version_range: None,
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `betweenApi`.
    pub(crate) fn between_api(opcode_value: u16, min_api: i32, max_api: i32) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: Some(min_api..=max_api),
            art_version_range: None,
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `firstArtVersion`.
    pub(crate) fn first_art_version(opcode_value: u16, art_version: i32) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: None,
            art_version_range: Some(art_version..=i32::MAX),
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `lastArtVersion`.
    pub(crate) fn last_art_version(opcode_value: u16, art_version: i32) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: None,
            art_version_range: Some(i32::MIN..=art_version),
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `allVersions`.
    pub(crate) fn all_versions(opcode_value: u16) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: Some(i32::MIN..=i32::MAX),
            art_version_range: Some(i32::MIN..=i32::MAX),
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `allApis`.
    pub(crate) fn all_apis(opcode_value: u16) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: Some(i32::MIN..=i32::MAX),
            art_version_range: None,
            opcode_value,
        }]
    }

    /// Helper function similar to Java's `allArtVersions`.
    pub(crate) fn all_art_versions(opcode_value: u16) -> Vec<VersionConstraint> {
        vec![VersionConstraint {
            api_range: None,
            art_version_range: Some(i32::MIN..=i32::MAX),
            opcode_value,
        }]
    }

    /// Combines multiple vectors of `VersionConstraint` into one.
    pub(crate) fn combine(constraints: Vec<VersionConstraint>, other: Vec<VersionConstraint>) -> Vec<VersionConstraint> {
        let mut combined = constraints;
        combined.extend(other);
        combined
    }

    /// Determines if the opcode can throw an exception.
    pub fn can_throw(&self) -> bool {
        self.flags.contains(OpcodeFlags::CAN_THROW)
    }

    /// Determines if the opcode is ODEX only.
    pub fn odex_only(&self) -> bool {
        self.flags.contains(OpcodeFlags::ODEX_ONLY)
    }

    /// Determines if execution can continue to the next instruction.
    pub fn can_continue(&self) -> bool {
        self.flags.contains(OpcodeFlags::CAN_CONTINUE)
    }

    /// Determines if the opcode sets the "hidden" result register.
    pub fn sets_result(&self) -> bool {
        self.flags.contains(OpcodeFlags::SETS_RESULT)
    }

    /// Determines if the opcode sets the value of its first register.
    pub fn sets_register(&self) -> bool {
        self.flags.contains(OpcodeFlags::SETS_REGISTER)
    }

    /// Determines if the opcode sets the value of its first register to a wide type.
    pub fn sets_wide_register(&self) -> bool {
        self.flags.contains(OpcodeFlags::SETS_WIDE_REGISTER)
    }

    /// Determines if the opcode is a quick field accessor.
    pub fn is_quick_field_accessor(&self) -> bool {
        self.flags.contains(OpcodeFlags::QUICK_FIELD_ACCESSOR)
    }

    /// Determines if the opcode is a volatile field accessor.
    pub fn is_volatile_field_accessor(&self) -> bool {
        self.flags.contains(OpcodeFlags::VOLATILE_FIELD_ACCESSOR)
    }

    /// Determines if the opcode is a static field accessor.
    pub fn is_static_field_accessor(&self) -> bool {
        self.flags.contains(OpcodeFlags::STATIC_FIELD_ACCESSOR)
    }

    /// Determines if the opcode is a jumbo opcode.
    pub fn is_jumbo_opcode(&self) -> bool {
        self.flags.contains(OpcodeFlags::JUMBO_OPCODE)
    }

    /// Determines if the opcode can initialize an uninitialized object reference.
    pub fn can_initialize_reference(&self) -> bool {
        self.flags.contains(OpcodeFlags::CAN_INITIALIZE_REFERENCE)
    }

    /// Retrieves the opcode value based on API and ART version.
    pub fn get_opcode_value(&self, api: i32, art_version: i32) -> Option<u16> {
        // Priority: API mapping first, then ART version mapping
        if let Some(&value) = self.api_to_value_map.get(&api) {
            return Some(value);
        }
        if let Some(&value) = self.art_version_to_value_map.get(&art_version) {
            return Some(value);
        }
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    Format10t,
    Format10x,
    Format11n,
    Format11x,
    Format12x,
    Format20bc,
    Format20t,
    Format21c,
    Format21ih,
    Format21lh,
    Format21s,
    Format21t,
    Format22b,
    Format22c,
    Format22cs,
    Format22s,
    Format22t,
    Format22x,
    Format23x,
    Format30t,
    Format31c,
    Format31i,
    Format31t,
    Format32x,
    Format35c,
    Format35mi,
    Format35ms,
    Format3rc,
    Format3rmi,
    Format3rms,
    Format45cc,
    Format4rcc,
    Format51l,
    ArrayPayload,
    PackedSwitchPayload,
    SparseSwitchPayload,
    UnresolvedOdexInstruction,
}

impl Format {
    /// Returns the size associated with the format.
    pub const fn size(&self) -> i32 {
        match self {
            // Size 2
            Format::Format10t
            | Format::Format10x
            | Format::Format11n
            | Format::Format11x
            | Format::Format12x => 2,

            // Size 4
            Format::Format20bc
            | Format::Format20t
            | Format::Format21c
            | Format::Format21ih
            | Format::Format21lh
            | Format::Format21s
            | Format::Format21t
            | Format::Format22b
            | Format::Format22c
            | Format::Format22cs
            | Format::Format22s
            | Format::Format22t
            | Format::Format22x
            | Format::Format23x => 4,

            // Size 6
            Format::Format30t
            | Format::Format31c
            | Format::Format31i
            | Format::Format31t
            | Format::Format32x
            | Format::Format35c
            | Format::Format35mi
            | Format::Format35ms
            | Format::Format3rc
            | Format::Format3rmi
            | Format::Format3rms => 6,

            // Size 8
            Format::Format45cc | Format::Format4rcc => 8,

            // Size 10
            Format::Format51l => 10,

            // Size -1 for payload formats and unresolved instructions
            Format::ArrayPayload
            | Format::PackedSwitchPayload
            | Format::SparseSwitchPayload
            | Format::UnresolvedOdexInstruction => -1,
        }
    }

    /// Indicates whether the format is a payload format.
    pub const fn is_payload_format(&self) -> bool {
        matches!(
            self,
            Format::ArrayPayload | Format::PackedSwitchPayload | Format::SparseSwitchPayload
        )
    }
}