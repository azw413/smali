/* Struct to represent a java object type identifer e.g. java.lang.Object */
/* They are stored in the smali native (also JNI) format e.g. Ljava/lang/Object; */

pub(crate) use crate::smali_ops::{DexOp, Label};
use crate::smali_parse::parse_class;
use crate::smali_write::write_class;
use nom::Err::Failure;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{alphanumeric0, char};
use nom::error::ErrorKind;
use nom::multi::many0;
use nom::sequence::terminated;
use nom::{IResult, Parser};
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::{fmt, fs};
use crate::dex::dex_file::{ACC_ABSTRACT, ACC_ANNOTATION, ACC_BRIDGE, ACC_CONSTRUCTOR, ACC_DECLARED_SYNCHRONIZED, ACC_ENUM, ACC_FINAL, ACC_INTERFACE, ACC_NATIVE, ACC_PRIVATE, ACC_PROTECTED, ACC_PUBLIC, ACC_STATIC, ACC_STRICT, ACC_SYNCHRONIZED, ACC_SYNTHETIC, ACC_TRANSIENT, ACC_VARARGS, ACC_VOLATILE};


/* Custom error for our command helper */
#[derive(Debug)]
pub struct SmaliError {
    pub details: String,
}

impl SmaliError {
    pub fn new(msg: &str) -> SmaliError {
        SmaliError {
            details: msg.to_string(),
        }
    }
}

impl fmt::Display for SmaliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl Error for SmaliError {
    fn description(&self) -> &str {
        &self.details
    }
}

/// Represents a Java object identifier
///
/// # Examples
///
/// ```
///
///
/// use smali::types::ObjectIdentifier;
///
/// let o = ObjectIdentifier::from_java_type("com.basic.Test");
///  assert_eq!(o.as_java_type(), "com.basic.Test");
///  assert_eq!(o.as_jni_type(), "Lcom/basic/Test;");
/// ```
#[derive(Debug, Eq, Serialize, Deserialize)]
pub struct ObjectIdentifier {
    pub(crate) class_name: String,
    pub(crate) type_arguments: Option<Vec<TypeSignature>>,
    pub(crate) suffix: Option<String>,
}

impl PartialEq<Self> for ObjectIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.class_name == other.class_name
    }
}

impl Hash for ObjectIdentifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.class_name.hash(state);
    }
}

impl fmt::Display for ObjectIdentifier {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write strictly the first element into the supplied output
        // stream: `f`. Returns `fmt::Result` which indicates whether the
        // operation succeeded or failed. Note that `write!` uses syntax which
        // is very similar to `println!`.
        write!(f, "{}", self.as_jni_type())
    }
}

impl ObjectIdentifier {
    #[allow(dead_code)]
    pub fn from_jni_type(t: &str) -> ObjectIdentifier {
        let ts = parse_typesignature(t).unwrap();
        if let (_, TypeSignature::Object(o)) = ts {
            o
        } else {
            ObjectIdentifier {
                class_name: t
                    .strip_prefix('L')
                    .unwrap()
                    .strip_suffix(';')
                    .unwrap()
                    .to_string(),
                type_arguments: None,
                suffix: None,
            }
        }
    }

    #[allow(dead_code)]
    pub fn from_java_type(t: &str) -> ObjectIdentifier {
        let class_name = t.replace('.', "/");
        ObjectIdentifier {
            class_name,
            type_arguments: None,
            suffix: None,
        }
    }

    pub fn as_jni_type(&self) -> String {
        let mut s = "L".to_string();
        s.push_str(&self.class_name);
        if let Some(v) = &self.type_arguments {
            s.push('<');
            for t in v {
                s.push_str(&t.to_jni());
            }
            s.push('>');
        }
        if let Some(suffix) = &self.suffix {
            s.push('.');
            s.push_str(suffix);
        }
        s.push(';');
        s
    }

    pub fn as_java_type(&self) -> String {
        self.class_name.replace('/', ".")
    }
}

/// Represents a Java type: array, object or primitive type
///
/// # Examples
///
/// ```
///  use smali::types::TypeSignature;
///
///  let t = TypeSignature::Bool;
///  assert_eq!(t.to_jni(), "Z");
/// ```
#[derive(Debug, Eq, Serialize, Deserialize)]
pub enum TypeSignature {
    Array(Box<TypeSignature>),
    Object(ObjectIdentifier),
    Int,
    Bool,
    Byte,
    Char,
    Short,
    Long,
    Float,
    Double,
    Void,
    TypeParameters(Vec<TypeSignature>, Box<TypeSignature>),
    TypeParameter(String, Box<TypeSignature>),
    TypeVariableSignature(String),
    WildcardPlus,
    WildcardMinus,
    WildcardStar,
}

impl PartialEq<Self> for TypeSignature {
    fn eq(&self, other: &Self) -> bool {
        self.to_jni() == other.to_jni()
    }
}

impl fmt::Display for TypeSignature {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write strictly the first element into the supplied output
        // stream: `f`. Returns `fmt::Result` which indicates whether the
        // operation succeeded or failed. Note that `write!` uses syntax which
        // is very similar to `println!`.
        write!(f, "{}", self.to_jni())
    }
}

impl TypeSignature {
    pub fn from_jni(s: &str) -> TypeSignature {
        let (_, ts) =
            parse_typesignature(s).unwrap_or_else(|_| panic!("Could not parse TypeSignature: {s}"));
        ts
    }

    pub fn to_jni(&self) -> String {
        match self {
            TypeSignature::Array(a) => "[".to_string() + &a.to_jni(),
            TypeSignature::Bool => "Z".to_string(),
            TypeSignature::Byte => "B".to_string(),
            TypeSignature::Char => "C".to_string(),
            TypeSignature::Short => "S".to_string(),
            TypeSignature::Int => "I".to_string(),
            TypeSignature::Long => "J".to_string(),
            TypeSignature::Float => "F".to_string(),
            TypeSignature::Double => "D".to_string(),
            TypeSignature::Object(o) => o.as_jni_type(),
            TypeSignature::Void => "V".to_string(),
            TypeSignature::TypeVariableSignature(i) => format!("T{i};"),
            TypeSignature::TypeParameters(params, rest) => {
                let mut s = "<".to_string();
                for p in params {
                    s.push_str(&p.to_jni())
                }
                s.push('>');
                s.push_str(&rest.to_jni());
                s
            }
            TypeSignature::TypeParameter(identifier, signature) => {
                format!("{}:{}", identifier, signature.to_jni())
            }
            TypeSignature::WildcardPlus => "+".to_string(),
            TypeSignature::WildcardMinus => "-".to_string(),
            TypeSignature::WildcardStar => "*".to_string(),
        }
    }

    pub fn to_java(&self) -> String {
        match self {
            TypeSignature::Array(a) => format!("{}[]", a.to_java()),
            TypeSignature::Bool => "boolean".to_string(),
            TypeSignature::Byte => "byte".to_string(),
            TypeSignature::Char => "char".to_string(),
            TypeSignature::Short => "short".to_string(),
            TypeSignature::Int => "int".to_string(),
            TypeSignature::Long => "long".to_string(),
            TypeSignature::Float => "float".to_string(),
            TypeSignature::Double => "double".to_string(),
            TypeSignature::Object(o) => o.as_java_type(),
            TypeSignature::Void => "void".to_string(),
            _ => "".to_string(),
        }
    }
}

/// Represents a Java method signature consisting of arguments and a return type
///
/// # Examples
///
/// ```
///  use smali::types::{MethodSignature, TypeSignature};
///
///  let m = MethodSignature::from_jni("([I)V");
///  assert_eq!(m.result, TypeSignature::Void);
/// ```
#[derive(Debug, Serialize, Deserialize)]
pub struct MethodSignature {
    pub(crate) type_parameters: Option<Vec<TypeSignature>>,
    pub args: Vec<TypeSignature>,
    pub result: TypeSignature,
    pub throws: Option<TypeSignature>,
}

impl MethodSignature {
    pub fn from_jni(s: &str) -> MethodSignature {
        let (_, m) = parse_methodsignature(s).expect("Can't parse MethodSignature");
        m
    }

    pub fn to_jni(&self) -> String {
        let mut s = String::new();
        if let Some(v) = &self.type_parameters {
            s.push('<');
            for t in v {
                s.push_str(&t.to_jni());
            }
            s.push('>');
        }
        s.push('(');
        for t in &self.args {
            let ts = t.to_jni();
            s.push_str(&ts);
        }
        s.push(')');
        s.push_str(&self.result.to_jni());
        if let Some(t) = &self.throws {
            s.push('^');
            s.push_str(&t.to_jni());
        }
        s
    }
}

pub(crate) fn parse_typesignature(smali: &str) -> IResult<&str, TypeSignature> {
    // Any type parameters
    let gt: IResult<&str, _> = char('<').parse(smali);
    if let Ok((o, _)) = gt {
        // Type Arguments
        let (o, ta) = many0(parse_typesignature).parse(o)?;
        let (o, _) = char('>')(o)?;
        let (nxt, ts_rest) = parse_typesignature(o)?;
        return Ok((nxt, TypeSignature::TypeParameters(ta, Box::new(ts_rest))));
    }

    // Any type identifiers ?
    let identifier_tag: IResult<&str, &str> = terminated(alphanumeric0, char(':')).parse(smali);
    if let Ok((o, i)) = identifier_tag {
        let (nxt, nxt_ts) = parse_typesignature(o)?;
        return Ok((
            nxt,
            TypeSignature::TypeParameter(i.to_string(), Box::new(nxt_ts)),
        ));
    }

    // Object
    let mut type_arguments = None;
    let mut suffix = None;
    let l: IResult<&str, &str> = tag("L").parse(smali);
    if let Ok((o, _)) = l {
        let mut nxt;
        let (o, t) = take_while(|x| (x != ';') && (x != '<'))(o)?;
        nxt = o;
        let gt: IResult<&str, std::primitive::char> = char('<')(o);
        if let Ok((o, _)) = gt {
            // Type Arguments
            let (o, ta) = many0(parse_typesignature).parse(o)?;
            let (o, _) = char('>')(o)?;
            type_arguments = Some(ta);
            nxt = o;
        }

        // Is there a suffix?
        let l: IResult<&str, &str> = tag(".")(nxt);
        if let Ok((o, _)) = l {
            let (o, t) = take_while(|x| x != ';')(o)?;
            suffix = Some(t.to_string());
            nxt = o;
        }

        let (o, _) = char(';')(nxt)?;
        let object = ObjectIdentifier {
            class_name: t.to_string(),
            type_arguments,
            suffix,
        };
        return Ok((o, TypeSignature::Object(object)));
    }

    // Type Variable
    let l: IResult<&str, &str> = tag("T").parse(smali);
    if let Ok((o, _)) = l {
        let (o, t) = take_while(|x| x != ';')(o)?;
        let nxt = o;
        let (o, _) = char(';')(nxt)?;
        let type_var = TypeSignature::TypeVariableSignature(t.to_string());
        return Ok((o, type_var));
    }

    // Array
    let b: IResult<&str, &str> = tag("[").parse(smali);
    if let Ok((o, _)) = b {
        let (o, t) = parse_typesignature(o)?;
        return Ok((o, TypeSignature::Array(Box::new(t))));
    }

    //Primitive Type
    let p: IResult<&str, &str> = alt((
        tag("Z"),
        tag("B"),
        tag("C"),
        tag("S"),
        tag("I"),
        tag("J"),
        tag("F"),
        tag("D"),
        tag("V"),
        tag("*"),
        tag("+"),
        tag("-"),
    ))
        .parse(smali);
    if let Ok((o, t)) = p {
        let ts = match t {
            "Z" => TypeSignature::Bool,
            "B" => TypeSignature::Byte,
            "C" => TypeSignature::Char,
            "S" => TypeSignature::Short,
            "I" => TypeSignature::Int,
            "J" => TypeSignature::Long,
            "F" => TypeSignature::Float,
            "D" => TypeSignature::Double,
            "+" => TypeSignature::WildcardPlus,
            "-" => TypeSignature::WildcardMinus,
            "*" => TypeSignature::WildcardStar,
            _ => TypeSignature::Void,
        };
        return Ok((o, ts));
    }

    Err(nom::Err::Error(nom::error::Error {
        input: smali,
        code: ErrorKind::Complete,
    }))
}

pub(crate) fn parse_methodsignature(smali: &str) -> IResult<&str, MethodSignature> {
    let mut type_parameters = None;
    let mut throws = None;
    let mut nxt = smali;
    let gt: IResult<&str, _> = char('<')(nxt);
    if let Ok((o, _)) = gt {
        // Type Arguments
        let (o, ta) = many0(parse_typesignature).parse(o)?;
        let (o, _) = char('>')(o)?;
        type_parameters = Some(ta);
        nxt = o;
    }
    let (o, _) = tag("(")(nxt)?;
    let (o, a) = many0(parse_typesignature).parse(o)?;
    let (o, _) = tag(")")(o)?;
    let (o, r) = parse_typesignature(o)?;
    nxt = o;
    let up: IResult<&str, std::primitive::char> = char('^')(nxt);
    if let Ok((o, _)) = up {
        // Throws
        let (o, ta) = parse_typesignature(o)?;
        throws = Some(ta);
        nxt = o;
    }
    Ok((
        nxt,
        MethodSignature {
            type_parameters,
            args: a,
            result: r,
            throws,
        },
    ))
}

/// Simple enum to represent Java method, field and class modifiers
///

#[derive(Debug, PartialEq)]
pub enum Modifier {
    Public,
    Private,
    Protected,
    Static,
    Final,
    Synchronized,
    Volatile,
    Bridge,
    Transient,
    Varargs,
    Native,
    Interface,
    Abstract,
    Strict,
    Synthetic,
    Annotation,
    Enum,
    Constructor,
    DeclaredSynchronized,
}

impl FromStr for Modifier {
    type Err = SmaliError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "public" => Self::Public,
            "protected" => Self::Protected,
            "private" => Self::Private,
            "static" => Self::Static,
            "final" => Self::Final,
            "abstract" => Self::Abstract,
            "interface" => Self::Interface,
            "synthetic" => Self::Synthetic,
            "transient" => Self::Transient,
            "volatile" => Self::Volatile,
            "synchronized" => Self::Synchronized,
            "native" => Self::Native,
            "varargs" => Self::Varargs,
            "annotation" => Self::Annotation,
            "enum" => Self::Enum,
            "strict" => Self::Static,
            "bridge" => Self::Bridge,
            "constructor" => Self::Constructor,
            _ => {
                return Err(SmaliError {
                    details: "Unknown modifier".to_string(),
                });
            }
        })
    }
}

impl Modifier {
    pub fn to_str(&self) -> &str {
        match self {
            Self::Public => "public",
            Self::Protected => "protected",
            Self::Private => "private",
            Self::Static => "static",
            Self::Final => "final",
            Self::Abstract => "abstract",
            Self::Interface => "interface",
            Self::Synthetic => "synthetic",
            Self::Transient => "transient",
            Self::Volatile => "volatile",
            Self::Synchronized => "synchronized",
            Self::Native => "native",
            Self::Varargs => "varargs",
            Self::Annotation => "annotation",
            Self::Enum => "enum",
            Self::Strict => "strict",
            Self::Bridge => "bridge",
            Self::Constructor => "constructor",
            Self::DeclaredSynchronized => "synchronized",
        }
    }
}

pub struct Modifiers;

impl Modifiers
{
    pub fn from_u32(u: u32) -> Vec<Modifier>
    {
        let mut m = vec![];
        if u & ACC_PUBLIC > 0 { m.push(Modifier::Public)};
        if u & ACC_PRIVATE > 0 { m.push(Modifier::Private)};
        if u & ACC_PROTECTED > 0 { m.push(Modifier::Protected)};
        if u & ACC_STATIC > 0 { m.push(Modifier::Static)};
        if u & ACC_FINAL > 0 { m.push(Modifier::Final)};
        if u & ACC_SYNCHRONIZED > 0 { m.push(Modifier::Synchronized)};
        if u & ACC_VOLATILE > 0 { m.push(Modifier::Volatile)};
        if u & ACC_BRIDGE > 0 { m.push(Modifier::Bridge)};
        if u & ACC_TRANSIENT > 0 { m.push(Modifier::Transient)};
        if u & ACC_VARARGS > 0 { m.push(Modifier::Varargs)};
        if u & ACC_NATIVE > 0 { m.push(Modifier::Native)};
        if u & ACC_INTERFACE > 0 { m.push(Modifier::Interface)};
        if u & ACC_ABSTRACT > 0 { m.push(Modifier::Abstract)};
        if u & ACC_STRICT > 0 { m.push(Modifier::Strict)};
        if u & ACC_SYNTHETIC > 0 { m.push(Modifier::Synthetic)};
        if u & ACC_ANNOTATION > 0 { m.push(Modifier::Annotation)};
        if u & ACC_ENUM > 0 { m.push(Modifier::Enum)};
        if u & ACC_CONSTRUCTOR > 0 { m.push(Modifier::Constructor)};
        if u & ACC_DECLARED_SYNCHRONIZED > 0 { m.push(Modifier::DeclaredSynchronized)};
        m
    }
}


/// Simple enum to represent annotation visibility: build, runtime, system.
///
#[derive(Debug)]
pub enum AnnotationVisibility {
    Build,
    Runtime,
    System,
}

impl AnnotationVisibility {
    pub fn to_str(&self) -> &str {
        match self {
            Self::Build => "build",
            Self::Runtime => "runtime",
            Self::System => "system",
        }
    }
}

/// Annotation values can be a Single value, Array, Enum or another Annotation
///
#[derive(Debug)]
pub enum AnnotationValue {
    Single(String),
    Array(Vec<String>),
    SubAnnotation(SmaliAnnotation),
    Enum(ObjectIdentifier, String),
}

impl FromStr for AnnotationVisibility {
    type Err = SmaliError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "build" => Self::Build,
            "runtime" => Self::Runtime,
            "system" => Self::System,
            _ => {
                return Err(SmaliError {
                    details: "Unknown Annotation visibility".to_string(),
                });
            }
        })
    }
}

/// Name, value pair for annotation elements. There can be several of these per annotation.
///
#[derive(Debug)]
pub struct AnnotationElement {
    pub name: String,
    pub value: AnnotationValue,
}

/// Struct representing a Java annotation, these can occur at class level, method level, within a field or within another annotation.
///
#[derive(Debug)]
pub struct SmaliAnnotation {
    pub visibility: AnnotationVisibility,
    pub annotation_type: TypeSignature,
    pub elements: Vec<AnnotationElement>,
}

/// Struct representing a Java field
///
#[derive(Debug)]
pub struct SmaliField {
    /// Name of the field
    pub name: String,
    /// Any modifiers
    pub modifiers: Vec<Modifier>,
    /// Type signature of the field
    pub signature: TypeSignature,
    /// If an initialiser is included
    pub initial_value: Option<String>,
    /// Field level annotations
    pub annotations: Vec<SmaliAnnotation>,
}

/// Represents a protected range in a try/catch directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TryRange {
    pub start: Label,
    pub end: Label,
}

impl fmt::Display for TryRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Format similar to smali: "{:try_start .. :try_end}"
        write!(f, "{{ {} .. {} }}", self.start, self.end)
    }
}

/// Represents a catch block directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CatchDirective {
    /// A catch directive with an exception type.
    Catch {
        exception: String, // e.g. "Ljava/lang/Exception;"
        try_range: TryRange,
        handler: Label,
    },
    /// A catch-all directive.
    CatchAll { try_range: TryRange, handler: Label },
}

impl fmt::Display for CatchDirective {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CatchDirective::Catch {
                exception,
                try_range,
                handler,
            } => {
                // Print as: .catch <exception> <try_range> <handler>
                write!(f, ".catch {exception} {try_range} {handler}")
            }
            CatchDirective::CatchAll { try_range, handler } => {
                // Print as: .catchall <try_range> <handler>
                write!(f, ".catchall {try_range} {handler}")
            }
        }
    }
}

#[derive(Debug)]
pub enum ArrayDataElement {
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl fmt::Display for ArrayDataElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArrayDataElement::Byte(b) => write!(f, "{b:#x}t"),
            ArrayDataElement::Short(s) => write!(f, "{s:#x}s"),
            ArrayDataElement::Int(i) => write!(f, "{i:#x}"),
            ArrayDataElement::Long(l) => write!(f, "{l:#x}l"),
            ArrayDataElement::Float(fl) => write!(f, "{:#x}f", fl.to_bits()),
            ArrayDataElement::Double(d) => write!(f, "{:#x}d", d.to_bits()),
        }
    }
}

/// Represents a .array-data directive.
#[derive(Debug)]
pub struct ArrayDataDirective {
    /// The element width as specified in the header.
    pub element_width: i32,
    /// The parsed array elements.
    pub elements: Vec<ArrayDataElement>,
}

impl fmt::Display for ArrayDataDirective {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print the header. We'll print the width in hex.
        writeln!(f, ".array-data {:#x}", self.element_width)?;
        // Print elements in groups (here, 4 per line).
        for chunk in self.elements.chunks(4) {
            write!(f, "    ")?;
            for elem in chunk {
                write!(f, "{elem} ")?;
            }
            writeln!(f)?;
        }
        write!(f, ".end array-data")
    }
}

#[derive(Debug)]
pub struct PackedSwitchDirective {
    pub first_key: i32,
    pub targets: Vec<Label>,
}

impl fmt::Display for PackedSwitchDirective {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print the header with the first key in hex.
        writeln!(f, ".packed-switch {:#x}", self.first_key)?;
        // Print each target label, indented.
        for target in &self.targets {
            writeln!(f, "    {target}")?;
        }
        // Print the footer without a trailing newline.
        write!(f, ".end packed-switch")
    }
}

/// An entry in a sparse-switch directive: a key and its corresponding target label.
#[derive(Debug)]
pub struct SparseSwitchEntry {
    pub key: i32,
    pub target: Label,
}

impl fmt::Display for SparseSwitchEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Format the key in hexadecimal followed by "->" and the label.
        write!(f, "{:#x} -> {}", self.key, self.target)
    }
}

/// The sparse-switch directive.
#[derive(Debug)]
pub struct SparseSwitchDirective {
    pub entries: Vec<SparseSwitchEntry>,
}

impl fmt::Display for SparseSwitchDirective {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print the header.
        writeln!(f, ".sparse-switch")?;
        // Print each entry indented.
        for entry in &self.entries {
            writeln!(f, "    {entry}")?;
        }
        // Print the footer.
        write!(f, ".end sparse-switch")
    }
}

/// An enum representing operations within a method, these can be a label, a line number or a dex operation as a String.
///
#[derive(Debug)]
pub enum SmaliOp {
    Label(Label),
    Line(u32),
    Op(DexOp),
    Catch(CatchDirective),
    ArrayData(ArrayDataDirective),
    PackedSwitch(PackedSwitchDirective),
    SparseSwitch(SparseSwitchDirective),
}

/// Struct representing a method parameter
#[derive(Debug)]
pub struct SmaliParam {
    /// Parameter name
    pub name: Option<String>,
    /// Register used for the parameter
    pub register: String,
    /// Parameter annotations
    pub annotations: Vec<SmaliAnnotation>,
}

/// Struct representing a Java method
///
#[derive(Debug)]
pub struct SmaliMethod {
    /// Method name
    pub name: String,
    /// Method modifiers
    pub modifiers: Vec<Modifier>,
    /// Is it a constructor
    pub constructor: bool,
    /// Method signature
    pub signature: MethodSignature,
    /// Number of local variables required by the operations
    pub locals: u32,
    /// Method params
    pub params: Vec<SmaliParam>,
    /// Any method level annotations
    pub annotations: Vec<SmaliAnnotation>,
    /// Method operations
    pub ops: Vec<SmaliOp>,
}

/// Represents a smali class i.e. the whole .smali file
///
/// # Examples
///
/// ```no_run
///  use std::path::Path;
///  use smali::types::SmaliClass;
///
///  let c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
///  println!("Java class: {}", c.name.as_java_type());
/// ```
#[derive(Debug)]
pub struct SmaliClass {
    /// The name of this class
    pub name: ObjectIdentifier,
    /// Class modifiers
    pub modifiers: Vec<Modifier>,
    /// The source filename if included in the smali doc
    pub source: Option<String>,
    /// The class' superclass (every Java class has one)
    pub super_class: ObjectIdentifier,
    /// List of all the interfaces the class implements
    pub implements: Vec<ObjectIdentifier>,
    /// Class level annotations
    pub annotations: Vec<SmaliAnnotation>,
    /// All the fields defined by the class
    pub fields: Vec<SmaliField>,
    /// All the methods defined by the class
    pub methods: Vec<SmaliMethod>,

    // Internal
    /// The file path where this class was loaded from (.smali file)
    pub file_path: Option<PathBuf>,
}

impl PartialEq<Self> for SmaliClass {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for SmaliClass {}

impl Hash for SmaliClass {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl SmaliClass {
    /// Creates a SmaliClass from a String containing a valid smali document
    ///
    /// # Examples
    ///
    /// ```no_run
    ///  use smali::types::SmaliClass;
    ///
    ///  let smali = ".class public Lokhttp3/OkHttpClient;";
    ///  let c = SmaliClass::from_smali(smali).expect("Parse error");
    ///
    /// ```
    pub fn from_smali(s: &str) -> Result<SmaliClass, SmaliError> {
        let d = parse_class(s);
        match d {
            Ok((_, cl)) => Ok(cl),
            Err(Failure(e)) => Err(SmaliError {
                details: format!("Class parse error at: {:?}", e.to_string()),
            }),
            _ => Err(SmaliError {
                details: "Unknown class parse error".to_string(),
            }),
        }
    }

    /// Creates a SmaliClass from a file containing a valid smali document
    ///
    /// # Examples
    ///
    /// ```no_run
    ///  use std::path::Path;
    ///  use smali::types::SmaliClass;
    ///
    ///  let c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
    ///
    /// ```
    pub fn read_from_file(path: &Path) -> Result<SmaliClass, SmaliError> {
        match fs::read_to_string(path) {
            Ok(s) => {
                let mut c = SmaliClass::from_smali(&s)?;
                c.file_path = Some(PathBuf::from(path));
                Ok(c)
            }
            Err(e) => Err(SmaliError {
                details: format!("Error loading file {}: {}", path.to_str().unwrap(), e),
            }),
        }
    }

    /// Creates a smali document string from the current class
    ///
    /// # Examples
    ///
    /// ```no_run
    ///  use std::path::Path;
    ///  use smali::types::SmaliClass;
    ///
    ///  let c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
    ///  println!("{}", c.to_smali());
    ///
    /// ```
    pub fn to_smali(&self) -> String {
        write_class(self)
    }

    /// Writes the current SmaliClass to the specified file path as a smali document
    ///
    /// # Examples
    ///
    /// ```no_run
    ///  use std::path::Path;
    ///  use smali::types::SmaliClass;
    ///
    ///  let c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
    ///  c.write_to_file(Path::new("smali_classes2/com/cool/Class.smali")).unwrap();
    ///
    /// ```
    pub fn write_to_file(&self, path: &Path) -> Result<(), SmaliError> {
        let smali = self.to_smali();
        if let Err(e) = fs::write(path, smali) {
            Err(SmaliError {
                details: e.to_string(),
            })
        } else {
            Ok(())
        }
    }

    /// Writes the current SmaliClass to the specified directory, automatically creating sub-directories for packages
    ///
    /// # Examples
    ///
    /// ```no_run
    ///  use std::path::Path;
    ///  use smali::types::SmaliClass;
    ///
    ///  let c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
    ///  c.write_to_directory(Path::new("smali_classes2")).unwrap();
    ///
    /// ```
    pub fn write_to_directory(&self, path: &Path) -> Result<(), SmaliError> {
        if !path.exists() {
            let _ = fs::create_dir(path);
        }

        // Create package dir structure
        let class_name = self.name.as_java_type();
        let package_dirs: Vec<&str> = class_name.split('.').collect();
        let mut dir = PathBuf::from(path);
        for p in package_dirs[0..package_dirs.len() - 1].iter().copied() {
            dir.push(p);
            if !dir.exists() {
                let _ = fs::create_dir(&dir);
            }
        }

        // Create file
        dir.push(package_dirs[package_dirs.len() - 1].to_string() + ".smali");

        self.write_to_file(&dir)
    }

    /// Writes the class back to the file it was loaded from
    ///
    /// # Examples
    ///
    /// ```no_run
    ///  use std::path::Path;
    ///  use smali::types::SmaliClass;
    ///
    ///  let mut c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
    ///  c.source = None;
    ///  c.save().unwrap();
    ///
    /// ```
    pub fn save(&self) -> Result<(), SmaliError> {
        if let Some(p) = &self.file_path {
            self.write_to_file(p)
        } else {
            Err(SmaliError {
                details: format!(
                    "Unable to save, no file_path set for class: {}",
                    self.name.as_java_type()
                ),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.

    use crate::types::{MethodSignature, TypeSignature};

    #[test]
    fn test_signature() {
        let ts = "Ljava/util/HashMap<Ljava/lang/Class<+Lorg/antlr/v4/runtime/atn/Transition;>;Ljava/lang/Integer;>;";
        let o = TypeSignature::from_jni(ts);
        assert_eq!(o.to_jni(), ts);
    }

    #[test]
    fn test_signature2() {
        let ts = "Lorg/jf/dexlib2/writer/DexWriter<Lorg/jf/dexlib2/writer/builder/BuilderStringReference;Lorg/jf/dexlib2/writer/builder/BuilderStringReference;Lorg/jf/dexlib2/writer/builder/BuilderTypeReference;Lorg/jf/dexlib2/writer/builder/BuilderTypeReference;Lorg/jf/dexlib2/writer/builder/BuilderMethodProtoReference;Lorg/jf/dexlib2/writer/builder/BuilderFieldReference;Lorg/jf/dexlib2/writer/builder/BuilderMethodReference;Lorg/jf/dexlib2/writer/builder/BuilderClassDef;Lorg/jf/dexlib2/writer/builder/BuilderCallSiteReference;Lorg/jf/dexlib2/writer/builder/BuilderMethodHandleReference;Lorg/jf/dexlib2/writer/builder/BuilderAnnotation;Lorg/jf/dexlib2/writer/builder/BuilderAnnotationSet;Lorg/jf/dexlib2/writer/builder/BuilderTypeList;Lorg/jf/dexlib2/writer/builder/BuilderField;Lorg/jf/dexlib2/writer/builder/BuilderMethod;Lorg/jf/dexlib2/writer/builder/BuilderEncodedValues$BuilderArrayEncodedValue;Lorg/jf/dexlib2/writer/builder/BuilderEncodedValues$BuilderEncodedValue;Lorg/jf/dexlib2/writer/builder/BuilderAnnotationElement;Lorg/jf/dexlib2/writer/builder/BuilderStringPool;Lorg/jf/dexlib2/writer/builder/BuilderTypePool;Lorg/jf/dexlib2/writer/builder/BuilderProtoPool;Lorg/jf/dexlib2/writer/builder/BuilderFieldPool;Lorg/jf/dexlib2/writer/builder/BuilderMethodPool;Lorg/jf/dexlib2/writer/builder/BuilderClassPool;Lorg/jf/dexlib2/writer/builder/BuilderCallSitePool;Lorg/jf/dexlib2/writer/builder/BuilderMethodHandlePool;Lorg/jf/dexlib2/writer/builder/BuilderTypeListPool;Lorg/jf/dexlib2/writer/builder/BuilderAnnotationPool;Lorg/jf/dexlib2/writer/builder/BuilderAnnotationSetPool;Lorg/jf/dexlib2/writer/builder/BuilderEncodedArrayPool;>.SectionProvider;";
        let o = TypeSignature::from_jni(ts);
        println!("{o:?}");
        assert_eq!(o.to_jni(), ts);
    }

    #[test]
    fn test_signature3() {
        let ts = "<TSource:Ljava/lang/Object;TAccumulate:Ljava/lang/Object;TResult:Ljava/lang/Object;>Ljava/lang/Object;";
        let o = TypeSignature::from_jni(ts);
        println!("{o:?}");
        assert_eq!(o.to_jni(), ts);
    }

    #[test]
    fn test_method_signature1() {
        let ts = "(TTSource;TTAccumulate;Lcom/strobel/core/Accumulator<TTSource;TTAccumulate;>;Lcom/strobel/core/Selector<TTAccumulate;TTResult;>;)TTResult;";
        let m = MethodSignature::from_jni(ts);
        println!("{m:?}");
        assert_eq!(m.to_jni(), ts);
    }

    #[test]
    fn test_method_signature2() {
        let ts = "<R2:Ljava/lang/Object;>(Lcom/strobel/core/Selector<-TR;+TR2;>;)Ljava/lang/Iterable<TR2;>;^Ljava/lang/Exception;";
        let m = MethodSignature::from_jni(ts);
        println!("{m:?}");
        assert_eq!(m.to_jni(), ts);
    }

    #[test]
    fn test_method_signature3() {
        let ts = "<U:TT;>(TU;)I";
        let m = MethodSignature::from_jni(ts);
        println!("{m:?}");
        assert_eq!(m.to_jni(), ts);
    }

    #[test]
    fn test_method_signature4() {
        let ts = "<R2:Ljava/lang/Object;>(Lcom/strobel/core/Selector<-TR;+TR2;>;)Ljava/lang/Iterable<TR2;>;";
        let m = MethodSignature::from_jni(ts);
        println!("{m:?}");
        assert_eq!(m.to_jni(), ts);
    }

    #[test]
    fn test_method_signature5() {
        let ts = "<T:Landroidx/lifecycle/ViewModel;>(Ljava/lang/Class<TT;>;)TT;";
        let m = MethodSignature::from_jni(ts);
        println!("{m:?}");
        assert_eq!(m.to_jni(), ts);
    }
}