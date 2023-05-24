
/* Struct to represent a java object type identifer e.g. java.lang.Object */
/* They are stored in the smali native (also JNI) format e.g. Ljava/lang/Object; */

use std::error::Error;
use std::{fmt, fs};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use nom::Err::Failure;
use nom::IResult;
use crate::smali_parse::parse_class;
use crate::smali_parse::parse_methodsignature;
use crate::smali_write::write_class;

/* Custom error for our command helper */
#[derive(Debug)]
pub struct SmaliError {
    pub details: String
}

impl SmaliError {
    pub fn new(msg: &str) -> SmaliError {
        SmaliError{details: msg.to_string()}
    }
}

impl fmt::Display for SmaliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,"{}",self.details)
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
///  let o = ObjectIdentifier::from_java_type("com.basic.Test");
///  assert_eq!(o.as_java_type(), "com.basic.Test");
///  assert_eq!(o.as_jni_type(), "Lcom/basic/Test;");
/// ```
#[derive(Debug, Eq)]
pub struct ObjectIdentifier {
    jni_type: String
}

impl PartialEq<Self> for ObjectIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.jni_type == other.jni_type
    }
}

impl Hash for ObjectIdentifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.jni_type.hash(state);
    }
}

impl ObjectIdentifier
{
    pub fn from_jni_type(t: &str) -> ObjectIdentifier
    {
        ObjectIdentifier { jni_type: t.to_string()}
    }

    pub fn from_java_type(t: &str) -> ObjectIdentifier
    {
        let jni_type = format!("L{};", t.replace(".", "/"));
        ObjectIdentifier { jni_type }
    }

    pub fn as_jni_type(&self) -> String
    {
        self.jni_type.to_string()
    }

    pub fn as_java_type(&self) -> String
    {
        let java_type = self.jni_type[1..self.jni_type.len() - 1].replace("/", ".");
        java_type
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
#[derive(Debug, Eq)]
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
}

impl PartialEq<Self> for TypeSignature {
    fn eq(&self, other: &Self) -> bool {
        self.to_jni() == other.to_jni()
    }
}

impl TypeSignature {
   pub fn from_jni(s: &str) -> TypeSignature
   {
       match s.chars().nth(0).unwrap()
       {
           '[' => TypeSignature::Array(Box::new(TypeSignature::from_jni(&s[1..].to_string()))),
           'Z' => TypeSignature::Bool,
           'B' => TypeSignature::Byte,
           'C' => TypeSignature::Char,
           'S' => TypeSignature::Short,
           'I' => TypeSignature::Int,
           'J' => TypeSignature::Long,
           'F' => TypeSignature::Float,
           'D' => TypeSignature::Double,
           'L' => TypeSignature::Object(ObjectIdentifier::from_jni_type(s)),
            _  => TypeSignature::Void
       }
   }

    pub fn to_jni(&self) -> String
    {
        match self
        {
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
            TypeSignature::Void => "V".to_string()
        }
    }
}

/// Simple enum to represent Java method, field and class modifiers
///

#[derive(Debug)]
pub enum Modifier {
    Public,
    Protected,
    Private,
    Static,
    Final,
    Abstract,
    Synthetic,
    Transient,
    Volatile,
    Synchronized,
    Native,
    Varargs
}

impl Modifier {
    pub fn from_str(s: &str) -> Modifier
    {
        match s {
            "public" => Modifier::Public,
            "protected" => Modifier::Protected,
            "private" => Modifier::Private,
            "static" => Modifier::Static,
            "final" => Modifier::Final,
            "abstract" => Modifier::Abstract,
            "synthetic" => Modifier::Synthetic,
            "transient" => Modifier::Transient,
            "volatile" => Modifier::Volatile,
            "synchronized" => Modifier::Synchronized,
            "native" => Modifier::Native,
            "varargs" => Modifier::Varargs,
            _ => Modifier::Public
        }
    }

    pub fn to_str(&self) -> &str
    {
        match self {
            Modifier::Public => "public",
            Modifier::Protected => "protected",
            Modifier::Private => "private",
            Modifier::Static => "static",
            Modifier::Final => "final",
            Modifier::Abstract => "abstract",
            Modifier::Synthetic => "synthetic",
            Modifier::Transient => "transient",
            Modifier::Volatile => "volatile",
            Modifier::Synchronized => "synchronized",
            Modifier::Native => "native",
            Modifier::Varargs => "varargs",
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
///  assert_eq!(m.return_type, TypeSignature::Void);
/// ```
#[derive(Debug)]
pub struct MethodSignature {
    pub args: Vec<TypeSignature>,
    pub return_type: TypeSignature,
}

impl MethodSignature {

    pub fn from_jni(s: &str) -> MethodSignature
    {
        let (_, m) = parse_methodsignature(s).expect("Can't parse MethodSignature");
        m
    }

    pub fn to_jni(&self) -> String
    {
        let mut s = "(".to_string();
        for t in &self.args
        {
            let ts = t.to_jni();
            s.push_str(&*ts);
        }
        s.push(')');
        s.push_str(&*self.return_type.to_jni());
        s
    }
}

/// Simple enum to represent annotation visibility: build, runtime, system.
///
#[derive(Debug)]
pub enum AnnotationVisibility {
    Build,
    Runtime,
    System
}

impl AnnotationVisibility {
    pub fn to_str(&self) -> &str
    {
        match self {
            Self::Build => "build",
            Self::Runtime => "runtime",
            Self::System => "system"
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
    Enum(ObjectIdentifier, String)
}

impl AnnotationVisibility {
    pub fn from_str(s: &str) -> Self
    {
        match s {
            "build" => Self::Build,
            "system" => Self::System,
            _ => Self::Runtime
        }
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

/// An enum representing instructions within a method, these can be a label, a line number or a dex instruction as a String.
///
#[derive(Debug)]
pub enum SmaliInstruction {
    Label(String),
    Line(u32),
    Instruction(String)
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
    /// Number of local variables required by the instructions
    pub locals: u32,
    /// Any method level annotations
    pub annotations: Vec<SmaliAnnotation>,
    /// Method instructions
    pub instructions: Vec<SmaliInstruction>,
}

/// Represents a smali class i.e. the whole .smali file
///
/// # Examples
///
/// ```
///  use std::path::Path;
///  use smali::types::SmaliClass;
///
///  let c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
///  println!("Java class: {}" c.name.as_java_type());
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
    pub file_path: Option<PathBuf>
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
    /// ```
    ///  use smali::types::SmaliClass;
    ///
    ///  let c = SmaliClass::from_smali(smali).expect("Parse error");
    ///
    /// ```
    pub fn from_smali(s: &str) -> Result<SmaliClass, SmaliError>
    {
        let d = parse_class(s);
        match d {
            IResult::Ok((_, cl)) => Ok(cl),
            IResult::Err(Failure(e)) => Err(SmaliError { details: format!("Class parse error at: {:?}", e.to_string()) }),
            _ => Err(SmaliError { details: "Unknown class parse error".to_string() })
        }
    }

    /// Creates a SmaliClass from a file containing a valid smali document
    ///
    /// # Examples
    ///
    /// ```
    ///  use std::path::Path;
    ///  use smali::types::SmaliClass;
    ///
    ///  let c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
    ///
    /// ```
    pub fn read_from_file(path: &Path) -> Result<SmaliClass, SmaliError>
    {
        match fs::read_to_string(path)
        {
            Ok(s) => {
               let mut c = SmaliClass::from_smali(&s)?;
                c.file_path = Some(PathBuf::from(path));
               Ok(c)
            }
            Err(e) => { Err(SmaliError { details: format!("Error loading file {}: {}", path.to_str().unwrap(), e.to_string()) }) }
        }
    }

    /// Creates a smali document string from the current class
    ///
    /// # Examples
    ///
    /// ```
    ///  use std::path::Path;
    ///  use smali::types::SmaliClass;
    ///
    ///  let c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
    ///  println!("{}", c.to_smali());
    ///
    /// ```
    pub fn to_smali(&self) -> String
    {
        write_class(self)
    }

    /// Writes the current SmaliClass to the specified file path as a smali document
    ///
    /// # Examples
    ///
    /// ```
    ///  use std::path::Path;
    ///  use smali::types::SmaliClass;
    ///
    ///  let c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
    ///  c.write_to_file(Path::new("smali_classes2/com/cool/Class.smali"))?;
    ///
    /// ```
    pub fn write_to_file(&self, path: &Path) -> Result<(), SmaliError>
    {
        let smali = self.to_smali();
        if let Err(e) = fs::write(path, smali)
        {
            Err(SmaliError { details: e.to_string() })
        }
        else { Ok(()) }
    }

    /// Writes the current SmaliClass to the specified directory, automatically creating sub-directories for packages
    ///
    /// # Examples
    ///
    /// ```
    ///  use std::path::Path;
    ///  use smali::types::SmaliClass;
    ///
    ///  let c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
    ///  c.write_to_directory(Path::new("smali_classes2"))?;
    ///
    /// ```
    pub fn write_to_directory(&self, path: &Path) -> Result<(), SmaliError>
    {

        if !path.exists() { let _ = fs::create_dir(&path); }

        // Create package dir structure
        let class_name = self.name.as_java_type();
        let package_dirs: Vec<&str> = class_name.split('.').collect();
        let mut dir = PathBuf::from(path);
        for p in package_dirs[0..package_dirs.len()-1].to_vec()
        {
            dir.push(p);
            if !dir.exists() { let _ = fs::create_dir(&dir); }
        }

        // Create file
        dir.push(package_dirs[package_dirs.len()-1].to_string() + ".smali");

        self.write_to_file(&dir)
    }

    /// Writes the class back to the file it was loaded from
    ///
    /// # Examples
    ///
    /// ```
    ///  use smali::types::SmaliClass;
    ///
    ///  let mut c = SmaliClass::read_from_file(Path::new("smali/com/cool/Class.smali")).expect("Uh oh, does the file exist?");
    ///  c.source = None;
    ///  c.save()?;
    ///
    /// ```
    pub fn save(&self) -> Result<(), SmaliError>
    {
        if let Some(p) = &self.file_path
        {
            self.write_to_file(p)
        }
        else { Err(SmaliError { details: format!("Unable to save, no file_path set for class: {}", self.name.as_java_type()) }) }
    }
}
