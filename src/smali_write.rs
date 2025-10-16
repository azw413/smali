use crate::types::{AnnotationValue, Modifier, SmaliAnnotation, SmaliClass, SmaliField, SmaliMethod, SmaliOp, SmaliParam};

fn write_modifiers(mods: &Vec<Modifier>) -> String {
    let mut out = "".to_string();

    for m in mods {
        out.push_str(&format!("{} ", m.to_str()));
    }

    out
}

fn write_annotation(ann: &SmaliAnnotation, subannotation: bool, indented: bool) -> String {
    let end_tag;
    let mut indent = "";
    let inset = "    ";
    if indented && subannotation {
        indent = "        ";
    } else if indented || subannotation {
        indent = "    ";
    }

    let mut out = if subannotation {
        end_tag = ".end subannotation";
        ".subannotation ".to_string()
    } else {
        end_tag = ".end annotation";
        format!("{}.annotation {} ", indent, ann.visibility.to_str())
    };
    out.push_str(&ann.annotation_type.to_jni());
    out.push('\n');

    for i in &ann.elements {
        out.push_str(&format!("{}{}{} = ", indent, inset, i.name));
        match &i.value {
            AnnotationValue::Array(a) => {
                out.push_str("{\n");
                let mut c = 0;
                for v in a {
                    out.push_str(indent);
                    out.push_str(inset);
                    out.push_str(inset);
                    out.push_str(v);
                    c += 1;
                    if c < a.len() {
                        out.push(',');
                    }
                    out.push('\n');
                }
                out.push_str(&format!("{indent}{inset}}}\n"));
            }
            AnnotationValue::SubAnnotation(s) => {
                out.push_str(&write_annotation(s, true, indented));
            }
            AnnotationValue::Enum(o, s) => {
                out.push_str(&format!(
                    ".enum {}->{}:{}\n",
                    o.as_jni_type(),
                    s,
                    o.as_jni_type()
                ));
            }
            AnnotationValue::Single(s) => {
                out.push_str(&format!("{s}\n"));
            }
        }
    }

    out.push_str(indent);
    out.push_str(end_tag);

    if indented {
        out.push('\n');
    }

    out
}

fn write_param(param: &SmaliParam) -> String 
{
    let mut output = String::new();
    
    // Handle only if annotations if present
    if !param.annotations.is_empty() 
    {
        // Start the .param directive
        output.push_str(".param ");
        output.push_str(&param.register);
    
        // Add name if present
        if let Some(name) = &param.name {
            output.push_str(", \"");
            output.push_str(name);
            output.push('"');
        }
        
        output.push('\n');

        // Write each annotation
        for annotation in &param.annotations {
            output.push_str(&write_annotation(annotation, false, true));
        }

        // Close the param block
        output.push_str(".end param");
    }

    output
}

pub(crate) fn write_method(method: &SmaliMethod) -> String {
    let mut out = format!(".method {}", write_modifiers(&method.modifiers));
    if method.constructor {
        out.push_str("constructor ");
    }
    out.push_str(&format!("{}{}\n", method.name, method.signature.to_jni()));

    // Write parameters
    for param in &method.params {
        let param_str = write_param(param);
        // Indent parameter lines
        for line in param_str.lines() {
            out.push_str("    ");
            out.push_str(line);
            out.push('\n');
        }
    }
    
    // Write method annotations
    for a in &method.annotations {
        out.push_str(&write_annotation(a, false, true));
    }

    if !method.ops.is_empty() {
        out.push_str(&format!("    .locals {:}\n", method.locals));
    }

    for i in &method.ops {
        match i {
            SmaliOp::Line(l) => {
                out.push_str(&format!("    .line {l:}\n"));
            }
            SmaliOp::Label(l) => {
                out.push_str(&format!("    {l}\n"));
            }
            SmaliOp::Op(s) => {
                out.push_str(&format!("    {s}\n"));
            }
            SmaliOp::Catch(c) => {
                out.push_str(&format!("    {c}\n"));
            }
            SmaliOp::ArrayData(ad) => {
                out.push_str(&format!("    {ad}\n"));
            }
            SmaliOp::PackedSwitch(ps) => {
                out.push_str(&format!("    {ps}\n"));
            }
            SmaliOp::SparseSwitch(ss) => {
                out.push_str(&format!("    {ss}\n"));
            }
        }
    }

    out.push_str(".end method\n\n");
    out
}

pub(crate) fn write_field(f: &SmaliField) -> String 
{
    let mut out = String::new();
    out.push_str(&format!(
        ".field {}{}:{}",
        write_modifiers(&f.modifiers),
        f.name,
        f.signature.to_jni()
    ));
    if let Some(iv) = &f.initial_value {
        out.push_str(&format!(" = {iv}"));
    }
    out.push('\n');
    if !f.annotations.is_empty() {
        for a in &f.annotations {
            out.push_str(&write_annotation(a, false, true));
        }
        out.push_str(".end field\n");
    }
    out.push('\n');
    out
}

pub(crate) fn write_class(dex: &SmaliClass) -> String {
    let mut out = format!(
        ".class {}{}\n",
        write_modifiers(&dex.modifiers),
        dex.name.as_jni_type()
    );
    out.push_str(&format!(".super {}\n", dex.super_class.as_jni_type()));
    if let Some(s) = &dex.source {
        out.push_str(&format!(".source \"{s}\"\n"));
    }

    if !dex.implements.is_empty() {
        out.push_str("\n# interfaces\n");
        for i in &dex.implements {
            out.push_str(".implements ");
            out.push_str(&i.as_jni_type());
            out.push('\n');
        }
    }

    if !dex.annotations.is_empty() {
        out.push_str("\n# annotations\n");
        for a in &dex.annotations {
            out.push_str(&write_annotation(a, false, false));
            out.push('\n');
        }
    }

    if !dex.fields.is_empty() {
        out.push_str("\n# fields\n");
        for f in &dex.fields { out.push_str(&write_field(f)); }
    }

    if !dex.methods.is_empty() {
        out.push_str("\n# methods\n");
        for m in &dex.methods {
            out.push_str(&write_method(m));
        }
    }

    out
}