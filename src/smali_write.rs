use crate::types::{AnnotationValue, Modifier, SmaliAnnotation, SmaliClass, SmaliInstruction, SmaliMethod};

fn write_modifiers(mods: &Vec<Modifier>) -> String
{
   let mut out = "".to_string();

   for m in mods
   {
       out.push_str(&format!("{} ", m.to_str()));
   }

   out
}

fn write_annotation(ann: &SmaliAnnotation, subannotation: bool, indented: bool) -> String
{
    let end_tag;
    let mut indent = "";
    let inset = "    ";
    if indented && subannotation { indent = "        "; }
    else if indented || subannotation { indent = "    "; }

    let mut out = if subannotation
    {
        end_tag = ".end subannotation";
        ".subannotation ".to_string()
    }
    else
    {
        end_tag = ".end annotation";
        format!("{}.annotation {} ", indent, ann.visibility.to_str())
    };
    out.push_str(&ann.annotation_type.to_jni());
    out.push('\n');

    for i in &ann.elements
    {
        out.push_str(&format!("{}{}{} = ", indent, inset, i.name));
        match &i.value
        {
            AnnotationValue::Array(a) => {
                out.push_str("{\n");
                let mut c = 0;
                for v in a
                {
                    out.push_str(indent);
                    out.push_str(inset);
                    out.push_str(inset);
                    out.push_str(v);
                    c += 1;
                    if c < a.len() { out.push(','); }
                    out.push('\n');
                }
                out.push_str(&format!("{}{}}}\n", indent, inset));
            }
            AnnotationValue::SubAnnotation(s) => { out.push_str( &write_annotation(s, true, indented)); }
            AnnotationValue::Enum(o, s) => { out.push_str(&format!(".enum {}->{}:{}\n", o.as_jni_type(), s, o.as_jni_type())); }
            AnnotationValue::Single(s) => { out.push_str(&format!("{}\n", s)); }
        }
    }

    out.push_str(indent);
    out.push_str(end_tag);
    out.push('\n');

    out
}

fn write_method(method: &SmaliMethod) -> String
{
    let mut out = format!(".method {}", write_modifiers(&method.modifiers));
    if method.constructor { out.push_str("constructor "); }
    out.push_str(&format!("{}{}\n", method.name, method.signature.to_jni()));
    if !method.instructions.is_empty()
    {
        out.push_str(&format!("    .locals {:}\n", method.locals));
    }

    for a in &method.annotations
    {
        out.push_str(&write_annotation(a, false, true));
    }

    for i in &method.instructions
    {
        match i
        {
            SmaliInstruction::Line(l) => { out.push_str(&format!("    .line {:}\n", l)); }
            SmaliInstruction::Label(l) => { out.push_str(&format!("    {}\n", l)); }
            SmaliInstruction::Instruction(s) => { out.push_str(&format!("    {}\n", s)); }
            SmaliInstruction::Catch(c) => { out.push_str(&format!("    {}\n", c)); }
            SmaliInstruction::ArrayData(ad) => { out.push_str(&format!("    {}\n", ad)); }
            SmaliInstruction::PackedSwitch(ps) => { out.push_str(&format!("    {}\n", ps)); }
            SmaliInstruction::SparseSwitch(ss) => { out.push_str(&format!("    {}\n", ss)); }
        }
    }

    out.push_str(".end method\n\n");
    out
}

pub(crate) fn write_class(dex: &SmaliClass) -> String
{
    let mut out = format!(".class {}{}\n", write_modifiers(&dex.modifiers), dex.name.as_jni_type());
    out.push_str(&format!(".super {}\n", dex.super_class.as_jni_type()));
    if let Some(s) = &dex.source
    {
        out.push_str(&format!(".source \"{}\"\n", s));
    }

    if !dex.implements.is_empty()
    {
        out.push_str("\n# interfaces\n");
        for i in &dex.implements
        {
            out.push_str(".implements ");
            out.push_str(&i.as_jni_type());
            out.push('\n');
        }
    }

    if !dex.annotations.is_empty()
    {
        out.push_str("\n# annotations\n");
        for a in &dex.annotations
        {
            out.push_str(&write_annotation(a, false, false));
            out.push('\n');
        }
    }

    if !dex.fields.is_empty()
    {
        out.push_str("\n# fields\n");
        for f in &dex.fields
        {
            out.push_str(&format!(".field {}{}:{}", write_modifiers(&f.modifiers), f.name, f.signature.to_jni()));
            if let Some(iv) = &f.initial_value
            {
                out.push_str(&format!(" = {}", iv));
            }
            out.push('\n');
            if !f.annotations.is_empty()
            {
                for a in &f.annotations {  out.push_str(&write_annotation(a, false, true)); }
                out.push_str(".end field\n");
            }
            out.push('\n');
        }
    }

    if !dex.methods.is_empty()
    {
        out.push_str("\n# methods\n");
        for m in &dex.methods
        {
            out.push_str(&write_method(m));
        }
    }

    out
}