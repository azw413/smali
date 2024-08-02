
use nom::bytes::complete::{escaped, is_not, tag, take_while};
use nom::branch::{ alt };
use nom::character::complete::{alphanumeric1, char, multispace0, multispace1, space0, newline, none_of, not_line_ending, one_of};
use nom::combinator::value;
use nom::Err::Failure;
use nom::error::{Error, ErrorKind};
use nom::{IResult};
use nom::multi::{many0};
use nom::sequence::{delimited, pair};
use crate::types::*;


fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
    where
        F: Fn(&'a str) -> IResult<&'a str, O>,
{
    delimited(
        multispace0,
        inner,
        multispace0
    )
}

fn quoted<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, &'a str>
{
   let esc = escaped(none_of("\\\""), '\\', one_of("'\"tbnrfu\\"));
   let esc_or_empty = alt((esc, tag("")));

   delimited(
        pair(multispace0, char('"')),
        esc_or_empty,
        pair(char('"'), multispace0)
   )
}

pub fn comment(i: &str) -> IResult<&str, ()>
{
    let (v, _) = value(
        (), // Output is thrown away.
        pair(char('#'), is_not("\n\r"))
    )(i)?;
    let (o, _) = newline(v)?;
    IResult::Ok((o, ()))
}

pub fn blank_line(i: &str) -> IResult<&str, &str>
{
    multispace1(i)
}

fn parse_modifiers(smali: &str) -> IResult<&str, Vec<Modifier>>
{
    let (input, mods) = many0(
                                        alt(( ws(tag("public ")),
                                              ws(tag("protected ")),
                                              ws(tag("private ")),
                                              ws(tag("static ")),
                                              ws(tag("final ")),
                                              ws(tag("abstract ")),
                                              ws(tag("interface ")),
                                              ws(tag("synthetic ")),
                                              ws(tag("transient ")),
                                              ws(tag("volatile ")),
                                              ws(tag("synchronized ")),
                                              ws(tag("native ")),
                                              ws(tag("varargs ")),
                                              ws(tag("annotation ")),
                                              ws(tag("enum ")),
                                              ws(tag("strict ")),
                                              ws(tag("bridge ")),
                                              ws(tag("constructor "))
                                        )))(smali)?;
    let mut v = vec![];
    for m in mods { v.push(Modifier::from_str(m.trim())); }
    IResult::Ok((input, v))
}

fn parse_visibility(smali: &str) -> IResult<&str, AnnotationVisibility>
{
    let (input, visibility) =
        alt(( ws(tag("build")),
              ws(tag("runtime")),
              ws(tag("system"))
        ))(smali)?;
    IResult::Ok((input, AnnotationVisibility::from_str(visibility)))
}


fn take_until_eol(s: &str) -> IResult<&str, &str> {
    let (input, r) = take_while(|c| c != '\n')(s)?;
    let (input, _) = newline(input)?;
    IResult::Ok((input, r))
}


fn parse_class_line(smali: &str) -> IResult<&str, (Vec<Modifier>, String)>
{
    let (input, _) = tag(".class")(smali)?;
    let (input, modifiers) = parse_modifiers(input)?;
    let (input, class_type) = take_until_eol(input)?;
    Ok((input, (modifiers, class_type.trim().to_string())))
}

fn parse_super_line(smali: &str) -> IResult<&str, String>
{
    let (input, _) = tag(".super")(smali)?;
    let (input, class_type) = take_until_eol(input)?;
    Ok((input, class_type.trim().to_string()))
}

fn parse_implements_line(smali: &str) -> IResult<&str, String>
{
    let (input, _) = tag(".implements")(smali)?;
    let (input, class_type) = take_until_eol(input)?;
    Ok((input, class_type.trim().to_string()))
}

fn parse_source_line(smali: &str) -> IResult<&str, String>
{
    let (input, _) = tag(".source")(smali)?;
    let (input, class_type) = quoted()(input)?;
    Ok((input, class_type.trim().to_string()))
}

fn parse_java_array(smali: &str) -> IResult<&str, Vec<String>>
{
    let (o, _) = ws(tag("{"))(smali)?;
    // Array of strings
    let mut v = vec![];

    let mut input= o;
    loop
    {
        if let IResult::Ok((o, s)) = quoted()(input)
        {
            v.push(format!("\"{}\"", s));
            input = o;
        }
        if let IResult::Ok((o, s)) = ws(is_not(",}}"))(input)
        {
            v.push(s.to_string());
            input = o;
        }

        if let IResult::Ok((o, _)) = ws(tag("}"))(input)
        {
            return Ok((o, v))
        }

        let (o, _) = tag(",")(input)?;
        input = o;
    }

}

fn parse_annotation_element(smali: &str) -> IResult<&str, AnnotationElement>
{
    let (input, name) = ws(alphanumeric1)(smali)?;
    let (i, _) = tag("=")(input)?;

    let mut input= i;

    let ae = AnnotationElement {
        name: name.to_string(),
        value:

        // Try subannotation
        if let IResult::Ok((i, e)) = parse_annotation(input, true)
        {
            input = i;
            AnnotationValue::SubAnnotation(e)
        }
        // Try enum
        else if let IResult::Ok((i, e)) = parse_enum(input)
        {
            input = i;
            e
        }
        // Try the array
        else if let IResult::Ok((i, elements)) = parse_java_array(input)
        {
            input = i;
            AnnotationValue::Array(elements)
        } else {
            let (i, v) = ws(not_line_ending)(input)?;
            input = i;
            AnnotationValue::Single(v.to_string())
        }
    };

    Ok((input, ae))
}

fn parse_enum(smali: &str) -> IResult<&str, AnnotationValue>
{
    let (input, _) = ws(tag(".enum"))(smali)?;
    let (input, object) = ws(take_while(|c| c != '-'))(input)?;
    let (input, _) = tag("->")(input)?;
    let (input, field) = take_while(|x| { x!= ':' && x != '\n'})(input)?;
    let (input, _) = take_until_eol(input)?;
    Ok((input, AnnotationValue::Enum(ObjectIdentifier::from_jni_type(object), field.to_string())))
}

fn parse_annotation(smali: &str, subannotation: bool) -> IResult<&str, SmaliAnnotation>
{
    let end_tag;
    let input;
    let mut visibility = AnnotationVisibility::System;
    if !subannotation
    {
        let (o, _) = ws(tag(".annotation"))(smali)?;
        let (o, v) = parse_visibility(o)?;
        end_tag = ".end annotation";
        input = o;
        visibility = v;
    }
    else
    {
        let (o, _) = ws(tag(".subannotation"))(smali)?;
        end_tag = ".end subannotation";
        input = o;
    }

    let (input, type_sig) = take_until_eol(input)?;
    let mut annotation = SmaliAnnotation {
        visibility,
        annotation_type: TypeSignature::from_jni(type_sig),
        elements: vec![]
    };

    // parse elements
    let mut input = input;

    loop
    {

        let mut found = false;

        // Is it the end
        let end: IResult<&str, &str> = ws(tag(end_tag))(input);
        if let IResult::Ok((o, _)) = end
        {
            return Ok((o, annotation))
        }

        // Another element
        if let IResult::Ok((o, element)) = parse_annotation_element(input)
        {
            annotation.elements.push(element);
            input = o; found = true;
        }

        // Can't parse this - error
        if !found
        {
            return IResult::Err(Failure(Error { input: input, code: ErrorKind::Fail }));
        }
    }
}

fn parse_field(smali: &str) -> IResult<&str, SmaliField>
{
    let (input, _) = tag(".field")(smali)?;
    let (input, modifiers) = parse_modifiers(input)?;
    let (input, name) = ws(take_while(|c| c != ':'))(input)?;
    let (input, _) = tag(":")(input)?;
    let (o, type_sig) = take_while(|x| { x != '=' && x != '\n'})(input)?;

    let mut field = SmaliField {
        name: name.to_string(),
        modifiers,
        signature: TypeSignature::from_jni(type_sig.trim()),
        annotations: vec![],
        initial_value: None
    };

    let mut input = o;

    // Check for initial value
    let eq = ws(tag("="))(input);
    if let IResult::Ok((o, _)) = eq
    {
        let (o, iv) = take_until_eol(o)?;
        input = o;
        field.initial_value = Some(iv.to_string());
    }

    // Check for any annotations
    if let IResult::Ok((o, a)) = parse_annotation(input, false )
    {
        field.annotations.push(a);

        let mut input = o;

        loop
        {
            let mut found = false;

            // Is it the end
            let end: IResult<&str, &str> = ws(tag(".end field"))(input);
            if let IResult::Ok((o, _)) = end
            {
                return Ok((o, field))
            }

            if let IResult::Ok((o, a)) = parse_annotation(input, false)
            {
                field.annotations.push(a);
                input = o;
                found = true;
            }

            // Can't parse this - error
            if !found
            {
                return IResult::Err(Failure(Error { input: input, code: ErrorKind::Fail }));
            }
        }
    }
    IResult::Ok((input, field))
}

fn parse_instruction(smali: &str) -> IResult<&str, SmaliInstruction>
{
    // Line
    if let IResult::Ok((o, _)) = ws(tag(".line"))(smali)
    {
        let (o, n) = take_until_eol(o)?;
        IResult::Ok((o, SmaliInstruction::Line(n.parse::<u32>().unwrap())))
    }

    // Label
    else if let IResult::Ok((o, _)) = ws(tag(":"))(smali)
    {
        let (o, n) = take_until_eol(o)?;
        IResult::Ok((o, SmaliInstruction::Label(n.to_string())))
    }

    // Actual instruction
    else if let IResult::Ok((o, n)) = take_until_eol(smali)
    {
        IResult::Ok((o, SmaliInstruction::Instruction(n.to_string())))
    }

    // Anything else - failure
    else
    {
        IResult::Err(Failure(Error { input: smali, code: ErrorKind::Fail }))
    }
}

fn parse_method(smali: &str) -> IResult<&str, SmaliMethod>
{
    let (input, _) = tag(".method")(smali)?;
    let (o, modifiers) = parse_modifiers(input)?;

    let mut input = o;

    // Is it a class initialiser or constructor
    let constructor = if let IResult::Ok((o, _)) = ws(tag("constructor "))(input)
    {
        input = o;
        true
    }
    else { false };

    let (o, name) = take_while(|c| c != '(')(input)?;
    let (o, ms) = parse_methodsignature(o)?;

    let (o, _) = pair(space0, newline)(o)?;

    // locals
    let l = ws(tag(".locals"))(o);
    let locals = if let IResult::Ok((o, _)) = l
    {
        let (o, local) = take_until_eol(o)?;
        input = o;
        local
    }
    else { input = o; "0" };

    let mut method = SmaliMethod {
        name: name.to_string(),
        constructor,
        modifiers,
        signature: ms,
        locals: locals.parse::<u32>().unwrap(),
        annotations: vec![],
        instructions: vec![]
    };

    // Check for any annotations
    if let IResult::Ok((o, a)) = parse_annotation(input, false )
    {
        method.annotations.push(a);
        input = o;

        loop
        {
            let mut found = false;

            // Is it the end
            let end: IResult<&str, &str> = ws(tag(".end method"))(input);
            if let IResult::Ok((o, _)) = end
            {
                return Ok((o, method))
            }

            if let IResult::Ok((o, a)) = parse_annotation(input, false)
            {
                method.annotations.push(a);
                input = o;
                found = true;
            }

            // Can't parse this - move on to instructions
            if !found {  break; }
        }
    }

    // Now parse the instructions
    loop
    {
        let mut found = false;

        // Try a blank line
        if let IResult::Ok((o, _)) = blank_line(input) { input = o; found = true; }

        // Is it the end
        let end: IResult<&str, &str> = ws(tag(".end method"))(input);
        if let IResult::Ok((o, _)) = end
        {
            return Ok((o, method))
        }

        if let IResult::Ok((o, i)) = parse_instruction(input)
        {
            method.instructions.push(i);
            input = o;
            found = true;
        }

        // Can't parse this - error
        if !found
        {
            return IResult::Err(Failure(Error { input: input, code: ErrorKind::Fail }));
        }
    }
}



pub(crate) fn parse_class(smali: &str) -> IResult<&str, SmaliClass>
{
    let mut input = smali;

    let mut dex = SmaliClass {
        name: ObjectIdentifier::from_java_type("java.lang.Object"),
        super_class: ObjectIdentifier::from_java_type("java.lang.Object"),
        source: None,
        implements: vec![],
        annotations: vec![],
        fields: vec![],
        methods: vec![],
        modifiers: vec![],
        file_path: None
    };

    loop
    {
        let mut found = false;

        // Try a blank line
        if let IResult::Ok((o, _)) = blank_line(input) { /* println!("blank line"); */ input = o; found = true; }

        // Try a comment
        if let IResult::Ok((o, _)) = comment(input) { /* println!("comment"); */ input = o; found = true; }

        // Is it a class line
        if let IResult::Ok((o, (m, c))) = parse_class_line(input )
        {
            // println!("class line: {:?} {}", m, c);
            dex.modifiers = m;
            dex.name = ObjectIdentifier::from_jni_type(&c);
            input = o; found = true;
        }

        // Is it a super line
        if let IResult::Ok((o, c)) = parse_super_line(input )
        {
            // println!("super line: {}", c);
            dex.super_class = ObjectIdentifier::from_jni_type(&c);
            input = o; found = true;
        }

        // Is it a source line
        if let IResult::Ok((o, c)) = parse_source_line(input )
        {
            // println!("source line: {}", c);
            dex.source = Some(c);
            input = o; found = true;
        }

        // Is it an implements line
        if let IResult::Ok((o, c)) = parse_implements_line(input )
        {
            // println!("implements line: {}", c);
            dex.implements.push(ObjectIdentifier::from_jni_type(&c));
            input = o; found = true;
        }

        // Is it an annotation line
        //let ann: IResult<&str, SmaliAnnotation>  = parse_annotation(input );
        if let IResult::Ok((o, a)) = parse_annotation(input, false )
        {
            // println!("annotation: {:?}", a);
            dex.annotations.push(a);
            input = o; found = true;
        }

        // Is it a field line
        //let ann: IResult<&str, SmaliAnnotation>  = parse_annotation(input );
        if let IResult::Ok((o, f)) = parse_field(input )
        {
            // println!("field: {:?}", f);
            dex.fields.push(f);
            input = o; found = true;
        }

        // Is it a field line
        //let ann: IResult<&str, SmaliAnnotation>  = parse_annotation(input );
        if let IResult::Ok((o, m)) = parse_method(input )
        {
            // println!("method: {:?}", m);
            dex.methods.push(m);
            input = o; found = true;
        }

        if input.len() == 0 { return IResult::Ok((input, dex)) }

        // Can't parse this - error
        if !found
        {
            return IResult::Err(Failure(Error { input: input, code: ErrorKind::Fail }));
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use super::*;
    use crate::types::{AnnotationValue, parse_typesignature};

    #[test]
    fn test_take_until_eol() {
        let (_, l) = take_until_eol("test this\n").unwrap();
        assert_eq!(l, "test this");
    }

    #[test]
    fn test_quoted() {
        let (_, l) = quoted()("\"test this\"\n").unwrap();
        assert_eq!(l, "test this");
        let (_, l) = quoted()("\"test\\\" this\"\n").unwrap();
        assert_eq!(l, "test\\\" this");
        let (_, l) = quoted()("\"\\u0008\\u001f\\u0010!R\\u0013\\u0010\\\"\\u001a\"").unwrap();
        assert_eq!(l, "\\u0008\\u001f\\u0010!R\\u0013\\u0010\\\"\\u001a");
        let (_, l) = quoted()("\"a\\nb\\rc\\t \\u0008 'boo' \\\"d\\\" \\f\"").unwrap();
        assert_eq!(l, "a\\nb\\rc\\t \\u0008 'boo' \\\"d\\\" \\f");
        let (_, a) = quoted()("\"\\\\J\"").unwrap();
        assert_eq!(a, "\\\\J");
    }

    #[test]
    fn test_typesignature() {
        let (_, t) = parse_typesignature("[B").unwrap();
        println!("{:?}", t);
        let (_, t) = parse_typesignature("V").unwrap();
        println!("{:?}", t);
        let (_, t) = parse_typesignature("Lcom/none/Class;").unwrap();
        println!("{:?}", t);
    }

    #[test]
    fn test_methodsignature() {
        let (_, t) = parse_methodsignature("([B)V").unwrap();
        println!("{:?}", t);

    }


    #[test]
    fn test_parse_class_line() {
        let (_, (modifiers, type_name)) = parse_class_line(".class public final interface Lokhttp3/OkHttpClient;\n").unwrap();
        assert_eq!(modifiers, [Modifier::Public, Modifier::Final, Modifier::Interface]);
        assert_eq!(type_name, "Lokhttp3/OkHttpClient;");
    }

    #[test]
    fn test_parse_super_line() {
        let (_, l) = parse_super_line(".super Ljava/lang/Object;\n").unwrap();
        assert_eq!(l, "Ljava/lang/Object;");
    }

    #[test]
    fn test_parse_implements_line() {
        let (_, l) = parse_implements_line(".implements Ljava/lang/Cloneable;\n").unwrap();
        assert_eq!(l, "Ljava/lang/Cloneable;");
    }

    #[test]
    fn test_parse_annotation_element_single() {
        let (_, a) = parse_annotation_element(" k = 0x1\n").unwrap();
        assert_eq!(a.name, "k");
        match a.value
        {
            AnnotationValue::Single(s) => { assert_eq!( s, "0x1".to_string()); }
            _ => { println!("{:?}", a); }
        }
    }

    #[test]
    fn test_parse_annotation_build() {
        let (_, a) = parse_annotation(".annotation build Landroid/annotation/TargetApi;\nvalue = 0x13\n.end annotation", false ).unwrap();
        assert!(matches!(a.visibility, AnnotationVisibility::Build));
        assert!(matches!(a.annotation_type, TypeSignature::Object(t) if t == ObjectIdentifier::from_java_type("android.annotation.TargetApi")));
        assert_eq!(a.elements.len(), 1);
        assert_eq!(a.elements[0].name, "value");
        assert!(matches!(&a.elements[0].value, AnnotationValue::Single(v) if v == "0x13"));
    }

    #[test]
    fn test_parse_enum() {
        let (_, a) = parse_enum(" .enum Lkotlin/DeprecationLevel;->ERROR:Lkotlin/DeprecationLevel;\n" ).unwrap();
        match a {
            AnnotationValue::Enum(o, f) => {
                assert_eq!(o.as_java_type(), "kotlin.DeprecationLevel");
                assert_eq!(f, "ERROR");
            }
            _ => { println!("{:?}", a); }
        }
    }

    #[test]
    fn test_parse_java_array() {
        let (_, a) = parse_java_array("{ 1, 2,\n 3, 4\n } " ).unwrap();
        assert_eq!(a.len(), 4);
        assert_eq!(a[1], "2".to_string());
        let (_, a) = parse_java_array("{ \"bo,o\", \"hoo\\\"\\u0000,boo\" } " ).unwrap();
        assert_eq!(a.len(), 2);

        let (_, a) = parse_java_array("{\n        \"\\u0000\\u00ee\\u0001\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0010\\u001a\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0010\\u0008\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0003\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0010 \\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0010\\u000b\\n\\u0002\\u0008\\u0003\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0000\\n\\u0002\\u0010\\t\\n\\u0002\\u0008\\u0004\\n\\u0002\\u0018\\u0002\\n\\u0000\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0003\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0004\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0003\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0002\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0004\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\u0013\\n\\u0002\\u0018\\u0002\\n\\u0000\\n\\u0002\\u0018\\u0002\\n\\u0000\\n\\u0002\\u0018\\u0002\\n\\u0000\\n\\u0002\\u0018\\u0002\\n\\u0002\\u0008\\n\\n\\u0002\\u0010\\u0002\\n\\u0002\\u0008\\u0004\\u0008\\u0016\\u0018\\u0000 y2\\u00020\\u00012\\u00020\\u00022\\u00020\\u0003:\\u0002xyB\\u0007\\u0008\\u0016\\u00a2\\u0006\\u0002\\u0010\\u0004B\\u000f\\u0008\\u0000\\u0012\\u0006\\u0010\\u0005\\u001a\\u00020\\u0006\\u00a2\\u0006\\u0002\\u0010\\u0007J\\r\\u0010\\u0008\\u001a\\u00020\\tH\\u0007\\u00a2\\u0006\\u0002\\u0008SJ\\u000f\\u0010\\u000b\\u001a\\u0004\\u0018\\u00010\\u000cH\\u0007\\u00a2\\u0006\\u0002\\u0008TJ\\r\\u0010\\u000e\\u001a\\u00020\\u000fH\\u0007\\u00a2\\u0006\\u0002\\u0008UJ\\r\\u0010\\u0014\\u001a\\u00020\\u0015H\\u0007\\u00a2\\u0006\\u0002\\u0008VJ\\r\\u0010\\u0017\\u001a\\u00020\\u000fH\\u0007\\u00a2\\u0006\\u0002\\u0008WJ\\r\\u0010\\u0018\\u001a\\u00020\\u0019H\\u0007\\u00a2\\u0006\\u0002\\u0008XJ\\u0013\\u0010\\u001b\\u001a\\u0008\\u0012\\u0004\\u0012\\u00020\\u001d0\\u001cH\\u0007\\u00a2\\u0006\\u0002\\u0008YJ\\r\\u0010\\u001f\\u001a\\u00020 H\\u0007\\u00a2\\u0006\\u0002\\u0008ZJ\\r\\u0010\\\"\\u001a\\u00020#H\\u0007\\u00a2\\u0006\\u0002\\u0008[J\\r\\u0010%\\u001a\\u00020&H\\u0007\\u00a2\\u0006\\u0002\\u0008\\\\J\\r\\u0010(\\u001a\\u00020)H\\u0007\\u00a2\\u0006\\u0002\\u0008]J\\r\\u0010+\\u001a\\u00020,H\\u0007\\u00a2\\u0006\\u0002\\u0008^J\\r\\u0010.\\u001a\\u00020,H\\u0007\\u00a2\\u0006\\u0002\\u0008_J\\r\\u0010/\\u001a\\u000200H\\u0007\\u00a2\\u0006\\u0002\\u0008`J\\u0013\\u00102\\u001a\\u0008\\u0012\\u0004\\u0012\\u0002030\\u001cH\\u0007\\u00a2\\u0006\\u0002\\u0008aJ\\u0013\\u00107\\u001a\\u0008\\u0012\\u0004\\u0012\\u0002030\\u001cH\\u0007\\u00a2\\u0006\\u0002\\u0008bJ\\u0008\\u0010c\\u001a\\u00020\\u0006H\\u0016J\\u0010\\u0010d\\u001a\\u00020e2\\u0006\\u0010f\\u001a\\u00020gH\\u0016J\\u0018\\u0010h\\u001a\\u00020i2\\u0006\\u0010f\\u001a\\u00020g2\\u0006\\u0010j\\u001a\\u00020kH\\u0016J\\r\\u00108\\u001a\\u00020\\u000fH\\u0007\\u00a2\\u0006\\u0002\\u0008lJ\\u0013\\u00109\\u001a\\u0008\\u0012\\u0004\\u0012\\u00020:0\\u001cH\\u0007\\u00a2\\u0006\\u0002\\u0008mJ\\u000f\\u0010;\\u001a\\u0004\\u0018\\u00010<H\\u0007\\u00a2\\u0006\\u0002\\u0008nJ\\r\\u0010>\\u001a\\u00020\\tH\\u0007\\u00a2\\u0006\\u0002\\u0008oJ\\r\\u0010?\\u001a\\u00020@H\\u0007\\u00a2\\u0006\\u0002\\u0008pJ\\r\\u0010B\\u001a\\u00020\\u000fH\\u0007\\u00a2\\u0006\\u0002\\u0008qJ\\r\\u0010C\\u001a\\u00020,H\\u0007\\u00a2\\u0006\\u0002\\u0008rJ\\r\\u0010H\\u001a\\u00020IH\\u0007\\u00a2\\u0006\\u0002\\u0008sJ\\r\\u0010K\\u001a\\u00020LH\\u0007\\u00a2\\u0006\\u0002\\u0008tJ\\u0008\\u0010u\\u001a\\u00020vH\\u0002J\\r\\u0010O\\u001a\\u00020\\u000fH\\u0007\\u00a2\\u0006\\u0002\\u0008wR\\u0013\\u0010\\u0008\\u001a\\u00020\\t8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008\\u0008\\u0010\\nR\\u0015\\u0010\\u000b\\u001a\\u0004\\u0018\\u00010\\u000c8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008\\u000b\\u0010\\rR\\u0013\\u0010\\u000e\\u001a\\u00020\\u000f8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008\\u000e\\u0010\\u0010R\\u0015\\u0010\\u0011\\u001a\\u0004\\u0018\\u00010\\u00128G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008\\u0011\\u0010\\u0013R\\u0013\\u0010\\u0014\\u001a\\u00020\\u00158G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008\\u0014\\u0010\\u0016R\\u0013\\u0010\\u0017\\u001a\\u00020\\u000f8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008\\u0017\\u0010\\u0010R\\u0013\\u0010\\u0018\\u001a\\u00020\\u00198G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008\\u0018\\u0010\\u001aR\\u0019\\u0010\\u001b\\u001a\\u0008\\u0012\\u0004\\u0012\\u00020\\u001d0\\u001c8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008\\u001b\\u0010\\u001eR\\u0013\\u0010\\u001f\\u001a\\u00020 8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008\\u001f\\u0010!R\\u0013\\u0010\\\"\\u001a\\u00020#8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008\\\"\\u0010$R\\u0013\\u0010%\\u001a\\u00020&8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008%\\u0010\\'R\\u0013\\u0010(\\u001a\\u00020)8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008(\\u0010*R\\u0013\\u0010+\\u001a\\u00020,8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008+\\u0010-R\\u0013\\u0010.\\u001a\\u00020,8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008.\\u0010-R\\u0013\\u0010/\\u001a\\u0002008G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008/\\u00101R\\u0019\\u00102\\u001a\\u0008\\u0012\\u0004\\u0012\\u0002030\\u001c8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u00082\\u0010\\u001eR\\u0013\\u00104\\u001a\\u0002058G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u00084\\u00106R\\u0019\\u00107\\u001a\\u0008\\u0012\\u0004\\u0012\\u0002030\\u001c8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u00087\\u0010\\u001eR\\u0013\\u00108\\u001a\\u00020\\u000f8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u00088\\u0010\\u0010R\\u0019\\u00109\\u001a\\u0008\\u0012\\u0004\\u0012\\u00020:0\\u001c8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u00089\\u0010\\u001eR\\u0015\\u0010;\\u001a\\u0004\\u0018\\u00010<8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008;\\u0010=R\\u0013\\u0010>\\u001a\\u00020\\t8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008>\\u0010\\nR\\u0013\\u0010?\\u001a\\u00020@8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008?\\u0010AR\\u0013\\u0010B\\u001a\\u00020\\u000f8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008B\\u0010\\u0010R\\u0013\\u0010C\\u001a\\u00020,8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008C\\u0010-R\\u0011\\u0010D\\u001a\\u00020E\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008F\\u0010GR\\u0013\\u0010H\\u001a\\u00020I8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008H\\u0010JR\\u0011\\u0010K\\u001a\\u00020L8G\\u00a2\\u0006\\u0006\\u001a\\u0004\\u0008K\\u0010MR\\u0010\\u0010N\\u001a\\u0004\\u0018\\u00010LX\\u0082\\u0004\\u00a2\\u0006\\u0002\\n\\u0000R\\u0013\\u0010O\\u001a\\u00020\\u000f8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008O\\u0010\\u0010R\\u0015\\u0010P\\u001a\\u0004\\u0018\\u00010Q8G\\u00a2\\u0006\\u0008\\n\\u0000\\u001a\\u0004\\u0008P\\u0010R\\u00a8\\u0006z\"\n    }").unwrap();
        assert_eq!(a.len(), 1);
        println!("|{}|", a[0]);

    }

    #[test]
    fn test_parse_annotation_element_array() {
        let (_, a) = parse_annotation_element(" key = {\n  \"a,\", \n \"b\\\"\", \"c\" \n }\n").unwrap();
        assert_eq!(a.name, "key");
        match a.value
        {
            AnnotationValue::Array(a) => { assert_eq!( a[0], "\"a,\"".to_string()); }
            _ => { println!("{:?}", a); }
        }

        let (_, a) = parse_annotation_element(" value = .enum Ljava/lang/annotation/RetentionPolicy;->SOURCE:Ljava/lang/annotation/RetentionPolicy;\n").unwrap();
        match a.value
        {
            AnnotationValue::Enum(obj, name) => {
                assert_eq!(name, "SOURCE");
                assert_eq!(obj.as_jni_type(), "Ljava/lang/annotation/RetentionPolicy;");
            }
            _ => { println!("{:?}", a); }
        }
    }

    #[test]
    fn test_parse_field() {
        let (_, f) = parse_field(".field private final callTimeoutMillis:I\n").unwrap();
        assert_eq!(f.name, "callTimeoutMillis".to_string());
        assert_eq!(f.modifiers.len(), 2);
        assert_eq!(f.signature.to_jni(), "I");
    }

    #[test]
    fn test_parse_class() {
        let smali = fs::read_to_string("tests/OkHttpClient.smali").unwrap();
        let (_, d) = parse_class(&smali).unwrap();
        assert_eq!(d.name.as_java_type(), "okhttp3.OkHttpClient");
    }
}
