# Smali Crate

A pure rust implementation of a parser, writer and set of types for the smali file format and a dex file reader and writer(not yet implemented).

Smali is used in the Android reversing community to examine and modify Android APK files. It's a human readable text format that represents the complete structure of a Dex VM class including all of its instructions. 
The serialization and deserialization aim to be 100% compatible with Jesus Freke's Smali/Baksmali implementation.

The examples folder contains two examples :- 

1. rootbeer.rs which uses apktool to unpack an APK and then parses all of the smali into this crate's representation which is then used to search for rootbeer's classes and patches them to disable the root check. It then calls apktool to rebuild the apk file. 
2. dex2smali.rs which takes a dex file and writes each contained class into the out directory in a package directory heirarchy.

With this crate you can use it to disassemble, analyse and patch Android DEX files. Currently writing of dex files is not implemented so if you want to roundtrip then you'll need to use apktool or the Java smali library to rebuild the dex file.

Here's the simple example from rootbeer.rs illustrating patching with apktool :-

```rust
/* Expand the APK and patch RootBeer */
fn process_apk(apk_file: &str) -> Result<(), Box<dyn Error>>
{
    // Call apktool to unpack the APK
    execute_command("apktool", &["decode", "-f", apk_file, "-o", "out"])?;

    // Load all the smali classes
    let mut p = PathBuf::from_str("out")?;
    p.push("smali");
    let mut classes = find_smali_files(&p)?;
    println!("{:} smali classes loaded.", classes.len());

    // Search for the RootBeer class
    for c in classes.iter_mut()
    {
        if is_rootbeer_class(&c)
        {
            // Patch all the methods returning a boolean
            for m in c.methods.iter_mut()
            {
                if m.signature.result == TypeSignature::Bool && m.signature.args.len() == 0
                {
                    let mut new_instructions = vec![];
                    new_instructions.push(Op(DexOp::Const4 { dest: v(0), value: 0 }));  // "const/4 v0, 0x0" - Set v0 to false
                    new_instructions.push(Op(DexOp::Return { src: v(0) }));  //  "return v0" - return v0
                    m.ops = new_instructions;
                    m.locals = 1;
                    println!("{} method {} successfully patched.", c.name.as_java_type(), &m.name);
                }
            }
        }
        // Save all classes, even unmodified so we can thoroughly test parser and writer
        c.save()?;
    }
 
    // Repack the APK
    execute_command("apktool", &["build", "out", "-o", "out.apk"])?;

    Ok(())
}
```

Take a look at dex2smali.rs on how to use DexFile to deserialise into smali.
