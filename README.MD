# Smali Crate

A pure rust implementation of a parser, writer and set of types for the smali file format.

Smali is used in the Android reversing community to examine and modify Android APK files. It's a human readable text format that represents the complete structure of a Dex VM class including all of its instructions. 

With this crate you can use it in conjunction with [apktool](https://ibotpeaches.github.io/Apktool/) to perform analysis and/or patches to Android applications. 

There is a simple example in examples/main.rs that illustrates this. The example, will invoke apktool to expand any application and then parse all the smali files looking for [RootBeer](https://github.com/scottyab/rootbeer) (an open source root detection framework), it will then patch RootBeer's methods to always return false so that the app can be run on a rooted device.
Finally, it calls apktool again to repackage the app.

Here's the simple example :-

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
                    new_instructions.push(Instruction(DexInstruction::Const4 { dest: v(0), value: 0 }));  // "const/4 v0, 0x0" - Set v0 to false
                    new_instructions.push(Instruction(DexInstruction::Return { src: v(0) }));  //  "return v0" - return v0
                    m.instructions = new_instructions;
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
