use std::env;
use std::error::Error;
use std::path::PathBuf;
use std::process::Command;
use std::str::FromStr;
use smali::{ find_smali_files };
use smali::types::*;
use smali::types::SmaliOp::Op;
use smali::smali_ops::{ DexOp, v };

// This demo unpacks an APK file with apktool (you need this on your path), searches for rootBeer and disables it.
// Try it on the RootBeer Sample app https://play.google.com/store/apps/details?id=com.scottyab.rootbeer.sample

//Usage: main <apk-file>
fn main()
{
    let args: Vec<String> = env::args().collect();

    // Do everything else with the error trap
    match process_apk(&args[1]) {
        Ok(_) => { println!("All done: written out.apk"); }
        Err(e) => { println!("Aborted due to error: {:?}", e); }
    }
}

// Matches the known field / method signatures for the RootBeer.class (remember this could be renamed)
fn is_rootbeer_class(c: &SmaliClass) -> bool
{
    if c.fields.len() == 2 && c.methods.len() == 25
    {
        // Probably unreliable if the ordering changes
        if c.fields[0].signature == TypeSignature::Bool && c.methods[1].signature.result == TypeSignature::Bool
            && c.methods[4].signature.args.len() == 0 && c.methods[4].signature.result == TypeSignature::Bool
        {
            println!("Detected RootBeer class: {}", c.name.as_java_type());
            return true;
        }
    }
    false
}

/* This is where all the processing takes place, to make error handling easier */
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

/* Wrapper around command for nicer error handling */
fn execute_command(cmd: &str, args: &[&str]) -> Result<String, Box<dyn Error>>
{
    match Command::new(cmd).args(args).output() {
        Ok(output) => {
            if output.status.success()
            {
                //info!("Successfully executed {}", cmd);
                Ok(String::from_utf8(output.stdout)?)
            }
            else
            {
                println!("Error executing command {} {:?}", cmd, args);
                println!("Return code: {:?}", output.status);
                println!("stderr: {:?}", String::from_utf8(output.stderr)?);
                Err(Box::new(SmaliError::new("Bad return code")))
            }
        }
        Err(e) => {
            println!("Error executing command {} {:?}", cmd, args);
            Err(Box::new(e))
        }
    }
}