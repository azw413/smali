use std::fmt;

macro_rules! err {
    ($base:ident, $msg:literal) => {
        DexError::with_context($base, $msg)
    };
    ($base:ident, $fmtstr:literal, $($args:tt)*) => {
        DexError::with_context($base, format!($fmtstr, $($args)*))
    };
    (($msg:literal), ($contextfmt:literal, $($contextargs:tt)*)) => {
        DexError::with_context(DexError::new($msg), format!($contextfmt, $($contextargs)*))
    };
    (($fmtstr:literal, $($args:tt)*), ($contextfmt:literal, $($contextargs:tt)*)) => {
        DexError::with_context(DexError::new(format!($fmtstr, $($args)*)), format!($contextfmt, $($contextargs)*))
    };
    ($msg:literal) => {
        DexError::new($msg)
    };
    ($fmtstr:literal, $($args:tt)*) => {
        DexError::new(format!($fmtstr, $($args)*))
    };
}


#[macro_export]
macro_rules! fail {
    ($msg:literal) => {
        return Err(DexError::new($msg))
    };
    (($msg:literal), ($context:literal)) => {
        return Err(DexError::with_context(DexError::new($msg), $context.to_string()))
    };
    ($fmtstr:literal, $($args:tt)*) => {
        return Err(DexError::new(&format!($fmtstr, $($args)*)))
    };
    (($fmtstr:literal, $($args:tt)*), ($context:literal)) => {
        return Err(DexError::with_context(DexError::new(&format!($fmtstr, $($args)*)), $context.to_string()))
    };
    (($fmtstr:literal, $($args:tt)*), ($contextfmt:literal, $($contextargs:tt)*)) => {
        return Err(DexError::with_context(DexError::new(&format!($fmtstr, $($args)*)), format!($contextfmt, $($contextargs)*)))
    };
}


#[derive(Debug, PartialEq, Eq)]
pub struct DexError
{
    msg: String,
    contexts: Vec<String>,
}

impl DexError
{
    pub(crate) fn new(msg: &str) -> Self
    {
        DexError {
            msg: msg.to_string(),
            contexts: Vec::new(),
        }
    }

    pub(crate) fn with_context(base: DexError, context: String) -> Self
    {
        let mut contexts = base.contexts;
        contexts.push(context);
        DexError { msg: base.msg, contexts }
    }
}

impl fmt::Display for DexError
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "{}", self.msg)?;
        let mut connector = " for ";
        for context in &self.contexts
        {
            write!(f, "{}{}", connector, context)?;
            connector = " of ";
        }
        Ok(())
    }
}

impl std::error::Error for DexError {}
