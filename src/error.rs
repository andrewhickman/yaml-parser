use core::fmt;

use crate::{decode::DecodeErrorKind, Span, Token};

/// An error encountered while parsing a YAML document.
#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    span: Span,
}

#[derive(Debug)]
pub(crate) enum ErrorKind {
    Parse(ParseErrorKind),
    Decode(DecodeErrorKind),
}

#[derive(Debug)]
pub(crate) enum ParseErrorKind {
    ExpectedToken(Token),
    Other,
}

impl From<DecodeErrorKind> for ErrorKind {
    fn from(error: DecodeErrorKind) -> Self {
        ErrorKind::Decode(error)
    }
}
 
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}
