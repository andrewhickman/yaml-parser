use crate::{cursor::Span, stream::DecodeError, Token};

/// An error encountered while parsing a YAML document.
#[derive(Debug, Clone)]
pub struct Error {
    span: Span,
    kind: ErrorKind,
}

#[derive(Debug, Clone)]
pub(crate) enum ErrorKind {
    Decode(DecodeError),
    ExpectedToken(Token),
    ExpectedChar(char),
}
