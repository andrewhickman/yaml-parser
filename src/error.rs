use crate::{stream::DecodeError, Span, Token};

/// An error encountered while parsing a YAML document.
#[derive(Debug, Clone)]
pub struct Error {
    span: Span,
}

pub(crate) enum ErrorKind {
    Decode(DecodeError),
    ExpectedToken(Token),
    ExpectedChar(char),
}
