use crate::{parser::DecodeError, Span};

/// An error encountered while parsing a YAML document.
#[derive(Debug, Clone)]
pub struct Error {
    span: Span,
}

pub(crate) enum ErrorKind {
    Decode(DecodeError),
}
