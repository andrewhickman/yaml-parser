use core::fmt;

use crate::{
    cursor::Span,
    stream::{DecodeError, DecodeErrorKind},
    Location, Token,
};

/// An error encountered while parsing a YAML document.
#[derive(Debug, Clone)]
pub struct Error {
    span: Span,
    kind: ErrorKind,
}

#[derive(Debug, Clone)]
pub(crate) enum ErrorKind {
    Decode(DecodeErrorKind),
    ExpectedToken(Token),
    ExpectedChar(char),
}

impl Error {
    /// The source code span where this error occurred.
    pub fn span(&self) -> Span {
        self.span
    }

    pub(crate) fn decode(err: DecodeErrorKind, location: Location) -> Error {
        Error {
            kind: ErrorKind::Decode(err),
            span: Span::empty(location),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl std::error::Error for Error {}
