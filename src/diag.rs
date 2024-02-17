use core::fmt;

use crate::{
    cursor::{Cursor, Span},
    stream::DecodeErrorKind,
    Location, Token,
};

/// An error or warning encountered while parsing a YAML document.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    span: Span,
    kind: DiagnosticKind,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum DiagnosticKind {
    Decode(DecodeErrorKind),
    ExpectedToken(Token),
    InvalidChar(char),
    ExpectedChar(char),
    DirectiveAfterUnterminatedDocument,
    DirectiveNotAtStartOfLine,
}

impl Diagnostic {
    /// Whether this diagnostic is an error or warning.
    pub fn severity(&self) -> Severity {
        match self.kind {
            DiagnosticKind::Decode(_)
            | DiagnosticKind::ExpectedToken(_)
            | DiagnosticKind::InvalidChar(_)
            | DiagnosticKind::ExpectedChar(_)
            | DiagnosticKind::DirectiveAfterUnterminatedDocument
            | DiagnosticKind::DirectiveNotAtStartOfLine => Severity::Error,
        }
    }

    /// The source code span where this error occurred.
    pub fn span(&self) -> Span {
        self.span
    }

    pub(crate) fn new(kind: DiagnosticKind, span: Span) -> Diagnostic {
        Diagnostic { kind, span }
    }

    pub(crate) fn decode(err: DecodeErrorKind, location: Location) -> Diagnostic {
        Diagnostic {
            kind: DiagnosticKind::Decode(err),
            span: Span::empty(location),
        }
    }

    pub(crate) fn expected_token(token: Token, cursor: &Cursor) -> Diagnostic {
        Diagnostic {
            kind: DiagnosticKind::ExpectedToken(token),
            span: Span::empty(cursor.location()),
        }
    }

    pub(crate) fn invalid_char(cursor: &Cursor) -> Diagnostic {
        todo!()
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl std::error::Error for Diagnostic {}
