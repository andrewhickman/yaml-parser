#![allow(clippy::box_collection)]

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

/// The severity of a [`Diagnostic`].
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Severity {
    /// An error caused by an invalid YAML stream.
    Error,
    /// A warning which doesn't prevent successful parsing of the YAML stream.
    Warning,
}

#[derive(Debug, Clone)]
pub(crate) enum DiagnosticKind {
    Decode(DecodeErrorKind),
    Expected(Expected, Option<char>),
    ExpectedChar(char),
    DirectiveAfterUnterminatedDocument,
    DirectiveNotAtStartOfLine,
    UnknownDirective(Box<String>),
    DuplicateYamlDirective,
    UnknownMinorVersion,
    UnknownMajorVersion,
    VersionOverflow,
    UnexpectedDiagnosticParameter,
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum Expected {
    Token(Token),
    Char(char),
    Printable,
    DecimalDigit,
}

impl Diagnostic {
    /// Whether this diagnostic is an error or warning.
    pub fn severity(&self) -> Severity {
        match self.kind {
            DiagnosticKind::UnknownDirective(_) | DiagnosticKind::UnknownMinorVersion => {
                Severity::Warning
            }
            _ => Severity::Error,
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
        Diagnostic::expected(Expected::Token(token), cursor)
    }

    pub(crate) fn expected(expected: Expected, cursor: &Cursor) -> Diagnostic {
        match cursor.peek() {
            Ok(found) => Diagnostic {
                kind: DiagnosticKind::Expected(expected, found),
                span: Span::empty(cursor.location()),
            },
            Err(err) => err,
        }
    }

    pub(crate) fn is_recoverable(&self) -> bool {
        match self.kind {
            DiagnosticKind::Decode(_) => false,
            _ => true,
        }
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

#[test]
fn size_of_diag() {
    assert_eq!(std::mem::size_of::<Diagnostic>(), 64);
}
