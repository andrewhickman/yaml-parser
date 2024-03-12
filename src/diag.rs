#![allow(clippy::box_collection)]

use core::fmt;

use crate::{
    char,
    cursor::{Cursor, Span},
    stream::DecodeError,
    Encoding, Location, Token,
};

/// An error or warning encountered while parsing a YAML document.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    span: Span,
    kind: DiagnosticKind,
}

/// The severity of a [`Diagnostic`].
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize), serde(rename_all = "lowercase"))]
pub enum Severity {
    /// An error caused by an invalid YAML stream.
    Error,
    /// A warning which doesn't prevent successful parsing of the YAML stream.
    Warning,
}

#[derive(Debug, Clone)]
pub(crate) enum DiagnosticKind {
    Decode(Encoding),
    Expected(Expected, Option<char>),
    DirectiveAfterUnterminatedDocument,
    DirectiveNotAtStartOfLine,
    UnknownDirective(Box<String>),
    DuplicateYamlDirective,
    UnknownMinorVersion,
    UnknownMajorVersion,
    VersionOverflow(Box<String>),
    DuplicateTagDirective(Box<String>),
    UnexpectedDiagnosticParameter,
    InvalidPercentEscape,
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum Expected {
    Token(Token),
    Char(char),
    Printable,
    DecimalDigit,
    Word,
    TagInitial,
    TagChar,
    HexDigit,
}

struct PercentEncode(char);

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

    pub(crate) fn decode(encoding: Encoding, location: Location) -> Diagnostic {
        Diagnostic {
            kind: DiagnosticKind::Decode(encoding),
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
                span: cursor.next_span(),
            },
            Err(err) => err,
        }
    }

    pub(crate) fn is_recoverable(&self) -> bool {
        !matches!(self.kind, DiagnosticKind::Decode(_))
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            DiagnosticKind::Decode(encoding) => write!(
                f,
                "invalid {encoding:?} at position {}",
                self.span.start.index
            ),
            DiagnosticKind::Expected(Expected::TagInitial, Some(ch))
                if char::flow_indicator(*ch) =>
            {
                write!(
                    f,
                    "the '{ch}' character must be escaped as '{}' at the start of a tag prefix",
                    PercentEncode(*ch)
                )
            }
            DiagnosticKind::Expected(Expected::TagInitial | Expected::TagChar, Some(ch))
                if char::non_space(*ch) =>
            {
                write!(
                    f,
                    "the '{ch}' character must be escaped as '{}' in a tag",
                    PercentEncode(*ch)
                )
            }
            DiagnosticKind::Expected(Expected::TagInitial | Expected::TagChar, Some(ch))
                if char::printable(*ch) =>
            {
                write!(
                    f,
                    "the '{}' character must be escaped as '{}' in a tag",
                    ch.escape_debug(),
                    PercentEncode(*ch)
                )
            }
            DiagnosticKind::Expected(expected, None) => {
                write!(f, "expected {expected}, but reached end of input")
            }
            DiagnosticKind::Expected(expected, Some(found)) if !char::printable(*found) => write!(
                f,
                "expected {expected}, but found non-printable character '{}'",
                found.escape_debug()
            ),
            DiagnosticKind::Expected(expected, Some('\r' | '\n')) => {
                write!(f, "expected {expected}, but reached end of line")
            }
            DiagnosticKind::Expected(expected, Some(found)) => {
                write!(f, "expected {expected}, but found '{found}'")
            }
            DiagnosticKind::DirectiveAfterUnterminatedDocument => write!(f, "directives must be separated from the preceding document by an end of document marker '...'"),
            DiagnosticKind::DirectiveNotAtStartOfLine => write!(f, "directives must be at the start of the line"),
            DiagnosticKind::UnknownDirective(name) => write!(f, "unknown directive '{name}'"),
            DiagnosticKind::DuplicateYamlDirective => {
                write!(f, "a yaml version directive has already been specified in this document")
            }
            DiagnosticKind::UnknownMinorVersion => write!(f, "unknown minor version"),
            DiagnosticKind::UnknownMajorVersion => write!(f, "unknown major version"),
            DiagnosticKind::VersionOverflow(version) => {
                write!(f, "version number {version} is too large")
            }
            DiagnosticKind::DuplicateTagDirective(handle) => write!(
                f,
                "a tag directive for '{handle}' has already been specified in this document"
            ),
            DiagnosticKind::UnexpectedDiagnosticParameter => {
                write!(f, "unexpected directive parameter")
            }
            DiagnosticKind::InvalidPercentEscape => write!(f, "invalid percent escape"),
        }
    }
}

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expected::Token(Token::Separator) => write!(f, "whitespace"),
            Expected::Token(Token::Break) => write!(f, "a line break"),
            Expected::Token(Token::YamlVersion) => write!(f, "a YAML version number"),
            Expected::Token(Token::TagHandle) => write!(f, "a tag handle"),
            Expected::Token(Token::TagPrefix) => write!(f, "a tag prefix"),
            Expected::Token(tok) => unimplemented!("unexpected token {:?}", tok),
            Expected::Char(ch) => write!(f, "'{}'", ch),
            Expected::Printable => write!(f, "a printable character"),
            Expected::DecimalDigit => write!(f, "a decimal digit (0-9)"),
            Expected::HexDigit => write!(f, "a hex digit (0-9, a-z or A-Z)"),
            Expected::Word => write!(f, "an alphanumeric character (a-z, A-Z, 0-9, or '-')"),
            Expected::TagInitial | Expected::TagChar => write!(f, "a valid tag character"),
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl std::error::Error for Diagnostic {}

#[test]
fn size_of_diag() {
    assert_eq!(std::mem::size_of::<Diagnostic>(), 64);
}

#[cfg(test)]
impl serde::Serialize for Diagnostic {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;

        let mut s = serializer.serialize_struct("Diagnostic", 3)?;
        s.serialize_field("severity", &self.severity())?;
        s.serialize_field("message", &format_args!("{}", self))?;
        s.serialize_field("span", &self.span())?;
        s.end()
    }
}

impl fmt::Display for PercentEncode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buf = [0; 4];
        for byte in self.0.encode_utf8(&mut buf).as_bytes() {
            write!(f, "%{:02x}", byte)?;
        }
        Ok(())
    }
}
