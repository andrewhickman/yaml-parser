//! A pure-rust, safe, YAML parser.
// #![cfg_attr(not(test), no_std)]
#![warn(missing_debug_implementations, missing_docs)]
#![deny(unsafe_code)]
#![doc(html_root_url = "https://docs.rs/yaml-parser/0.1.0/")]

extern crate alloc;

mod parser;

use core::ops::Range;

pub use self::parser::parse;

/// A handler for events and tokens in a YAML stream.
pub trait Receiver {
    /// Called on each event in the YAML stream.
    fn event(&mut self, event: Event, span: Span);

    /// Called for every token of the input stream. The stream may be reconstructed by joining the returned tokens.
    fn token(&mut self, token: Token, span: Span) {
        let _ = (token, span);
    }
}

/// A high level event in a YAML stream.
#[derive(Copy, Clone, Debug)]
pub enum Event {
    /// Emitted at the start of parsing a YAML stream.
    StreamStart,
    /// Emitted at the end of parsing a YAML stream.
    StreamEnd,
    /// Emitted at the start of each document within a YAML stream.
    DocumentStart,
    /// Emitted at the end of each document within a YAML stream.
    DocumentEnd,
    /// Emitted at the start of a mapping node.
    MappingStart,
    /// Emitted at the end of a mapping node.
    MappingEnd,
    /// Emitted at the start of a sequence node.
    SequenceStart,
    /// Emitted at the end of a sequence node.
    SequenceEnd,
    /// Emitted when encountering an alias node.
    Alias,
    /// Emitted when encountering a scalar node.
    Scalar,
}

/// A token type in a YAML stream.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Token {
    /// Identation whitespace at the start of a line,
    Indent,
    /// Whitespace separating tokens.
    Separator,
    /// A byte order mark.
    ByteOrderMark,
    /// A `-` token.
    SequenceEntry,
    /// A `?` token.
    MappingKey,
    /// A `:` token.
    MappingValue,
    /// A ',' token.
    CollectionEntry,
    /// A `[` token.
    SequenceStart,
    /// A `]` token.
    SequenceEnd,
    /// A `{` token.
    MappingStart,
    /// A `}` token.
    MappingEnd,
    /// A `#` token.
    Comment,
    /// The body of a comment.
    CommentText,
    /// A `&` token.
    Anchor,
    /// The name of an anchor on an anchor property of a node, or an alias node.
    AnchorName,
    /// A `*` token.
    Alias,
    /// A `|` token.
    Literal,
    /// A `>` token.
    Folded,
    /// A number indicating the indentation of the following block scalar.
    IndentationIndicator,
    /// A '-' or '+' token indicating the chomping behaviour of the following block scalar.
    ChompingIndicator,
    /// A `'` token.
    SingleQuote,
    /// A single quoted scalar.
    SingleQuoted,
    /// A `''` token in a single quoted string.
    QuotedQuote,
    /// A `"` token.
    DoubleQuote,
    /// A single line of double quoted scalar.
    DoubleQuoted,
    /// A '\' token.
    Escape,
    /// An escape code following a '\' token.
    EscapeCode,
    /// A `%` token.
    Directive,
    /// A line break (`\r`, `\n` or `\r\n`) outside of scalar content.
    Break,
    /// The name of a directive.
    DirectiveName,
    /// A parameter passed to a reserved directive.
    DirectiveParameter,
    /// The parameter for the `YAML` version directive.
    YamlVersion,
    /// The handle parameter for a `TAG` directive or shorthand tag property on a node.
    TagHandle,
    /// The prefix parameter for a `TAG` directive.
    TagPrefix,
    /// The suffix for a shorthand tag property on a node.
    TagSuffix,
    /// A verbatim tag property on a node.
    VerbatimTag,
    /// A non-specific tag property on a node.
    NonSpecificTag,
    /// An empty scalar node.
    Empty,
    /// A single line of a scalar node.
    Scalar,
    /// A line break in a scalar node.
    ScalarBreak,
    /// A line break in a folded scalar node, converted to a space.
    ScalarSpace,
    /// A `---` token marking the end of directives.
    DirectivesEnd,
    /// A `...` token marking the end of a document.
    DocumentEnd,
}

/// A range of characters within a source file.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Span {
    /// The start of the span (inclusive).
    pub start: Location,
    /// The end of the span (exclusive).
    pub end: Location,
}

/// The position of a character in a source file.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Location {
    /// The byte offset of the character.
    pub index: usize,
    /// The 0-based line number of the character.
    pub line: usize,
    /// The 0-based byte offset of the character within the line.
    pub column: usize,
}

impl Span {
    /// Creates a new span between two locations.
    pub fn new(start: Location, end: Location) -> Self {
        Span { start, end }
    }

    /// Creates an empty span at a location.
    pub fn empty(location: Location) -> Self {
        Span {
            start: location,
            end: location,
        }
    }

    /// Returns `true` if this span contains zero characters.
    pub fn is_empty(&self) -> bool {
        self.range().is_empty()
    }

    /// The number of bytes covered by the characters in this span.
    pub fn len(&self) -> usize {
        self.range().len()
    }

    /// Gets the range of bytes covered by this span.
    pub fn range(&self) -> Range<usize> {
        self.start.index..self.end.index
    }
}
