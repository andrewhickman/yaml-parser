use core::fmt;

use crate::{
    cursor::Cursor,
    grammar::{self, State},
    Error, Event, Location, Span,
};

/// An iterator over events encountered while parsing a YAML document.
#[derive(Debug, Clone)]
pub struct Parser<'s, R = DefaultReceiver> {
    cursor: Cursor<'s>,
    receiver: R,
    state: Vec<State>,
}

/// A handler for diagnostics and tokens in a YAML stream.
pub trait Receiver {
    /// Called when a warning message is emitted.
    fn warning(&mut self, message: &dyn fmt::Display, span: Span) {
        let _ = (message, span);
    }

    /// Called for every token of the input stream. The stream may be reconstructed by joining the returned tokens.
    fn token(&mut self, token: Token, span: Span) {
        let _ = (token, span);
    }
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

impl<'s> Parser<'s> {
    /// Creates a YAML parser from an `&str`.
    pub fn from_str(stream: &'s str) -> Self {
        todo!()
    }

    /// Creates a YAML parser from an `&[u8]`.
    ///
    /// The character encoding of the stream will be detected from the initial bytes.
    pub fn from_slice(stream: &'s [u8]) -> Self {
        todo!()
    }

    /// Overrides the [`Receiver`] implementation for handling token and diagnostic events.
    pub fn with_receiver<R>(self, receiver: R) -> Parser<'s, R> {
        Parser {
            cursor: self.cursor,
            receiver,
            state: self.state,
        }
    }
}

impl<'s, R> Parser<'s, R> {
    /// Gets the current position of the parser in the YAML stream.
    pub fn location(&self) -> Location {
        self.cursor.location()
    }

    /// Gets a reference to the [`Receiver`] for this parser.
    pub fn receiver(&self) -> &R {
        todo!()
    }

    /// Gets a mutable reference to the [`Receiver`] for this parser.
    pub fn receiver_mut(&mut self) -> &mut R {
        todo!()
    }
}

impl<'s, R> Iterator for Parser<'s, R>
where
    R: Receiver,
{
    type Item = Result<(Event<'s>, Span), Error>;

    fn next(&mut self) -> Option<Self::Item> {
        grammar::event(&mut self.state, &mut self.cursor).transpose()
    }
}

/// The default implementation of [`Receiver`] which does nothing.
#[derive(Debug, Copy, Clone)]
pub struct DefaultReceiver;

impl Receiver for DefaultReceiver {}

impl<'r, R> Receiver for &'r mut R
where
    R: Receiver,
{
    fn warning(&mut self, message: &dyn fmt::Display, span: Span) {
        (*self).warning(message, span)
    }

    fn token(&mut self, token: Token, span: Span) {
        (*self).token(token, span)
    }
}
