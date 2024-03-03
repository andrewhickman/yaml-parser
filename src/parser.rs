use core::{fmt, iter::FusedIterator};

use alloc::collections::VecDeque;

use crate::{
    cursor::Cursor,
    diag::Severity,
    grammar::{self, State},
    stream::{DecodeError, Stream},
    Diagnostic, Event, Location, Span,
};

/// An iterator over events encountered while parsing a YAML document.
#[derive(Debug, Clone)]
pub struct Parser<'s, R = DefaultReceiver> {
    cursor: Cursor<'s>,
    receiver: R,
    state: Vec<State>,
    buffer: Buffer<'s>,
}

/// A handler for diagnostics and tokens in a YAML stream.
pub trait Receiver {
    /// Called when a warning message is emitted.
    fn diagnostic(&mut self, diag: Diagnostic) {
        let _ = diag;
    }

    /// Called for every token of the input stream. The stream may be reconstructed by joining the returned tokens.
    fn token(&mut self, token: Token, span: Span) {
        let _ = (token, span);
    }
}

/// A token type in a YAML stream.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize), serde(rename_all = "snake_case"))]
pub enum Token {
    /// A part of the YAML stream which could not be parsed due to an error.
    Error,
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
    /// A line comment, beginning with a '#' character.
    Comment,
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

#[derive(Default, Clone)]
pub(crate) struct Buffer<'s> {
    buffer: VecDeque<Buffered<'s>>,
}

#[derive(Debug, Clone)]
pub(crate) enum Buffered<'s> {
    Event { event: Event<'s>, span: Span },
    Diagnostic { diag: Diagnostic },
    Token { token: Token, span: Span },
}

impl<'s> Parser<'s> {
    /// Creates a YAML parser from an `&str`.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(stream: &'s str) -> Self {
        Parser::from_stream(Ok(Stream::from_str(stream)))
    }

    /// Creates a YAML parser from an `&[u8]`.
    ///
    /// The character encoding of the stream will be detected from the initial bytes.
    pub fn from_slice(stream: &'s [u8]) -> Self {
        Parser::from_stream(Stream::from_slice(stream))
    }

    fn from_stream(stream: Result<Stream<'s>, DecodeError>) -> Self {
        let mut state = Vec::with_capacity(16);

        let cursor = match stream {
            Ok(stream) => {
                state.push(State::Stream);
                Cursor::new(stream)
            }
            Err(err) => {
                state.push(State::Error(Diagnostic::decode(
                    err.encoding(),
                    Location {
                        index: err.index(),
                        line: 0,
                        column: err.index(),
                    },
                )));
                Cursor::default()
            }
        };

        Parser {
            cursor,
            receiver: DefaultReceiver,
            state,
            buffer: Buffer::default(),
        }
    }

    /// Overrides the [`Receiver`] implementation for handling token and diagnostic events.
    pub fn with_receiver<R>(self, receiver: R) -> Parser<'s, R> {
        Parser {
            cursor: self.cursor,
            receiver,
            state: self.state,
            buffer: self.buffer,
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
    type Item = Result<(Event<'s>, Span), Diagnostic>;

    fn next(&mut self) -> Option<Self::Item> {
        for buffered in self.buffer.by_ref() {
            match buffered {
                Buffered::Event { event, span } => return Some(Ok((event, span))),
                Buffered::Diagnostic { diag } => {
                    self.receiver.diagnostic(diag.clone());
                    if diag.severity() == Severity::Error {
                        return Some(Err(diag));
                    }
                }
                Buffered::Token { token, span } => self.receiver.token(token, span),
            }
        }
        grammar::event(
            &mut self.cursor,
            &mut self.receiver,
            &mut self.buffer,
            &mut self.state,
        )
    }
}

impl<'s, R> FusedIterator for Parser<'s, R> where R: Receiver {}

/// The default implementation of [`Receiver`] which does nothing.
#[derive(Debug, Copy, Clone)]
pub struct DefaultReceiver;

impl Receiver for DefaultReceiver {}

impl<'r, R> Receiver for &'r mut R
where
    R: Receiver,
{
    fn diagnostic(&mut self, diagnostic: Diagnostic) {
        (*self).diagnostic(diagnostic)
    }

    fn token(&mut self, token: Token, span: Span) {
        (*self).token(token, span)
    }
}

impl<'s> Buffer<'s> {
    pub(crate) fn event(&mut self, event: Event<'s>, span: Span) {
        self.buffer.push_back(Buffered::Event { event, span })
    }

    pub(crate) fn error(&mut self, error: Diagnostic) {
        self.buffer.push_back(Buffered::Diagnostic { diag: error })
    }

    pub(crate) fn peek_token(&mut self) -> Option<(Token, Span)> {
        match self.buffer.front() {
            Some(&Buffered::Token { token, span }) => Some((token, span)),
            _ => None,
        }
    }

    pub(crate) fn pop(&mut self) {
        self.buffer.pop_front();
    }

    pub(crate) fn len(&mut self) -> usize {
        self.buffer.len()
    }
}

impl<'s> Iterator for Buffer<'s> {
    type Item = Buffered<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        self.buffer.pop_front()
    }
}

impl<'s> Receiver for Buffer<'s> {
    fn diagnostic(&mut self, diag: Diagnostic) {
        self.buffer.push_back(Buffered::Diagnostic { diag });
    }

    fn token(&mut self, token: Token, span: Span) {
        self.buffer.push_back(Buffered::Token { token, span });
    }
}

impl<'s> fmt::Debug for Buffer<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.buffer.fmt(f)
    }
}
