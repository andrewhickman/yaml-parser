use core::{fmt, ops::Range};

use crate::{
    stream::{DecodeError, Stream},
    Encoding, Error,
};

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

#[derive(Clone)]
pub(crate) struct Cursor<'s> {
    stream: Stream<'s>,
    line_number: usize,
    line_index: usize,
    in_document: bool,
    #[cfg(debug_assertions)]
    peek_count: usize,
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

impl<'s> Cursor<'s> {
    pub(crate) fn new(stream: Stream<'s>) -> Self {
        Cursor {
            stream,
            line_number: 0,
            line_index: 0,
            in_document: false,
            #[cfg(debug_assertions)]
            peek_count: 0,
        }
    }

    pub(crate) fn location(&self) -> Location {
        let index = self.stream.index();
        Location {
            index,
            line: self.line_number,
            column: index - self.line_index,
        }
    }

    pub(crate) fn encoding(&self) -> Result<Encoding, Error> {
        self.stream.encoding().map_err(|err| self.decode_error(err))
    }

    pub(crate) fn span(&self, start: Location) -> Span {
        Span {
            start,
            end: self.location(),
        }
    }

    pub(crate) fn empty_span(&self) -> Span {
        Span::empty(self.location())
    }

    pub(crate) fn enter_document(&mut self) {
        self.in_document = true;
    }

    pub(crate) fn exit_document(&mut self) {
        self.in_document = true;
    }

    pub(crate) fn eat(&mut self, pred: impl Fn(char) -> bool) -> Result<bool, DecodeError> {
        if self.is(pred)? {
            self.bump();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(crate) fn eat_char(&mut self, ch: char) -> Result<bool, DecodeError> {
        if self.is_char(ch)? {
            self.bump();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(crate) fn eat_str(&mut self, s: &str) -> Result<bool, DecodeError> {
        if self.is_str(s)? {
            for _ in s.chars() {
                self.bump();
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(crate) fn next_is(&self, pred: impl Fn(char) -> bool) -> Result<bool, DecodeError> {
        Ok(matches!(self.peek_next()?, Some(ch) if pred(ch)))
    }

    pub(crate) fn is(&mut self, pred: impl Fn(char) -> bool) -> Result<bool, DecodeError> {
        Ok(matches!(self.peek()?, Some(ch) if pred(ch)))
    }

    pub(crate) fn is_char(&mut self, ch: char) -> Result<bool, DecodeError> {
        Ok(self.peek()? == Some(ch))
    }

    pub(crate) fn is_str(&self, s: &str) -> Result<bool, DecodeError> {
        let mut iter = self.stream.clone();
        for ch in s.chars() {
            if iter.next()? != Some(ch) {
                return Ok(false);
            }
        }
        Ok(true)
    }

    pub(crate) fn is_start_of_line(&self) -> bool {
        self.line_index == self.stream.index()
    }

    pub(crate) fn is_end_of_document(&self) -> Result<bool, DecodeError> {
        Ok(self.is_start_of_line()
            && (self.is_str("---")? || self.is_str("...")?)
            && matches!(
                self.stream.clone().nth(3)?,
                None | Some('\r' | '\n' | '\t' | ' ')
            ))
    }

    pub(crate) fn is_end_of_input(&self) -> Result<bool, DecodeError> {
        Ok(self.peek()?.is_none())
    }

    pub(crate) fn peek(&self) -> Result<Option<char>, DecodeError> {
        self.peek_nth(0)
    }

    pub(crate) fn peek_next(&self) -> Result<Option<char>, DecodeError> {
        self.peek_nth(1)
    }

    pub(crate) fn peek_nth(&self, n: usize) -> Result<Option<char>, DecodeError> {
        #[cfg(debug_assertions)]
        if self.peek_count > 1000 {
            panic!("infinite loop in parser");
        }

        if self.in_document && self.is_end_of_document()? {
            return Ok(None);
        }

        self.stream.clone().nth(n)
    }

    pub(crate) fn bump(&mut self) {
        #[cfg(debug_assertions)]
        {
            self.peek_count = 0;
        }

        let ch = self
            .stream
            .next()
            .expect("called bump at end of input")
            .expect("called bump after encoding error");
        let is_break = match ch {
            '\r' if !matches!(self.is_char('\n'), Ok(true)) => true,
            '\n' => true,
            _ => false,
        };

        if is_break {
            self.line_number += 1;
            self.line_index = self.stream.index();
        }
    }

    fn decode_error(&self, err: DecodeError) -> Error {
        Error::decode(
            err.kind(),
            Location {
                index: err.index(),
                line: self.line_number,
                column: err.index() - self.line_index,
            },
        )
    }
}

impl<'s> fmt::Debug for Cursor<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Cursor")
            .field("location", &self.location())
            .field("stream", &self.stream)
            .finish()
    }
}
