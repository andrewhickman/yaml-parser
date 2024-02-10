use core::{fmt, ops::Range};

use crate::stream::{DecodeError, Stream};

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

    fn eat(&mut self, pred: impl Fn(char) -> bool) -> Result<(), ()> {
        if self.is(pred) {
            self.bump();
            Ok(())
        } else {
            Err(())
        }
    }

    fn eat_char(&mut self, ch: char) -> Result<(), ()> {
        if self.is_char(ch) {
            self.bump();
            Ok(())
        } else {
            Err(())
        }
    }

    fn eat_str(&mut self, s: &str) -> Result<(), ()> {
        if self.is_str(s) {
            for _ in s.chars() {
                self.bump();
            }
            Ok(())
        } else {
            Err(())
        }
    }

    fn next_is(&self, pred: impl Fn(char) -> bool) -> bool {
        matches!(self.peek_next(), Some(ch) if pred(ch))
    }

    fn prev_is(&self, pred: impl Fn(char) -> bool) -> bool {
        matches!(self.peek_prev(), Some(ch) if pred(ch))
    }

    fn is(&mut self, pred: impl Fn(char) -> bool) -> bool {
        matches!(self.peek(), Some(ch) if pred(ch))
    }

    fn is_char(&mut self, ch: char) -> bool {
        self.peek() == Some(ch)
    }

    fn is_str(&self, s: &str) -> bool {
        self.iter.as_str().starts_with(s)
    }

    fn is_start_of_line(&self) -> bool {
        self.line_offset == self.offset()
    }

    fn is_end_of_document(&self) -> bool {
        self.is_start_of_line()
            && (self.is_str("---") || self.is_str("..."))
            && matches!(
                self.iter.clone().nth(3),
                None | Some('\r' | '\n' | '\t' | ' ')
            )
    }

    fn is_end_of_input(&self) -> bool {
        self.iter.as_str().is_empty()
    }

    fn span(&self, start: Location) -> Span {
        Span {
            start,
            end: self.location(),
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.peek_nth(0)
    }

    fn peek_next(&self) -> Option<char> {
        self.peek_nth(1)
    }

    fn peek_nth(&self, n: usize) -> Result<Option<char>, DecodeError> {
        #[cfg(debug_assertions)]
        if self.peek_count > 1000 {
            panic!("detected infinite loop in parser");
        }

        if self.in_document && self.is_end_of_document() {
            return None;
        }

        self.stream.clone().nth(n)
    }

    fn peek_prev(&self) -> Option<char> {
        self.stream[..self.offset()].chars().last()
    }

    fn bump(&mut self) {
        #[cfg(debug_assertions)]
        {
            self.peek_count = 0;
        }

        let ch = self.stream.next().expect("called bump at end of input").expect("called bump after encoding error");
        let is_break = match ch {
            '\r' if !self.is_char('\n') => true,
            '\n' => true,
            _ => false,
        };

        if is_break {
            self.line_number += 1;
            self.line_index = self.stream.index();
        }
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
