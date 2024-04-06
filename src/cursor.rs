use core::{fmt, ops::Range, sync::atomic::Ordering};

use crate::{
    char,
    stream::{DecodeError, Stream},
    Diagnostic, Encoding,
};

/// A range of characters within a source file.
#[derive(Copy, Clone, Default, Debug, Hash, Eq, PartialEq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Span {
    /// The start of the span (inclusive).
    pub start: Location,
    /// The end of the span (exclusive).
    pub end: Location,
}

/// The position of a character in a source file.
#[derive(Copy, Clone, Default, Debug, Hash, Eq, PartialEq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Location {
    /// The byte offset of the character.
    pub index: usize,
    /// The 0-based line number of the character.
    pub line: usize,
    /// The 0-based byte offset of the character within the line.
    pub column: usize,
}

pub(crate) struct Cursor<'s> {
    stream: Stream<'s>,
    line_number: usize,
    line_index: usize,
    indent: Option<u32>,
    separated: bool,
    in_document: bool,
    token: Location,
    #[cfg(all(feature = "std", debug_assertions))]
    peek_count: std::sync::atomic::AtomicU32,
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
            indent: Some(0),
            separated: true,
            in_document: false,
            token: Location::default(),
            #[cfg(all(feature = "std", debug_assertions))]
            peek_count: std::sync::atomic::AtomicU32::new(0),
        }
    }

    #[cfg(test)]
    pub(crate) fn with_stream(mut self, stream: Stream<'_>) -> Cursor<'_> {
        Cursor {
            stream,
            line_number: 0,
            line_index: 0,
            indent: self.indent,
            separated: self.separated,
            in_document: self.in_document,
            token: Location::default(),
            #[cfg(all(feature = "std", debug_assertions))]
            peek_count: std::sync::atomic::AtomicU32::new(
                self.peek_count.load(std::sync::atomic::Ordering::Relaxed),
            ),
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

    pub(crate) fn encoding(&self) -> Encoding {
        self.stream.encoding()
    }

    pub(crate) fn as_str(&self) -> Option<&'s str> {
        self.stream.as_str()
    }

    pub(crate) fn index(&self) -> usize {
        self.stream.index()
    }

    pub(crate) fn span(&self, start: Location) -> Span {
        Span::new(start, self.location())
    }

    pub(crate) fn token(&mut self) -> Span {
        let start = self.token;
        self.token = self.location();
        Span::new(start, self.token)
    }

    pub(crate) fn next_span(&self) -> Span {
        let start = self.location();
        let mut cursor = self.clone();
        let _ = cursor.eat(|_| true);
        cursor.span(start)
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

    pub(crate) fn eat(&mut self, pred: impl Fn(char) -> bool) -> Result<bool, Diagnostic> {
        if self.is(pred)? {
            self.bump();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(crate) fn eat_char(&mut self, ch: char) -> Result<bool, Diagnostic> {
        if self.is_char(ch)? {
            self.bump();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(crate) fn eat_str(&mut self, s: &str) -> Result<bool, Diagnostic> {
        if self.is_str(s)? {
            for _ in s.chars() {
                self.bump();
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(crate) fn eat_while(
        &mut self,
        pred: impl Fn(char) -> bool + Clone,
    ) -> Result<(), Diagnostic> {
        while self.is(pred.clone())? {
            self.bump();
        }
        Ok(())
    }

    pub(crate) fn next_is(&self, pred: impl Fn(char) -> bool) -> Result<bool, Diagnostic> {
        Ok(matches!(self.peek_next()?, Some(ch) if pred(ch)))
    }

    pub(crate) fn is(&self, pred: impl Fn(char) -> bool) -> Result<bool, Diagnostic> {
        Ok(matches!(self.peek()?, Some(ch) if pred(ch)))
    }

    pub(crate) fn is_char(&self, ch: char) -> Result<bool, Diagnostic> {
        Ok(self.peek()? == Some(ch))
    }

    pub(crate) fn is_str(&self, s: &str) -> Result<bool, Diagnostic> {
        let mut iter = self.stream.clone();
        for expected in s.chars() {
            match iter.next() {
                Ok(Some(ch)) if ch == expected => continue,
                Ok(_) => return Ok(false),
                Err(err) => return Err(self.decode_error(err)),
            }
        }
        Ok(true)
    }

    pub(crate) fn is_start_of_line(&self) -> bool {
        self.indent == Some(0)
    }

    pub(crate) fn is_separated(&self) -> bool {
        self.separated
    }

    pub(crate) fn is_token_boundary(&self) -> bool {
        self.token == self.location()
    }

    pub(crate) fn indent(&self) -> Option<u32> {
        self.indent
    }

    pub(crate) fn is_end_of_document(&self) -> Result<bool, Diagnostic> {
        Ok(self.is_start_of_line()
            && (self.is_str("---")? || self.is_str("...")?)
            && matches!(
                self.stream
                    .clone()
                    .nth(3)
                    .map_err(|err| self.decode_error(err))?,
                None | Some('\r' | '\n' | '\t' | ' ')
            ))
    }

    pub(crate) fn is_end_of_input(&self) -> Result<bool, Diagnostic> {
        Ok(self.peek()?.is_none())
    }

    pub(crate) fn peek(&self) -> Result<Option<char>, Diagnostic> {
        self.peek_nth(0)
    }

    pub(crate) fn peek_next(&self) -> Result<Option<char>, Diagnostic> {
        self.peek_nth(1)
    }

    pub(crate) fn peek_nth(&self, n: usize) -> Result<Option<char>, Diagnostic> {
        #[cfg(debug_assertions)]
        if self.peek_count.fetch_add(1, Ordering::Relaxed) > 1000 {
            panic!("infinite loop in parser");
        }

        if self.in_document && self.is_end_of_document()? {
            return Ok(None);
        }

        self.stream
            .clone()
            .nth(n)
            .map_err(|err| self.decode_error(err))
    }

    pub(crate) fn bump(&mut self) -> char {
        #[cfg(debug_assertions)]
        {
            *self.peek_count.get_mut() = 0;
        }

        let ch = self
            .stream
            .next()
            .expect("called bump at end of input")
            .expect("called bump after encoding error");

        let is_break = ch == '\n' || (ch == '\r' && !matches!(self.is_char('\n'), Ok(true)));
        if is_break {
            self.line_number += 1;
            self.line_index = self.stream.index();
            self.indent = Some(0);
            self.separated = true;
        } else if let Some(indent) = &mut self.indent {
            if ch == ' ' {
                *indent += 1;
            } else if !(ch == char::BYTE_ORDER_MARK && self.is_start_of_line()) {
                self.indent = None;
            }
        }

        self.separated =
            is_break || char::space(ch) || (self.separated && ch == char::BYTE_ORDER_MARK);

        ch
    }

    fn decode_error(&self, err: DecodeError) -> Diagnostic {
        Diagnostic::decode(
            err.encoding(),
            Location {
                index: err.index(),
                line: self.line_number,
                column: err.index() - self.line_index,
            },
        )
    }
}

impl<'s> Clone for Cursor<'s> {
    fn clone(&self) -> Self {
        Cursor {
            stream: self.stream.clone(),
            line_number: self.line_number,
            line_index: self.line_index,
            indent: self.indent,
            separated: self.separated,
            in_document: self.in_document,
            token: self.token,
            #[cfg(all(feature = "std", debug_assertions))]
            peek_count: std::sync::atomic::AtomicU32::new(
                self.peek_count.load(std::sync::atomic::Ordering::Relaxed),
            ),
        }
    }
}

impl<'s> Default for Cursor<'s> {
    fn default() -> Self {
        Cursor::new(Stream::from_str(""))
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
