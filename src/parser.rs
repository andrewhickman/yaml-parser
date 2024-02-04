mod char;
// mod cursor;
mod grammar;

use alloc::{borrow::Cow, collections::BTreeMap, string::String, vec::Vec};
use core::{mem, ops::Range, str::Chars, unreachable};

use crate::{CollectionStyle, Diagnostic, Event, Location, Receiver, Span, Token};

/// Parse a YAML stream, emitting events into the given receiver.
pub fn parse<'t, R: Receiver>(receiver: &mut R, text: &'t str) -> Result<(), Vec<Diagnostic>>
where
    R: 't,
{
    let mut parser = Parser::new(receiver, text);
    while !parser.state.is_empty() {
        match grammar::event(&mut parser) {
            Ok((event, span)) => parser.receiver.event(event, span),
            Err(()) => return Err(parser.diagnostics),
        }
    }
    Ok(())
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
enum State {
    // Expecting a StreamStart event.
    Stream,
    // Expecting a document.

    // Expecting a DocumentStart or StreamEnd event
    DocumentStart {
        prev_terminated: bool,
    },
    // Expecting a DocumentEnd event
    DocumentEnd,

    /// Expecting a SequenceStart, MappingStart, Alias or Scalar event
    Node {
        empty: bool,
        indent: i32,
        context: Context,
    },

    /// Expecting a SequenceStart, MappingStart, Alias, Scalar or SequenceEnd event
    SequenceNode {
        style: CollectionStyle,
        indent: i32,
        context: Context,
    },

    /// Expecting a SequenceStart, MappingStart, Alias, Scalar or MappingEnd event
    MappingKey {
        style: CollectionStyle,
        indent: i32,
        context: Context,
    },
    /// Expecting a SequenceStart, MappingStart, Alias or Scalar event
    MappingValue,
}

struct Parser<'t, R> {
    text: &'t str,
    iter: Chars<'t>,

    events: Vec<(EventOrToken<'t>, Span)>,
    diagnostics: Vec<Diagnostic>,
    yaml_version: Option<&'t str>,
    tags: BTreeMap<Cow<'t, str>, Cow<'t, str>>,
    value: CowBuilder,
    state: Vec<State>,

    receiver: &'t mut R,
    alt_depth: u32,
    line_number: usize,
    line_offset: usize,

    in_token: bool,
    in_document: bool,
    length_limit: Option<usize>,

    #[cfg(debug_assertions)]
    peek_count: u32,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum Context {
    BlockIn,
    BlockOut,
    BlockKey,
    FlowIn,
    FlowOut,
    FlowKey,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum Chomping {
    Strip,
    Clip,
    Keep,
}

#[derive(Clone, Debug)]
enum EventOrToken<'t> {
    Event(Event<'t>),
    Token(Token),
}

type Properties<'t> = (Option<Cow<'t, str>>, Option<Cow<'t, str>>);

#[derive(Clone, Debug, PartialEq, Eq)]
enum CowBuilder {
    Borrowed { range: Range<usize> },
    Owned { value: String },
}

impl<'t, R> Parser<'t, R>
where
    R: Receiver,
{
    fn new(receiver: &'t mut R, text: &'t str) -> Self {
        let mut state = Vec::with_capacity(16);
        state.push(State::Stream);

        Parser {
            text,
            iter: text.chars(),
            events: Vec::new(),
            diagnostics: Vec::new(),
            value: CowBuilder::new(),
            yaml_version: None,
            tags: BTreeMap::new(),
            receiver,
            in_token: false,
            in_document: false,
            length_limit: None,
            alt_depth: 0,
            line_number: 0,
            line_offset: 0,
            peek_count: 0,
            state,
        }
    }

    fn state(&self) -> State {
        *self.state.last().unwrap()
    }

    fn replace_state(&mut self, state: State) {
        self.state.pop().unwrap();
        self.state.push(state);
    }

    fn push_state(&mut self, state: State) {
        self.state.push(state);
    }

    fn pop_state(&mut self) {
        self.state.pop().unwrap();
    }

    fn detect_scalar_indent(&self, n: i32) -> Result<i32, ()> {
        let mut iter = self.iter.clone();
        let mut max_len = 0i32;
        let mut len = 0i32;

        let m = loop {
            match iter.next() {
                Some(' ') => len += 1,
                Some('\r' | '\n') => {
                    max_len = max_len.max(len);
                    len = 0;
                }
                Some(_) if len < max_len => return Err(()),
                Some(_) => break len,
                None => break max_len.max(len),
            }
        };

        Ok((m - n).max(1))
    }

    fn detect_collection_indent(&self, n: i32) -> i32 {
        let mut iter = self.iter.clone();
        let mut len = 0;
        let mut is_comment = false;

        loop {
            match iter.next() {
                Some(' ') => len += 1,
                Some('#') => is_comment = true,
                Some('\r' | '\n') => {
                    len = 0;
                    is_comment = false;
                }
                Some(_) if is_comment => continue,
                _ => break if len >= n { len - n } else { 0 },
            }
        }
    }

    fn detect_compact_indent(&self) -> i32 {
        debug_assert!(matches!(self.peek_prev(), Some('-' | '?' | ':')));

        let mut iter = self.iter.clone();
        let mut len = 0;

        loop {
            match iter.next() {
                Some(' ') => len += 1,
                _ => break len,
            }
        }
    }

    fn with_rollback<T>(&mut self, mut f: impl FnMut(&mut Self) -> Result<T, ()>) -> Result<T, ()> {
        let events_len = self.events.len();
        let diagnostics_len = self.diagnostics.len();
        let value = self.value.clone();
        let offset = self.offset();
        let line_number = self.line_number;
        let line_offset = self.line_offset;
        let length_limit = self.length_limit;

        self.alt_depth += 1;
        let res = f(self);
        self.alt_depth -= 1;

        match res {
            Ok(r) => {
                if self.alt_depth == 0 {
                    for (event, span) in self.events.drain(events_len..) {
                        match event {
                            EventOrToken::Event(event) => self.receiver.event(event, span),
                            EventOrToken::Token(token) => self.receiver.token(token, span),
                        }
                    }
                }

                Ok(r)
            }
            Err(()) => {
                self.iter = self.text[offset..].chars();
                self.events.truncate(events_len);
                self.diagnostics.truncate(diagnostics_len);
                self.value = value;
                self.line_number = line_number;
                self.line_offset = line_offset;
                self.length_limit = length_limit;
                Err(())
            }
        }
    }

    fn lookahead<T>(&mut self, mut f: impl FnMut(&mut Self) -> Result<T, ()>) -> bool {
        let mut result: Option<bool> = None;
        self.with_rollback::<()>(|parser| {
            result = Some(f(parser).is_ok());
            Err(())
        })
        .unwrap_err();
        result.unwrap()
    }

    fn with_length_limit(
        &mut self,
        max_len: usize,
        mut f: impl FnMut(&mut Self) -> Result<(), ()>,
    ) -> Result<(), ()> {
        let prev_length_limit = self.length_limit;
        self.length_limit = Some(match prev_length_limit {
            None => max_len,
            Some(prev) => prev.min(max_len),
        });
        let res = f(self);
        self.length_limit = match prev_length_limit {
            None => None,
            Some(prev) => Some(prev - (max_len - self.length_limit.unwrap())),
        };
        res
    }

    fn token<T>(
        &mut self,
        token: Token,
        mut f: impl FnMut(&mut Self) -> Result<T, ()>,
    ) -> Result<T, ()> {
        let start = self.location();

        debug_assert!(!self.in_token, "nested tokens");
        self.in_token = true;
        let res = f(self);
        self.in_token = false;

        match res {
            Ok(value) => {
                self.queue(EventOrToken::Token(token), self.span(start));
                Ok(value)
            }
            Err(()) => Err(()),
        }
    }

    fn queue(&mut self, event: EventOrToken<'t>, span: Span) {
        if self.alt_depth > 0 {
            self.events.push((event, span));
        } else {
            match event {
                EventOrToken::Event(event) => self.receiver.event(event, span),
                EventOrToken::Token(token) => self.receiver.token(token, span),
            }
        }
    }

    fn begin_value(&mut self) {
        debug_assert!(self.value.is_empty());
        self.value = CowBuilder::new();
    }

    fn end_value(&mut self) -> Cow<'t, str> {
        mem::take(&mut self.value).finish(self.text)
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
        if let Some(length_limit) = self.length_limit {
            s.chars().count() < length_limit && self.iter.as_str().starts_with(s)
        } else {
            self.iter.as_str().starts_with(s)
        }
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

    fn offset(&self) -> usize {
        self.text.len() - self.iter.as_str().len()
    }

    fn location(&self) -> Location {
        let index = self.offset();
        Location {
            index,
            line: self.line_number,
            column: index - self.line_offset,
        }
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

    fn peek_nth(&self, n: usize) -> Option<char> {
        #[cfg(debug_assertions)]
        if self.peek_count > 1000 {
            panic!("detected infinite loop in parser");
        }

        if matches!(self.length_limit, Some(limit) if n > limit) {
            return None;
        }

        if self.in_document && self.is_end_of_document() {
            return None;
        }

        self.iter.clone().nth(n)
    }

    fn peek_prev(&self) -> Option<char> {
        self.text[..self.offset()].chars().last()
    }

    fn bump(&mut self) {
        #[cfg(debug_assertions)]
        {
            self.peek_count = 0;
        }
        debug_assert!(
            self.in_token,
            "character {:?} not covered by token",
            self.peek()
        );

        if let Some(limit) = &mut self.length_limit {
            debug_assert!(*limit != 0);
            *limit -= 1;
        }

        let ch = self.iter.next().expect("called bump at end of input");
        let is_break = match ch {
            '\r' if !self.is_char('\n') => true,
            '\n' => true,
            _ => false,
        };

        if is_break {
            self.line_number += 1;
            self.line_offset = self.offset();
        }
    }
}

impl Context {
    fn in_flow(&self) -> Context {
        match self {
            Context::BlockKey | Context::FlowKey => Context::FlowKey,
            Context::FlowIn | Context::FlowOut => Context::FlowIn,
            Context::BlockIn | Context::BlockOut => unimplemented!(),
        }
    }
}

impl CowBuilder {
    fn new() -> Self {
        CowBuilder::Borrowed { range: 0..0 }
    }

    fn is_empty(&self) -> bool {
        match self {
            CowBuilder::Borrowed { range } => range.start == range.end,
            CowBuilder::Owned { value } => value.is_empty(),
        }
    }

    fn push_range(&mut self, text: &str, value: Range<usize>) {
        if value.is_empty() {
            return;
        }

        if self.is_empty() {
            *self = CowBuilder::Borrowed { range: value }
        } else {
            match self {
                CowBuilder::Borrowed { range } if range.end == value.start => range.end = value.end,
                _ => self.to_mut(text, value.len()).push_str(&text[value]),
            }
        }
    }

    fn push_char(&mut self, text: &str, char: char) {
        self.to_mut(text, char.len_utf8()).push(char)
    }

    fn to_mut(&mut self, text: &str, reserve: usize) -> &mut String {
        let value = match self {
            CowBuilder::Borrowed { range } => {
                let mut value = String::with_capacity(range.len() + reserve);
                value.push_str(&text[range.clone()]);
                value
            }
            CowBuilder::Owned { value } => return value,
        };

        *self = CowBuilder::Owned { value };

        match self {
            CowBuilder::Borrowed { .. } => unreachable!(),
            CowBuilder::Owned { value } => value,
        }
    }

    fn finish(self, text: &str) -> Cow<'_, str> {
        match self {
            CowBuilder::Borrowed { range } => Cow::Borrowed(&text[range]),
            CowBuilder::Owned { value } => Cow::Owned(value),
        }
    }
}

impl Default for CowBuilder {
    fn default() -> Self {
        CowBuilder::new()
    }
}
