mod char;
mod grammar;

use core::str::Chars;

use alloc::vec::Vec;

use crate::{Location, Receiver, Span, Token};

/// Parse a YAML stream, emitting events into the given receiver.
pub fn parse<R: Receiver>(receiver: &mut R, text: &str) -> Result<(), Vec<Diagnostic>> {
    let mut state = State {
        text,
        iter: text.chars(),
        tokens: Vec::new(),
        diagnostics: Vec::new(),
        receiver,
        in_token: false,
        length_limit: None,
        alt_depth: 0,
        line_number: 0,
        line_offset: 0,
        peek_count: 0,
    };

    match grammar::l_yaml_stream(&mut state) {
        Ok(()) => Ok(()),
        Err(()) => Err(state.diagnostics),
    }
}

struct State<'t, R> {
    text: &'t str,
    iter: Chars<'t>,

    tokens: Vec<(Token, Span)>,
    diagnostics: Vec<Diagnostic>,

    receiver: &'t mut R,
    alt_depth: u32,
    line_number: usize,
    line_offset: usize,

    in_token: bool,
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

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: &'static str,
    pub span: Span,
}

impl<'t, R> State<'t, R>
where
    R: Receiver,
{
    fn detect_scalar_indent(&self, n: i32) -> i32 {
        let mut iter = self.iter.clone();
        let mut longest_empty = 0;
        let mut len = 0;

        loop {
            match iter.next() {
                Some(' ') => len += 1,
                Some('\r' | '\n') => {
                    if len > longest_empty {
                        longest_empty = len;
                    }
                    len = 0;
                }
                _ => {
                    break if len > n {
                        len - n
                    } else if longest_empty > n {
                        longest_empty - n
                    } else {
                        1
                    }
                }
            }
        }
    }

    fn detect_collection_indent(&self, n: i32) -> i32 {
        // l+block-sequence
        // l+block-mapping

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

    fn detect_entry_indent(&self, n: i32) -> i32 {
        debug_assert!(matches!(self.peek_prev(), Some('-' | '?' | ':')));
        // ("?" | ":" | "-") s-l+block-indented

        let mut iter = self.iter.clone();
        let mut len = if n == -1 { 1 } else { 0 };
        let mut is_comment = false;

        loop {
            match iter.next() {
                Some(' ') => len += 1,
                Some('#') => {
                    is_comment = true;
                }
                Some('\r' | '\n') => {
                    len = 0;
                    is_comment = false;
                }
                Some(_) if is_comment => continue,
                _ => break if len >= n { len - n } else { 0 },
            }
        }
    }

    fn with_rollback(&mut self, f: impl Fn(&mut Self) -> Result<(), ()>) -> Result<(), ()> {
        let tokens_len = self.tokens.len();
        let diagnostics_len = self.tokens.len();
        let offset = self.offset();
        let line_number = self.line_number;
        let line_offset = self.line_offset;
        let length_limit = self.length_limit;

        self.alt_depth += 1;
        let res = f(self);
        self.alt_depth -= 1;

        match res {
            Ok(()) => {
                if self.alt_depth == 0 {
                    for (token, span) in self.tokens.drain(tokens_len..) {
                        self.receiver.token(token, span);
                    }
                }

                Ok(())
            }
            Err(()) => {
                self.iter = self.text[offset..].chars();
                self.tokens.truncate(tokens_len);
                self.diagnostics.truncate(diagnostics_len);
                self.line_number = line_number;
                self.line_offset = line_offset;
                self.length_limit = length_limit;
                Err(())
            }
        }
    }

    fn with_length_limit(
        &mut self,
        max_len: usize,
        f: impl Fn(&mut Self) -> Result<(), ()>,
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

    fn token(&mut self, token: Token, f: impl Fn(&mut Self) -> Result<(), ()>) -> Result<(), ()> {
        let start = self.location();
        debug_assert!(!self.in_token, "nested tokens");
        f(self)?;
        if self.alt_depth > 0 {
            self.tokens.push((token, self.span(start)));
        } else {
            self.receiver.token(token, self.span(start));
        }
        Ok(())
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

    fn is(&self, pred: impl Fn(char) -> bool) -> bool {
        matches!(self.peek(), Some(ch) if pred(ch))
    }

    fn is_char(&self, ch: char) -> bool {
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
        self.line_offset == 0
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

    fn peek(&self) -> Option<char> {
        #[cfg(debug_assertions)]
        if self.peek_count > 1000 {
            panic!("detected infinite loop in parser");
        }

        if self.length_limit == Some(0) {
            return None;
        }

        self.iter.clone().next()
    }

    fn peek_next(&self) -> Option<char> {
        #[cfg(debug_assertions)]
        if self.peek_count > 1000 {
            panic!("detected infinite loop in parser");
        }

        if matches!(self.length_limit, Some(0 | 1)) {
            return None;
        }

        self.iter.clone().nth(1)
    }

    fn peek_prev(&self) -> Option<char> {
        self.text[..self.offset()].chars().last()
    }

    fn bump(&mut self) {
        #[cfg(debug_assertions)]
        {
            self.peek_count = 0;
        }
        debug_assert!(self.in_token, "character not covered by token");

        if let Some(limit) = &mut self.length_limit {
            debug_assert!(*limit != 0);
            *limit -= 1;
        }

        let ch = self.iter.next().expect("called bump at end of input");
        let is_break = match ch {
            '\n' if !self.is_char('\r') => true,
            '\r' => true,
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

    fn seq_spaces(&self, n: i32) -> i32 {
        match self {
            Context::BlockIn => n,
            Context::BlockOut => n - 1,
            Context::BlockKey | Context::FlowIn | Context::FlowOut | Context::FlowKey => {
                unimplemented!()
            }
        }
    }
}
