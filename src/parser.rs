use core::str::Chars;

use alloc::vec::Vec;

use crate::{Location, Receiver, Span, Token};

#[derive(Clone)]
#[allow(unused)]
struct State<'t, R> {
    text: &'t str,
    iter: Chars<'t>,

    tokens: Vec<(Token, Span)>,
    diagnostics: Vec<Diagnostic>,

    receiver: R,
    alt_depth: u32,
    line_number: usize,
    line_offset: usize,

    #[cfg(debug_assertions)]
    peek_count: u32,
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum Context {
    BlockIn,
    BlockOut,
    BlockKey,
    FlowIn,
    FlowOut,
    FlowKey,
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum Chomping {
    Strip,
    Clip,
    Keep,
}

#[derive(Clone)]
#[allow(unused)]
struct Diagnostic {
    message: &'static str,
    span: Span,
}

#[allow(unused)]
impl<'t, R> State<'t, R>
where
    R: Receiver,
{
    fn with_rollback(&mut self, f: impl Fn(&mut Self) -> Result<(), ()>) -> Result<(), ()> {
        let tokens_len = self.tokens.len();
        let diagnostics_len = self.tokens.len();
        let offset = self.offset();
        let line_number = self.line_number;
        let line_offset = self.line_offset;

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
                Err(())
            }
        }
    }

    fn token(&mut self, token: Token, f: impl Fn(&mut Self) -> Result<(), ()>) -> Result<(), ()> {
        let start = self.location();
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
        if self.iter.as_str().starts_with(s) {
            self.iter = self.iter.as_str()[s.len()..].chars();
            Ok(())
        } else {
            Err(())
        }
    }

    fn is(&self, pred: impl Fn(char) -> bool) -> bool {
        matches!(self.peek(), Some(ch) if pred(ch))
    }

    fn is_char(&self, ch: char) -> bool {
        self.peek() == Some(ch)
    }

    fn is_start_of_line(&self) -> bool {
        self.line_offset == 0
    }

    fn is_end_of_input(&self) -> bool {
        self.peek().is_none()
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

        self.iter.clone().next()
    }

    fn peek_next(&self) -> Option<char> {
        #[cfg(debug_assertions)]
        if self.peek_count > 1000 {
            panic!("detected infinite loop in parser");
        }

        self.iter.clone().nth(1)
    }

    fn peek_str(&self) -> &str {
        #[cfg(debug_assertions)]
        if self.peek_count > 1000 {
            panic!("detected infinite loop in parser");
        }

        self.iter.as_str()
    }

    fn peek_prev(&self) -> Option<char> {
        self.text[..self.offset()].chars().last()
    }

    fn bump(&mut self) {
        #[cfg(debug_assertions)]
        {
            self.peek_count = 0;
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

macro_rules! star {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        while question!($state, $production(state $(, $p)*)) {}
    };
}

macro_rules! star_fast {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        while question_fast!($state, $production(state $(, $p)*)) {}
    };
}

#[allow(unused)]
macro_rules! plus {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        {
            $production($state $(, $p)*)?;
            star!($state, $production(state $(, $p)*));
            Result::<(), ()>::Ok(())
        }
    };
}

macro_rules! plus_fast {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        {
            $production($state $(, $p)*)?;
            star_fast!($state, $production(state $(, $p)*));
            Result::<(), ()>::Ok(())
        }
    };
}

macro_rules! question {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        {
            let start = $state.location();
            let res = $state.with_rollback(|state| $production(state $(, $p)*));
            if res.is_err() {
                debug_assert_eq!(start, $state.location());
                false
            } else {
                true
            }
        }
    };
}

macro_rules! question_fast {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        {
            let start = $state.location();
            let res = $production($state $(, $p)*);
            if res.is_err() {
                debug_assert_eq!(start, $state.location());
                false
            } else {
                true
            }
        }
    };
}

macro_rules! alt {
    ($state:expr, $($production:ident($state_param:ident $(, $p:expr)*)),*) => {
        'alt: {
            let start = $state.location();

            $(
                match $state.with_rollback(|state| $production(state $(, $p)*)) {
                    Ok(()) => break 'alt Ok(()),
                    Err(()) => (),
                }

                debug_assert_eq!(start, $state.location());
            )*

            break 'alt Err(())
        }
    };
}

macro_rules! alt_fast {
    ($state:expr, $($production:ident($state_param:ident $(, $p:expr)*)),*) => {
        'alt: {
            let start = $state.location();

            $(
                match $production($state $(, $p)*) {
                    Ok(()) => break 'alt Ok(()),
                    Err(()) => (),
                }

                debug_assert_eq!(start, $state.location());
            )*

            break 'alt Err(())
        }
    };
}

fn c_printable<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| {
        matches!(
            ch,
                '\t'
                | '\n'
                | '\x20'..='\x7e'
                | '\u{85}'
                | '\u{a0}'..='\u{fffd}'
                | '\u{010000}'..,
        )
    })
}

fn nb_json<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| matches!(ch, '\x09' | '\x20'..='\u{10ffff}'))
}

fn c_byte_order_mark<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::ByteOrderMark, |state| state.eat_char('\u{feff}'))
}

fn c_sequence_entry<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::SequenceEntry, |state| state.eat_char('-'))
}

fn c_mapping_key<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::MappingKey, |state| state.eat_char('?'))
}

fn c_mapping_value<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::MappingValue, |state| state.eat_char(':'))
}

fn c_collect_entry<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::CollectionEntry, |state| state.eat_char(','))
}

fn c_sequence_start<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::SequenceStart, |state| state.eat_char('['))
}

fn c_sequence_end<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::SequenceEnd, |state| state.eat_char(']'))
}

fn c_mapping_start<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::MappingStart, |state| state.eat_char('{'))
}

fn c_mapping_end<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::MappingEnd, |state| state.eat_char('}'))
}

fn c_comment<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('#')
}

fn c_anchor<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Anchor, |state| state.eat_char('&'))
}

fn c_alias<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Alias, |state| state.eat_char('*'))
}

fn c_tag<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Tag, |state| state.eat_char('!'))
}

fn c_literal<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Literal, |state| state.eat_char('|'))
}

fn c_folded<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Folded, |state| state.eat_char('>'))
}

fn c_single_quote<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::SingleQuote, |state| state.eat_char('\''))
}

fn c_double_quote<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::DoubleQuote, |state| state.eat_char('"'))
}

fn c_directive<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Directive, |state| state.eat_char('%'))
}

fn c_reserved<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Reserved, |state| {
        state.eat(|ch| matches!(ch, '@' | '`'))
    })
}

fn c_indicator<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| {
        matches!(
            ch,
            '-' | '?'
                | ':'
                | ','
                | '['
                | ']'
                | '{'
                | '}'
                | '#'
                | '&'
                | '*'
                | '!'
                | '|'
                | '>'
                | '\''
                | '"'
                | '%'
                | '@'
                | '`'
        )
    })
}

fn c_flow_indicator<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| matches!(ch, ',' | '[' | ']' | '{' | '}'))
}

fn b_line_feed<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('\n')
}

fn b_carriage_return<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('\r')
}

fn b_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| matches!(ch, '\n' | '\r'))
}

fn nb_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| {
        matches!(
            ch,
                '\t'
                | '\x20'..='\x7e'
                | '\u{85}'
                | '\u{a0}'..='\u{fefe}'
                | '\u{ff00}'..='\u{fffd}'
                | '\u{010000}'..,
        )
    })
}

fn b_break<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Break, |state| {
        state.eat_char('\r').or(state.eat_char('\n'))
    })
}

fn b_as_line_feed<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    b_break(state)
}

fn b_non_content<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    b_break(state)
}

fn s_space<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char(' ')
}

fn s_tab<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('\t')
}

fn s_white<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| matches!(ch, ' ' | '\t'))
}

fn ns_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| {
        matches!(
            ch,
                | '\x21'..='\x7e'
                | '\u{85}'
                | '\u{a0}'..='\u{fefe}'
                | '\u{ff00}'..='\u{fffd}'
                | '\u{010000}'..,
        )
    })
}

fn ns_dec_digit<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| ch.is_ascii_digit())
}

fn ns_hex_digit<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| ch.is_ascii_hexdigit())
}

fn is_ascii_letter<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| ch.is_ascii_alphabetic())
}

fn ns_word_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| ch.is_ascii_alphanumeric() || ch == '-')
}

fn ns_uri_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.is_char('%') {
        let start = state.location();
        state.eat_char('%').unwrap();
        if ns_hex_digit(state).is_ok() && ns_hex_digit(state).is_ok() {
            Ok(())
        } else {
            state.diagnostics.push(Diagnostic {
                message: "invalid percent escape",
                span: state.span(start),
            });
            Err(())
        }
    } else {
        state.eat(|ch| {
            ch.is_ascii_alphanumeric()
                || matches!(
                    ch,
                    '#' | ';'
                        | '/'
                        | '?'
                        | ':'
                        | '@'
                        | '&'
                        | '='
                        | '+'
                        | '$'
                        | ','
                        | '_'
                        | '.'
                        | '!'
                        | '~'
                        | '*'
                        | '\''
                        | '('
                        | ')'
                        | '['
                        | ']'
                )
        })
    }
}

fn ns_tag_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.is(|ch| matches!(ch, '!' | ',' | '[' | ']' | '{' | '}')) {
        Err(())
    } else {
        ns_uri_char(state)
    }
}

fn c_escape<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('\\')
}

fn ns_esc_null<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('0')
}

fn ns_esc_bell<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('a')
}

fn ns_esc_backspace<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('a')
}

fn ns_esc_horizontal_tab<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| matches!(ch, 't' | '\t'))
}

fn ns_esc_line_feed<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('n')
}

fn ns_esc_vertical_tab<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('v')
}

fn ns_esc_form_feed<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('f')
}

fn ns_esc_carriage_return<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('r')
}

fn ns_esc_escape<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('e')
}

fn ns_esc_space<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char(' ')
}

fn ns_esc_double_quote<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('"')
}

fn ns_esc_slash<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('/')
}

fn ns_esc_backslash<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('\\')
}

fn ns_esc_next_line<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('N')
}

fn ns_esc_non_breaking_space<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('_')
}

fn ns_esc_line_separator<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('L')
}

fn ns_esc_paragraph_separator<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('P')
}

fn ns_esc_8_bit<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    let start = state.location();
    state.eat_char('x')?;
    if (0..2).try_for_each(|_| ns_hex_digit(state)).is_ok() {
        Ok(())
    } else {
        state.diagnostics.push(Diagnostic {
            message: "invalid 8 bit escape",
            span: state.span(start),
        });
        Err(())
    }
}

fn ns_esc_16_bit<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    let start = state.location();
    state.eat_char('x')?;
    if (0..4).try_for_each(|_| ns_hex_digit(state)).is_ok() {
        Ok(())
    } else {
        state.diagnostics.push(Diagnostic {
            message: "invalid 16 bit escape",
            span: state.span(start),
        });
        Err(())
    }
}

fn ns_esc_32_bit<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    let start = state.location();
    state.eat_char('x')?;
    if (0..8).try_for_each(|_| ns_hex_digit(state)).is_ok() {
        Ok(())
    } else {
        state.diagnostics.push(Diagnostic {
            message: "invalid 16 bit escape",
            span: state.span(start),
        });
        Err(())
    }
}

fn c_ns_esc_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_escape(state)?;
    alt_fast!(
        state,
        ns_esc_null(state),
        ns_esc_bell(state),
        ns_esc_backspace(state),
        ns_esc_horizontal_tab(state),
        ns_esc_line_feed(state),
        ns_esc_vertical_tab(state),
        ns_esc_form_feed(state),
        ns_esc_carriage_return(state),
        ns_esc_escape(state),
        ns_esc_space(state),
        ns_esc_double_quote(state),
        ns_esc_slash(state),
        ns_esc_backslash(state),
        ns_esc_next_line(state),
        ns_esc_non_breaking_space(state),
        ns_esc_line_separator(state),
        ns_esc_paragraph_separator(state),
        ns_esc_8_bit(state),
        ns_esc_16_bit(state),
        ns_esc_32_bit(state)
    )
}

fn s_indent<R: Receiver>(state: &mut State<R>, n: u32) -> Result<(), ()> {
    state.token(Token::Indent, |state| {
        for _ in 0..n {
            s_space(state)?;
        }
        Ok(())
    })
}

fn s_indent_less_than<R: Receiver>(state: &mut State<R>, n: u32) -> Result<(), ()> {
    debug_assert_ne!(n, 1);
    state.token(Token::Indent, |state| {
        for _ in 1..n {
            if s_space(state).is_err() {
                return Ok(());
            }
        }
        Ok(())
    })
}

fn s_indent_less_or_equal<R: Receiver>(state: &mut State<R>, n: u32) -> Result<(), ()> {
    state.token(Token::Indent, |state| {
        for _ in 0..n {
            if s_space(state).is_err() {
                return Ok(());
            }
        }
        Ok(())
    })
}

fn s_separate_in_line<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Separator, |state| {
        if plus_fast!(state, s_space(state)).is_ok() || state.is_start_of_line() {
            Ok(())
        } else {
            Err(())
        }
    })
}

fn s_line_prefix<R: Receiver>(state: &mut State<R>, n: u32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockIn | Context::BlockOut => s_block_line_prefix(state, n),
        Context::FlowIn | Context::FlowOut => s_flow_line_prefix(state, n),
        Context::BlockKey | Context::FlowKey => unimplemented!(),
    }
}

fn s_block_line_prefix<R: Receiver>(state: &mut State<R>, n: u32) -> Result<(), ()> {
    s_indent(state, n)
}

fn s_flow_line_prefix<R: Receiver>(state: &mut State<R>, n: u32) -> Result<(), ()> {
    s_indent(state, n)?;
    s_separate_in_line(state)
}

fn l_empty<R: Receiver>(state: &mut State<R>, n: u32, c: Context) -> Result<(), ()> {
    alt!(
        state,
        s_line_prefix(state, n, c),
        s_indent_less_than(state, n)
    )?;
    b_as_line_feed(state)
}

fn b_l_trimmed<R: Receiver>(state: &mut State<R>, n: u32, c: Context) -> Result<(), ()> {
    b_non_content(state)?;
    star_fast!(state, l_empty(state, n, c));
    Ok(())
}

fn b_as_space<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    b_break(state)
}

fn b_l_folded<R: Receiver>(state: &mut State<R>, n: u32, c: Context) -> Result<(), ()> {
    alt!(state, b_l_trimmed(state, n, c), b_as_space(state))
}

fn s_flow_folded<R: Receiver>(state: &mut State<R>, n: u32) -> Result<(), ()> {
    question_fast!(state, s_separate_in_line(state));
    b_l_folded(state, n, Context::FlowIn)?;
    s_flow_line_prefix(state, n)
}

fn c_nb_comment_text<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_comment(state)?;
    star_fast!(state, nb_char(state));
    Ok(())
}

fn b_comment<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if b_non_content(state).is_ok() || state.is_end_of_input() {
        Ok(())
    } else {
        Err(())
    }
}

fn s_b_comment<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if s_separate_in_line(state).is_ok() {
        question_fast!(state, c_nb_comment_text(state));
    }
    b_comment(state)
}

fn l_comment<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    s_separate_in_line(state)?;
    question_fast!(state, c_nb_comment_text(state));
    b_comment(state)
}

fn s_l_comments<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if question!(state, s_b_comment(state)) || state.is_start_of_line() {
        star!(state, l_comment(state));
        Ok(())
    } else {
        Err(())
    }
}

fn s_separate<R: Receiver>(state: &mut State<R>, n: u32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockIn | Context::BlockOut | Context::FlowIn | Context::FlowOut => {
            s_separate_lines(state, n)
        }
        Context::BlockKey | Context::FlowKey => s_separate_in_line(state),
    }
}

fn s_separate_lines<R: Receiver>(state: &mut State<R>, n: u32) -> Result<(), ()> {
    fn comments<R: Receiver>(state: &mut State<R>, n: u32) -> Result<(), ()> {
        s_l_comments(state)?;
        s_flow_line_prefix(state, n)
    }

    alt!(state, comments(state, n), s_separate_in_line(state))
}

fn l_directive<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_directive(state)?;
    if state.peek_str().starts_with("YAML ") {
        ns_yaml_directive(state)?;
    } else if state.peek_str().starts_with("TAG ") {
        ns_tag_directive(state)?;
    } else {
        ns_reserved_directive(state)?;
    }
    s_l_comments(state)
}

fn ns_reserved_directive<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    fn param<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
        s_separate_in_line(state)?;
        ns_directive_parameter(state)
    }

    ns_directive_name(state)?;
    star!(state, param(state));
    Ok(())
}

fn ns_directive_name<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::DirectiveName, |state| {
        plus_fast!(state, ns_char(state))
    })
}

fn ns_directive_parameter<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::DirectiveParameter, |state| {
        plus_fast!(state, ns_char(state))
    })
}

fn ns_yaml_directive<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.peek_str().starts_with("YAML ") {
        state.token(Token::DirectiveName, |state| state.eat_str("YAML"))?;
        s_separate_in_line(state)?;
        ns_yaml_version(state)?;
    }
    Ok(())
}

fn ns_yaml_version<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::YamlVersion, |state| {
        plus_fast!(state, ns_dec_digit(state))?;
        state.eat_char('.')?;
        plus_fast!(state, ns_dec_digit(state))
    })
}

fn ns_tag_directive<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.peek_str().starts_with("TAG ") {
        state.token(Token::DirectiveName, |state| state.eat_str("TAG"))?;
        s_separate_in_line(state)?;
        c_tag_handle(state)?;
        s_separate_in_line(state)?;
        ns_tag_prefix(state)?;
    }
    Ok(())
}

fn c_tag_handle<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::TagHandle, |state| match state.peek_next() {
        Some(ch) if ch.is_ascii_alphabetic() || ch == '-' => c_named_tag_handle(state),
        Some('!') => c_secondary_tag_handle(state),
        _ => c_primary_tag_handle(state),
    })
}

fn c_primary_tag_handle<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('!')
}

fn c_secondary_tag_handle<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_str("!!")
}

fn c_named_tag_handle<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('!')?;
    plus_fast!(state, ns_word_char(state))?;
    state.eat_char('!')
}

fn ns_tag_prefix<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::TagPrefix, |state| {
        if state.is_char('!') {
            c_ns_local_tag_prefix(state)
        } else {
            ns_global_tag_prefix(state)
        }
    })
}

fn c_ns_local_tag_prefix<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('!')?;
    star!(state, ns_uri_char(state));
    Ok(())
}

fn ns_global_tag_prefix<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    ns_tag_char(state)?;
    star!(state, ns_uri_char(state));
    Ok(())
}

fn c_ns_properties<R: Receiver>(state: &mut State<R>, n: u32, c: Context) -> Result<(), ()> {
    fn seperated_anchor<R: Receiver>(state: &mut State<R>, n: u32, c: Context) -> Result<(), ()> {
        s_separate(state, n, c)?;
        c_ns_anchor_property(state)
    }

    fn seperated_tag<R: Receiver>(state: &mut State<R>, n: u32, c: Context) -> Result<(), ()> {
        s_separate(state, n, c)?;
        c_ns_tag_property(state)
    }

    match state.peek() {
        Some('!') => {
            c_ns_tag_property(state)?;
            question!(state, seperated_anchor(state, n, c));
            Ok(())
        },
        Some('&') => {
            c_ns_anchor_property(state)?;
            question!(state, seperated_tag(state, n, c));
            Ok(())
        },
        _ => Err(()),
    }
}

fn c_ns_tag_property<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.peek_next() == Some('<') {
        c_verbatim_tag(state)
    } else {
        alt!(
            state,
            c_ns_shorthand_tag(state),
            c_non_specific_tag(state)
        )
    }
}

fn c_verbatim_tag<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::VerbatimTag, |state| {
        state.eat_str("!<")?;
        plus!(state, ns_uri_char(state))?;
        state.eat_str(">")?;
        Ok(())
    })
}

fn c_ns_shorthand_tag<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_tag_handle(state)?;
    state.token(Token::TagSuffix, |state| {
        plus!(state, ns_tag_char(state))
    })
}

fn c_non_specific_tag<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::NonSpecificTag, |state| state.eat_char('!'))
}

fn c_ns_anchor_property<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_anchor(state)?;
    ns_anchor_name(state)
}

fn ns_anchor_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.is(|ch| matches!(ch, ',' | '[' | ']' | '{' | '}')) {
        Err(())
    } else {
        ns_char(state)
    }
}

fn ns_anchor_name<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::AnchorName, |state| {
        plus_fast!(state, ns_anchor_char(state))
    })
}

fn c_ns_alias_node<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_alias(state)?;
    ns_anchor_name(state)
}

fn e_scalar<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Empty, |_| Ok(()))
}

fn e_node<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    e_scalar(state)
}

fn nb_double_char<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn ns_double_char<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn c_double_quoted<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn nb_double_text<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn nb_double_one_line<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn s_double_escaped<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn s_double_break<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn nb_ns_double_in_line<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn s_double_next_line<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn nb_double_multi_line<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn c_quoted_quote<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn nb_single_char<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn ns_single_char<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn c_single_quoted<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn nb_single_text<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn nb_single_one_line<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn nb_ns_single_in_line<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn s_single_next_line<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn nb_single_multi_line<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn ns_plain_first<R: Receiver>(_state: &mut State<R>, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_plain_safe<R: Receiver>(_state: &mut State<R>, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_plain_safe_out<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn ns_plain_safe_in<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn ns_plain_char<R: Receiver>(_state: &mut State<R>, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_plain<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_plain_in_line<R: Receiver>(_state: &mut State<R>, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_plain_one_line<R: Receiver>(_state: &mut State<R>, _c: Context) -> Result<(), ()> {
    todo!()
}

fn s_ns_plain_next_line<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_plain_multi_line<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

// https://github.com/yaml/yaml-spec/issues/299
fn in_flow(_c: Context) -> Context {
    todo!()
}

fn c_flow_sequence<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_s_flow_seq_entries<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_flow_seq_entry<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn c_flow_mapping<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_s_flow_map_entries<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_flow_map_entry<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    // add whitespace lookahead after '?'
    todo!()
}

fn ns_flow_map_explicit_entry<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_flow_map_implicit_entry<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_flow_map_yaml_key_entry<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn c_ns_flow_map_empty_key_entry<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn c_ns_flow_map_separate_value<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn c_ns_flow_map_json_key_entry<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn c_ns_flow_map_adjacent_value<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_flow_pair<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    // add whitespace lookahead after '?'
    todo!()
}

fn ns_flow_pair_entry<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_flow_pair_yaml_key_entry<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn c_ns_flow_pair_json_key_entry<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_s_implicit_yaml_key<R: Receiver>(_state: &mut State<R>, _c: Context) -> Result<(), ()> {
    todo!()
}

fn c_s_implicit_json_key<R: Receiver>(_state: &mut State<R>, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_flow_yaml_content<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn c_flow_json_content<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_flow_content<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_flow_yaml_node<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    // ns-flow-yaml-content to ns-flow-content?
    todo!()
}

fn c_flow_json_node<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn ns_flow_node<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn c_b_block_header<R: Receiver>(_state: &mut State<R>, _t: Chomping) -> Result<(), ()> {
    // where does `t` come from?
    todo!()
}

fn c_identation_indicator<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    // input `n`, output `m` to somewhere
    todo!()
}

fn c_chomping_indicator<R: Receiver>(_state: &mut State<R>, _t: Chomping) -> Result<(), ()> {
    // set `t` somewhere
    todo!()
}

fn b_chomped_last<R: Receiver>(_state: &mut State<R>, _t: Chomping) -> Result<(), ()> {
    todo!()
}

fn l_chomped_empty<R: Receiver>(_state: &mut State<R>, _n: u32, _t: Chomping) -> Result<(), ()> {
    todo!()
}

fn l_strip_empty<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn l_keep_empty<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn l_trail_comments<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn c_l_literal<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    // pass n to c_b_block_header
    todo!()
}

fn l_nb_literal_text<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn b_nb_literal_next<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn l_literal_content<R: Receiver>(_state: &mut State<R>, _n: u32, _t: Chomping) -> Result<(), ()> {
    todo!()
}

fn c_l_folded<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn s_nb_folded_text<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn l_nb_folded_lines<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn s_nb_spaced_text<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn b_l_spaced<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn l_nb_spaced_lines<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn l_nb_same_lines<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn l_nb_diff_lines<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn l_folded_content<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn l_block_sequence<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    // change to autodetection of ident
    todo!()
}

fn c_l_block_seq_entry<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn s_l_block_indented<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    // change to autodetection of ident
    todo!()
}

fn ns_l_compact_sequence<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn l_block_mapping<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    // change to autodetection of ident
    todo!()
}

fn ns_l_block_map_entry<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn c_l_block_map_explicit_entry<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn c_l_block_map_explicit_key<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    // whitespace lookahead assertion after '?'
    todo!()
}

fn l_block_map_explicit_value<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn ns_l_block_map_implicit_entry<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn ns_s_block_map_implicit_key<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn c_l_block_map_implicit_value<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn ns_l_compact_mapping<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn s_l_block_node<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn s_l_flow_in_block<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn s_l_block_in_block<R: Receiver>(_state: &mut State<R>, _n: u32) -> Result<(), ()> {
    todo!()
}

fn s_l_block_scalar<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn s_l_block_collection<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    // issue when consuming property that might be the key of the first pair of a mapping
    todo!()
}

fn seq_space<R: Receiver>(_state: &mut State<R>, _n: u32, _c: Context) -> Result<(), ()> {
    todo!()
}

fn l_document_prefix<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn c_directives_end<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    // add whitespace lookahead to end
    todo!()
}

fn c_document_end<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn l_document_suffix<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn c_forbidden<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn l_bare_document<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn l_explicit_document<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn l_directive_document<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn l_any_document<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    todo!()
}

fn l_yaml_stream<R: Receiver>(_state: &mut State<R>) -> Result<(), ()> {
    // only try l-document-prefix, l-document-suffix once
    todo!()
}
