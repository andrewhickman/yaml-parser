use alloc::{
    borrow::{Cow, ToOwned},
    format,
};

use crate::{
    parser::{
        char,
        grammar::{
            b_break, b_comment, b_non_content, c_nb_comment_text, l_comment, nb_char, ns_char,
            ns_hex_digit, s_b_comment, s_flow_line_prefix, s_indent, s_indent_less_or_equal,
            s_indent_less_than, s_separate_in_line, s_white,
        },
        Context, Diagnostic, Parser,
    },
    Receiver, Span, Token,
};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum Chomping {
    Strip,
    Clip,
    Keep,
}

pub(super) fn c_double_quoted<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    c: Context,
) -> Result<Cow<'s, str>, ()> {
    c_double_quote(parser)?;
    let value = nb_double_text(parser, n, c)?;
    c_double_quote(parser)?;
    Ok(value)
}

pub(super) fn c_single_quoted<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    c: Context,
) -> Result<Cow<'s, str>, ()> {
    c_single_quote(parser)?;
    let value = nb_single_text(parser, n, c)?;
    c_single_quote(parser)?;
    Ok(value)
}

pub(super) fn ns_plain<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    c: Context,
) -> Result<Cow<'s, str>, ()> {
    parser.begin_value();
    match c {
        Context::BlockKey | Context::FlowKey => ns_plain_one_line(parser, c)?,
        Context::FlowIn | Context::FlowOut => ns_plain_multi_line(parser, n, c)?,
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    }
    Ok(parser.end_value())
}

pub(super) fn ns_plain_first<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    let start = parser.offset();
    match parser.peek() {
        Some('?' | ':' | '-') if parser.next_is(|ch| char::plain_safe(ch, c)) => {
            parser.bump();
        }
        Some(ch) if char::indicator(ch) => return Err(()),
        _ => ns_char(parser)?,
    }
    parser
        .value
        .push_range(parser.stream, start..parser.offset());
    Ok(())
}

pub(super) fn c_l_literal<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
) -> Result<Cow<'s, str>, ()> {
    c_literal(parser)?;
    let (m, t) = c_b_block_header(parser, n)?;
    l_literal_content(parser, n + m, t)
}

pub(super) fn c_l_folded<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
) -> Result<Cow<'s, str>, ()> {
    c_folded(parser)?;
    let (m, t) = c_b_block_header(parser, n)?;
    l_folded_content(parser, n + m, t)
}

/////////////////////////////
// Double quoted
/////////////////////////////

fn c_ns_esc_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_escape(parser)?;
    parser.token(Token::EscapeCode, |parser| match parser.peek() {
        Some('x') => ns_esc_hex(parser, 'x', 2),
        Some('u') => ns_esc_hex(parser, 'u', 4),
        Some('U') => ns_esc_hex(parser, 'U', 8),
        _ => ns_esc_char(parser),
    })
}

fn c_escape<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::Escape, char::ESCAPE)
}

fn ns_esc_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    let ch = match parser.peek() {
        Some('0') => '\0',
        Some('a') => '\x07',
        Some('b') => '\x08',
        Some('t' | '\x09') => '\x09',
        Some('n') => '\x0a',
        Some('v') => '\x0b',
        Some('f') => '\x0c',
        Some('r') => '\x0d',
        Some('e') => '\x1b',
        Some(' ') => '\x20',
        Some('"') => '\x22',
        Some('/') => '\x2f',
        Some('\\') => '\x5c',
        Some('N') => '\u{85}',
        Some('_') => '\u{a0}',
        Some('L') => '\u{2028}',
        Some('P') => '\u{2029}',
        _ => return Err(()),
    };

    parser.value.push_char(parser.stream, ch);
    parser.bump();
    Ok(())
}

fn ns_esc_hex<R: Receiver>(parser: &mut Parser<R>, initial: char, len: u32) -> Result<(), ()> {
    parser.eat_char(initial)?;
    let start = parser.location();
    if (0..len).try_for_each(|_| ns_hex_digit(parser)).is_ok() {
        let hex = &parser.stream[start.index..parser.offset()];
        let codepoint: u32 = u32::from_str_radix(hex, 16).unwrap();
        match core::char::from_u32(codepoint) {
            Some(ch) => {
                parser.value.push_char(parser.stream, ch);
                Ok(())
            }
            None => {
                parser.diagnostics.push(Diagnostic {
                    message: format!("'{:x}' is not a valid character", codepoint),
                    span: parser.span(start),
                });
                Err(())
            }
        }
    } else {
        parser.diagnostics.push(Diagnostic {
            message: "invalid escape sequence".to_owned(),
            span: parser.span(start),
        });
        Err(())
    }
}

fn nb_json<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.eat(char::json)?;
    parser
        .value
        .push_range(parser.stream, start..parser.offset());
    Ok(())
}

fn c_double_quote<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::DoubleQuote, char::DOUBLE_QUOTE)
}

fn nb_double_text<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    c: Context,
) -> Result<Cow<'s, str>, ()> {
    parser.begin_value();
    let mut span = Span::empty(parser.location());
    loop {
        match parser.peek() {
            Some(char::DOUBLE_QUOTE) => {
                span.end = parser.location();
                if !span.is_empty() {
                    parser.queue(Token::DoubleQuoted, span);
                    parser.value.push_range(parser.stream, span.range());
                }
                break;
            }
            Some(char::ESCAPE) if parser.next_is(char::r#break) => {
                if matches!(c, Context::BlockKey | Context::FlowKey) {
                    return Err(());
                }
                span.end = parser.location();
                if !span.is_empty() {
                    parser.queue(Token::DoubleQuoted, span);
                    parser.value.push_range(parser.stream, span.range());
                }
                c_escape(parser)?;
                b_non_content(parser)?;
                star!(parser, l_empty(parser, n, Context::FlowIn));
                s_flow_line_prefix(parser, n)?;
                span = Span::empty(parser.location());
            }
            Some(char::ESCAPE) => {
                span.end = parser.location();
                if !span.is_empty() {
                    parser.queue(Token::DoubleQuoted, span);
                    parser.value.push_range(parser.stream, span.range());
                }
                c_ns_esc_char(parser)?;
                span = Span::empty(parser.location());
            }
            Some(' ' | '\t') => parser.bump(),
            Some(ch) if char::json(ch) => {
                parser.bump();
                span.end = parser.location();
            }
            Some('\r' | '\n') => {
                if matches!(c, Context::BlockKey | Context::FlowKey) {
                    return Err(());
                }
                if !span.is_empty() {
                    parser.queue(Token::DoubleQuoted, span);
                    parser.value.push_range(parser.stream, span.range());
                }
                if span.end != parser.location() {
                    parser.queue(Token::Separator, parser.span(span.end));
                }
                b_l_folded(parser, n, Context::FlowIn)?;
                s_flow_line_prefix(parser, n)?;
                span = Span::empty(parser.location());
            }
            _ => return Err(()),
        }
    }
    Ok(parser.end_value())
}

/////////////////////////////
// Single quoted
/////////////////////////////

fn c_single_quote<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::SingleQuote, char::SINGLE_QUOTE)
}

fn c_quoted_quote<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::EscapedQuote, |parser| parser.eat_str("''"))?;
    parser.value.push_char(parser.stream, '\'');
    Ok(())
}

fn nb_single_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.is_char(char::SINGLE_QUOTE) {
        c_quoted_quote(parser)
    } else {
        parser.token(Token::SingleQuoted, nb_json)
    }
}

fn ns_single_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.is(char::space) {
        Err(())
    } else {
        nb_single_char(parser)
    }
}

fn nb_single_text<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    c: Context,
) -> Result<Cow<'s, str>, ()> {
    parser.begin_value();
    match c {
        Context::BlockKey | Context::FlowKey => nb_single_one_line(parser)?,
        Context::FlowIn | Context::FlowOut => nb_single_multi_line(parser, n)?,
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    };
    Ok(parser.end_value())
}

fn nb_single_one_line<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    star!(parser, nb_single_char(parser));
    Ok(())
}

fn nb_ns_single_in_line<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    fn char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        let start = parser.offset();
        parser.token(Token::SingleQuoted, s_whites)?;
        parser
            .value
            .push_range(parser.stream, start..parser.offset());
        ns_single_char(parser)
    }

    star!(parser, char(parser));
    Ok(())
}

fn s_single_next_line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn trailing_whites<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        let start = parser.offset();
        parser.token(Token::SingleQuoted, s_whites)?;
        parser
            .value
            .push_range(parser.stream, start..parser.offset());
        Ok(())
    }

    fn line<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        ns_single_char(parser)?;
        nb_ns_single_in_line(parser)
    }

    loop {
        if question!(parser, s_flow_folded(parser, n)).is_none() {
            break;
        }
        if question!(parser, line(parser)).is_none() {
            break;
        }
    }
    trailing_whites(parser)
}

fn nb_single_multi_line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    nb_ns_single_in_line(parser)?;
    s_single_next_line(parser, n)
}

/////////////////////////////
// Plain
/////////////////////////////

fn ns_plain_safe<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    parser.eat(|ch| char::plain_safe(ch, c))
}

fn ns_plain_char<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    let start = parser.offset();
    match parser.peek() {
        Some('#') => {
            if parser.prev_is(char::non_space) {
                parser.bump();
            } else {
                return Err(());
            }
        }
        Some(':') => {
            if parser.next_is(|ch| char::plain_safe(ch, c)) {
                parser.bump();
            } else {
                return Err(());
            }
        }
        _ => ns_plain_safe(parser, c)?,
    }
    parser
        .value
        .push_range(parser.stream, start..parser.offset());
    Ok(())
}

fn nb_ns_plain_in_line<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    fn char<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
        let start = parser.offset();
        s_whites(parser)?;
        parser
            .value
            .push_range(parser.stream, start..parser.offset());
        ns_plain_char(parser, c)
    }

    star!(parser, char(parser, c));
    Ok(())
}

fn ns_plain_one_line<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    parser.token(Token::Scalar, |parser| {
        ns_plain_first(parser, c)?;
        nb_ns_plain_in_line(parser, c)
    })
}

fn s_ns_plain_next_line<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    s_flow_folded(parser, n)?;
    parser.token(Token::Scalar, |parser| {
        ns_plain_char(parser, c)?;
        nb_ns_plain_in_line(parser, c)
    })
}

fn ns_plain_multi_line<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    ns_plain_one_line(parser, c)?;
    star!(parser, s_ns_plain_next_line(parser, n, c));
    Ok(())
}

/////////////////////////////
// Literal
/////////////////////////////

fn c_literal<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::Literal, char::LITERAL)
}

fn l_nb_literal_text<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    star!(parser, l_empty(parser, n, Context::BlockIn));
    s_indent(parser, n)?;
    let start = parser.offset();
    parser.token(Token::Scalar, |parser| plus_fast!(parser, nb_char(parser)))?;
    parser
        .value
        .push_range(parser.stream, start..parser.offset());
    Ok(())
}

fn b_nb_literal_next<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    b_as_line_feed(parser)?;
    l_nb_literal_text(parser, n)
}

fn l_literal_content<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    t: Chomping,
) -> Result<Cow<'s, str>, ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32, t: Chomping) -> Result<(), ()> {
        l_nb_literal_text(parser, n)?;
        star!(parser, b_nb_literal_next(parser, n));
        b_chomped_last(parser, t)
    }

    parser.begin_value();
    question!(parser, line(parser, n, t));
    l_chomped_empty(parser, n, t)?;
    Ok(parser.end_value())
}

/////////////////////////////
// Flow
/////////////////////////////

fn c_folded<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::Folded, char::FOLDED)
}

fn l_folded_content<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    t: Chomping,
) -> Result<Cow<'s, str>, ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32, t: Chomping) -> Result<(), ()> {
        l_nb_diff_lines(parser, n)?;
        b_chomped_last(parser, t)
    }

    parser.begin_value();
    question!(parser, line(parser, n, t));
    l_chomped_empty(parser, n, t)?;
    Ok(parser.end_value())
}

fn s_nb_folded_text<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    let start = parser.offset();
    parser.token(Token::Scalar, |parser| {
        ns_char(parser)?;
        star!(parser, nb_char(parser));
        Ok(())
    })?;
    parser
        .value
        .push_range(parser.stream, start..parser.offset());
    Ok(())
}

fn l_nb_folded_lines<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        b_l_folded(parser, n, Context::BlockIn)?;
        s_nb_folded_text(parser, n)
    }

    s_nb_folded_text(parser, n)?;
    star!(parser, line(parser, n));
    Ok(())
}

fn s_nb_spaced_text<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    let start = parser.offset();
    parser.token(Token::Scalar, |parser| {
        s_white(parser)?;
        star!(parser, nb_char(parser));
        Ok(())
    })?;
    parser
        .value
        .push_range(parser.stream, start..parser.offset());
    Ok(())
}

fn b_l_spaced<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    b_as_line_feed(parser)?;
    star!(parser, l_empty(parser, n, Context::BlockIn));
    Ok(())
}

fn l_nb_spaced_lines<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        b_l_spaced(parser, n)?;
        s_nb_spaced_text(parser, n)
    }

    s_nb_spaced_text(parser, n)?;
    star!(parser, line(parser, n));
    Ok(())
}

fn l_nb_same_lines<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    star!(parser, l_empty(parser, n, Context::BlockIn));
    alt!(
        parser,
        l_nb_folded_lines(parser, n),
        l_nb_spaced_lines(parser, n)
    )
}

fn l_nb_diff_lines<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        b_as_line_feed(parser)?;
        l_nb_same_lines(parser, n)
    }

    l_nb_same_lines(parser, n)?;
    star!(parser, line(parser, n));
    Ok(())
}

fn b_l_folded<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(parser, b_l_trimmed(parser, n, c), b_as_space(parser))
}

fn b_as_space<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::ScalarSpace, b_break)?;
    parser.value.push_char(parser.stream, ' ');
    Ok(())
}

/////////////////////////////
// Block header
/////////////////////////////

fn c_b_block_header<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(i32, Chomping), ()> {
    let (m, t) = match parser.peek() {
        Some('1'..='9') => (
            c_indentation_indicator(parser)?,
            c_chomping_indicator(parser)?,
        ),
        Some('-' | '+') => {
            let t = c_chomping_indicator(parser)?;
            let m = c_indentation_indicator(parser)?;
            (m, t)
        }
        _ => (None, Chomping::Clip),
    };

    s_b_comment(parser)?;

    match m {
        Some(m) => Ok((m, t)),
        None => {
            let m = parser.detect_scalar_indent(n)?;
            Ok((m, t))
        }
    }
}

fn c_indentation_indicator<R: Receiver>(parser: &mut Parser<R>) -> Result<Option<i32>, ()> {
    match parser.peek() {
        Some(ch @ '1'..='9') => {
            parser.token(Token::IndentationIndicator, |parser| {
                parser.bump();
                Ok(())
            })?;
            Ok(Some(ch.to_digit(10).unwrap() as i32))
        }
        _ => Ok(None),
    }
}

fn c_chomping_indicator<R: Receiver>(parser: &mut Parser<R>) -> Result<Chomping, ()> {
    if parser
        .token(Token::ChompingIndicator, |parser| parser.eat_char('-'))
        .is_ok()
    {
        Ok(Chomping::Strip)
    } else if parser
        .token(Token::ChompingIndicator, |parser| parser.eat_char('+'))
        .is_ok()
    {
        Ok(Chomping::Keep)
    } else {
        Ok(Chomping::Clip)
    }
}

/////////////////////////////
// Whitespace
/////////////////////////////

fn s_flow_folded<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    question_fast!(parser, s_separate_in_line(parser));
    b_l_folded(parser, n, Context::FlowIn)?;
    s_flow_line_prefix(parser, n)
}

fn s_whites<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    star_fast!(parser, s_white(parser));
    Ok(())
}

fn b_as_line_feed<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.token(Token::ScalarBreak, b_break)?;
    if &parser.stream[start..parser.offset()] == "\n" {
        parser
            .value
            .push_range(parser.stream, start..parser.offset());
    } else {
        parser.value.push_char(parser.stream, '\n');
    }
    Ok(())
}

fn l_empty<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    if s_indent_less_or_equal(parser, n)? == n && matches!(c, Context::FlowIn | Context::FlowOut) {
        question_fast!(parser, s_separate_in_line(parser));
    }
    b_as_line_feed(parser)
}

fn b_l_trimmed<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    b_non_content(parser)?;
    plus!(parser, l_empty(parser, n, c))
}

fn b_chomped_last<R: Receiver>(parser: &mut Parser<R>, t: Chomping) -> Result<(), ()> {
    if parser.is_end_of_input() {
        if matches!(t, Chomping::Clip | Chomping::Keep) {
            parser.value.push_char(parser.stream, '\n');
        }
        Ok(())
    } else {
        match t {
            Chomping::Strip => b_non_content(parser),
            Chomping::Clip | Chomping::Keep => b_as_line_feed(parser),
        }
    }
}

fn l_chomped_empty<R: Receiver>(parser: &mut Parser<R>, n: i32, t: Chomping) -> Result<(), ()> {
    let t = match t {
        Chomping::Strip | Chomping::Clip => Chomping::Strip,
        Chomping::Keep => Chomping::Keep,
    };

    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32, t: Chomping) -> Result<(), ()> {
        if parser.is_end_of_input() {
            return Err(());
        }

        s_indent_less_or_equal(parser, n)?;
        b_chomped_last(parser, t)
    }

    star!(parser, line(parser, n, t));
    question!(parser, l_trail_comments(parser, n));
    Ok(())
}

fn l_trail_comments<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_indent_less_than(parser, n)?;
    c_nb_comment_text(parser)?;
    b_comment(parser)?;
    star!(parser, l_comment(parser));
    Ok(())
}
