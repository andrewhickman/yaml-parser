use crate::{
    parser::{char, Chomping, Context, Diagnostic, Parser},
    Receiver, Token,
};

macro_rules! star {
    ($parser:expr, $production:ident($parser_param:ident $(, $p:expr)*)) => {
        {
            let mut start = $parser.offset();
            while question!($parser, $production(parser $(, $p)*)).is_some() && $parser.offset() != start {
                start = $parser.offset();
            }
        }
    };
}

macro_rules! star_fast {
    ($parser:expr, $production:ident($parser_param:ident $(, $p:expr)*)) => {
        {
            let mut start = $parser.offset();
            while question_fast!($parser, $production(parser $(, $p)*)).is_some() && $parser.offset() != start {
                start = $parser.offset();
            }
        }
    };
}

macro_rules! plus {
    ($parser:expr, $production:ident($parser_param:ident $(, $p:expr)*)) => {
        match $production($parser $(, $p)*) {
            Ok(()) => {
                star!($parser, $production(parser $(, $p)*));
                Result::<(), ()>::Ok(())
            },
            Err(()) => Result::<(), ()>::Err(()),
        }
    };
}

macro_rules! plus_fast {
    ($parser:expr, $production:ident($parser_param:ident $(, $p:expr)*)) => {
        match $production($parser $(, $p)*) {
            Ok(()) => {
                star_fast!($parser, $production(parser $(, $p)*));
                Result::<(), ()>::Ok(())
            },
            Err(()) => Result::<(), ()>::Err(()),
        }
    };
}

macro_rules! question {
    ($parser:expr, $production:ident($parser_param:ident $(, $p:expr)*)) => {
        {
            let start = $parser.location();
            let res = $parser.with_rollback(|parser| $production(parser $(, $p)*));
            match res {
                Ok(r) => Some(r),
                Err(()) => {
                    debug_assert_eq!(start, $parser.location());
                    None
                }
            }
        }
    };
}

macro_rules! question_fast {
    ($parser:expr, $production:ident($parser_param:ident $(, $p:expr)*)) => {
        {
            let start = $parser.location();
            let res = $production($parser $(, $p)*);
            match res {
                Ok(r) => Some(r),
                Err(()) => {
                    debug_assert_eq!(start, $parser.location());
                    None
                }
            }
        }
    };
}

macro_rules! alt {
    ($parser:expr, $($production:ident($parser_param:ident $(, $p:expr)*)),*) => {
        'alt: {
            let start = $parser.location();

            $(
                match $parser.with_rollback(|parser| $production(parser $(, $p)*)) {
                    Ok(r) => break 'alt Ok(r),
                    Err(()) => (),
                }

                debug_assert_eq!(start, $parser.location());
            )*

            break 'alt Err(())
        }
    };
}

macro_rules! alt_fast {
    ($parser:expr, $($production:ident($parser_param:ident $(, $p:expr)*)),*) => {
        'alt: {
            let start = $parser.location();

            $(
                match $production($parser $(, $p)*) {
                    Ok(r) => break 'alt Ok(r),
                    Err(()) => (),
                }

                debug_assert_eq!(start, $parser.location());
            )*

            break 'alt Err(())
        }
    };
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_json<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(|ch| matches!(ch, '\x09' | '\x20'..='\u{10ffff}'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_byte_order_mark<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.is_char(char::BYTE_ORDER_MARK) {
        parser.token(Token::ByteOrderMark, |parser| {
            parser.eat_char(char::BYTE_ORDER_MARK)
        })
    } else {
        Ok(())
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_sequence_entry<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::SequenceEntry, |parser| parser.eat_char('-'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_mapping_key<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::MappingKey, |parser| parser.eat_char('?'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_mapping_value<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::MappingValue, |parser| parser.eat_char(':'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_collect_entry<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::CollectionEntry, |parser| parser.eat_char(','))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_sequence_start<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::SequenceStart, |parser| parser.eat_char('['))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_sequence_end<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::SequenceEnd, |parser| parser.eat_char(']'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_mapping_start<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::MappingStart, |parser| parser.eat_char('{'))
}

fn c_mapping_end<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::MappingEnd, |parser| parser.eat_char('}'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_comment<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::Comment, |parser| parser.eat_char('#'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_anchor<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::Anchor, |parser| parser.eat_char('&'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_alias<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::Alias, |parser| parser.eat_char('*'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_literal<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::Literal, |parser| parser.eat_char('|'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_folded<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::Folded, |parser| parser.eat_char('>'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_single_quote<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::SingleQuote, |parser| parser.eat_char('\''))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_double_quote<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DoubleQuote, |parser| parser.eat_char('"'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_directive<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::Directive, |parser| parser.eat_char('%'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(char::non_break)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn b_break<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('\r').or(parser.eat_char('\n'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn b_as_line_feed<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::ScalarBreak, b_break)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn b_non_content<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::Break, b_break)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_space<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char(' ')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_white<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(char::space)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_whites<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    star_fast!(parser, s_white(parser));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(char::non_space)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_dec_digit<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(|ch| ch.is_ascii_digit())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_hex_digit<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(|ch| ch.is_ascii_hexdigit())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_word_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(|ch| ch.is_ascii_alphanumeric() || ch == '-')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_uri_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.is_char('%') {
        let start = parser.location();
        parser.eat_char('%').unwrap();
        if ns_hex_digit(parser).is_ok() && ns_hex_digit(parser).is_ok() {
            Ok(())
        } else {
            parser.diagnostics.push(Diagnostic {
                message: "invalid percent escape",
                span: parser.span(start),
            });
            Err(())
        }
    } else {
        parser.eat(|ch| {
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

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_tag_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.is_char('!') || parser.is(char::flow_indicator) {
        Err(())
    } else {
        ns_uri_char(parser)
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_escape<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('\\')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_null<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('0')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_bell<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('a')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_backspace<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('a')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_horizontal_tab<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(|ch| matches!(ch, 't' | '\t'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_line_feed<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('n')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_vertical_tab<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('v')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_form_feed<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('f')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_carriage_return<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('r')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_escape<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('e')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_space<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char(' ')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_double_quote<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('"')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_slash<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('/')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_backslash<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('\\')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_next_line<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('N')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_non_breaking_space<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('_')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_line_separator<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('L')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_paragraph_separator<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('P')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_8_bit<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    let start = parser.location();
    parser.eat_char('x')?;
    if (0..2).try_for_each(|_| ns_hex_digit(parser)).is_ok() {
        Ok(())
    } else {
        parser.diagnostics.push(Diagnostic {
            message: "invalid 8 bit escape",
            span: parser.span(start),
        });
        Err(())
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_16_bit<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    let start = parser.location();
    parser.eat_char('x')?;
    if (0..4).try_for_each(|_| ns_hex_digit(parser)).is_ok() {
        Ok(())
    } else {
        parser.diagnostics.push(Diagnostic {
            message: "invalid 16 bit escape",
            span: parser.span(start),
        });
        Err(())
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_esc_32_bit<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    let start = parser.location();
    parser.eat_char('x')?;
    if (0..8).try_for_each(|_| ns_hex_digit(parser)).is_ok() {
        Ok(())
    } else {
        parser.diagnostics.push(Diagnostic {
            message: "invalid 16 bit escape",
            span: parser.span(start),
        });
        Err(())
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_esc_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_escape(parser)?;
    alt_fast!(
        parser,
        ns_esc_null(parser),
        ns_esc_bell(parser),
        ns_esc_backspace(parser),
        ns_esc_horizontal_tab(parser),
        ns_esc_line_feed(parser),
        ns_esc_vertical_tab(parser),
        ns_esc_form_feed(parser),
        ns_esc_carriage_return(parser),
        ns_esc_escape(parser),
        ns_esc_space(parser),
        ns_esc_double_quote(parser),
        ns_esc_slash(parser),
        ns_esc_backslash(parser),
        ns_esc_next_line(parser),
        ns_esc_non_breaking_space(parser),
        ns_esc_line_separator(parser),
        ns_esc_paragraph_separator(parser),
        ns_esc_8_bit(parser),
        ns_esc_16_bit(parser),
        ns_esc_32_bit(parser)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_indent<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    parser.token(Token::Indent, |parser| {
        for _ in 0..n {
            s_space(parser)?;
        }
        Ok(())
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_indent_less_than<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    parser.token(Token::Indent, |parser| {
        for _ in 1..n {
            if s_space(parser).is_err() {
                return Ok(());
            }
        }
        Ok(())
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_indent_less_or_equal<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    parser.token(Token::Indent, |parser| {
        for _ in 0..n {
            if s_space(parser).is_err() {
                return Ok(());
            }
        }
        Ok(())
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_separate_in_line<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.is(char::space) {
        parser.token(Token::Separator, |parser| {
            Ok(star_fast!(parser, s_white(parser)))
        })
    } else if parser.is_start_of_line() {
        Ok(())
    } else {
        Err(())
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_line_prefix<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockIn | Context::BlockOut => s_block_line_prefix(parser, n),
        Context::FlowIn | Context::FlowOut => s_flow_line_prefix(parser, n),
        Context::BlockKey | Context::FlowKey => unimplemented!(),
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_block_line_prefix<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_flow_line_prefix<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    question!(parser, s_separate_in_line(parser));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_empty<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        parser,
        s_line_prefix(parser, n, c),
        s_indent_less_than(parser, n)
    )?;
    b_as_line_feed(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn b_l_trimmed<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    b_non_content(parser)?;
    star!(parser, l_empty(parser, n, c));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn b_as_space<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::ScalarSpace, b_break)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn b_l_folded<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(parser, b_l_trimmed(parser, n, c), b_as_space(parser))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_flow_folded<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    question_fast!(parser, s_separate_in_line(parser));
    b_l_folded(parser, n, Context::FlowIn)?;
    s_flow_line_prefix(parser, n)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_nb_comment_text<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_comment(parser)?;
    parser.token(Token::CommentText, |parser| {
        star_fast!(parser, nb_char(parser));
        Ok(())
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn b_comment<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if b_non_content(parser).is_ok() || parser.is_end_of_input() {
        Ok(())
    } else {
        Err(())
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_b_comment<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if s_separate_in_line(parser).is_ok() && parser.peek() == Some('#') {
        c_nb_comment_text(parser)?;
    }
    b_comment(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_comment<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    s_separate_in_line(parser)?;
    if parser.peek() == Some('#') {
        c_nb_comment_text(parser)?;
    }
    b_comment(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_l_comments<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if question!(parser, s_b_comment(parser)).is_some() || parser.is_start_of_line() {
        star!(parser, l_comment(parser));
        Ok(())
    } else {
        Err(())
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_separate<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockIn | Context::BlockOut | Context::FlowIn | Context::FlowOut => {
            s_separate_lines(parser, n)
        }
        Context::BlockKey | Context::FlowKey => s_separate_in_line(parser),
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_separate_lines<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn comments<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        s_l_comments(parser)?;
        s_flow_line_prefix(parser, n)
    }

    alt!(parser, comments(parser, n), s_separate_in_line(parser))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_directive<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_directive(parser)?;
    if parser.is_str("YAML ") {
        ns_yaml_directive(parser)?;
    } else if parser.is_str("TAG ") {
        ns_tag_directive(parser)?;
    } else {
        ns_reserved_directive(parser)?;
    }
    s_l_comments(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_reserved_directive<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    fn param<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        s_separate_in_line(parser)?;
        if parser.is_char('#') {
            return Err(());
        }
        ns_directive_parameter(parser)
    }

    ns_directive_name(parser)?;
    star!(parser, param(parser));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_directive_name<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DirectiveName, |parser| {
        plus_fast!(parser, ns_char(parser))
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_directive_parameter<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DirectiveParameter, |parser| {
        plus_fast!(parser, ns_char(parser))
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_yaml_directive<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DirectiveName, |parser| parser.eat_str("YAML"))?;
    s_separate_in_line(parser)?;
    ns_yaml_version(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_yaml_version<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::YamlVersion, |parser| {
        plus_fast!(parser, ns_dec_digit(parser))?;
        parser.eat_char('.')?;
        plus_fast!(parser, ns_dec_digit(parser))
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_tag_directive<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DirectiveName, |parser| parser.eat_str("TAG"))?;
    s_separate_in_line(parser)?;
    c_tag_handle(parser)?;
    s_separate_in_line(parser)?;
    ns_tag_prefix(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_tag_handle<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::TagHandle, |parser| {
        alt!(
            parser,
            c_named_tag_handle(parser),
            c_secondary_tag_handle(parser),
            c_primary_tag_handle(parser)
        )
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_primary_tag_handle<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('!')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_secondary_tag_handle<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_str("!!")
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_named_tag_handle<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('!')?;
    plus_fast!(parser, ns_word_char(parser))?;
    parser.eat_char('!')
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_tag_prefix<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::TagPrefix, |parser| {
        if parser.is_char('!') {
            c_ns_local_tag_prefix(parser)
        } else {
            ns_global_tag_prefix(parser)
        }
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_local_tag_prefix<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('!')?;
    star!(parser, ns_uri_char(parser));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_global_tag_prefix<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    ns_tag_char(parser)?;
    star!(parser, ns_uri_char(parser));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_properties<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    fn separated_anchor<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(parser, n, c)?;
        c_ns_anchor_property(parser)
    }

    fn separated_tag<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(parser, n, c)?;
        c_ns_tag_property(parser)
    }

    match parser.peek() {
        Some('!') => {
            c_ns_tag_property(parser)?;
            question!(parser, separated_anchor(parser, n, c));
            Ok(())
        }
        Some('&') => {
            c_ns_anchor_property(parser)?;
            question!(parser, separated_tag(parser, n, c));
            Ok(())
        }
        _ => Err(()),
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_tag_property<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.peek_next() == Some('<') {
        c_verbatim_tag(parser)
    } else {
        alt!(
            parser,
            c_ns_shorthand_tag(parser),
            c_non_specific_tag(parser)
        )
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_verbatim_tag<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::VerbatimTag, |parser| {
        parser.eat_str("!<")?;
        plus!(parser, ns_uri_char(parser))?;
        parser.eat_str(">")?;
        Ok(())
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_shorthand_tag<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_tag_handle(parser)?;
    parser.token(Token::TagSuffix, |parser| {
        plus!(parser, ns_tag_char(parser))
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_non_specific_tag<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::NonSpecificTag, |parser| parser.eat_char('!'))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_anchor_property<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_anchor(parser)?;
    ns_anchor_name(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_anchor_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.is(char::flow_indicator) {
        Err(())
    } else {
        ns_char(parser)
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_anchor_name<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::AnchorName, |parser| {
        plus_fast!(parser, ns_anchor_char(parser))
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_alias_node<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_alias(parser)?;
    ns_anchor_name(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn e_scalar<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::Empty, |_| Ok(()))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn e_node<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    e_scalar(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_double_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    match parser.peek() {
        Some('\\') => c_ns_esc_char(parser),
        Some('"') => Err(()),
        _ => nb_json(parser),
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_double_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.is(char::space) {
        Err(())
    } else {
        nb_double_char(parser)
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_double_quoted<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    c_double_quote(parser)?;
    nb_double_text(parser, n, c)?;
    c_double_quote(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_double_text<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockKey | Context::FlowKey => nb_double_one_line(parser),
        Context::FlowIn | Context::FlowOut => nb_double_multi_line(parser, n),
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_double_one_line<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DoubleQuoted, |parser| {
        star!(parser, nb_double_char(parser));
        Ok(())
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_double_escaped<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_whites(parser)?;
    c_escape(parser)?;
    b_non_content(parser)?;
    star!(parser, l_empty(parser, n, Context::FlowIn));
    s_flow_line_prefix(parser, n)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_double_break<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    alt!(
        parser,
        s_double_escaped(parser, n),
        s_flow_folded(parser, n)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_ns_double_in_line<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    fn char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        s_whites(parser)?;
        ns_double_char(parser)
    }

    star!(parser, char(parser));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_double_next_line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        parser.token(Token::DoubleQuoted, |parser| {
            ns_double_char(parser)?;
            nb_ns_double_in_line(parser)?;
            alt!(parser, s_double_next_line(parser, n), s_whites(parser))
        })
    }

    s_double_break(parser, n)?;
    question!(parser, line(parser, n));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_double_multi_line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    parser.token(Token::DoubleQuoted, nb_ns_double_in_line)?;
    alt!(parser, s_double_next_line(parser, n), s_whites(parser))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_quoted_quote<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_str("''")
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_single_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.is_char('\'') {
        c_quoted_quote(parser)
    } else {
        nb_json(parser)
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_single_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.is(char::space) {
        Err(())
    } else {
        nb_single_char(parser)
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_single_quoted<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    c_single_quote(parser)?;
    parser.token(Token::SingleQuoted, |parser| nb_single_text(parser, n, c))?;
    c_single_quote(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_single_text<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockKey | Context::FlowKey => nb_single_one_line(parser),
        Context::FlowIn | Context::FlowOut => nb_single_multi_line(parser, n),
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_single_one_line<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    star!(parser, nb_single_char(parser));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_ns_single_in_line<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    fn char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        s_whites(parser)?;
        ns_single_char(parser)
    }

    star!(parser, char(parser));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_single_next_line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        ns_double_char(parser)?;
        nb_ns_single_in_line(parser)?;
        alt!(parser, s_single_next_line(parser, n), s_whites(parser))
    }

    s_flow_folded(parser, n)?;
    question!(parser, line(parser, n));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_single_multi_line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    nb_ns_single_in_line(parser)?;
    alt!(parser, s_single_next_line(parser, n), s_whites(parser))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_plain_first<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    match parser.peek() {
        Some('?' | ':' | '-') if parser.next_is(|ch| char::plain_safe(ch, c)) => {
            parser.bump();
            Ok(())
        }
        Some(ch) if char::indicator(ch) => Err(()),
        _ => ns_char(parser),
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_plain_safe<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    parser.eat(|ch| char::plain_safe(ch, c))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_plain_char<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    match parser.peek() {
        Some('#') => {
            if parser.prev_is(char::non_space) {
                parser.bump();
                Ok(())
            } else {
                Err(())
            }
        }
        Some(':') => {
            if parser.next_is(|ch| char::plain_safe(ch, c)) {
                parser.bump();
                Ok(())
            } else {
                Err(())
            }
        }
        _ => ns_plain_safe(parser, c),
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_plain<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockKey | Context::FlowKey => ns_plain_one_line(parser, c),
        Context::FlowIn | Context::FlowOut => ns_plain_multi_line(parser, n, c),
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn nb_ns_plain_in_line<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    fn char<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
        s_whites(parser)?;
        ns_plain_char(parser, c)
    }

    star!(parser, char(parser, c));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_plain_one_line<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    parser.token(Token::Scalar, |parser| {
        ns_plain_first(parser, c)?;
        nb_ns_plain_in_line(parser, c)
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_ns_plain_next_line<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    s_flow_folded(parser, n)?;
    parser.token(Token::Scalar, |parser| {
        ns_plain_char(parser, c)?;
        nb_ns_plain_in_line(parser, c)
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_plain_multi_line<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    ns_plain_one_line(parser, c)?;
    star!(parser, s_ns_plain_next_line(parser, n, c));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_flow_sequence<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    c_sequence_start(parser)?;
    question!(parser, s_separate(parser, n, c));
    question!(parser, ns_s_flow_seq_entries(parser, n, c.in_flow()));
    c_sequence_end(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_s_flow_seq_entries<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        c_collect_entry(parser)?;
        question!(parser, s_separate(parser, n, c));
        // todo unroll recursion
        ns_s_flow_seq_entries(parser, n, c)
    }

    ns_flow_seq_entry(parser, n, c)?;
    question!(parser, s_separate(parser, n, c));
    question_fast!(parser, entry(parser, n, c));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_seq_entry<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        parser,
        ns_flow_pair(parser, n, c),
        ns_flow_node(parser, n, c)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_flow_mapping<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    c_mapping_start(parser)?;
    question!(parser, s_separate(parser, n, c));
    question!(parser, ns_s_flow_map_entries(parser, n, c.in_flow()));
    c_mapping_end(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_s_flow_map_entries<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        c_collect_entry(parser)?;
        question!(parser, s_separate(parser, n, c));
        question!(parser, ns_s_flow_map_entries(parser, n, c));
        Ok(())
    }

    ns_flow_map_entry(parser, n, c)?;
    question!(parser, s_separate(parser, n, c));
    question_fast!(parser, entry(parser, n, c));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_map_entry<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    if parser.is_char('?') && !parser.next_is(char::non_space) {
        c_mapping_key(parser)?;
        s_separate(parser, n, c)?;
        ns_flow_map_explicit_entry(parser, n, c)
    } else {
        ns_flow_map_implicit_entry(parser, n, c)
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_map_explicit_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn empty<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        e_node(parser)?;
        e_node(parser)
    }

    alt!(
        parser,
        ns_flow_map_implicit_entry(parser, n, c),
        empty(parser)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_map_implicit_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    alt!(
        parser,
        ns_flow_map_yaml_key_entry(parser, n, c),
        c_ns_flow_map_empty_key_entry(parser, n, c),
        c_ns_flow_map_json_key_entry(parser, n, c)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_map_yaml_key_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn value<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        question!(parser, s_separate(parser, n, c));
        c_ns_flow_map_separate_value(parser, n, c)
    }

    ns_flow_yaml_node(parser, n, c)?;
    alt!(parser, value(parser, n, c), e_node(parser))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_flow_map_empty_key_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    e_node(parser)?;
    c_ns_flow_map_separate_value(parser, n, c)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_flow_map_separate_value<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn node<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(parser, n, c)?;
        ns_flow_node(parser, n, c)
    }

    c_mapping_value(parser)?;
    if parser.is(|ch| char::plain_safe(ch, c)) {
        return Err(());
    }
    alt!(parser, node(parser, n, c), e_node(parser))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_flow_map_json_key_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn value<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        question!(parser, s_separate(parser, n, c));
        c_ns_flow_map_adjacent_value(parser, n, c)
    }

    c_flow_json_node(parser, n, c)?;
    alt!(parser, value(parser, n, c), e_node(parser))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_flow_map_adjacent_value<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn node<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        question!(parser, s_separate(parser, n, c));
        ns_flow_node(parser, n, c)
    }

    c_mapping_value(parser)?;
    alt!(parser, node(parser, n, c), e_node(parser))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_pair<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    if parser.is_char('?') && !parser.next_is(char::non_space) {
        c_mapping_key(parser)?;
        s_separate(parser, n, c)?;
        ns_flow_map_explicit_entry(parser, n, c)
    } else {
        ns_flow_pair_entry(parser, n, c)
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_pair_entry<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        parser,
        ns_flow_pair_yaml_key_entry(parser, n, c),
        c_ns_flow_map_empty_key_entry(parser, n, c),
        c_ns_flow_pair_json_key_entry(parser, n, c)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_pair_yaml_key_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    ns_s_implicit_yaml_key(parser, Context::FlowKey)?;
    c_ns_flow_map_separate_value(parser, n, c)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_ns_flow_pair_json_key_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    c_s_implicit_json_key(parser, Context::FlowKey)?;
    c_ns_flow_map_adjacent_value(parser, n, c)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_s_implicit_yaml_key<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    parser.with_length_limit(1024, |parser| {
        ns_flow_yaml_node(parser, 0, c)?;
        question!(parser, s_separate_in_line(parser));
        Ok(())
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_s_implicit_json_key<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    parser.with_length_limit(1024, |parser| {
        c_flow_json_node(parser, 0, c)?;
        question!(parser, s_separate_in_line(parser));
        Ok(())
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_yaml_content<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    ns_plain(parser, n, c)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_flow_json_content<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    match parser.peek() {
        Some('[') => c_flow_sequence(parser, n, c),
        Some('{') => c_flow_mapping(parser, n, c),
        Some('\'') => c_single_quoted(parser, n, c),
        Some('"') => c_double_quoted(parser, n, c),
        _ => Err(()),
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_content<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        parser,
        ns_flow_yaml_content(parser, n, c),
        c_flow_json_content(parser, n, c)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_yaml_node<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    fn content<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(parser, n, c)?;
        ns_flow_content(parser, n, c)
    }

    fn props<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        c_ns_properties(parser, n, c)?;
        alt!(parser, content(parser, n, c), e_scalar(parser))
    }

    alt!(
        parser,
        c_ns_alias_node(parser),
        ns_flow_yaml_content(parser, n, c),
        props(parser, n, c)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_flow_json_node<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    if parser.is(|ch| matches!(ch, '!' | '&')) {
        c_ns_properties(parser, n, c)?;
        s_separate(parser, n, c)?;
    }
    c_flow_json_content(parser, n, c)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_flow_node<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    fn content<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(parser, n, c)?;
        ns_flow_content(parser, n, c)
    }

    fn props<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        c_ns_properties(parser, n, c)?;
        alt!(parser, content(parser, n, c), e_scalar(parser))
    }

    alt!(
        parser,
        c_ns_alias_node(parser),
        ns_flow_content(parser, n, c),
        props(parser, n, c)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
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

    let m = m.unwrap_or_else(|| parser.detect_scalar_indent(n));
    Ok((m, t))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
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

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
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

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn b_chomped_last<R: Receiver>(parser: &mut Parser<R>, t: Chomping) -> Result<(), ()> {
    if parser.is_end_of_input() {
        Ok(())
    } else {
        match t {
            Chomping::Strip => b_non_content(parser),
            Chomping::Clip | Chomping::Keep => b_as_line_feed(parser),
        }
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_chomped_empty<R: Receiver>(parser: &mut Parser<R>, n: i32, t: Chomping) -> Result<(), ()> {
    match t {
        Chomping::Strip => l_strip_empty(parser, n),
        Chomping::Clip => l_strip_empty(parser, n),
        Chomping::Keep => l_keep_empty(parser, n),
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_strip_empty<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        s_indent_less_or_equal(parser, n)?;
        b_non_content(parser)
    }

    star!(parser, line(parser, n));
    question!(parser, l_trail_comments(parser, n));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_keep_empty<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    star!(parser, l_empty(parser, n, Context::BlockIn));
    question!(parser, l_trail_comments(parser, n));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_trail_comments<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_indent_less_than(parser, n)?;
    c_nb_comment_text(parser)?;
    b_comment(parser)?;
    star!(parser, l_comment(parser));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_l_literal<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    c_literal(parser)?;
    let (m, t) = c_b_block_header(parser, n)?;
    l_literal_content(parser, n + m, t)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_nb_literal_text<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    star!(parser, l_empty(parser, n, Context::BlockIn));
    s_indent(parser, n)?;
    parser.token(Token::Scalar, |parser| plus_fast!(parser, nb_char(parser)))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn b_nb_literal_next<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    b_as_line_feed(parser)?;
    l_nb_literal_text(parser, n)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_literal_content<R: Receiver>(parser: &mut Parser<R>, n: i32, t: Chomping) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32, t: Chomping) -> Result<(), ()> {
        l_nb_literal_text(parser, n)?;
        star!(parser, b_nb_literal_next(parser, n));
        b_chomped_last(parser, t)
    }

    question!(parser, line(parser, n, t));
    l_chomped_empty(parser, n, t)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_l_folded<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    c_folded(parser)?;
    let (m, t) = c_b_block_header(parser, n)?;
    l_folded_content(parser, n + m, t)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_nb_folded_text<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    parser.token(Token::Scalar, |parser| {
        ns_char(parser)?;
        star!(parser, nb_char(parser));
        Ok(())
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_nb_folded_lines<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        b_l_folded(parser, n, Context::BlockIn)?;
        s_nb_folded_text(parser, n)
    }

    s_nb_folded_text(parser, n)?;
    star!(parser, line(parser, n));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_nb_spaced_text<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    parser.token(Token::Scalar, |parser| {
        s_white(parser)?;
        star!(parser, nb_char(parser));
        Ok(())
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn b_l_spaced<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    b_as_line_feed(parser)?;
    star!(parser, l_empty(parser, n, Context::BlockIn));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_nb_spaced_lines<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        b_l_spaced(parser, n)?;
        s_nb_spaced_text(parser, n)
    }

    s_nb_spaced_text(parser, n)?;
    star!(parser, line(parser, n));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_nb_same_lines<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    star!(parser, l_empty(parser, n, Context::BlockIn));
    alt!(
        parser,
        l_nb_folded_lines(parser, n),
        l_nb_spaced_lines(parser, n)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_nb_diff_lines<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        b_as_line_feed(parser)?;
        l_nb_same_lines(parser, n)
    }

    l_nb_same_lines(parser, n)?;
    star!(parser, line(parser, n));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_folded_content<R: Receiver>(parser: &mut Parser<R>, n: i32, t: Chomping) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<R>, n: i32, t: Chomping) -> Result<(), ()> {
        l_nb_diff_lines(parser, n)?;
        b_chomped_last(parser, t)
    }

    question!(parser, line(parser, n, t));
    l_chomped_empty(parser, n, t)
}

#[cfg_attr(feature = "tracing", tracing::instrument(level = "info", skip(parser)))]
fn l_block_sequence<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        s_indent(parser, n)?;
        c_l_block_seq_entry(parser, n)
    }

    let m = parser.detect_collection_indent(n);
    plus!(parser, entry(parser, n + m))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_l_block_seq_entry<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    c_sequence_entry(parser)?;
    if parser.is(char::non_space) {
        return Err(());
    }
    s_l_block_indented(parser, n, Context::BlockIn)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_l_block_indented<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    fn collection<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        let m: i32 = parser.detect_compact_indent(n);
        s_indent(parser, m)?;
        alt!(
            parser,
            ns_l_compact_sequence(parser, n + 1 + m),
            ns_l_compact_mapping(parser, n + 1 + m)
        )
    }

    fn empty<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        e_node(parser)?;
        s_l_comments(parser)
    }

    alt!(
        parser,
        collection(parser, n),
        s_l_block_node(parser, n, c),
        empty(parser)
    )
}

#[cfg_attr(feature = "tracing", tracing::instrument(level = "info", skip(parser)))]
fn ns_l_compact_sequence<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        s_indent(parser, n)?;
        c_l_block_seq_entry(parser, n)
    }

    c_l_block_seq_entry(parser, n)?;
    star!(parser, entry(parser, n));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_block_mapping<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        parser.location();
        s_indent(parser, n)?;
        ns_l_block_map_entry(parser, n)
    }

    let m = parser.detect_collection_indent(n);
    plus!(parser, entry(parser, n + m))
}

#[cfg_attr(feature = "tracing", tracing::instrument(level = "info", skip(parser)))]
fn ns_l_block_map_entry<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    if parser.is_char('?') && !parser.next_is(char::non_space) {
        c_l_block_map_explicit_entry(parser, n)
    } else {
        ns_l_block_map_implicit_entry(parser, n)
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_l_block_map_explicit_entry<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    c_l_block_map_explicit_key(parser, n)?;
    alt!(
        parser,
        l_block_map_explicit_value(parser, n),
        e_node(parser)
    )
}

#[cfg_attr(feature = "tracing", tracing::instrument(level = "info", skip(parser)))]
fn c_l_block_map_explicit_key<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    c_mapping_key(parser)?;
    if parser.is(char::non_space) {
        return Err(());
    }
    s_l_block_indented(parser, n, Context::BlockOut)
}

#[cfg_attr(feature = "tracing", tracing::instrument(level = "info", skip(parser)))]
fn l_block_map_explicit_value<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    c_mapping_value(parser)?;
    if parser.is(char::non_space) {
        return Err(());
    }
    s_l_block_indented(parser, n, Context::BlockOut)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_l_block_map_implicit_entry<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    alt!(parser, ns_s_block_map_implicit_key(parser), e_node(parser))?;
    c_l_block_map_implicit_value(parser, n)
}

#[cfg_attr(feature = "tracing", tracing::instrument(level = "info", skip(parser)))]
fn ns_s_block_map_implicit_key<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    alt!(
        parser,
        c_s_implicit_json_key(parser, Context::BlockKey),
        ns_s_implicit_yaml_key(parser, Context::BlockKey)
    )
}

#[cfg_attr(feature = "tracing", tracing::instrument(level = "info", skip(parser)))]
fn c_l_block_map_implicit_value<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn empty<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        e_node(parser)?;
        s_l_comments(parser)
    }

    c_mapping_value(parser)?;
    if parser.is(char::non_space) {
        return Err(());
    }

    alt!(
        parser,
        s_l_block_node(parser, n, Context::BlockOut),
        empty(parser)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn ns_l_compact_mapping<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        s_indent(parser, n)?;
        ns_l_block_map_entry(parser, n)
    }

    ns_l_block_map_entry(parser, n)?;
    star!(parser, entry(parser, n));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_l_block_node<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        parser,
        s_l_block_in_block(parser, n, c),
        s_l_flow_in_block(parser, n)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_l_flow_in_block<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_separate(parser, n + 1, Context::FlowOut)?;
    ns_flow_node(parser, n + 1, Context::FlowOut)?;
    s_l_comments(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_l_block_in_block<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        parser,
        s_l_block_scalar(parser, n, c),
        s_l_block_collection(parser, n, c)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_l_block_scalar<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    s_separate(parser, n + 1, c)?;
    if parser.is(|ch| matches!(ch, '!' | '&')) {
        c_ns_properties(parser, n + 1, c)?;
        s_separate(parser, n + 1, c)?;
    }
    if parser.is_char('|') {
        c_l_literal(parser, n)
    } else if parser.is_char('>') {
        c_l_folded(parser, n)
    } else {
        Err(())
    }
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn s_l_block_collection<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    fn props<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        c_ns_properties(parser, n, c)?;
        s_l_comments(parser)
    }
    fn anchor<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        c_ns_anchor_property(parser)?;
        s_l_comments(parser)
    }
    fn tag<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        c_ns_tag_property(parser)?;
        s_l_comments(parser)
    }

    fn collection_props<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(parser, n, c)?;
        alt!(parser, props(state, n, c), anchor(state), tag(state))
    }

    question!(parser, collection_props(parser, n + 1, c));
    s_l_comments(parser)?;
    alt!(
        parser,
        l_block_sequence(parser, c.seq_spaces(n)),
        l_block_mapping(parser, n)
    )
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_document_prefix<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_byte_order_mark(parser)?;
    star!(parser, l_comment(parser));
    Ok(())
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_directives_end<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DirectivesEnd, |parser| parser.eat_str("---"))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn c_document_end<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DocumentEnd, |parser| {
        parser.eat_str("...")?;
        if parser.is(char::non_space) {
            return Err(());
        }
        Ok(())
    })
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_document_suffix<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_document_end(parser)?;
    s_l_comments(parser)
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_bare_document<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.document(|parser| s_l_block_node(parser, -1, Context::BlockIn))
}

#[cfg_attr(
    feature = "tracing",
    tracing::instrument(level = "trace", skip(parser))
)]
fn l_explicit_document<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    fn empty<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        e_node(parser)?;
        s_l_comments(parser)
    }

    c_directives_end(parser)?;
    alt!(parser, l_bare_document(parser), empty(parser))
}

// #[cfg_attr(feature = "tracing", tracing::instrument(level = "trace", skip(parser)))]
fn l_directive_document<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    while parser.is_char('%') {
        l_directive(parser)?;
    }
    l_explicit_document(parser)
}

// #[cfg_attr(feature = "tracing", tracing::instrument(level = "trace", skip(parser)))]
fn l_any_document<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    alt!(
        parser,
        l_directive_document(parser),
        l_bare_document(parser)
    )
}

// #[cfg_attr(feature = "tracing", tracing::instrument(level = "trace", skip(parser)))]
pub(super) fn l_yaml_stream<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    let mut terminated = true;
    loop {
        l_document_prefix(parser)?;
        let read_document = if !terminated {
            question!(parser, l_explicit_document(parser)).is_some()
        } else {
            question!(parser, l_any_document(parser)).is_some()
        };
        terminated = question!(parser, l_document_suffix(parser)).is_some();
        if !terminated && !read_document {
            if parser.is_end_of_input() {
                return Ok(());
            } else {
                #[cfg(feature = "tracing")]
                tracing::error!("remaining tokens {:?}", parser.iter.as_str());
                return Err(());
            }
        }
    }
}
