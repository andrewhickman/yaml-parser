use alloc::{
    borrow::{Cow, ToOwned},
    format,
};

use crate::{
    parser::{char, Chomping, Context, Diagnostic, EventOrToken, Parser, Properties},
    CollectionStyle, Event, Location, Receiver, ScalarStyle, Span, Token,
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

fn nb_json<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.eat(|ch| matches!(ch, '\x09' | '\x20'..='\u{10ffff}'))?;
    parser.value.push_range(parser.text, start..parser.offset());
    Ok(())
}

fn c_byte_order_mark<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if parser.is_char(char::BYTE_ORDER_MARK) {
        parser.token(Token::ByteOrderMark, |parser| {
            parser.eat_char(char::BYTE_ORDER_MARK)
        })
    } else {
        Ok(())
    }
}

fn c_sequence_entry<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::SequenceEntry, |parser| parser.eat_char('-'))
}

fn c_mapping_key<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::MappingKey, |parser| parser.eat_char('?'))
}

fn c_mapping_value<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::MappingValue, |parser| parser.eat_char(':'))
}

fn c_collect_entry<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::CollectionEntry, |parser| parser.eat_char(','))
}

fn c_sequence_start<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::SequenceStart, |parser| parser.eat_char('['))
}

fn c_sequence_end<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::SequenceEnd, |parser| parser.eat_char(']'))
}

fn c_mapping_start<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::MappingStart, |parser| parser.eat_char('{'))
}

fn c_mapping_end<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::MappingEnd, |parser| parser.eat_char('}'))
}

fn c_comment<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::Comment, |parser| parser.eat_char('#'))
}

fn c_anchor<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::Anchor, |parser| parser.eat_char('&'))
}

fn c_alias<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::Alias, |parser| parser.eat_char('*'))
}

fn c_literal<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::Literal, |parser| parser.eat_char('|'))
}

fn c_folded<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::Folded, |parser| parser.eat_char('>'))
}

fn c_single_quote<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::SingleQuote, |parser| parser.eat_char('\''))
}

fn c_double_quote<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::DoubleQuote, |parser| parser.eat_char('"'))
}

fn c_directive<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::Directive, |parser| parser.eat_char('%'))
}

fn nb_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.eat(char::non_break)
}

fn b_break<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.eat_char('\r').or(parser.eat_char('\n'))
}

fn b_as_line_feed<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.token(Token::ScalarBreak, b_break)?;
    if &parser.text[start..parser.offset()] == "\n" {
        parser.value.push_range(parser.text, start..parser.offset());
    } else {
        parser.value.push_char(parser.text, '\n');
    }
    Ok(())
}

fn b_non_content<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::Break, b_break)
}

fn s_space<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.eat_char(' ')
}

fn s_white<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.eat(char::space)
}

fn s_whites<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    star_fast!(parser, s_white(parser));
    Ok(())
}

fn ns_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.eat(char::non_space)
}

fn ns_dec_digit<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.eat(|ch| ch.is_ascii_digit())
}

fn ns_hex_digit<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.eat(|ch| ch.is_ascii_hexdigit())
}

fn ns_word_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.eat(char::word)
}

fn ns_uri_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if parser.is_char('%') {
        let start = parser.location();
        parser.eat_char('%').unwrap();
        if ns_hex_digit(parser).is_ok() && ns_hex_digit(parser).is_ok() {
            let hex = &parser.text[start.index..parser.offset()];
            let codepoint = u32::from_str_radix(hex, 16).unwrap();
            match char::try_from(codepoint) {
                Ok(ch) => {
                    parser.value.push_char(parser.text, ch);
                    Ok(())
                }
                Err(_) => {
                    parser.diagnostics.push(Diagnostic {
                        message: "invalid percent escape".to_owned(),
                        span: parser.span(start),
                    });
                    Err(())
                }
            }
        } else {
            parser.diagnostics.push(Diagnostic {
                message: "invalid percent escape".to_owned(),
                span: parser.span(start),
            });
            Err(())
        }
    } else {
        let start = parser.offset();
        parser.eat(|ch| {
            char::word(ch)
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
        })?;
        parser.value.push_range(parser.text, start..parser.offset());
        Ok(())
    }
}

fn ns_tag_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if parser.is_char('!') || parser.is(char::flow_indicator) {
        Err(())
    } else {
        ns_uri_char(parser)
    }
}

fn c_escape<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::Escape, |parser| parser.eat_char('\\'))
}

fn ns_esc_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
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
        Some('L') => '\u{2208}',
        Some('P') => '\u{2209}',
        _ => return Err(()),
    };

    parser.value.push_char(parser.text, ch);
    parser.bump();
    Ok(())
}

fn ns_esc_hex<R: Receiver>(parser: &mut Parser<'_, R>, initial: char, len: u32) -> Result<(), ()> {
    parser.eat_char(initial)?;
    let start = parser.location();
    if (0..len).try_for_each(|_| ns_hex_digit(parser)).is_ok() {
        let hex = &parser.text[start.index..parser.offset()];
        let codepoint: u32 = u32::from_str_radix(hex, 16).unwrap();
        match core::char::from_u32(codepoint) {
            Some(ch) => {
                parser.value.push_char(parser.text, ch);
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

fn c_ns_esc_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    c_escape(parser)?;
    parser.token(Token::EscapeCode, |parser| match parser.peek() {
        Some('x') => ns_esc_hex(parser, 'x', 8),
        Some('u') => ns_esc_hex(parser, 'u', 16),
        Some('U') => ns_esc_hex(parser, 'U', 32),
        _ => ns_esc_char(parser),
    })
}

fn s_indent<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    parser.token(Token::Indent, |parser| {
        for _ in 0..n {
            s_space(parser)?;
        }
        Ok(())
    })
}

fn s_indent_less_than<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    parser.token(Token::Indent, |parser| {
        for _ in 1..n {
            if s_space(parser).is_err() {
                return Ok(());
            }
        }
        Ok(())
    })
}

fn s_indent_less_or_equal<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    parser.token(Token::Indent, |parser| {
        for _ in 0..n {
            if s_space(parser).is_err() {
                return Ok(());
            }
        }
        Ok(())
    })
}

fn s_separate_in_line<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if parser.is(char::space) {
        parser.token(Token::Separator, s_whites)
    } else if parser.is_start_of_line() {
        Ok(())
    } else {
        Err(())
    }
}

fn s_line_prefix<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockIn | Context::BlockOut => s_block_line_prefix(parser, n),
        Context::FlowIn | Context::FlowOut => s_flow_line_prefix(parser, n),
        Context::BlockKey | Context::FlowKey => unimplemented!(),
    }
}

fn s_block_line_prefix<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)
}

fn s_flow_line_prefix<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    question!(parser, s_separate_in_line(parser));
    Ok(())
}

fn l_empty<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        parser,
        s_line_prefix(parser, n, c),
        s_indent_less_than(parser, n)
    )?;
    b_as_line_feed(parser)
}

fn b_l_trimmed<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    b_non_content(parser)?;
    plus!(parser, l_empty(parser, n, c))
}

fn b_as_space<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::ScalarSpace, b_break)?;
    parser.value.push_char(parser.text, ' ');
    Ok(())
}

fn b_l_folded<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(parser, b_l_trimmed(parser, n, c), b_as_space(parser))
}

fn s_flow_folded<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    question_fast!(parser, s_separate_in_line(parser));
    b_l_folded(parser, n, Context::FlowIn)?;
    s_flow_line_prefix(parser, n)
}

fn c_nb_comment_text<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    c_comment(parser)?;
    parser.token(Token::CommentText, |parser| {
        star_fast!(parser, nb_char(parser));
        Ok(())
    })
}

fn b_comment<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if b_non_content(parser).is_ok() || parser.is_end_of_input() {
        Ok(())
    } else {
        Err(())
    }
}

fn s_b_comment<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if s_separate_in_line(parser).is_ok() && parser.peek() == Some('#') {
        c_nb_comment_text(parser)?;
    }
    b_comment(parser)
}

fn l_comment<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    s_separate_in_line(parser)?;
    if parser.peek() == Some('#') {
        c_nb_comment_text(parser)?;
    }
    b_comment(parser)
}

fn s_l_comments<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if question!(parser, s_b_comment(parser)).is_some() || parser.is_start_of_line() {
        star!(parser, l_comment(parser));
        Ok(())
    } else {
        Err(())
    }
}

fn s_separate<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockIn | Context::BlockOut | Context::FlowIn | Context::FlowOut => {
            s_separate_lines(parser, n)
        }
        Context::BlockKey | Context::FlowKey => s_separate_in_line(parser),
    }
}

fn s_separate_lines<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn comments<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        s_l_comments(parser)?;
        s_flow_line_prefix(parser, n)
    }

    alt!(parser, comments(parser, n), s_separate_in_line(parser))
}

fn l_directive<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
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

fn ns_reserved_directive<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    fn param<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
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

fn ns_directive_name<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::DirectiveName, |parser| {
        plus_fast!(parser, ns_char(parser))
    })
}

fn ns_directive_parameter<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::DirectiveParameter, |parser| {
        plus_fast!(parser, ns_char(parser))
    })
}

fn ns_yaml_directive<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if parser.yaml_version.is_some() {
        return Err(());
    }

    parser.token(Token::DirectiveName, |parser| parser.eat_str("YAML"))?;
    s_separate_in_line(parser)?;
    ns_yaml_version(parser)
}

fn ns_yaml_version<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.token(Token::YamlVersion, |parser| {
        plus_fast!(parser, ns_dec_digit(parser))?;
        parser.eat_char('.')?;
        plus_fast!(parser, ns_dec_digit(parser))
    })?;
    let end = parser.offset();
    parser.yaml_version = Some(&parser.text[start..end]);
    Ok(())
}

fn ns_tag_directive<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::DirectiveName, |parser| parser.eat_str("TAG"))?;
    s_separate_in_line(parser)?;
    let start = parser.offset();
    c_tag_handle(parser)?;
    let tag_handle = &parser.text[start..parser.offset()];
    s_separate_in_line(parser)?;
    let prefix = ns_tag_prefix(parser)?;
    parser.tags.insert(tag_handle, prefix);
    Ok(())
}

fn c_tag_handle<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::TagHandle, |parser| {
        alt!(
            parser,
            c_named_tag_handle(parser),
            c_secondary_tag_handle(parser),
            c_primary_tag_handle(parser)
        )
    })
}

fn c_primary_tag_handle<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.eat_char('!')?;
    if let Some(tag) = parser.tags.get("!") {
        parser.value.push_str(parser.text, tag);
    } else {
        parser.value.push_range(parser.text, start..parser.offset());
    }
    Ok(())
}

fn c_secondary_tag_handle<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.eat_str("!!")?;
    if let Some(tag) = parser.tags.get("!!") {
        parser.value.push_str(parser.text, tag);
    } else {
        parser.value.push_range(parser.text, start..parser.offset());
    }
    Ok(())
}

fn c_named_tag_handle<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.location();
    parser.eat_char('!')?;
    plus_fast!(parser, ns_word_char(parser))?;
    parser.eat_char('!')?;

    if let Some(tag) = parser.tags.get(parser.value.as_str(parser.text)) {
        parser.value.push_str(parser.text, tag);
        Ok(())
    } else {
        parser.diagnostics.push(Diagnostic {
            message: format!("unknown tag {}", parser.value.as_str(parser.text)),
            span: parser.span(start),
        });
        Err(())
    }
}

fn ns_tag_prefix<'t, R: Receiver>(parser: &mut Parser<'t, R>) -> Result<Cow<'t, str>, ()> {
    parser.begin_value();
    parser.token(Token::TagPrefix, |parser| {
        if parser.is_char('!') {
            c_ns_local_tag_prefix(parser)
        } else {
            ns_global_tag_prefix(parser)
        }
    })?;
    Ok(parser.end_value())
}

fn c_ns_local_tag_prefix<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.eat_char('!')?;
    parser.value.push_range(parser.text, start..parser.offset());
    star!(parser, ns_uri_char(parser));
    Ok(())
}

fn ns_global_tag_prefix<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    ns_tag_char(parser)?;
    star!(parser, ns_uri_char(parser));
    Ok(())
}

fn c_ns_properties<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    n: i32,
    c: Context,
) -> Result<Properties<'t>, ()> {
    fn separated_anchor<'t, R: Receiver>(
        parser: &mut Parser<'t, R>,
        n: i32,
        c: Context,
    ) -> Result<Cow<'t, str>, ()> {
        s_separate(parser, n, c)?;
        c_ns_anchor_property(parser)
    }

    fn separated_tag<'t, R: Receiver>(
        parser: &mut Parser<'t, R>,
        n: i32,
        c: Context,
    ) -> Result<Cow<'t, str>, ()> {
        s_separate(parser, n, c)?;
        c_ns_tag_property(parser)
    }

    match parser.peek() {
        Some('!') => {
            let tag = c_ns_tag_property(parser)?;
            let anchor = question!(parser, separated_anchor(parser, n, c));
            Ok((anchor, Some(tag)))
        }
        Some('&') => {
            let anchor = c_ns_anchor_property(parser)?;
            let tag = question!(parser, separated_tag(parser, n, c));
            Ok((Some(anchor), tag))
        }
        _ => Err(()),
    }
}

fn c_ns_tag_property<'t, R: Receiver>(parser: &mut Parser<'t, R>) -> Result<Cow<'t, str>, ()> {
    parser.begin_value();
    if parser.peek_next() == Some('<') {
        c_verbatim_tag(parser)?
    } else {
        alt!(
            parser,
            c_ns_shorthand_tag(parser),
            c_non_specific_tag(parser)
        )?
    }
    Ok(parser.end_value())
}

fn c_verbatim_tag<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::VerbatimTag, |parser| {
        let start = parser.offset();
        parser.eat_str("!<")?;
        parser.value.push_range(parser.text, start..parser.offset());
        plus!(parser, ns_uri_char(parser))?;
        let start = parser.offset();
        parser.eat_str(">")?;
        parser.value.push_range(parser.text, start..parser.offset());
        Ok(())
    })
}

fn c_ns_shorthand_tag<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    c_tag_handle(parser)?;
    parser.token(Token::TagSuffix, |parser| {
        plus!(parser, ns_tag_char(parser))
    })
}

fn c_non_specific_tag<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.token(Token::NonSpecificTag, |parser| parser.eat_char('!'))?;
    Ok(())
}

fn c_ns_anchor_property<'t, R: Receiver>(parser: &mut Parser<'t, R>) -> Result<Cow<'t, str>, ()> {
    c_anchor(parser)?;
    ns_anchor_name(parser)
}

fn ns_anchor_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if parser.is(char::flow_indicator) {
        Err(())
    } else {
        ns_char(parser)
    }
}

fn ns_anchor_name<'t, R: Receiver>(parser: &mut Parser<'t, R>) -> Result<Cow<'t, str>, ()> {
    let start = parser.offset();
    parser.token(Token::AnchorName, |parser| {
        plus_fast!(parser, ns_anchor_char(parser))
    })?;
    Ok(Cow::Borrowed(&parser.text[start..parser.offset()]))
}

fn c_ns_alias_node<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.location();
    c_alias(parser)?;
    let name_start = parser.offset();
    ns_anchor_name(parser)?;
    parser.queue(
        EventOrToken::Event(Event::Alias {
            value: Cow::Borrowed(&parser.text[name_start..parser.offset()]),
        }),
        parser.span(start),
    );
    Ok(())
}

fn e_scalar<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    (anchor, tag): Properties<'t>,
) -> Result<(), ()> {
    parser.queue(
        EventOrToken::Event(Event::Scalar {
            style: ScalarStyle::Plain,
            value: Cow::Borrowed(""),
            anchor,
            tag,
        }),
        Span::empty(parser.location()),
    );
    parser.token(Token::Empty, |_| Ok(()))
}

fn e_node<'t, R: Receiver>(parser: &mut Parser<'t, R>, props: Properties<'t>) -> Result<(), ()> {
    e_scalar(parser, props)
}

fn nb_double_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    match parser.peek() {
        Some('\\') => c_ns_esc_char(parser),
        Some('"') => Err(()),
        _ => parser.token(Token::DoubleQuoted, |parser| nb_json(parser)),
    }
}

fn ns_double_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if parser.is(char::space) {
        Err(())
    } else {
        nb_double_char(parser)
    }
}

fn c_double_quoted<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    start: Location,
    (anchor, tag): Properties<'t>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    c_double_quote(parser)?;
    let value = nb_double_text(parser, n, c)?;
    c_double_quote(parser)?;
    parser.queue(
        EventOrToken::Event(Event::Scalar {
            style: ScalarStyle::DoubleQuoted,
            value,
            anchor,
            tag,
        }),
        parser.span(start),
    );
    Ok(())
}

fn nb_double_text<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    n: i32,
    c: Context,
) -> Result<Cow<'t, str>, ()> {
    parser.begin_value();
    match c {
        Context::BlockKey | Context::FlowKey => nb_double_one_line(parser)?,
        Context::FlowIn | Context::FlowOut => nb_double_multi_line(parser, n)?,
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    };
    Ok(parser.end_value())
}

fn nb_double_one_line<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    star!(parser, nb_double_char(parser));
    Ok(())
}

fn s_double_escaped<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    // should be joined to start of double quoted string
    let start = parser.offset();
    parser.token(Token::DoubleQuoted, s_whites)?;
    parser.value.push_range(parser.text, start..parser.offset());
    c_escape(parser)?;
    b_non_content(parser)?;
    star!(parser, l_empty(parser, n, Context::FlowIn));
    s_flow_line_prefix(parser, n)
}

fn s_double_break<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    alt!(
        parser,
        s_double_escaped(parser, n),
        s_flow_folded(parser, n)
    )
}

fn nb_ns_double_in_line<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    fn char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        let start = parser.offset();
        parser.token(Token::DoubleQuoted, s_whites)?;
        parser.value.push_range(parser.text, start..parser.offset());
        ns_double_char(parser)
    }

    star!(parser, char(parser));
    Ok(())
}

fn s_double_next_line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn trailing_whites<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        parser.token(Token::Separator, s_whites)
    }

    fn line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        ns_double_char(parser)?;
        nb_ns_double_in_line(parser)?;
        // todo unroll recursion
        alt!(
            parser,
            s_double_next_line(parser, n),
            trailing_whites(parser)
        )
    }

    s_double_break(parser, n)?;
    question!(parser, line(parser, n));
    Ok(())
}

fn nb_double_multi_line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn trailing_whites<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        parser.token(Token::Separator, s_whites)
    }

    nb_ns_double_in_line(parser)?;
    alt!(
        parser,
        s_double_next_line(parser, n),
        trailing_whites(parser)
    )
}

fn c_quoted_quote<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::QuotedQuote, |parser| parser.eat_str("''"))?;
    parser.value.push_char(parser.text, '\'');
    Ok(())
}

fn nb_single_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if parser.is_char('\'') {
        c_quoted_quote(parser)
    } else {
        parser.token(Token::SingleQuoted, |parser| nb_json(parser))
    }
}

fn ns_single_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if parser.is(char::space) {
        Err(())
    } else {
        nb_single_char(parser)
    }
}

fn c_single_quoted<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    start: Location,
    (anchor, tag): Properties<'t>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    c_single_quote(parser)?;
    let value = nb_single_text(parser, n, c)?;
    c_single_quote(parser)?;
    parser.queue(
        EventOrToken::Event(Event::Scalar {
            style: ScalarStyle::SingleQuoted,
            value,
            anchor,
            tag,
        }),
        parser.span(start),
    );
    Ok(())
}

fn nb_single_text<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    n: i32,
    c: Context,
) -> Result<Cow<'t, str>, ()> {
    parser.begin_value();
    match c {
        Context::BlockKey | Context::FlowKey => nb_single_one_line(parser)?,
        Context::FlowIn | Context::FlowOut => nb_single_multi_line(parser, n)?,
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    };
    Ok(parser.end_value())
}

fn nb_single_one_line<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    star!(parser, nb_single_char(parser));
    Ok(())
}

fn nb_ns_single_in_line<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    fn char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        parser.token(Token::SingleQuoted, s_whites)?;
        ns_single_char(parser)
    }

    star!(parser, char(parser));
    Ok(())
}

fn s_single_next_line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn trailing_whites<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        parser.token(Token::Separator, s_whites)
    }

    fn line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        ns_single_char(parser)?;
        nb_ns_single_in_line(parser)?;
        alt!(
            parser,
            s_single_next_line(parser, n),
            trailing_whites(parser)
        )
    }

    s_flow_folded(parser, n)?;
    question!(parser, line(parser, n));
    Ok(())
}

fn nb_single_multi_line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn trailing_whites<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        parser.token(Token::Separator, s_whites)
    }

    nb_ns_single_in_line(parser)?;
    alt!(
        parser,
        s_single_next_line(parser, n),
        trailing_whites(parser)
    )
}

fn ns_plain_first<R: Receiver>(parser: &mut Parser<'_, R>, c: Context) -> Result<(), ()> {
    let start = parser.offset();
    match parser.peek() {
        Some('?' | ':' | '-') if parser.next_is(|ch| char::plain_safe(ch, c)) => {
            parser.bump();
        }
        Some(ch) if char::indicator(ch) => return Err(()),
        _ => ns_char(parser)?,
    }
    parser.value.push_range(parser.text, start..parser.offset());
    Ok(())
}

fn ns_plain_safe<R: Receiver>(parser: &mut Parser<'_, R>, c: Context) -> Result<(), ()> {
    parser.eat(|ch| char::plain_safe(ch, c))
}

fn ns_plain_char<R: Receiver>(parser: &mut Parser<'_, R>, c: Context) -> Result<(), ()> {
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
    parser.value.push_range(parser.text, start..parser.offset());
    Ok(())
}

fn ns_plain<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    n: i32,
    c: Context,
) -> Result<Cow<'t, str>, ()> {
    parser.begin_value();
    match c {
        Context::BlockKey | Context::FlowKey => ns_plain_one_line(parser, c)?,
        Context::FlowIn | Context::FlowOut => ns_plain_multi_line(parser, n, c)?,
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    }
    Ok(parser.end_value())
}

fn nb_ns_plain_in_line<R: Receiver>(parser: &mut Parser<'_, R>, c: Context) -> Result<(), ()> {
    fn char<R: Receiver>(parser: &mut Parser<'_, R>, c: Context) -> Result<(), ()> {
        let start = parser.offset();
        s_whites(parser)?;
        parser.value.push_range(parser.text, start..parser.offset());
        ns_plain_char(parser, c)
    }

    star!(parser, char(parser, c));
    Ok(())
}

fn ns_plain_one_line<R: Receiver>(parser: &mut Parser<'_, R>, c: Context) -> Result<(), ()> {
    parser.token(Token::Scalar, |parser| {
        ns_plain_first(parser, c)?;
        nb_ns_plain_in_line(parser, c)
    })
}

fn s_ns_plain_next_line<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    s_flow_folded(parser, n)?;
    parser.token(Token::Scalar, |parser| {
        ns_plain_char(parser, c)?;
        nb_ns_plain_in_line(parser, c)
    })
}

fn ns_plain_multi_line<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    ns_plain_one_line(parser, c)?;
    star!(parser, s_ns_plain_next_line(parser, n, c));
    Ok(())
}

fn c_flow_sequence<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    start: Location,
    (anchor, tag): Properties<'t>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    c_sequence_start(parser)?;
    parser.queue(
        EventOrToken::Event(Event::SequenceStart {
            style: CollectionStyle::Flow,
            anchor,
            tag,
        }),
        parser.span(start),
    );
    question!(parser, s_separate(parser, n, c));
    question!(parser, ns_s_flow_seq_entries(parser, n, c.in_flow()));
    let end_start = parser.location();
    c_sequence_end(parser)?;
    parser.queue(
        EventOrToken::Event(Event::SequenceEnd),
        parser.span(end_start),
    );
    Ok(())
}

fn ns_s_flow_seq_entries<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    ns_flow_seq_entry(parser, n, c)?;
    question!(parser, s_separate(parser, n, c));
    while parser.is_char(',') {
        c_collect_entry(parser)?;
        question!(parser, s_separate(parser, n, c));
        if question!(parser, ns_flow_seq_entry(parser, n, c)).is_some() {
            question!(parser, s_separate(parser, n, c));
        } else {
            break;
        }
    }
    Ok(())
}

fn ns_flow_seq_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    alt!(
        parser,
        ns_flow_pair(parser, n, c),
        ns_flow_node(parser, n, c)
    )
}

fn c_flow_mapping<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    start: Location,
    (anchor, tag): Properties<'t>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    c_mapping_start(parser)?;
    parser.queue(
        EventOrToken::Event(Event::MappingStart {
            style: CollectionStyle::Flow,
            anchor,
            tag,
        }),
        parser.span(start),
    );
    question!(parser, s_separate(parser, n, c));
    question!(parser, ns_s_flow_map_entries(parser, n, c.in_flow()));
    let end_start = parser.location();
    c_mapping_end(parser)?;
    parser.queue(
        EventOrToken::Event(Event::MappingEnd),
        parser.span(end_start),
    );
    Ok(())
}

fn ns_s_flow_map_entries<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    ns_flow_map_entry(parser, n, c)?;
    question!(parser, s_separate(parser, n, c));
    while parser.is_char(',') {
        c_collect_entry(parser)?;
        question!(parser, s_separate(parser, n, c));
        if question!(parser, ns_flow_map_entry(parser, n, c)).is_some() {
            question!(parser, s_separate(parser, n, c));
        } else {
            break;
        }
    }
    Ok(())
}

fn ns_flow_map_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    if parser.is_char('?') && !parser.next_is(char::non_space) {
        c_mapping_key(parser)?;
        s_separate(parser, n, c)?;
        ns_flow_map_explicit_entry(parser, n, c)
    } else {
        ns_flow_map_implicit_entry(parser, n, c)
    }
}

fn ns_flow_map_explicit_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn empty<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        e_node(parser, (None, None))?;
        e_node(parser, (None, None))
    }

    alt!(
        parser,
        ns_flow_map_implicit_entry(parser, n, c),
        empty(parser)
    )
}

fn ns_flow_map_implicit_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
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

fn ns_flow_map_yaml_key_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn value<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
        question!(parser, s_separate(parser, n, c));
        c_ns_flow_map_separate_value(parser, n, c)
    }

    ns_flow_yaml_node(parser, n, c)?;
    alt!(parser, value(parser, n, c), e_node(parser, (None, None)))
}

fn c_ns_flow_map_empty_key_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    e_node(parser, (None, None))?;
    c_ns_flow_map_separate_value(parser, n, c)
}

fn c_ns_flow_map_separate_value<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn node<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(parser, n, c)?;
        ns_flow_node(parser, n, c)
    }

    c_mapping_value(parser)?;
    if parser.is(|ch| char::plain_safe(ch, c)) {
        return Err(());
    }
    alt!(parser, node(parser, n, c), e_node(parser, (None, None)))
}

fn c_ns_flow_map_json_key_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn value<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
        question!(parser, s_separate(parser, n, c));
        c_ns_flow_map_adjacent_value(parser, n, c)
    }

    c_flow_json_node(parser, n, c)?;
    alt!(parser, value(parser, n, c), e_node(parser, (None, None)))
}

fn c_ns_flow_map_adjacent_value<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn node<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
        question!(parser, s_separate(parser, n, c));
        ns_flow_node(parser, n, c)
    }

    c_mapping_value(parser)?;
    alt!(parser, node(parser, n, c), e_node(parser, (None, None)))
}

fn ns_flow_pair<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    if parser.is_char('?') && !parser.next_is(char::non_space) {
        c_mapping_key(parser)?;
        s_separate(parser, n, c)?;
        ns_flow_map_explicit_entry(parser, n, c)
    } else {
        ns_flow_pair_entry(parser, n, c)
    }
}

fn ns_flow_pair_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    alt!(
        parser,
        ns_flow_pair_yaml_key_entry(parser, n, c),
        c_ns_flow_map_empty_key_entry(parser, n, c),
        c_ns_flow_pair_json_key_entry(parser, n, c)
    )
}

fn ns_flow_pair_yaml_key_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    ns_s_implicit_yaml_key(parser, Context::FlowKey)?;
    c_ns_flow_map_separate_value(parser, n, c)
}

fn c_ns_flow_pair_json_key_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    c_s_implicit_json_key(parser, Context::FlowKey)?;
    c_ns_flow_map_adjacent_value(parser, n, c)
}

fn ns_s_implicit_yaml_key<R: Receiver>(parser: &mut Parser<'_, R>, c: Context) -> Result<(), ()> {
    parser.with_length_limit(1024, |parser| {
        ns_flow_yaml_node(parser, 0, c)?;
        question!(parser, s_separate_in_line(parser));
        Ok(())
    })
}

fn c_s_implicit_json_key<R: Receiver>(parser: &mut Parser<'_, R>, c: Context) -> Result<(), ()> {
    parser.with_length_limit(1024, |parser| {
        c_flow_json_node(parser, 0, c)?;
        question!(parser, s_separate_in_line(parser));
        Ok(())
    })
}

fn ns_flow_yaml_content<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    start: Location,
    (anchor, tag): Properties<'t>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    let value = ns_plain(parser, n, c)?;
    parser.queue(
        EventOrToken::Event(Event::Scalar {
            style: ScalarStyle::Plain,
            value,
            anchor,
            tag,
        }),
        parser.span(start),
    );
    Ok(())
}

fn c_flow_json_content<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    start: Location,
    props: Properties<'t>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    match parser.peek() {
        Some('[') => c_flow_sequence(parser, start, props, n, c),
        Some('{') => c_flow_mapping(parser, start, props, n, c),
        Some('\'') => c_single_quoted(parser, start, props, n, c),
        Some('"') => c_double_quoted(parser, start, props, n, c),
        _ => Err(()),
    }
}

fn ns_flow_content<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    start: Location,
    props: Properties<'t>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    alt!(
        parser,
        ns_flow_yaml_content(parser, start, props.clone(), n, c),
        c_flow_json_content(parser, start, props.clone(), n, c)
    )
}

fn ns_flow_yaml_node<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn content<'t, R: Receiver>(
        parser: &mut Parser<'t, R>,
        start: Location,
        props: Properties<'t>,
        n: i32,
        c: Context,
    ) -> Result<(), ()> {
        s_separate(parser, n, c)?;
        ns_flow_content(parser, start, props, n, c)
    }

    fn props<R: Receiver>(
        parser: &mut Parser<'_, R>,
        start: Location,
        n: i32,
        c: Context,
    ) -> Result<(), ()> {
        let props = c_ns_properties(parser, n, c)?;
        alt!(
            parser,
            content(parser, start, props.clone(), n, c),
            e_scalar(parser, props.clone())
        )
    }

    let start = parser.location();
    alt!(
        parser,
        c_ns_alias_node(parser),
        ns_flow_yaml_content(parser, start, (None, None), n, c),
        props(parser, start, n, c)
    )
}

fn c_flow_json_node<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    let start = parser.location();
    let props = if parser.is(|ch| matches!(ch, '!' | '&')) {
        let props = c_ns_properties(parser, n, c)?;
        s_separate(parser, n, c)?;
        props
    } else {
        (None, None)
    };
    c_flow_json_content(parser, start, props, n, c)
}

fn ns_flow_node<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    fn content<'t, R: Receiver>(
        parser: &mut Parser<'t, R>,
        start: Location,
        props: Properties<'t>,
        n: i32,
        c: Context,
    ) -> Result<(), ()> {
        s_separate(parser, n, c)?;
        ns_flow_content(parser, start, props, n, c)
    }

    fn props<R: Receiver>(
        parser: &mut Parser<'_, R>,
        start: Location,
        n: i32,
        c: Context,
    ) -> Result<(), ()> {
        let props = c_ns_properties(parser, n, c)?;
        alt!(
            parser,
            content(parser, start, props.clone(), n, c),
            e_scalar(parser, props.clone())
        )
    }

    let start = parser.location();
    alt!(
        parser,
        c_ns_alias_node(parser),
        ns_flow_content(parser, start, (None, None), n, c),
        props(parser, start, n, c)
    )
}

fn c_b_block_header<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
) -> Result<(i32, Chomping), ()> {
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

fn c_indentation_indicator<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<Option<i32>, ()> {
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

fn c_chomping_indicator<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<Chomping, ()> {
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

fn b_chomped_last<R: Receiver>(parser: &mut Parser<'_, R>, t: Chomping) -> Result<(), ()> {
    if parser.is_end_of_input() {
        Ok(())
    } else {
        match t {
            Chomping::Strip => b_non_content(parser),
            Chomping::Clip | Chomping::Keep => b_as_line_feed(parser),
        }
    }
}

fn l_chomped_empty<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, t: Chomping) -> Result<(), ()> {
    match t {
        Chomping::Strip | Chomping::Clip => l_strip_empty(parser, n),
        Chomping::Keep => l_keep_empty(parser, n),
    }
}

fn l_strip_empty<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        s_indent_less_or_equal(parser, n)?;
        b_non_content(parser)
    }

    star!(parser, line(parser, n));
    question!(parser, l_trail_comments(parser, n));
    Ok(())
}

fn l_keep_empty<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    star!(parser, l_empty(parser, n, Context::BlockIn));
    question!(parser, l_trail_comments(parser, n));
    Ok(())
}

fn l_trail_comments<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    s_indent_less_than(parser, n)?;
    c_nb_comment_text(parser)?;
    b_comment(parser)?;
    star!(parser, l_comment(parser));
    Ok(())
}

fn c_l_literal<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    start: Location,
    (anchor, tag): Properties<'t>,
    n: i32,
) -> Result<(), ()> {
    c_literal(parser)?;
    let (m, t) = c_b_block_header(parser, n)?;
    let value = l_literal_content(parser, n + m, t)?;
    parser.queue(
        EventOrToken::Event(Event::Scalar {
            style: ScalarStyle::Literal,
            value,
            anchor,
            tag,
        }),
        parser.span(start),
    );
    Ok(())
}

fn l_nb_literal_text<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    star!(parser, l_empty(parser, n, Context::BlockIn));
    s_indent(parser, n)?;
    let start = parser.offset();
    parser.token(Token::Scalar, |parser| plus_fast!(parser, nb_char(parser)))?;
    parser.value.push_range(parser.text, start..parser.offset());
    Ok(())
}

fn b_nb_literal_next<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    b_as_line_feed(parser)?;
    l_nb_literal_text(parser, n)
}

fn l_literal_content<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    n: i32,
    t: Chomping,
) -> Result<Cow<'t, str>, ()> {
    fn line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, t: Chomping) -> Result<(), ()> {
        l_nb_literal_text(parser, n)?;
        star!(parser, b_nb_literal_next(parser, n));
        b_chomped_last(parser, t)
    }

    parser.begin_value();
    question!(parser, line(parser, n, t));
    l_chomped_empty(parser, n, t)?;
    Ok(parser.end_value())
}

fn c_l_folded<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    start: Location,
    (anchor, tag): Properties<'t>,
    n: i32,
) -> Result<(), ()> {
    c_folded(parser)?;
    let (m, t) = c_b_block_header(parser, n)?;
    let value = l_folded_content(parser, n + m, t)?;
    parser.queue(
        EventOrToken::Event(Event::Scalar {
            style: ScalarStyle::Folded,
            value,
            anchor,
            tag,
        }),
        parser.span(start),
    );
    Ok(())
}

fn s_nb_folded_text<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    let start = parser.offset();
    parser.token(Token::Scalar, |parser| {
        ns_char(parser)?;
        star!(parser, nb_char(parser));
        Ok(())
    })?;
    parser.value.push_range(parser.text, start..parser.offset());
    Ok(())
}

fn l_nb_folded_lines<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        b_l_folded(parser, n, Context::BlockIn)?;
        s_nb_folded_text(parser, n)
    }

    s_nb_folded_text(parser, n)?;
    star!(parser, line(parser, n));
    Ok(())
}

fn s_nb_spaced_text<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    let start = parser.offset();
    parser.token(Token::Scalar, |parser| {
        s_white(parser)?;
        star!(parser, nb_char(parser));
        Ok(())
    })?;
    parser.value.push_range(parser.text, start..parser.offset());
    Ok(())
}

fn b_l_spaced<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    b_as_line_feed(parser)?;
    star!(parser, l_empty(parser, n, Context::BlockIn));
    Ok(())
}

fn l_nb_spaced_lines<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        b_l_spaced(parser, n)?;
        s_nb_spaced_text(parser, n)
    }

    s_nb_spaced_text(parser, n)?;
    star!(parser, line(parser, n));
    Ok(())
}

fn l_nb_same_lines<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    star!(parser, l_empty(parser, n, Context::BlockIn));
    alt!(
        parser,
        l_nb_folded_lines(parser, n),
        l_nb_spaced_lines(parser, n)
    )
}

fn l_nb_diff_lines<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        b_as_line_feed(parser)?;
        l_nb_same_lines(parser, n)
    }

    l_nb_same_lines(parser, n)?;
    star!(parser, line(parser, n));
    Ok(())
}

fn l_folded_content<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    n: i32,
    t: Chomping,
) -> Result<Cow<'t, str>, ()> {
    fn line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, t: Chomping) -> Result<(), ()> {
        l_nb_diff_lines(parser, n)?;
        b_chomped_last(parser, t)
    }

    parser.begin_value();
    question!(parser, line(parser, n, t));
    l_chomped_empty(parser, n, t)?;
    Ok(parser.end_value())
}

fn l_block_sequence<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    start: Location,
    (anchor, tag): Properties<'t>,
    n: i32,
) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        s_indent(parser, n)?;
        c_l_block_seq_entry(parser, n)
    }

    let m = parser.detect_collection_indent(n);
    parser.queue(
        EventOrToken::Event(Event::SequenceStart {
            style: CollectionStyle::Block,
            anchor,
            tag,
        }),
        parser.span(start),
    );
    plus!(parser, entry(parser, n + m))?;
    parser.queue(
        EventOrToken::Event(Event::SequenceEnd),
        Span::empty(parser.location()),
    );
    Ok(())
}

fn c_l_block_seq_entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    c_sequence_entry(parser)?;
    if parser.is(char::non_space) {
        return Err(());
    }
    s_l_block_indented(parser, n, Context::BlockIn)
}

fn s_l_block_indented<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn collection<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        let m: i32 = parser.detect_compact_indent(n);
        s_indent(parser, m)?;
        alt!(
            parser,
            ns_l_compact_sequence(parser, n + 1 + m),
            ns_l_compact_mapping(parser, n + 1 + m)
        )
    }

    fn empty<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        e_node(parser, (None, None))?;
        s_l_comments(parser)
    }

    alt!(
        parser,
        collection(parser, n),
        s_l_block_node(parser, n, c),
        empty(parser)
    )
}

fn ns_l_compact_sequence<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        s_indent(parser, n)?;
        c_l_block_seq_entry(parser, n)
    }

    parser.queue(
        EventOrToken::Event(Event::SequenceStart {
            style: CollectionStyle::Block,
            anchor: None,
            tag: None,
        }),
        Span::empty(parser.location()),
    );
    c_l_block_seq_entry(parser, n)?;
    star!(parser, entry(parser, n));
    parser.queue(
        EventOrToken::Event(Event::SequenceEnd),
        Span::empty(parser.location()),
    );
    Ok(())
}

fn l_block_mapping<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    start: Location,
    (anchor, tag): Properties<'t>,
    n: i32,
) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        s_indent(parser, n)?;
        ns_l_block_map_entry(parser, n)
    }

    let m = parser.detect_collection_indent(n);
    parser.queue(
        EventOrToken::Event(Event::MappingStart {
            style: CollectionStyle::Block,
            anchor,
            tag,
        }),
        parser.span(start),
    );
    plus!(parser, entry(parser, n + m))?;
    parser.queue(
        EventOrToken::Event(Event::MappingEnd),
        Span::empty(parser.location()),
    );
    Ok(())
}

fn ns_l_block_map_entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    if parser.is_char('?') && !parser.next_is(char::non_space) {
        c_l_block_map_explicit_entry(parser, n)
    } else {
        ns_l_block_map_implicit_entry(parser, n)
    }
}

fn c_l_block_map_explicit_entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    c_l_block_map_explicit_key(parser, n)?;
    alt!(
        parser,
        l_block_map_explicit_value(parser, n),
        e_node(parser, (None, None))
    )
}

fn c_l_block_map_explicit_key<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    c_mapping_key(parser)?;
    if parser.is(char::non_space) {
        return Err(());
    }
    s_l_block_indented(parser, n, Context::BlockOut)
}

fn l_block_map_explicit_value<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    c_mapping_value(parser)?;
    if parser.is(char::non_space) {
        return Err(());
    }
    s_l_block_indented(parser, n, Context::BlockOut)
}

fn ns_l_block_map_implicit_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
) -> Result<(), ()> {
    alt!(
        parser,
        ns_s_block_map_implicit_key(parser),
        e_node(parser, (None, None))
    )?;
    c_l_block_map_implicit_value(parser, n)
}

fn ns_s_block_map_implicit_key<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    alt!(
        parser,
        c_s_implicit_json_key(parser, Context::BlockKey),
        ns_s_implicit_yaml_key(parser, Context::BlockKey)
    )
}

fn c_l_block_map_implicit_value<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn empty<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        e_node(parser, (None, None))?;
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

fn ns_l_compact_mapping<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        s_indent(parser, n)?;
        ns_l_block_map_entry(parser, n)
    }

    parser.queue(
        EventOrToken::Event(Event::MappingStart {
            style: CollectionStyle::Block,
            anchor: None,
            tag: None,
        }),
        Span::empty(parser.location()),
    );
    ns_l_block_map_entry(parser, n)?;
    star!(parser, entry(parser, n));
    parser.queue(
        EventOrToken::Event(Event::MappingEnd),
        Span::empty(parser.location()),
    );
    Ok(())
}

fn s_l_block_node<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        parser,
        s_l_block_in_block(parser, n, c),
        s_l_flow_in_block(parser, n)
    )
}

fn s_l_flow_in_block<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    s_separate(parser, n + 1, Context::FlowOut)?;
    ns_flow_node(parser, n + 1, Context::FlowOut)?;
    s_l_comments(parser)
}

fn s_l_block_in_block<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    alt!(
        parser,
        s_l_block_scalar(parser, n, c),
        s_l_block_collection(parser, n, c)
    )
}

fn s_l_block_scalar<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    s_separate(parser, n + 1, c)?;
    let start = parser.location();
    let props = if parser.is(|ch| matches!(ch, '!' | '&')) {
        let props = c_ns_properties(parser, n + 1, c)?;
        s_separate(parser, n + 1, c)?;
        props
    } else {
        (None, None)
    };
    if parser.is_char('|') {
        c_l_literal(parser, start, props, n)?;
    } else if parser.is_char('>') {
        c_l_folded(parser, start, props, n)?;
    } else {
        return Err(());
    }
    Ok(())
}

fn s_l_block_collection<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn props<'t, R: Receiver>(
        parser: &mut Parser<'t, R>,
        n: i32,
        c: Context,
    ) -> Result<Properties<'t>, ()> {
        let props = c_ns_properties(parser, n, c)?;
        s_l_comments(parser)?;
        Ok(props)
    }
    fn anchor<'t, R: Receiver>(parser: &mut Parser<'t, R>) -> Result<Properties<'t>, ()> {
        let anchor = c_ns_anchor_property(parser)?;
        s_l_comments(parser)?;
        Ok((Some(anchor), None))
    }
    fn tag<'t, R: Receiver>(parser: &mut Parser<'t, R>) -> Result<Properties<'t>, ()> {
        let tag = c_ns_tag_property(parser)?;
        s_l_comments(parser)?;
        Ok((None, Some(tag)))
    }

    fn collection_props<'t, R: Receiver>(
        parser: &mut Parser<'t, R>,
        n: i32,
        c: Context,
    ) -> Result<(Location, Properties<'t>), ()> {
        s_separate(parser, n, c)?;
        let start = parser.location();
        let props = alt!(parser, props(state, n, c), anchor(state), tag(state))?;
        Ok((start, props))
    }

    let (start, props) = match question!(parser, collection_props(parser, n + 1, c)) {
        Some((start, props)) => (Some(start), props),
        None => (None, (None, None)),
    };
    s_l_comments(parser)?;
    let start = start.unwrap_or_else(|| parser.location());
    alt!(
        parser,
        l_block_sequence(parser, start, props.clone(), c.seq_spaces(n)),
        l_block_mapping(parser, start, props.clone(), n)
    )
}

fn l_document_prefix<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    c_byte_order_mark(parser)?;
    star!(parser, l_comment(parser));
    Ok(())
}

fn c_directives_end<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.location();
    parser.token(Token::DirectivesEnd, |parser| parser.eat_str("---"))?;
    parser.queue(
        EventOrToken::Event(Event::DocumentStart {
            version: parser.yaml_version.map(Cow::Borrowed),
        }),
        parser.span(start),
    );
    Ok(())
}

fn c_document_end<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.location();
    parser.token(Token::DocumentEnd, |parser| {
        parser.eat_str("...")?;
        if parser.is(char::non_space) {
            return Err(());
        }
        Ok(())
    })?;
    parser.queue(EventOrToken::Event(Event::DocumentEnd), parser.span(start));
    Ok(())
}

fn l_document_suffix<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    c_document_end(parser)?;
    s_l_comments(parser)
}

fn l_bare_document<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.document(|parser| s_l_block_node(parser, -1, Context::BlockIn))
}

fn l_explicit_document<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    fn empty<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        e_node(parser, (None, None))?;
        s_l_comments(parser)
    }

    c_directives_end(parser)?;
    alt!(parser, l_bare_document(parser), empty(parser))
}

fn l_directive_document<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    while parser.is_char('%') {
        l_directive(parser)?;
    }
    l_explicit_document(parser)
}

fn l_any_document<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    if parser.is_char('%') {
        l_directive_document(parser)
    } else if parser.is_str("---") {
        l_explicit_document(parser)
    } else {
        parser.queue(
            EventOrToken::Event(Event::DocumentStart {
                version: parser.yaml_version.map(Cow::Borrowed),
            }),
            Span::empty(parser.location()),
        );
        l_bare_document(parser)
    }
}

pub(super) fn l_yaml_stream<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.queue(
        EventOrToken::Event(Event::StreamStart),
        Span::empty(parser.location()),
    );

    let mut terminated = true;
    loop {
        l_document_prefix(parser)?;
        let read_document = if !terminated {
            question!(parser, l_explicit_document(parser)).is_some()
        } else {
            question!(parser, l_any_document(parser)).is_some()
        };
        terminated = question!(parser, l_document_suffix(parser)).is_some();
        if !terminated {
            if read_document {
                parser.queue(
                    EventOrToken::Event(Event::DocumentEnd),
                    Span::empty(parser.location()),
                );
            } else if parser.is_end_of_input() {
                parser.queue(
                    EventOrToken::Event(Event::StreamEnd),
                    Span::empty(parser.location()),
                );
                return Ok(());
            } else {
                #[cfg(feature = "tracing")]
                tracing::error!("remaining tokens {:?}", parser.iter.as_str());
                return Err(());
            }
        }
    }
}
