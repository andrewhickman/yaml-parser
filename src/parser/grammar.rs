use alloc::{
    borrow::{Cow, ToOwned},
    format,
};

use crate::{
    parser::{char, Chomping, Context, Diagnostic, EventOrToken, Parser, Properties, State},
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
    parser.eat(char::json)?;
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
        let hex_start = parser.offset();
        if ns_hex_digit(parser).is_ok() && ns_hex_digit(parser).is_ok() {
            let hex = &parser.text[hex_start..parser.offset()];
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
        Some('L') => '\u{2028}',
        Some('P') => '\u{2029}',
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
        Some('x') => ns_esc_hex(parser, 'x', 2),
        Some('u') => ns_esc_hex(parser, 'u', 4),
        Some('U') => ns_esc_hex(parser, 'U', 8),
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

fn s_indent_less_than<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<i32, ()> {
    parser.token(Token::Indent, |parser| {
        for i in 0..(n - 1) {
            if s_space(parser).is_err() {
                return Ok(i);
            }
        }
        Ok(n - 1)
    })
}

fn s_indent_less_or_equal<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<i32, ()> {
    parser.token(Token::Indent, |parser| {
        for i in 0..n {
            if s_space(parser).is_err() {
                return Ok(i);
            }
        }
        Ok(n)
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

fn s_flow_line_prefix<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    question!(parser, s_separate_in_line(parser));
    Ok(())
}

fn l_empty<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    if s_indent_less_or_equal(parser, n)? == n && matches!(c, Context::FlowIn | Context::FlowOut) {
        question!(parser, s_separate_in_line(parser));
    }
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
    let tag_handle = c_tag_handle(parser)?;
    s_separate_in_line(parser)?;
    let prefix = ns_tag_prefix(parser)?;
    parser.tags.insert(tag_handle, prefix);
    Ok(())
}

fn c_tag_handle<'t, R: Receiver>(parser: &mut Parser<'t, R>) -> Result<Cow<'t, str>, ()> {
    parser.begin_value();
    parser.token(Token::TagHandle, |parser| {
        alt!(
            parser,
            c_named_tag_handle(parser),
            c_secondary_tag_handle(parser),
            c_primary_tag_handle(parser)
        )
    })?;
    Ok(parser.end_value())
}

fn c_primary_tag_handle<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.eat_char('!')?;
    parser.value.push_range(parser.text, start..parser.offset());
    Ok(())
}

fn c_secondary_tag_handle<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.eat_str("!!")?;
    parser.value.push_range(parser.text, start..parser.offset());
    Ok(())
}

fn c_named_tag_handle<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.eat_char('!')?;
    plus_fast!(parser, ns_word_char(parser))?;
    parser.eat_char('!')
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

fn c_verbatim_tag<'t, R: Receiver>(parser: &mut Parser<'t, R>) -> Result<Cow<'t, str>, ()> {
    parser.begin_value();
    parser.token(Token::VerbatimTag, |parser| {
        parser.eat_char('!')?;
        let start = parser.offset();
        parser.eat_char('<')?;
        parser.value.push_range(parser.text, start..parser.offset());
        plus!(parser, ns_uri_char(parser))?;
        let start = parser.offset();
        parser.eat_char('>')?;
        parser.value.push_range(parser.text, start..parser.offset());
        Ok(())
    })?;
    Ok(parser.end_value())
}

fn c_ns_shorthand_tag<'t, R: Receiver>(parser: &mut Parser<'t, R>) -> Result<Cow<'t, str>, ()> {
    let handle = c_tag_handle(parser)?;

    parser.begin_value();
    parser.token(Token::TagSuffix, |parser| {
        plus!(parser, ns_tag_char(parser))
    })?;
    let suffix = parser.end_value();

    let resolved = match parser.tags.get(handle.as_ref()) {
        Some(prefix) => Cow::Owned(format!("<{}{}>", prefix, suffix)),
        None if handle.as_ref() == "!" => Cow::Owned(format!("<!{}>", suffix)),
        None if handle.as_ref() == "!!" => Cow::Owned(format!("<tag:yaml.org,2002:{}>", suffix)),
        None => {
            parser.diagnostics.push(Diagnostic {
                message: format!("unknown tag handle {}", handle),
                span: Span::empty(Location {
                    index: 0,
                    line: 0,
                    column: 0,
                }),
            });
            return Err(());
        }
    };

    Ok(resolved)
}

fn c_non_specific_tag<'t, R: Receiver>(parser: &mut Parser<'t, R>) -> Result<Cow<'t, str>, ()> {
    parser.token(Token::NonSpecificTag, |parser| parser.eat_char('!'))?;
    Ok(Cow::Borrowed("<!>"))
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

fn c_ns_alias_node<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
) -> Result<(Cow<'t, str>, Span), ()> {
    let start = parser.location();
    c_alias(parser)?;
    let name_start = parser.offset();
    ns_anchor_name(parser)?;
    Ok((
        Cow::Borrowed(&parser.text[name_start..parser.offset()]),
        parser.span(start),
    ))
}

fn c_ns_alias_node2<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    let (value, span) = c_ns_alias_node(parser)?;
    parser.queue(EventOrToken::Event(Event::Alias { value }), span);
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

fn e_node<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::Empty, |_| Ok(()))
}

fn e_node2<'t, R: Receiver>(parser: &mut Parser<'t, R>, props: Properties<'t>) -> Result<(), ()> {
    e_scalar(parser, props)
}

fn nb_double_char<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    match parser.peek() {
        Some('\\') => c_ns_esc_char(parser),
        Some('"') => Err(()),
        _ => parser.token(Token::DoubleQuoted, nb_json),
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
        let start = parser.offset();
        parser.token(Token::DoubleQuoted, s_whites)?;
        parser.value.push_range(parser.text, start..parser.offset());
        Ok(())
    }

    fn line<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        ns_double_char(parser)?;
        nb_ns_double_in_line(parser)
    }

    loop {
        if question!(parser, s_double_break(parser, n)).is_none() {
            break;
        }
        if question!(parser, line(parser)).is_none() {
            break;
        }
    }
    trailing_whites(parser)
}

fn nb_double_multi_line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    nb_ns_double_in_line(parser)?;
    s_double_next_line(parser, n)
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
        parser.token(Token::SingleQuoted, nb_json)
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
        let start = parser.offset();
        parser.token(Token::SingleQuoted, s_whites)?;
        parser.value.push_range(parser.text, start..parser.offset());
        ns_single_char(parser)
    }

    star!(parser, char(parser));
    Ok(())
}

fn s_single_next_line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn trailing_whites<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
        let start = parser.offset();
        parser.token(Token::SingleQuoted, s_whites)?;
        parser.value.push_range(parser.text, start..parser.offset());
        Ok(())
    }

    fn line<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
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

fn nb_single_multi_line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    nb_ns_single_in_line(parser)?;
    s_single_next_line(parser, n)
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
    c_flow_sequence_remainder(parser, n, c)
}

fn c_flow_sequence_remainder<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
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
    c_flow_mapping_remainder(parser, n, c)
}

fn c_flow_mapping_remainder<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
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
        e_node2(parser, (None, None))?;
        e_node2(parser, (None, None))
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
    alt!(parser, value(parser, n, c), e_node2(parser, (None, None)))
}

fn c_ns_flow_map_empty_key_entry<R: Receiver>(
    parser: &mut Parser<'_, R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    e_node2(parser, (None, None))?;
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
    alt!(parser, node(parser, n, c), e_node2(parser, (None, None)))
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
    alt!(parser, value(parser, n, c), e_node2(parser, (None, None)))
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
    alt!(parser, node(parser, n, c), e_node2(parser, (None, None)))
}

fn ns_flow_pair<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, c: Context) -> Result<(), ()> {
    parser.queue(
        EventOrToken::Event(Event::MappingStart {
            style: CollectionStyle::Flow,
            anchor: None,
            tag: None,
        }),
        Span::empty(parser.location()),
    );
    if parser.is_char('?') && !parser.next_is(char::non_space) {
        c_mapping_key(parser)?;
        s_separate(parser, n, c)?;
        ns_flow_map_explicit_entry(parser, n, c)?;
    } else {
        ns_flow_pair_entry(parser, n, c)?;
    }
    parser.queue(
        EventOrToken::Event(Event::MappingEnd),
        Span::empty(parser.location()),
    );
    Ok(())
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
        if !parser.is_char(':') {
            return Err(());
        }
        Ok(())
    })
}

fn c_s_implicit_json_key<R: Receiver>(parser: &mut Parser<'_, R>, c: Context) -> Result<(), ()> {
    parser.with_length_limit(1024, |parser| {
        c_flow_json_node(parser, 0, c)?;
        question!(parser, s_separate_in_line(parser));
        if !parser.is_char(':') {
            return Err(());
        }
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
        c_ns_alias_node2(parser),
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
        c_ns_alias_node2(parser),
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
        if matches!(t, Chomping::Clip | Chomping::Keep) {
            parser.value.push_char(parser.text, '\n');
        }
        Ok(())
    } else {
        match t {
            Chomping::Strip => b_non_content(parser),
            Chomping::Clip | Chomping::Keep => b_as_line_feed(parser),
        }
    }
}

fn l_chomped_empty<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, t: Chomping) -> Result<(), ()> {
    let t = match t {
        Chomping::Strip | Chomping::Clip => Chomping::Strip,
        Chomping::Keep => Chomping::Keep,
    };

    fn line<R: Receiver>(parser: &mut Parser<'_, R>, n: i32, t: Chomping) -> Result<(), ()> {
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

fn l_trail_comments<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    s_indent_less_than(parser, n)?;
    c_nb_comment_text(parser)?;
    b_comment(parser)?;
    star!(parser, l_comment(parser));
    Ok(())
}

fn c_l_literal<'t, R: Receiver>(parser: &mut Parser<'t, R>, n: i32) -> Result<Cow<'t, str>, ()> {
    c_literal(parser)?;
    let (m, t) = c_b_block_header(parser, n)?;
    l_literal_content(parser, n + m, t)
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

fn c_l_folded<'t, R: Receiver>(parser: &mut Parser<'t, R>, n: i32) -> Result<Cow<'t, str>, ()> {
    c_folded(parser)?;
    let (m, t) = c_b_block_header(parser, n)?;
    l_folded_content(parser, n + m, t)
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

fn l_block_sequence<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        s_indent(parser, n)?;
        c_l_block_seq_entry(parser, n)
    }

    c_l_block_seq_entry(parser, n)?;
    star!(parser, entry(parser, n));
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
    s_l_block_node(parser, true, n, c)
}

fn ns_l_compact_sequence<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        s_indent(parser, n)?;
        c_l_block_seq_entry(parser, n)
    }

    c_l_block_seq_entry(parser, n)?;
    star!(parser, entry(parser, n));
    parser.queue(
        EventOrToken::Event(Event::SequenceEnd),
        Span::empty(parser.location()),
    );
    Ok(())
}

fn l_block_mapping<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        s_indent(parser, n)?;
        ns_l_block_map_entry(parser, n)
    }

    ns_l_block_map_entry(parser, n)?;
    star!(parser, entry(parser, n));
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
        e_node2(parser, (None, None))
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
        e_node2(parser, (None, None))
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
    c_mapping_value(parser)?;
    if parser.is(char::non_space) {
        return Err(());
    }

    s_l_block_node(parser, false, n, Context::BlockOut)
}

fn ns_l_compact_mapping<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
        s_indent(parser, n)?;
        ns_l_block_map_entry(parser, n)
    }

    ns_l_block_map_entry(parser, n)?;
    star!(parser, entry(parser, n));
    parser.queue(
        EventOrToken::Event(Event::MappingEnd),
        Span::empty(parser.location()),
    );
    Ok(())
}

fn s_l_block_node<R: Receiver>(
    parser: &mut Parser<'_, R>,
    allow_compact: bool,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    match peek_block_node(parser, allow_compact, true, n, c)? {
        BlockNodeKind::Alias { value, span } => {
            parser.queue(EventOrToken::Event(Event::Alias { value }), span);
            Ok(())
        }
        BlockNodeKind::Scalar {
            style,
            properties: (anchor, tag),
            value,
            span,
        } => {
            parser.queue(
                EventOrToken::Event(Event::Scalar {
                    style,
                    value,
                    anchor,
                    tag,
                }),
                span,
            );
            Ok(())
        }
        BlockNodeKind::MappingStart {
            style,
            compact,
            properties: (anchor, tag),
            span,
            indent,
            context,
        } => {
            parser.queue(
                EventOrToken::Event(Event::MappingStart { style, anchor, tag }),
                span,
            );

            match style {
                CollectionStyle::Block if compact => ns_l_compact_mapping(parser, indent),
                CollectionStyle::Block => l_block_mapping(parser, indent),
                CollectionStyle::Flow => {
                    c_flow_mapping_remainder(parser, indent, context)?;
                    s_l_comments(parser)
                }
            }
        }
        BlockNodeKind::SequenceStart {
            style,
            compact,
            properties: (anchor, tag),
            span,
            indent,
            context,
        } => {
            parser.queue(
                EventOrToken::Event(Event::SequenceStart { style, anchor, tag }),
                span,
            );

            match style {
                CollectionStyle::Block if compact => ns_l_compact_sequence(parser, indent),
                CollectionStyle::Block => l_block_sequence(parser, indent),
                CollectionStyle::Flow => {
                    c_flow_sequence_remainder(parser, indent, context)?;
                    s_l_comments(parser)
                }
            }
        }
    }
}

#[derive(Debug)]
enum BlockNodeKind<'t> {
    Alias {
        value: Cow<'t, str>,
        span: Span,
    },
    Scalar {
        style: ScalarStyle,
        properties: Properties<'t>,
        value: Cow<'t, str>,
        span: Span,
    },
    MappingStart {
        style: CollectionStyle,
        compact: bool,
        properties: Properties<'t>,
        span: Span,
        indent: i32,
        context: Context,
    },
    SequenceStart {
        style: CollectionStyle,
        compact: bool,
        properties: Properties<'t>,
        span: Span,
        indent: i32,
        context: Context,
    },
}

fn peek_block_node<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    allow_compact: bool,
    allow_empty: bool,
    n: i32,
    c: Context,
) -> Result<BlockNodeKind<'t>, ()> {
    if allow_compact {
        if let Some(kind) = question!(parser, peek_compact_collection(parser, n, c)) {
            return Ok(kind);
        }
    }

    if let Some((style, properties, value, span)) =
        question!(parser, s_l_block_scalar(parser, n, c))
    {
        return Ok(BlockNodeKind::Scalar {
            style,
            properties,
            value,
            span,
        });
    }

    if let Some(kind) = question!(parser, peek_s_l_block_collection(parser, n, c)) {
        return Ok(kind);
    }

    if let Some(kind) = question!(
        parser,
        peek_s_l_flow_in_block(parser, n + 1, Context::FlowOut)
    ) {
        return Ok(kind);
    }

    if allow_empty {
        e_node(parser)?;
        let span = Span::empty(parser.location());
        s_l_comments(parser)?;

        return Ok(BlockNodeKind::Scalar {
            style: ScalarStyle::Plain,
            properties: (None, None),
            value: Cow::Borrowed(""),
            span,
        });
    }

    parser.diagnostics.push(Diagnostic {
        message: format!("invalid block node at {}", parser.iter.as_str()),
        span: Span::empty(parser.location()),
    });
    Err(())
}

fn peek_compact_collection<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    n: i32,
    c: Context,
) -> Result<BlockNodeKind<'t>, ()> {
    let m = parser.detect_compact_indent()?;
    let span = Span::empty(parser.location());
    s_indent(parser, m)?;
    if lookahead_is_block_sequence(parser) {
        Ok(BlockNodeKind::SequenceStart {
            style: CollectionStyle::Block,
            compact: true,
            properties: (None, None),
            span,
            indent: n + 1 + m,
            context: c,
        })
    } else if lookahead_is_block_mapping(parser) {
        Ok(BlockNodeKind::MappingStart {
            style: CollectionStyle::Block,
            compact: true,
            properties: (None, None),
            span,
            indent: n + 1 + m,
            context: c,
        })
    } else {
        Err(())
    }
}

fn s_l_block_scalar<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    n: i32,
    c: Context,
) -> Result<(ScalarStyle, Properties<'t>, Cow<'t, str>, Span), ()> {
    s_separate(parser, n + 1, c)?;
    let start = parser.location();
    let props = if parser.is(|ch| matches!(ch, '!' | '&')) {
        let props = c_ns_properties(parser, n + 1, c)?;
        s_separate(parser, n + 1, c)?;
        props
    } else {
        (None, None)
    };
    let (style, value) = if parser.is_char('|') {
        (ScalarStyle::Literal, c_l_literal(parser, n)?)
    } else if parser.is_char('>') {
        (ScalarStyle::Folded, c_l_folded(parser, n)?)
    } else {
        return Err(());
    };
    let span = parser.span(start);
    Ok((style, props, value, span))
}

fn peek_s_l_block_collection<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    n: i32,
    c: Context,
) -> Result<BlockNodeKind<'t>, ()> {
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

    let (start, properties) = match question!(parser, collection_props(parser, n + 1, c)) {
        Some((start, props)) => (Some(start), props),
        None => (None, (None, None)),
    };
    s_l_comments(parser)?;

    let m = parser.detect_collection_indent(n);

    s_indent(parser, n + m)?;
    let start = start.unwrap_or_else(|| parser.location());
    if lookahead_is_block_sequence(parser) {
        if m <= 0 && c != Context::BlockOut {
            return Err(());
        }

        Ok(BlockNodeKind::SequenceStart {
            style: CollectionStyle::Block,
            compact: false,
            properties,
            span: parser.span(start),
            indent: n + m,
            context: c,
        })
    } else if lookahead_is_block_mapping(parser) {
        if m <= 0 {
            return Err(());
        }

        return Ok(BlockNodeKind::MappingStart {
            style: CollectionStyle::Block,
            compact: false,
            properties,
            span: parser.span(start),
            indent: n + m,
            context: c,
        });
    } else {
        Err(())
    }
}

fn lookahead_is_block_sequence<R: Receiver>(parser: &mut Parser<R>) -> bool {
    parser.is_char('-') && !parser.next_is(char::non_space)
}

fn lookahead_is_block_mapping<R: Receiver>(parser: &mut Parser<R>) -> bool {
    ((parser.is_char('?') || parser.is_char(':')) && !parser.next_is(char::non_space))
        || parser.lookahead(ns_s_block_map_implicit_key)
}

fn peek_s_l_flow_in_block<'t, R: Receiver>(
    parser: &mut Parser<'t, R>,
    n: i32,
    c: Context,
) -> Result<BlockNodeKind<'t>, ()> {
    s_separate(parser, n, c)?;

    if parser.is_char('*') {
        let (value, span) = c_ns_alias_node(parser)?;
        s_l_comments(parser)?;
        return Ok(BlockNodeKind::Alias { value, span });
    }

    let start = parser.location();
    let properties = if parser.is_char('!') || parser.is_char('&') {
        c_ns_properties(parser, n, c)?
    } else {
        (None, None)
    };

    let separated = properties.0.is_some() || properties.1.is_some();

    if lookahead_is_maybe_separated_char(parser, separated, n, c, '[') {
        if separated {
            s_separate(parser, n, c)?;
        }
        c_sequence_start(parser)?;
        Ok(BlockNodeKind::SequenceStart {
            style: CollectionStyle::Flow,
            compact: false,
            properties,
            span: parser.span(start),
            indent: n,
            context: c,
        })
    } else if lookahead_is_maybe_separated_char(parser, separated, n, c, '{') {
        if separated {
            s_separate(parser, n, c)?;
        }
        c_mapping_start(parser)?;
        Ok(BlockNodeKind::MappingStart {
            style: CollectionStyle::Flow,
            compact: false,
            properties,
            span: parser.span(start),
            indent: n,
            context: c,
        })
    } else if lookahead_is_maybe_separated_char(parser, separated, n, c, '\'') {
        if separated {
            s_separate(parser, n, c)?;
        }
        c_single_quote(parser)?;
        let value = nb_single_text(parser, n, c)?;
        c_single_quote(parser)?;
        let span = parser.span(start);
        s_l_comments(parser)?;
        Ok(BlockNodeKind::Scalar {
            style: ScalarStyle::SingleQuoted,
            properties,
            value,
            span,
        })
    } else if lookahead_is_maybe_separated_char(parser, separated, n, c, '"') {
        if separated {
            s_separate(parser, n, c)?;
        }
        c_double_quote(parser)?;
        let value = nb_double_text(parser, n, c)?;
        c_double_quote(parser)?;
        let span = parser.span(start);
        s_l_comments(parser)?;
        Ok(BlockNodeKind::Scalar {
            style: ScalarStyle::DoubleQuoted,
            properties,
            value,
            span,
        })
    } else if lookahead_is_maybe_separated_plain(parser, separated, n, c) {
        if separated {
            s_separate(parser, n, c)?;
        }
        let value = ns_plain(parser, n, c)?;
        let span = parser.span(start);
        s_l_comments(parser)?;
        Ok(BlockNodeKind::Scalar {
            style: ScalarStyle::Plain,
            properties,
            value,
            span,
        })
    } else if separated {
        let span = Span::empty(parser.location());
        e_node(parser)?;
        s_l_comments(parser)?;
        Ok(BlockNodeKind::Scalar {
            style: ScalarStyle::Plain,
            properties,
            value: Cow::Borrowed(""),
            span,
        })
    } else {
        Err(())
    }
}

fn lookahead_is_maybe_separated_char<R: Receiver>(
    parser: &mut Parser<R>,
    separated: bool,
    n: i32,
    c: Context,
    ch: char,
) -> bool {
    if separated {
        parser.lookahead(|parser| {
            s_separate(parser, n, c)?;
            parser.eat_char(ch)
        })
    } else {
        parser.is_char(ch)
    }
}

fn lookahead_is_maybe_separated_plain<R: Receiver>(
    parser: &mut Parser<R>,
    separated: bool,
    n: i32,
    c: Context,
) -> bool {
    parser.lookahead(|parser| {
        if separated {
            s_separate(parser, n, c)?;
        }
        ns_plain_first(parser, c)
    })
}

fn l_document_prefix<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    c_byte_order_mark(parser)?;
    star!(parser, l_comment(parser));
    Ok(())
}

fn c_directives_end<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::DirectivesEnd, |parser| parser.eat_str("---"))
}

fn c_document_end<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<(), ()> {
    parser.token(Token::DocumentEnd, |parser| {
        parser.eat_str("...")?;
        if parser.is(char::non_space) {
            return Err(());
        }
        Ok(())
    })
}

fn l_document_suffix<R: Receiver>(parser: &mut Parser<'_, R>) -> Result<Span, ()> {
    let start = parser.location();
    c_document_end(parser)?;
    let span = parser.span(start);
    s_l_comments(parser)?;
    Ok(span)
}

fn nb_document_prefix<R: Receiver>(
    parser: &mut Parser<'_, R>,
    terminated: bool,
) -> Result<Option<(bool, Span)>, ()> {
    let start = parser.location();
    if parser.is_char('%') && !terminated {
        return Err(());
    }
    if parser.is_char('%')
        || (parser.is_str("---")
            && matches!(parser.peek_nth(3), None | Some('\r' | '\n' | '\t' | ' ')))
    {
        while parser.is_char('%') {
            l_directive(parser)?;
        }
        c_directives_end(parser)?;
        let span = parser.span(start);
        if lookahead_l_bare_document(parser) {
            Ok(Some((false, span)))
        } else {
            Ok(Some((true, span)))
        }
    } else if terminated {
        let span = parser.span(start);
        if lookahead_l_bare_document(parser) {
            Ok(Some((false, span)))
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

fn find_next_document<R: Receiver>(
    parser: &mut Parser<'_, R>,
    mut terminated: bool,
) -> Result<Option<(bool, Span)>, ()> {
    loop {
        l_document_prefix(parser)?;
        match nb_document_prefix(parser, terminated)? {
            Some(result) => return Ok(Some(result)),
            None => {
                let suffix_span = question!(parser, l_document_suffix(parser));
                terminated = suffix_span.is_some();
                if !terminated {
                    if parser.is_end_of_input() {
                        return Ok(None);
                    } else {
                        parser.diagnostics.push(Diagnostic {
                            message: format!("remaining tokens: {}", parser.iter.as_str()),
                            span: Span::empty(parser.location()),
                        });
                        return Err(());
                    }
                } else {
                    continue;
                }
            }
        };
    }
}

fn lookahead_l_bare_document<R: Receiver>(parser: &mut Parser<'_, R>) -> bool {
    parser.lookahead(|parser| {
        s_separate_lines(parser, 0)?;
        if parser.is_end_of_document() || parser.is_end_of_input() {
            Err(())
        } else {
            Ok(())
        }
    })
}

pub(super) fn event<'t, R: Receiver>(parser: &mut Parser<'t, R>) -> Result<(Event<'t>, Span), ()> {
    // println!("state: {:?}", parser.state);
    // println!("remaining: {:?}", parser.iter.as_str());

    match parser.state() {
        State::Stream => {
            parser.replace_state(State::DocumentStart {
                prev_terminated: true,
            });
            Ok((Event::StreamStart, Span::empty(parser.location())))
        }
        State::DocumentStart { prev_terminated } => {
            parser.in_document = true;
            match find_next_document(parser, prev_terminated)? {
                Some((empty, span)) => {
                    parser.replace_state(State::DocumentEnd);
                    parser.push_state(State::BlockNode {
                        allow_empty: empty,
                        allow_compact: false,
                        indent: -1,
                        context: Context::BlockIn,
                    });
                    Ok((
                        Event::DocumentStart {
                            version: parser.yaml_version.map(Cow::Borrowed),
                        },
                        span,
                    ))
                }
                None => {
                    parser.pop_state();
                    Ok((Event::StreamEnd, Span::empty(parser.location())))
                }
            }
        }
        State::DocumentEnd => {
            parser.in_document = false;
            parser.tags.clear();
            parser.yaml_version = None;
            let suffix_span = question!(parser, l_document_suffix(parser));
            parser.replace_state(State::DocumentStart {
                prev_terminated: suffix_span.is_some(),
            });
            Ok((
                Event::DocumentEnd,
                suffix_span.unwrap_or_else(|| Span::empty(parser.location())),
            ))
        }
        State::BlockNode {
            allow_empty,
            allow_compact,
            indent,
            context,
        } => match peek_block_node(parser, allow_compact, allow_empty, indent, context)? {
            BlockNodeKind::Alias { value, span } => {
                parser.pop_state();
                Ok((Event::Alias { value }, span))
            }
            BlockNodeKind::Scalar {
                style,
                properties: (anchor, tag),
                value,
                span,
            } => {
                parser.pop_state();
                Ok((
                    Event::Scalar {
                        style,
                        value,
                        anchor,
                        tag,
                    },
                    span,
                ))
            }
            BlockNodeKind::MappingStart {
                style,
                compact,
                properties: (anchor, tag),
                span,
                indent,
                context,
            } => {
                parser.replace_state(State::MappingKey {
                    style,
                    compact,
                    indent,
                    context,
                });
                Ok((Event::MappingStart { style, anchor, tag }, span))
            }
            BlockNodeKind::SequenceStart {
                style,
                compact,
                properties: (anchor, tag),
                span,
                indent,
                context,
            } => {
                parser.replace_state(State::SequenceNode {
                    style,
                    compact,
                    indent,
                    context,
                });
                Ok((Event::SequenceStart { style, anchor, tag }, span))
            }
        },
        State::SequenceNode {
            style,
            compact,
            indent,
            context,
        } => {
            assert!(!compact);

            let span = match style {
                CollectionStyle::Block => {
                    fn entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
                        s_indent(parser, n)?;
                        c_l_block_seq_entry(parser, n)
                    }

                    c_l_block_seq_entry(parser, indent)?;
                    star!(parser, entry(parser, indent));
                    Span::empty(parser.location())
                }
                CollectionStyle::Flow => {
                    question!(parser, s_separate(parser, indent, context));
                    question!(
                        parser,
                        ns_s_flow_seq_entries(parser, indent, context.in_flow())
                    );
                    let start = parser.location();
                    c_sequence_end(parser)?;
                    let span = parser.span(start);
                    s_l_comments(parser)?;
                    span
                }
            };

            parser.pop_state();
            Ok((Event::SequenceEnd, span))
        }
        State::MappingKey {
            style,
            compact,
            indent,
            context,
        } => {
            assert!(!compact);

            let span = match style {
                CollectionStyle::Block => {
                    fn entry<R: Receiver>(parser: &mut Parser<'_, R>, n: i32) -> Result<(), ()> {
                        s_indent(parser, n)?;
                        ns_l_block_map_entry(parser, n)
                    }

                    ns_l_block_map_entry(parser, indent)?;
                    star!(parser, entry(parser, indent));
                    Span::empty(parser.location())
                }
                CollectionStyle::Flow => {
                    question!(parser, s_separate(parser, indent, context));
                    question!(
                        parser,
                        ns_s_flow_map_entries(parser, indent, context.in_flow())
                    );
                    let start = parser.location();
                    c_mapping_end(parser)?;
                    let span = parser.span(start);
                    s_l_comments(parser)?;
                    span
                }
            };

            parser.pop_state();
            Ok((Event::MappingEnd, span))
        }
        State::MappingValue => todo!(),
    }
}
