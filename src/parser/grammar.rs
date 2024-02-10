use alloc::borrow::{Cow, ToOwned};

use crate::{
    parser::{char, Context, Diagnostic, Parser, Properties, State},
    CollectionStyle, Event, Location, Receiver, ScalarStyle, Span, Token,
};

use self::scalar::{c_double_quoted, c_single_quoted};

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

mod properties;
mod scalar;

fn c_byte_order_mark<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::ByteOrderMark, char::BYTE_ORDER_MARK)
}

fn c_sequence_entry<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::SequenceEntry, char::SEQUENCE_ENTRY)
}

fn c_mapping_key<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::MappingKey, char::MAPPING_KEY)
}

fn c_mapping_value<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::MappingValue, char::MAPPING_VALUE)
}

fn c_collect_entry<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::CollectionEntry, char::COLLECTION_ENTRY)
}

fn c_sequence_start<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::SequenceStart, char::SEQUENCE_START)
}

fn c_sequence_end<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::SequenceEnd, char::SEQUENCE_END)
}

fn c_mapping_start<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::MappingStart, char::MAPPING_START)
}

fn c_mapping_end<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::MappingEnd, char::MAPPING_END)
}

fn c_comment<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::Comment, char::COMMENT)
}

fn c_directive<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token_char(Token::Directive, char::DIRECTIVE)
}

fn nb_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(char::non_break)
}

fn b_break<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char('\r').or(parser.eat_char('\n'))
}

fn b_non_content<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::Break, b_break)
}

fn s_space<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat_char(' ')
}

fn s_white<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(char::space)
}

fn ns_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(char::non_space)
}

fn ns_dec_digit<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(|ch| ch.is_ascii_digit())
}

fn ns_hex_digit<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(|ch| ch.is_ascii_hexdigit())
}

fn s_indent<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    parser.token(Token::Indent, |parser| {
        for _ in 0..n {
            s_space(parser)?;
        }
        Ok(())
    })
}

fn s_indent_less_than<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<i32, ()> {
    parser.token(Token::Indent, |parser| {
        for i in 0..(n - 1) {
            if s_space(parser).is_err() {
                return Ok(i);
            }
        }
        Ok(n - 1)
    })
}

fn s_indent_less_or_equal<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<i32, ()> {
    parser.token(Token::Indent, |parser| {
        for i in 0..n {
            if s_space(parser).is_err() {
                return Ok(i);
            }
        }
        Ok(n)
    })
}

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

fn s_flow_line_prefix<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    s_indent(parser, n)?;
    question_fast!(parser, s_separate_in_line(parser));
    Ok(())
}

fn c_nb_comment_text<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_comment(parser)?;
    parser.token(Token::CommentText, |parser| {
        star_fast!(parser, nb_char(parser));
        Ok(())
    })
}

fn b_comment<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if b_non_content(parser).is_ok() || parser.is_end_of_input() {
        Ok(())
    } else {
        Err(())
    }
}

fn s_b_comment<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if s_separate_in_line(parser).is_ok() && parser.peek() == Some(char::COMMENT) {
        c_nb_comment_text(parser)?;
    }
    b_comment(parser)
}

fn l_comment<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    s_separate_in_line(parser)?;
    if parser.peek() == Some('#') {
        c_nb_comment_text(parser)?;
    }
    b_comment(parser)
}

fn s_l_comments<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if question!(parser, s_b_comment(parser)).is_some() || parser.is_start_of_line() {
        star!(parser, l_comment(parser));
        Ok(())
    } else {
        Err(())
    }
}

fn s_separate<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockIn | Context::BlockOut | Context::FlowIn | Context::FlowOut => {
            s_separate_lines(parser, n)
        }
        Context::BlockKey | Context::FlowKey => s_separate_in_line(parser),
    }
}

fn s_separate_lines<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
    fn comments<R: Receiver>(parser: &mut Parser<R>, n: i32) -> Result<(), ()> {
        s_l_comments(parser)?;
        s_flow_line_prefix(parser, n)
    }

    alt!(parser, comments(parser, n), s_separate_in_line(parser))
}

fn l_directive<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_directive(parser)?;
    if parser.is_str("YAML ") {
        ns_yaml_directive(parser)?;
    } else if parser.is_str("TAG ") {
        properties::ns_tag_directive(parser)?;
    } else {
        ns_reserved_directive(parser)?;
    }
    s_l_comments(parser)
}

fn ns_reserved_directive<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    fn param<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        s_separate_in_line(parser)?;
        if parser.is_char(char::COMMENT) {
            return Err(());
        }
        ns_directive_parameter(parser)
    }

    ns_directive_name(parser)?;
    star!(parser, param(parser));
    Ok(())
}

fn ns_directive_name<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DirectiveName, |parser| {
        plus_fast!(parser, ns_char(parser))
    })
}

fn ns_directive_parameter<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DirectiveParameter, |parser| {
        plus_fast!(parser, ns_char(parser))
    })
}

fn ns_yaml_directive<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    if parser.yaml_version.is_some() {
        return Err(());
    }

    parser.token(Token::DirectiveName, |parser| parser.eat_str("YAML"))?;
    s_separate_in_line(parser)?;
    ns_yaml_version(parser)
}

fn ns_yaml_version<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    let start = parser.offset();
    parser.token(Token::YamlVersion, |parser| {
        plus_fast!(parser, ns_dec_digit(parser))?;
        parser.eat_char('.')?;
        plus_fast!(parser, ns_dec_digit(parser))
    })?;
    let end = parser.offset();
    parser.yaml_version = Some(&parser.stream[start..end]);
    Ok(())
}

fn e_node<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::Empty, |_| Ok(()))
}

fn c_flow_sequence<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    c_sequence_start(parser)?;
    question_fast!(parser, s_separate(parser, n, c));
    c_flow_sequence_remainder(parser, n, c)
}

fn c_flow_sequence_remainder<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    question!(parser, ns_s_flow_seq_entries(parser, n, c.in_flow()));
    c_sequence_end(parser)
}

fn ns_s_flow_seq_entries<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    ns_flow_seq_entry(parser, n, c)?;
    question_fast!(parser, s_separate(parser, n, c));
    while question_fast!(parser, c_flow_collection_separator(parser, n, c)).is_some()
        && !parser.is_char(char::SEQUENCE_END)
    {
        ns_flow_seq_entry(parser, n, c)?;
        question_fast!(parser, s_separate(parser, n, c));
    }
    Ok(())
}

fn c_flow_collection_separator<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    c_collect_entry(parser)?;
    question_fast!(parser, s_separate(parser, n, c));
    Ok(())
}

fn ns_flow_seq_entry<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    if ((parser.is_char(char::MAPPING_KEY) || parser.is_char(char::MAPPING_VALUE))
        && !parser.next_is(char::non_space))
        || parser.lookahead(ns_s_flow_map_implicit_key)
    {
        ns_flow_pair(parser, n, c)
    } else {
        ns_flow_node(parser, n, c)
    }
}

fn ns_s_flow_map_implicit_key<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    alt!(
        parser,
        c_s_implicit_json_key(parser, Context::FlowKey),
        ns_s_implicit_yaml_key(parser, Context::FlowKey)
    )
}

fn c_flow_mapping<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    c_mapping_start(parser)?;
    c_flow_mapping_remainder(parser, n, c)
}

fn c_flow_mapping_remainder<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    question_fast!(parser, s_separate(parser, n, c));
    question!(parser, ns_s_flow_map_entries(parser, n, c.in_flow()));
    c_mapping_end(parser)
}

fn ns_s_flow_map_entries<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    ns_flow_map_entry(parser, n, c)?;
    question_fast!(parser, s_separate(parser, n, c));
    while question_fast!(parser, c_flow_collection_separator(parser, n, c)).is_some()
        && !parser.is_char(char::MAPPING_END)
    {
        if question!(parser, ns_flow_map_entry(parser, n, c)).is_some() {
            question_fast!(parser, s_separate(parser, n, c));
        } else {
            break;
        }
    }
    Ok(())
}

fn ns_flow_map_entry<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    if parser.is_char(char::MAPPING_KEY) && !parser.next_is(char::non_space) {
        c_mapping_key(parser)?;
        s_separate(parser, n, c)?;
        ns_flow_map_explicit_entry(parser, n, c)
    } else {
        ns_flow_map_implicit_entry(parser, n, c)
    }
}

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

fn ns_flow_map_yaml_key_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn value<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        question_fast!(parser, s_separate(parser, n, c));
        c_ns_flow_map_separate_value(parser, n, c)
    }

    ns_flow_yaml_node(parser, n, c)?;
    alt!(parser, value(parser, n, c), e_node(parser))
}

fn c_ns_flow_map_empty_key_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    e_node(parser)?;
    c_ns_flow_map_separate_value(parser, n, c)
}

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

fn c_ns_flow_map_json_key_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn value<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        question_fast!(parser, s_separate(parser, n, c));
        c_ns_flow_map_adjacent_value(parser, n, c)
    }

    c_flow_json_node(parser, n, c)?;
    alt!(parser, value(parser, n, c), e_node(parser))
}

fn c_ns_flow_map_adjacent_value<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn node<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        question_fast!(parser, s_separate(parser, n, c));
        ns_flow_node(parser, n, c)
    }

    c_mapping_value(parser)?;
    alt!(parser, node(parser, n, c), e_node(parser))
}

fn ns_flow_pair<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    if parser.is_char(char::MAPPING_KEY) && !parser.next_is(char::non_space) {
        c_mapping_key(parser)?;
        s_separate(parser, n, c)?;
        ns_flow_map_explicit_entry(parser, n, c)?;
    } else {
        ns_flow_pair_entry(parser, n, c)?;
    }
    Ok(())
}

fn ns_flow_pair_entry<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        parser,
        ns_flow_pair_yaml_key_entry(parser, n, c),
        c_ns_flow_map_empty_key_entry(parser, n, c),
        c_ns_flow_pair_json_key_entry(parser, n, c)
    )
}

fn ns_flow_pair_yaml_key_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    ns_s_implicit_yaml_key(parser, Context::FlowKey)?;
    c_ns_flow_map_separate_value(parser, n, c)
}

fn c_ns_flow_pair_json_key_entry<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    c_s_implicit_json_key(parser, Context::FlowKey)?;
    c_ns_flow_map_adjacent_value(parser, n, c)
}

fn ns_s_implicit_yaml_key<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    parser.with_length_limit(1024, |parser| {
        ns_flow_yaml_node(parser, 0, c)?;
        question_fast!(parser, s_separate_in_line(parser));
        if !parser.is_char(char::MAPPING_VALUE) {
            return Err(());
        }
        Ok(())
    })
}

fn c_s_implicit_json_key<R: Receiver>(parser: &mut Parser<R>, c: Context) -> Result<(), ()> {
    parser.with_length_limit(1024, |parser| {
        c_flow_json_node(parser, 0, c)?;
        question_fast!(parser, s_separate_in_line(parser));
        if !parser.is_char(char::MAPPING_VALUE) {
            return Err(());
        }
        Ok(())
    })
}

fn ns_flow_yaml_content<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    scalar::ns_plain(parser, n, c)?;
    Ok(())
}

fn c_flow_json_content<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    match parser.peek() {
        Some(char::SEQUENCE_START) => c_flow_sequence(parser, n, c),
        Some(char::MAPPING_START) => c_flow_mapping(parser, n, c),
        Some(char::SINGLE_QUOTE) => scalar::c_single_quoted(parser, n, c).map(drop),
        Some(char::DOUBLE_QUOTE) => scalar::c_double_quoted(parser, n, c).map(drop),
        _ => Err(()),
    }
}

fn ns_flow_content<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        parser,
        ns_flow_yaml_content(parser, n, c),
        c_flow_json_content(parser, n, c)
    )
}

fn ns_flow_yaml_node<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    fn alias<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
        properties::c_ns_alias_node(parser)?;
        Ok(())
    }

    fn content<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(parser, n, c)?;
        ns_flow_content(parser, n, c)
    }

    fn props<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
        properties::c_ns_properties(parser, n, c)?;
        alt!(parser, content(parser, n, c), e_node(parser))
    }

    alt!(
        parser,
        alias(parser),
        ns_flow_yaml_content(parser, n, c),
        props(parser, n, c)
    )
}

fn c_flow_json_node<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    if parser.is(|ch| matches!(ch, char::TAG | char::ANCHOR)) {
        properties::c_ns_properties(parser, n, c)?;
        s_separate(parser, n, c)?;
    }
    c_flow_json_content(parser, n, c)
}

fn ns_flow_node<R: Receiver>(parser: &mut Parser<R>, n: i32, c: Context) -> Result<(), ()> {
    match peek_flow_node(parser, false, n, c)? {
        NodeKind::Alias { .. } | NodeKind::Scalar { .. } => Ok(()),
        NodeKind::MappingStart {
            style,
            indent,
            context,
            ..
        } => match style {
            CollectionStyle::Block => unreachable!(),
            CollectionStyle::Flow => c_flow_mapping_remainder(parser, indent, context.in_flow()),
        },
        NodeKind::SequenceStart {
            style,
            indent,
            context,
            ..
        } => match style {
            CollectionStyle::Block => unreachable!(),
            CollectionStyle::Flow => c_flow_sequence_remainder(parser, indent, context.in_flow()),
        },
    }
}

fn s_ns_block_map_explicit_value_separator<R: Receiver>(
    parser: &mut Parser<R>,
    n: i32,
) -> Result<(), ()> {
    s_indent(parser, n)?;
    c_block_map_implicit_value_separator(parser)
}

fn c_block_map_implicit_value_separator<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    c_mapping_value(parser)?;
    if parser.is(char::non_space) {
        return Err(());
    }
    Ok(())
}

fn ns_s_block_map_implicit_key<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    alt!(
        parser,
        c_s_implicit_json_key(parser, Context::BlockKey),
        ns_s_implicit_yaml_key(parser, Context::BlockKey)
    )
}

#[derive(Debug)]
enum NodeKind<'s> {
    Alias {
        value: Cow<'s, str>,
        span: Span,
    },
    Scalar {
        style: ScalarStyle,
        properties: Properties<'s>,
        value: Cow<'s, str>,
        span: Span,
    },
    MappingStart {
        style: CollectionStyle,
        properties: Properties<'s>,
        span: Span,
        indent: i32,
        context: Context,
    },
    SequenceStart {
        style: CollectionStyle,
        properties: Properties<'s>,
        span: Span,
        indent: i32,
        context: Context,
    },
}

fn peek_block_node<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    allow_compact: bool,
    allow_empty: bool,
    n: i32,
    c: Context,
) -> Result<NodeKind<'s>, ()> {
    if allow_compact {
        if let Some(kind) = question!(parser, peek_compact_collection(parser, n, c)) {
            return Ok(kind);
        }
    }

    if let Some((style, properties, value, span)) =
        question!(parser, s_l_block_scalar(parser, n, c))
    {
        return Ok(NodeKind::Scalar {
            style,
            properties,
            value,
            span,
        });
    }

    if let Some(kind) = question!(parser, peek_s_l_block_collection(parser, n, c)) {
        return Ok(kind);
    }

    if let Some(kind) = question!(parser, peek_flow_in_block(parser, n + 1, Context::FlowOut)) {
        return Ok(kind);
    }

    if allow_empty {
        e_node(parser)?;
        let span = Span::empty(parser.location());
        s_l_comments(parser)?;

        return Ok(NodeKind::Scalar {
            style: ScalarStyle::Plain,
            properties: (None, None),
            value: Cow::Borrowed(""),
            span,
        });
    }

    parser.diagnostics.push(Diagnostic {
        message: "invalid block node".to_owned(),
        span: Span::empty(parser.location()),
    });
    Err(())
}

fn peek_compact_collection<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    c: Context,
) -> Result<NodeKind<'s>, ()> {
    let m = parser.detect_compact_indent()?;
    let span = Span::empty(parser.location());
    s_indent(parser, m)?;
    if lookahead_is_block_sequence(parser) {
        Ok(NodeKind::SequenceStart {
            style: CollectionStyle::Block,
            properties: (None, None),
            span,
            indent: n + 1 + m,
            context: c,
        })
    } else if lookahead_is_block_mapping(parser) {
        Ok(NodeKind::MappingStart {
            style: CollectionStyle::Block,
            properties: (None, None),
            span,
            indent: n + 1 + m,
            context: c,
        })
    } else {
        Err(())
    }
}

fn s_l_block_scalar<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    c: Context,
) -> Result<(ScalarStyle, Properties<'s>, Cow<'s, str>, Span), ()> {
    s_separate(parser, n + 1, c)?;
    let start = parser.location();
    let props = if parser.is(|ch| matches!(ch, char::TAG | char::ANCHOR)) {
        let props = properties::c_ns_properties(parser, n + 1, c)?;
        s_separate(parser, n + 1, c)?;
        props
    } else {
        (None, None)
    };
    let (style, value) = if parser.is_char(char::LITERAL) {
        (ScalarStyle::Literal, scalar::c_l_literal(parser, n)?)
    } else if parser.is_char(char::FOLDED) {
        (ScalarStyle::Folded, scalar::c_l_folded(parser, n)?)
    } else {
        return Err(());
    };
    let span = parser.span(start);
    Ok((style, props, value, span))
}

fn peek_s_l_block_collection<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    c: Context,
) -> Result<NodeKind<'s>, ()> {
    fn props<'s, R: Receiver>(
        parser: &mut Parser<'s, R>,
        n: i32,
        c: Context,
    ) -> Result<Properties<'s>, ()> {
        let props = properties::c_ns_properties(parser, n, c)?;
        s_l_comments(parser)?;
        Ok(props)
    }
    fn anchor<'s, R: Receiver>(parser: &mut Parser<'s, R>) -> Result<Properties<'s>, ()> {
        let anchor = properties::c_ns_anchor_property(parser)?;
        s_l_comments(parser)?;
        Ok((Some(anchor), None))
    }
    fn tag<'s, R: Receiver>(parser: &mut Parser<'s, R>) -> Result<Properties<'s>, ()> {
        let tag = properties::c_ns_tag_property(parser)?;
        s_l_comments(parser)?;
        Ok((None, Some(tag)))
    }

    fn collection_props<'s, R: Receiver>(
        parser: &mut Parser<'s, R>,
        n: i32,
        c: Context,
    ) -> Result<(Location, Properties<'s>), ()> {
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

        Ok(NodeKind::SequenceStart {
            style: CollectionStyle::Block,
            properties,
            span: parser.span(start),
            indent: n + m,
            context: c,
        })
    } else if lookahead_is_block_mapping(parser) {
        if m <= 0 {
            return Err(());
        }

        return Ok(NodeKind::MappingStart {
            style: CollectionStyle::Block,
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
    parser.is_char(char::SEQUENCE_ENTRY) && !parser.next_is(char::non_space)
}

fn lookahead_is_block_mapping<R: Receiver>(parser: &mut Parser<R>) -> bool {
    ((parser.is_char(char::MAPPING_KEY) || parser.is_char(char::MAPPING_VALUE))
        && !parser.next_is(char::non_space))
        || parser.lookahead(ns_s_block_map_implicit_key)
}

fn peek_flow_in_block<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    c: Context,
) -> Result<NodeKind<'s>, ()> {
    s_separate(parser, n, c)?;
    let kind = peek_flow_node(parser, false, n, c)?;
    if matches!(kind, NodeKind::Scalar { .. } | NodeKind::Alias { .. }) {
        s_l_comments(parser)?;
    }
    Ok(kind)
}

fn peek_flow_node<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    allow_empty: bool,
    n: i32,
    c: Context,
) -> Result<NodeKind<'s>, ()> {
    if parser.is_char(char::ALIAS) {
        let (value, span) = properties::c_ns_alias_node(parser)?;
        return Ok(NodeKind::Alias { value, span });
    }

    let start = parser.location();
    let properties = if parser.is_char(char::TAG) || parser.is_char(char::ANCHOR) {
        properties::c_ns_properties(parser, n, c)?
    } else {
        (None, None)
    };

    let separated = properties.0.is_some() || properties.1.is_some();

    if lookahead_is_maybe_separated_char(parser, separated, n, c, char::SEQUENCE_START) {
        if separated {
            s_separate(parser, n, c)?;
        }
        c_sequence_start(parser)?;
        question_fast!(parser, s_separate(parser, n, c));
        Ok(NodeKind::SequenceStart {
            style: CollectionStyle::Flow,
            properties,
            span: parser.span(start),
            indent: n,
            context: c,
        })
    } else if lookahead_is_maybe_separated_char(parser, separated, n, c, char::MAPPING_START) {
        if separated {
            s_separate(parser, n, c)?;
        }
        c_mapping_start(parser)?;
        question_fast!(parser, s_separate(parser, n, c));
        Ok(NodeKind::MappingStart {
            style: CollectionStyle::Flow,
            properties,
            span: parser.span(start),
            indent: n,
            context: c,
        })
    } else if lookahead_is_maybe_separated_char(parser, separated, n, c, char::SINGLE_QUOTE) {
        if separated {
            s_separate(parser, n, c)?;
        }
        let value = c_single_quoted(parser, n, c)?;
        let span = parser.span(start);
        Ok(NodeKind::Scalar {
            style: ScalarStyle::SingleQuoted,
            properties,
            value,
            span,
        })
    } else if lookahead_is_maybe_separated_char(parser, separated, n, c, char::DOUBLE_QUOTE) {
        if separated {
            s_separate(parser, n, c)?;
        }
        let value = c_double_quoted(parser, n, c)?;
        let span = parser.span(start);
        Ok(NodeKind::Scalar {
            style: ScalarStyle::DoubleQuoted,
            properties,
            value,
            span,
        })
    } else if lookahead_is_maybe_separated_plain(parser, separated, n, c) {
        if separated {
            s_separate(parser, n, c)?;
        }
        let value = scalar::ns_plain(parser, n, c)?;
        let span = parser.span(start);
        Ok(NodeKind::Scalar {
            style: ScalarStyle::Plain,
            properties,
            value,
            span,
        })
    } else if separated || allow_empty {
        let span = Span::empty(parser.location());
        e_node(parser)?;
        Ok(NodeKind::Scalar {
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
        scalar::ns_plain_first(parser, c)
    })
}

fn l_document_prefix<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    question_fast!(parser, c_byte_order_mark(parser));
    star!(parser, l_comment(parser));
    Ok(())
}

fn c_directives_end<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DirectivesEnd, |parser| parser.eat_str("---"))
}

fn c_document_end<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.token(Token::DocumentEnd, |parser| {
        parser.eat_str("...")?;
        if parser.is(char::non_space) {
            return Err(());
        }
        Ok(())
    })
}

fn l_document_suffix<R: Receiver>(parser: &mut Parser<R>) -> Result<Span, ()> {
    let start = parser.location();
    c_document_end(parser)?;
    let span = parser.span(start);
    s_l_comments(parser)?;
    Ok(span)
}

fn nb_document_prefix<R: Receiver>(
    parser: &mut Parser<R>,
    terminated: bool,
) -> Result<Option<(bool, Span)>, ()> {
    let start = parser.location();
    if parser.is_char(char::DIRECTIVE) && !terminated {
        return Err(());
    }
    if parser.is_char(char::DIRECTIVE)
        || (parser.is_str("---")
            && matches!(parser.peek_nth(3), None | Some('\r' | '\n' | '\t' | ' ')))
    {
        while parser.is_char(char::DIRECTIVE) {
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
    parser: &mut Parser<R>,
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
                            message: "invalid document".to_owned(),
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

fn lookahead_l_bare_document<R: Receiver>(parser: &mut Parser<R>) -> bool {
    parser.lookahead(|parser| {
        s_separate_lines(parser, 0)?;
        if parser.is_end_of_document() || parser.is_end_of_input() {
            Err(())
        } else {
            Ok(())
        }
    })
}

pub(super) fn event_flow_node<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    allow_empty: bool,
    n: i32,
    c: Context,
) -> Result<(Event<'s>, Span), ()> {
    match peek_flow_node(parser, allow_empty, n, c)? {
        NodeKind::Alias { value, span } => Ok((Event::Alias { value }, span)),
        NodeKind::Scalar {
            style,
            properties: (anchor, tag),
            value,
            span,
        } => Ok((
            Event::Scalar {
                style,
                value,
                anchor,
                tag,
            },
            span,
        )),
        NodeKind::MappingStart {
            style,
            properties: (anchor, tag),
            span,
            indent,
            context,
        } => {
            assert_eq!(style, CollectionStyle::Flow);
            parser.push_state(State::FlowMapping {
                indent,
                context,
                first: true,
            });
            Ok((Event::MappingStart { style, anchor, tag }, span))
        }
        NodeKind::SequenceStart {
            style,
            properties: (anchor, tag),
            span,
            indent,
            context,
        } => {
            assert_eq!(style, CollectionStyle::Flow);
            parser.push_state(State::FlowSequence {
                indent,
                context,
                first: true,
            });
            Ok((Event::SequenceStart { style, anchor, tag }, span))
        }
    }
}

pub(super) fn event_block_node<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    allow_compact: bool,
    allow_empty: bool,
    n: i32,
    c: Context,
) -> Result<(Event<'s>, Span), ()> {
    match peek_block_node(parser, allow_compact, allow_empty, n, c)? {
        NodeKind::Alias { value, span } => Ok((Event::Alias { value }, span)),
        NodeKind::Scalar {
            style,
            properties: (anchor, tag),
            value,
            span,
        } => Ok((
            Event::Scalar {
                style,
                value,
                anchor,
                tag,
            },
            span,
        )),
        NodeKind::MappingStart {
            style,
            properties: (anchor, tag),
            span,
            indent,
            context,
        } => {
            match style {
                CollectionStyle::Block => parser.push_state(State::BlockMapping {
                    indent,
                    context,
                    first: true,
                }),
                CollectionStyle::Flow => parser.push_state(State::FlowMapping {
                    indent,
                    context,
                    first: true,
                }),
            }
            Ok((Event::MappingStart { style, anchor, tag }, span))
        }
        NodeKind::SequenceStart {
            style,
            properties: (anchor, tag),
            span,
            indent,
            context,
        } => {
            match style {
                CollectionStyle::Block => parser.push_state(State::BlockSequence {
                    indent,
                    context,
                    first: true,
                }),
                CollectionStyle::Flow => parser.push_state(State::FlowSequence {
                    indent,
                    context,
                    first: true,
                }),
            };
            Ok((Event::SequenceStart { style, anchor, tag }, span))
        }
    }
}

pub(super) fn event_empty_node<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
) -> Result<(Event<'s>, Span), ()> {
    e_node(parser)?;
    return Ok((
        Event::Scalar {
            style: ScalarStyle::Plain,
            value: Cow::Borrowed(""),
            anchor: None,
            tag: None,
        },
        Span::empty(parser.location()),
    ));
}

pub(super) fn event<'s, R: Receiver>(parser: &mut Parser<'s, R>) -> Result<(Event<'s>, Span), ()> {
    match parser.state() {
        State::Stream => {
            parser.replace_state(State::Document {
                prev_terminated: true,
            });
            Ok((Event::StreamStart, Span::empty(parser.location())))
        }
        State::Document { prev_terminated } => {
            parser.in_document = true;
            match find_next_document(parser, prev_terminated)? {
                Some((empty, span)) => {
                    parser.replace_state(State::DocumentNode {
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
            parser.replace_state(State::Document {
                prev_terminated: suffix_span.is_some(),
            });
            Ok((
                Event::DocumentEnd,
                suffix_span.unwrap_or_else(|| Span::empty(parser.location())),
            ))
        }
        State::DocumentNode {
            allow_empty,
            allow_compact,
            indent,
            context,
        } => {
            assert!(
                matches!(context, Context::BlockIn),
                "unexpected context {:?}",
                context
            );
            parser.replace_state(State::DocumentEnd);
            event_block_node(parser, allow_compact, allow_empty, indent, context)
        }
        State::BlockSequence {
            indent,
            context,
            first,
        } => {
            assert!(
                matches!(context, Context::BlockIn | Context::BlockOut),
                "unexpected context {:?}",
                context
            );

            if first
                || parser.lookahead(|parser| {
                    s_indent(parser, indent)?;
                    if lookahead_is_block_sequence(parser) {
                        Ok(())
                    } else {
                        Err(())
                    }
                })
            {
                if !first {
                    s_indent(parser, indent)?;
                } else {
                    parser.replace_state(State::BlockSequence {
                        indent,
                        context,
                        first: false,
                    });
                }

                c_sequence_entry(parser)?;
                event_block_node(parser, true, true, indent, Context::BlockIn)
            } else {
                let span = Span::empty(parser.location());
                parser.pop_state();
                Ok((Event::SequenceEnd, span))
            }
        }
        State::FlowSequence {
            indent,
            context,
            first,
        } => {
            assert!(
                matches!(
                    context,
                    Context::FlowIn | Context::FlowOut | Context::BlockKey
                ),
                "unexpected context {:?}",
                context
            );

            question_fast!(parser, s_separate(parser, indent, context));

            if (first
                || question_fast!(
                    parser,
                    c_flow_collection_separator(parser, indent, context.in_flow())
                )
                .is_some())
                && !parser.is_char(char::SEQUENCE_END)
            {
                parser.replace_state(State::FlowSequence {
                    indent,
                    context,
                    first: false,
                });

                if ((parser.is_char(char::MAPPING_KEY) || parser.is_char(char::MAPPING_VALUE))
                    && !parser.next_is(char::non_space))
                    || parser.lookahead(ns_s_flow_map_implicit_key)
                {
                    parser.push_state(State::FlowPair { indent, context });
                    Ok((
                        Event::MappingStart {
                            style: CollectionStyle::Flow,
                            anchor: None,
                            tag: None,
                        },
                        Span::empty(parser.location()),
                    ))
                } else {
                    event_flow_node(parser, false, indent, context.in_flow())
                }
            } else {
                let start = parser.location();
                c_sequence_end(parser)?;
                let span = parser.span(start);
                if context == Context::FlowOut {
                    s_l_comments(parser)?;
                }

                parser.pop_state();
                Ok((Event::SequenceEnd, span))
            }
        }
        State::BlockMapping {
            indent,
            context,
            first,
        } => {
            assert!(
                matches!(context, Context::BlockIn | Context::BlockOut),
                "unexpected context {:?}",
                context
            );

            if first
                || parser.lookahead(|parser| {
                    s_indent(parser, indent)?;
                    if lookahead_is_block_mapping(parser) {
                        Ok(())
                    } else {
                        Err(())
                    }
                })
            {
                if !first {
                    s_indent(parser, indent)?;
                } else {
                    parser.replace_state(State::BlockMapping {
                        indent,
                        context,
                        first: false,
                    });
                }

                let explicit =
                    parser.is_char(char::MAPPING_KEY) && !parser.next_is(char::non_space);
                parser.push_state(State::BlockMappingValue {
                    explicit,
                    indent,
                    context,
                });

                if explicit {
                    c_mapping_key(parser)?;
                    if parser.is(char::non_space) {
                        return Err(());
                    }

                    event_block_node(parser, true, true, indent, Context::BlockOut)
                } else {
                    // todo length limit
                    event_flow_node(parser, true, 0, Context::BlockKey)
                }
            } else {
                parser.pop_state();
                let span = Span::empty(parser.location());
                Ok((Event::MappingEnd, span))
            }
        }
        State::FlowMapping {
            indent,
            context,
            first,
        } => {
            assert!(
                matches!(
                    context,
                    Context::FlowIn | Context::FlowOut | Context::BlockKey
                ),
                "unexpected context {:?}",
                context
            );

            question_fast!(parser, s_separate(parser, indent, context));

            if (first
                || question_fast!(
                    parser,
                    c_flow_collection_separator(parser, indent, context.in_flow())
                )
                .is_some())
                && !parser.is_char(char::MAPPING_END)
            {
                parser.replace_state(State::FlowMapping {
                    indent,
                    context,
                    first: false,
                });

                let explicit =
                    parser.is_char(char::MAPPING_KEY) && !parser.next_is(char::non_space);

                if explicit {
                    c_mapping_key(parser)?;
                    s_separate(parser, indent, context.in_flow())?;
                }

                let state_len = parser.state.len();
                parser.push_state(State::FlowMappingValue {
                    allow_adjacent: false,
                    allow_empty: true,
                    indent,
                    context: context.in_flow(),
                });

                let (kind, span) = event_flow_node(parser, true, indent, context.in_flow())?;
                let allow_adjacent = matches!(
                    kind,
                    Event::MappingStart {
                        style: CollectionStyle::Flow,
                        ..
                    } | Event::SequenceStart {
                        style: CollectionStyle::Flow,
                        ..
                    } | Event::Scalar {
                        style: ScalarStyle::SingleQuoted | ScalarStyle::DoubleQuoted,
                        ..
                    }
                );

                parser.state[state_len] = State::FlowMappingValue {
                    allow_adjacent,
                    allow_empty: true,
                    indent,
                    context: context.in_flow(),
                };

                Ok((kind, span))
            } else {
                let start = parser.location();
                c_mapping_end(parser)?;
                let span = parser.span(start);
                if context == Context::FlowOut {
                    s_l_comments(parser)?;
                }

                parser.pop_state();
                Ok((Event::MappingEnd, span))
            }
        }
        State::BlockMappingValue {
            explicit,
            indent,
            context,
            ..
        } => {
            assert!(
                matches!(context, Context::BlockIn | Context::BlockOut),
                "unexpected context {:?}",
                context
            );

            parser.pop_state();
            if explicit {
                if question!(
                    parser,
                    s_ns_block_map_explicit_value_separator(parser, indent)
                )
                .is_some()
                {
                    event_block_node(parser, true, true, indent, Context::BlockOut)
                } else {
                    event_empty_node(parser)
                }
            } else {
                if !parser.is_char(':') {
                    s_separate_in_line(parser)?;
                }

                c_block_map_implicit_value_separator(parser)?;
                return event_block_node(parser, false, true, indent, Context::BlockOut);
            }
        }
        State::FlowMappingValue {
            allow_adjacent,
            allow_empty,
            indent,
            context,
            ..
        } => {
            assert!(
                matches!(context, Context::FlowIn | Context::FlowKey),
                "unexpected context {:?}",
                context
            );

            parser.pop_state();

            if allow_empty
                && !parser.lookahead(|parser| {
                    question_fast!(parser, s_separate(parser, indent, context));
                    c_mapping_value(parser)
                })
            {
                return event_empty_node(parser);
            }

            question_fast!(parser, s_separate(parser, indent, context));
            c_mapping_value(parser)?;
            if !allow_adjacent && parser.is(|ch| char::plain_safe(ch, context)) {
                return Err(());
            }

            if parser.lookahead(|parser| {
                question_fast!(parser, s_separate(parser, indent, context));
                if parser.is_char(char::COLLECTION_ENTRY) || parser.is_char(char::MAPPING_END) {
                    Ok(())
                } else {
                    Err(())
                }
            }) {
                return event_empty_node(parser);
            }

            if allow_adjacent {
                question_fast!(parser, s_separate(parser, indent, context));
            } else {
                s_separate(parser, indent, context)?;
            }

            event_flow_node(parser, true, indent, context)
        }
        State::FlowPair { indent, context } => {
            assert!(
                matches!(context, Context::FlowIn | Context::FlowOut),
                "unexpected context {:?}",
                context
            );

            parser.replace_state(State::FlowPairEnd { indent, context });

            let explicit = parser.is_char(char::MAPPING_KEY) && !parser.next_is(char::non_space);
            if explicit {
                c_mapping_key(parser)?;
                s_separate(parser, indent, context.in_flow())?;
            }

            let state_len = parser.state.len();
            parser.push_state(State::FlowMappingValue {
                allow_adjacent: false,
                allow_empty: explicit,
                indent,
                context: context.in_flow(),
            });

            let (kind, span) = event_flow_node(parser, true, indent, context.in_flow())?;
            let allow_adjacent = matches!(
                kind,
                Event::MappingStart {
                    style: CollectionStyle::Flow,
                    ..
                } | Event::SequenceStart {
                    style: CollectionStyle::Flow,
                    ..
                } | Event::Scalar {
                    style: ScalarStyle::SingleQuoted | ScalarStyle::DoubleQuoted,
                    ..
                }
            );

            parser.state[state_len] = State::FlowMappingValue {
                allow_adjacent,
                allow_empty: true,
                indent,
                context: context.in_flow(),
            };

            Ok((kind, span))
        }
        State::FlowPairEnd { indent, context } => {
            let span = Span::empty(parser.location());
            question!(parser, s_separate(parser, indent, context));
            parser.pop_state();
            Ok((Event::MappingEnd, span))
        }
    }
}
