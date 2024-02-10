use alloc::{
    borrow::{Cow, ToOwned},
    format,
};

use crate::{
    error::ErrorKind,
    parser::{
        char,
        grammar::{
            lookahead_is_maybe_separated_char, ns_char, ns_hex_digit, s_separate,
            s_separate_in_line,
        },
        Context, Diagnostic, Parser, Properties,
    },
    Receiver, Span, Token,
};

pub(super) fn ns_tag_directive<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ErrorKind> {
    parser
        .token(Token::DirectiveName, |parser| parser.eat_str("TAG"))
        .map_err(|()| ErrorKind::ExpectedToken(Token::DirectiveName));
    s_separate_in_line(parser)?;
    let tag_handle =
        c_tag_handle(parser).map_err(|()| ErrorKind::ExpectedToken(Token::TagHandle))?;
    s_separate_in_line(parser)?;
    let prefix = ns_tag_prefix(parser)?;
    parser.tags.insert(tag_handle, prefix);
    Ok(())
}

pub(super) fn c_ns_properties<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
    n: i32,
    c: Context,
) -> Result<Properties<'s>, ErrorKind> {
    fn separated_anchor<'s, R: Receiver>(
        parser: &mut Parser<'s, R>,
        n: i32,
        c: Context,
    ) -> Result<Cow<'s, str>, ErrorKind> {
        s_separate(parser, n, c)?;
        c_ns_anchor_property(parser)
    }

    fn separated_tag<'s, R: Receiver>(
        parser: &mut Parser<'s, R>,
        n: i32,
        c: Context,
    ) -> Result<Cow<'s, str>, ErrorKind> {
        s_separate(parser, n, c)?;
        c_ns_tag_property(parser)
    }

    match parser.peek() {
        Some(char::TAG) => {
            let tag = c_ns_tag_property(parser)?;
            let anchor = if lookahead_is_maybe_separated_char(parser, true, n, c, char::ANCHOR) {
                Some(c_ns_anchor_property(parser)?)
            } else {
                None
            };
            Ok((anchor, Some(tag)))
        }
        Some(char::ANCHOR) => {
            let anchor = c_ns_anchor_property(parser)?;
            let tag = if lookahead_is_maybe_separated_char(parser, true, n, c, char::TAG) {
                Some(c_ns_tag_property(parser)?)
            } else {
                None
            };
            Ok((Some(anchor), tag))
        }
        _ => Err(ErrorKind::Todo),
    }
}

pub(super) fn c_ns_anchor_property<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
) -> Result<Cow<'s, str>, ErrorKind> {
    c_anchor(parser)?;
    ns_anchor_name(parser)
}

pub(super) fn c_ns_tag_property<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
) -> Result<Cow<'s, str>, ErrorKind> {
    if parser.peek_next() == Some('<') {
        c_verbatim_tag(parser).map_err(|_| ErrorKind::InvalidTagProperty)
    } else {
        alt!(
            parser,
            c_ns_shorthand_tag(parser),
            c_non_specific_tag(parser)
        )
        .map_err(|_| ErrorKind::InvalidTagProperty)
    }
}

pub(super) fn c_ns_alias_node<'s, R: Receiver>(
    parser: &mut Parser<'s, R>,
) -> Result<(Cow<'s, str>, Span), ErrorKind> {
    let start = parser.location();
    c_alias(parser)?;
    let name_start = parser.offset();
    ns_anchor_name(parser)?;
    Ok((
        Cow::Borrowed(&parser.stream[name_start..parser.offset()]),
        parser.span(start),
    ))
}

fn ns_uri_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ErrorKind> {
    let start = parser.location();
    if parser.eat_char('%').is_ok() {
        let hex_start = parser.offset();
        if ns_hex_digit(parser).is_ok() && ns_hex_digit(parser).is_ok() {
            let hex = &parser.stream[hex_start..parser.offset()];
            let codepoint = u32::from_str_radix(hex, 16).unwrap();
            match char::try_from(codepoint) {
                Ok(ch) => {
                    parser.value.push_char(parser.stream, ch);
                    Ok(())
                }
                Err(_) => {
                    parser.diagnostics.push(Diagnostic {
                        message: "invalid percent escape".to_owned(),
                        span: parser.span(start),
                    });
                    Err(ErrorKind::InvalidPercentEscape)
                }
            }
        } else {
            parser.diagnostics.push(Diagnostic {
                message: "invalid percent escape".to_owned(),
                span: parser.span(start),
            });
            Err(ErrorKind::InvalidPercentEscape)
        }
    } else {
        parser
            .eat(|ch| {
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
            })
            .map_err(|()| ErrorKind::InvalidUriChar)?;
        parser
            .value
            .push_range(parser.stream, start.index..parser.offset());
        Ok(())
    }
}

fn ns_tag_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ErrorKind> {
    if parser.is_char(char::TAG) || parser.is(char::flow_indicator) {
        Err(ErrorKind::InvalidUriChar)
    } else {
        ns_uri_char(parser)
    }
}

fn c_tag_handle<'s, R: Receiver>(parser: &mut Parser<'s, R>) -> Result<Cow<'s, str>, ()> {
    parser.begin_value();
    parser.token(Token::TagHandle, |parser| {
        alt!(
            parser,
            c_named_tag_handle(parser),
            c_secondary_tag_handle(parser),
            c_primary_tag_handle(parser)
        )
    });
    Ok(parser.end_value())
}

fn c_primary_tag_handle<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ErrorKind> {
    let start = parser.offset();
    parser
        .eat_char(char::TAG)
        .map_err(|()| ErrorKind::ExpectedToken(Token::TagHandle));
    parser
        .value
        .push_range(parser.stream, start..parser.offset());
    Ok(())
}

fn c_secondary_tag_handle<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ErrorKind> {
    let start = parser.offset();
    parser
        .eat_char(char::TAG)
        .map_err(|()| ErrorKind::ExpectedToken(Token::TagHandle));
    parser
        .eat_char(char::TAG)
        .map_err(|()| ErrorKind::ExpectedToken(Token::TagHandle));
    parser
        .value
        .push_range(parser.stream, start..parser.offset());
    Ok(())
}

fn c_named_tag_handle<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ErrorKind> {
    parser
        .eat_char(char::TAG)
        .map_err(|()| ErrorKind::ExpectedToken(Token::TagHandle))?;
    plus_fast!(parser, ns_word_char(parser))
        .map_err(|_| ErrorKind::ExpectedToken(Token::TagHandle))?;
    parser
        .eat_char(char::TAG)
        .map_err(|()| ErrorKind::ExpectedToken(Token::TagHandle))?;
    Ok(())
}

fn ns_tag_prefix<'s, R: Receiver>(parser: &mut Parser<'s, R>) -> Result<Cow<'s, str>, ErrorKind> {
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

fn c_ns_local_tag_prefix<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ErrorKind> {
    let start = parser.offset();
    parser
        .eat_char('!')
        .map_err(|()| ErrorKind::ExpectedToken(Token::TagPrefix));
    parser
        .value
        .push_range(parser.stream, start..parser.offset());
    star!(parser, ns_uri_char(parser));
    Ok(())
}

fn ns_global_tag_prefix<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ErrorKind> {
    ns_tag_char(parser)?;
    star!(parser, ns_uri_char(parser));
    Ok(())
}

fn c_verbatim_tag<'s, R: Receiver>(parser: &mut Parser<'s, R>) -> Result<Cow<'s, str>, ErrorKind> {
    parser.begin_value();
    parser.token(Token::VerbatimTag, |parser| {
        parser.eat_char(char::TAG).map_err(|()| ErrorKind::ExpectedToken(Token::VerbatimTag));
        let start = parser.offset();
        parser.eat_char('<').map_err(|()| ErrorKind::Todo)?;
        parser
            .value
            .push_range(parser.stream, start..parser.offset());
        plus!(parser, ns_uri_char(parser))?;
        let start = parser.offset();
        parser.eat_char('>').map_err(|()| ErrorKind::Todo)?;
        parser
            .value
            .push_range(parser.stream, start..parser.offset());
        Ok(())
    })?;
    Ok(parser.end_value())
}

fn c_ns_shorthand_tag<'s, R: Receiver>(parser: &mut Parser<'s, R>) -> Result<Cow<'s, str>, ()> {
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
                span: Span::empty(parser.location()),
            });
            return Err(());
        }
    };

    Ok(resolved)
}

fn c_non_specific_tag<'s, R: Receiver>(parser: &mut Parser<'s, R>) -> Result<Cow<'s, str>, ()> {
    parser.token_char(Token::NonSpecificTag, char::TAG)?;
    Ok(Cow::Borrowed("<!>"))
}

fn c_anchor<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ErrorKind> {
    parser.token_char(Token::Anchor, char::ANCHOR)
}

fn ns_anchor_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ErrorKind> {
    if parser.is(char::flow_indicator) {
        Err(())
    } else {
        ns_char(parser)
    }
}

fn ns_anchor_name<'s, R: Receiver>(parser: &mut Parser<'s, R>) -> Result<Cow<'s, str>, ErrorKind> {
    let start = parser.offset();
    parser
        .token(Token::AnchorName, |parser| {
            plus_fast!(parser, ns_anchor_char(parser))
        })
        .map_err(|()| ErrorKind::ExpectedToken(Token::AnchorName))?;
    Ok(Cow::Borrowed(&parser.stream[start..parser.offset()]))
}

fn c_alias<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ErrorKind> {
    parser.token_char(Token::Alias, char::ALIAS)
}

fn ns_word_char<R: Receiver>(parser: &mut Parser<R>) -> Result<(), ()> {
    parser.eat(char::word)
}
