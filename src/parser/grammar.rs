use crate::{
    parser::{char, Chomping, Context, Diagnostic, State},
    Receiver, Token,
};

macro_rules! star {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        {
            let mut start = $state.offset();
            while question!($state, $production(state $(, $p)*)).is_some() && $state.offset() != start {
                start = $state.offset();
            }
        }
    };
}

macro_rules! star_fast {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        {
            let mut start = $state.offset();
            while question_fast!($state, $production(state $(, $p)*)).is_some() && $state.offset() != start {
                start = $state.offset();
            }
        }
    };
}

macro_rules! plus {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        match $production($state $(, $p)*) {
            Ok(()) => {
                star!($state, $production(state $(, $p)*));
                Result::<(), ()>::Ok(())
            },
            Err(()) => Result::<(), ()>::Err(()),
        }
    };
}

macro_rules! plus_fast {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        match $production($state $(, $p)*) {
            Ok(()) => {
                star_fast!($state, $production(state $(, $p)*));
                Result::<(), ()>::Ok(())
            },
            Err(()) => Result::<(), ()>::Err(()),
        }
    };
}

macro_rules! question {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        {
            let start = $state.location();
            let res = $state.with_rollback(|state| $production(state $(, $p)*));
            match res {
                Ok(r) => Some(r),
                Err(()) => {
                    debug_assert_eq!(start, $state.location());
                    None
                }
            }
        }
    };
}

macro_rules! question_fast {
    ($state:expr, $production:ident($state_param:ident $(, $p:expr)*)) => {
        {
            let start = $state.location();
            let res = $production($state $(, $p)*);
            match res {
                Ok(r) => Some(r),
                Err(()) => {
                    debug_assert_eq!(start, $state.location());
                    None
                }
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
                    Ok(r) => break 'alt Ok(r),
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
                    Ok(r) => break 'alt Ok(r),
                    Err(()) => (),
                }

                debug_assert_eq!(start, $state.location());
            )*

            break 'alt Err(())
        }
    };
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_json<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| matches!(ch, '\x09' | '\x20'..='\u{10ffff}'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_byte_order_mark<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.is_char(char::BYTE_ORDER_MARK) {
        state.token(Token::ByteOrderMark, |state| {
            state.eat_char(char::BYTE_ORDER_MARK)
        })
    } else {
        Ok(())
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_sequence_entry<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::SequenceEntry, |state| state.eat_char('-'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_mapping_key<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::MappingKey, |state| state.eat_char('?'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_mapping_value<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::MappingValue, |state| state.eat_char(':'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_collect_entry<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::CollectionEntry, |state| state.eat_char(','))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_sequence_start<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::SequenceStart, |state| state.eat_char('['))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_sequence_end<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::SequenceEnd, |state| state.eat_char(']'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_mapping_start<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::MappingStart, |state| state.eat_char('{'))
}

fn c_mapping_end<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::MappingEnd, |state| state.eat_char('}'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_comment<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Comment, |state| state.eat_char('#'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_anchor<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Anchor, |state| state.eat_char('&'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_alias<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Alias, |state| state.eat_char('*'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_literal<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Literal, |state| state.eat_char('|'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_folded<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Folded, |state| state.eat_char('>'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_single_quote<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::SingleQuote, |state| state.eat_char('\''))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_double_quote<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::DoubleQuote, |state| state.eat_char('"'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_directive<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Directive, |state| state.eat_char('%'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(char::non_break)
}

#[tracing::instrument(level = "trace", skip(state))]
fn b_break<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('\r').or(state.eat_char('\n'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn b_as_line_feed<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::ScalarBreak, b_break)
}

#[tracing::instrument(level = "trace", skip(state))]
fn b_non_content<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Break, b_break)
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_space<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char(' ')
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_white<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(char::space)
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_whites<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    star_fast!(state, s_white(state));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(char::non_space)
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_dec_digit<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| ch.is_ascii_digit())
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_hex_digit<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| ch.is_ascii_hexdigit())
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_word_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| ch.is_ascii_alphanumeric() || ch == '-')
}

#[tracing::instrument(level = "trace", skip(state))]
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

#[tracing::instrument(level = "trace", skip(state))]
fn ns_tag_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.is_char('!') || state.is(char::flow_indicator) {
        Err(())
    } else {
        ns_uri_char(state)
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_escape<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('\\')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_null<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('0')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_bell<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('a')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_backspace<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('a')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_horizontal_tab<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat(|ch| matches!(ch, 't' | '\t'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_line_feed<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('n')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_vertical_tab<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('v')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_form_feed<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('f')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_carriage_return<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('r')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_escape<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('e')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_space<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char(' ')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_double_quote<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('"')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_slash<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('/')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_backslash<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('\\')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_next_line<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('N')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_non_breaking_space<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('_')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_line_separator<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('L')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_esc_paragraph_separator<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('P')
}

#[tracing::instrument(level = "trace", skip(state))]
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

#[tracing::instrument(level = "trace", skip(state))]
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

#[tracing::instrument(level = "trace", skip(state))]
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

#[tracing::instrument(level = "trace", skip(state))]
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

#[tracing::instrument(level = "trace", skip(state))]
fn s_indent<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    state.token(Token::Indent, |state| {
        for _ in 0..n {
            s_space(state)?;
        }
        Ok(())
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_indent_less_than<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    state.token(Token::Indent, |state| {
        for _ in 1..n {
            if s_space(state).is_err() {
                return Ok(());
            }
        }
        Ok(())
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_indent_less_or_equal<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    state.token(Token::Indent, |state| {
        for _ in 0..n {
            if s_space(state).is_err() {
                return Ok(());
            }
        }
        Ok(())
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_separate_in_line<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.is(char::space) {
        state.token(Token::Separator, |state| {
            Ok(star_fast!(state, s_white(state)))
        })
    } else if state.is_start_of_line() {
        Ok(())
    } else {
        Err(())
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_line_prefix<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockIn | Context::BlockOut => s_block_line_prefix(state, n),
        Context::FlowIn | Context::FlowOut => s_flow_line_prefix(state, n),
        Context::BlockKey | Context::FlowKey => unimplemented!(),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_block_line_prefix<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    s_indent(state, n)
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_flow_line_prefix<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    s_indent(state, n)?;
    s_separate_in_line(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_empty<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        state,
        s_line_prefix(state, n, c),
        s_indent_less_than(state, n)
    )?;
    b_as_line_feed(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn b_l_trimmed<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    b_non_content(state)?;
    star!(state, l_empty(state, n, c));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn b_as_space<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::ScalarSpace, b_break)
}

#[tracing::instrument(level = "trace", skip(state))]
fn b_l_folded<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(state, b_l_trimmed(state, n, c), b_as_space(state))
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_flow_folded<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    question_fast!(state, s_separate_in_line(state));
    b_l_folded(state, n, Context::FlowIn)?;
    s_flow_line_prefix(state, n)
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_nb_comment_text<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_comment(state)?;
    star_fast!(state, nb_char(state));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn b_comment<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if b_non_content(state).is_ok() || state.is_end_of_input() {
        Ok(())
    } else {
        Err(())
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_b_comment<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if s_separate_in_line(state).is_ok() && state.peek() == Some('#') {
        c_nb_comment_text(state)?;
    }
    b_comment(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_comment<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    s_separate_in_line(state)?;
    if state.peek() == Some('#') {
        c_nb_comment_text(state)?;
    }
    b_comment(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_l_comments<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if question!(state, s_b_comment(state)).is_some() || state.is_start_of_line() {
        star!(state, l_comment(state));
        Ok(())
    } else {
        Err(())
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_separate<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockIn | Context::BlockOut | Context::FlowIn | Context::FlowOut => {
            s_separate_lines(state, n)
        }
        Context::BlockKey | Context::FlowKey => s_separate_in_line(state),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_separate_lines<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn comments<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        s_l_comments(state)?;
        s_flow_line_prefix(state, n)
    }

    alt!(state, comments(state, n), s_separate_in_line(state))
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_directive<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_directive(state)?;
    if state.is_str("YAML ") {
        ns_yaml_directive(state)?;
    } else if state.is_str("TAG ") {
        ns_tag_directive(state)?;
    } else {
        ns_reserved_directive(state)?;
    }
    s_l_comments(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_reserved_directive<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    fn param<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
        s_separate_in_line(state)?;
        ns_directive_parameter(state)
    }

    ns_directive_name(state)?;
    star!(state, param(state));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_directive_name<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::DirectiveName, |state| {
        plus_fast!(state, ns_char(state))
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_directive_parameter<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::DirectiveParameter, |state| {
        plus_fast!(state, ns_char(state))
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_yaml_directive<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::DirectiveName, |state| state.eat_str("YAML"))?;
    s_separate_in_line(state)?;
    ns_yaml_version(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_yaml_version<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::YamlVersion, |state| {
        plus_fast!(state, ns_dec_digit(state))?;
        state.eat_char('.')?;
        plus_fast!(state, ns_dec_digit(state))
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_tag_directive<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::DirectiveName, |state| state.eat_str("TAG"))?;
    s_separate_in_line(state)?;
    c_tag_handle(state)?;
    s_separate_in_line(state)?;
    ns_tag_prefix(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_tag_handle<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::TagHandle, |state| alt!(
        state,
        c_named_tag_handle(state),
        c_secondary_tag_handle(state),
        c_primary_tag_handle(state)
    ))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_primary_tag_handle<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('!')
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_secondary_tag_handle<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_str("!!")
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_named_tag_handle<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('!')?;
    plus_fast!(state, ns_word_char(state))?;
    state.eat_char('!')
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_tag_prefix<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::TagPrefix, |state| {
        if state.is_char('!') {
            c_ns_local_tag_prefix(state)
        } else {
            ns_global_tag_prefix(state)
        }
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_ns_local_tag_prefix<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_char('!')?;
    star!(state, ns_uri_char(state));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_global_tag_prefix<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    ns_tag_char(state)?;
    star!(state, ns_uri_char(state));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_ns_properties<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    fn separated_anchor<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(state, n, c)?;
        c_ns_anchor_property(state)
    }

    fn separated_tag<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(state, n, c)?;
        c_ns_tag_property(state)
    }

    match state.peek() {
        Some('!') => {
            c_ns_tag_property(state)?;
            question!(state, separated_anchor(state, n, c));
            Ok(())
        }
        Some('&') => {
            c_ns_anchor_property(state)?;
            question!(state, separated_tag(state, n, c));
            Ok(())
        }
        _ => Err(()),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_ns_tag_property<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.peek_next() == Some('<') {
        c_verbatim_tag(state)
    } else {
        alt!(state, c_ns_shorthand_tag(state), c_non_specific_tag(state))
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_verbatim_tag<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::VerbatimTag, |state| {
        state.eat_str("!<")?;
        plus!(state, ns_uri_char(state))?;
        state.eat_str(">")?;
        Ok(())
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_ns_shorthand_tag<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_tag_handle(state)?;
    state.token(Token::TagSuffix, |state| plus!(state, ns_tag_char(state)))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_non_specific_tag<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::NonSpecificTag, |state| state.eat_char('!'))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_ns_anchor_property<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_anchor(state)?;
    ns_anchor_name(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_anchor_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.is(char::flow_indicator) {
        Err(())
    } else {
        ns_char(state)
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_anchor_name<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::AnchorName, |state| {
        plus_fast!(state, ns_anchor_char(state))
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_ns_alias_node<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_alias(state)?;
    ns_anchor_name(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn e_scalar<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::Empty, |_| Ok(()))
}

#[tracing::instrument(level = "trace", skip(state))]
fn e_node<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    e_scalar(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_double_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    match state.peek() {
        Some('\\') => c_ns_esc_char(state),
        Some('"') => Err(()),
        _ => nb_json(state),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_double_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.is(char::space) {
        Err(())
    } else {
        nb_double_char(state)
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_double_quoted<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    c_double_quote(state)?;
    nb_double_text(state, n, c)?;
    c_double_quote(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_double_text<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockKey | Context::FlowKey => nb_double_one_line(state),
        Context::FlowIn | Context::FlowOut => nb_double_multi_line(state, n),
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_double_one_line<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::DoubleQuoted, |state| {
        star!(state, nb_double_char(state));
        Ok(())
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_double_escaped<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    s_whites(state)?;
    c_escape(state)?;
    b_non_content(state)?;
    star!(state, l_empty(state, n, Context::FlowIn));
    s_flow_line_prefix(state, n)
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_double_break<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    alt!(state, s_double_escaped(state, n), s_flow_folded(state, n))
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_ns_double_in_line<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    fn char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
        s_whites(state)?;
        ns_double_char(state)
    }

    star!(state, char(state));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_double_next_line<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        state.token(Token::DoubleQuoted, |state| {
            ns_double_char(state)?;
            nb_ns_double_in_line(state)?;
            alt!(state, s_double_next_line(state, n), s_whites(state))
        })
    }

    s_double_break(state, n)?;
    question!(state, line(state, n));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_double_multi_line<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    state.token(Token::DoubleQuoted, nb_ns_double_in_line)?;
    alt!(state, s_double_next_line(state, n), s_whites(state))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_quoted_quote<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.eat_str("''")
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_single_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.is_char('\'') {
        c_quoted_quote(state)
    } else {
        nb_json(state)
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_single_char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    if state.is(char::space) {
        Err(())
    } else {
        nb_single_char(state)
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_single_quoted<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    c_single_quote(state)?;
    state.token(Token::SingleQuoted, |state| nb_single_text(state, n, c))?;
    c_single_quote(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_single_text<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockKey | Context::FlowKey => nb_single_one_line(state),
        Context::FlowIn | Context::FlowOut => nb_single_multi_line(state, n),
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_single_one_line<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    star!(state, nb_single_char(state));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_ns_single_in_line<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    fn char<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
        s_whites(state)?;
        ns_single_char(state)
    }

    star!(state, char(state));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_single_next_line<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        ns_double_char(state)?;
        nb_ns_single_in_line(state)?;
        alt!(state, s_single_next_line(state, n), s_whites(state))
    }

    s_flow_folded(state, n)?;
    question!(state, line(state, n));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_single_multi_line<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    nb_ns_single_in_line(state)?;
    alt!(state, s_single_next_line(state, n), s_whites(state))
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_plain_first<R: Receiver>(state: &mut State<R>, c: Context) -> Result<(), ()> {
    match state.peek() {
        Some('?' | ':' | '-') if state.next_is(|ch| char::plain_safe(ch, c)) => {
            state.bump();
            Ok(())
        }
        Some(ch) if char::indicator(ch) => Err(()),
        _ => ns_char(state),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_plain_safe<R: Receiver>(state: &mut State<R>, c: Context) -> Result<(), ()> {
    state.eat(|ch| char::plain_safe(ch, c))
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_plain_char<R: Receiver>(state: &mut State<R>, c: Context) -> Result<(), ()> {
    match state.peek() {
        Some('#') => {
            if state.prev_is(char::non_space) {
                state.bump();
                Ok(())
            } else {
                Err(())
            }
        }
        Some(':') => {
            if state.next_is(|ch| char::plain_safe(ch, c)) {
                state.bump();
                Ok(())
            } else {
                Err(())
            }
        }
        _ => ns_plain_safe(state, c),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_plain<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    match c {
        Context::BlockKey | Context::FlowKey => ns_plain_one_line(state, c),
        Context::FlowIn | Context::FlowOut => ns_plain_multi_line(state, n, c),
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn nb_ns_plain_in_line<R: Receiver>(state: &mut State<R>, c: Context) -> Result<(), ()> {
    fn char<R: Receiver>(state: &mut State<R>, c: Context) -> Result<(), ()> {
        s_whites(state)?;
        ns_plain_char(state, c)
    }

    star!(state, char(state, c));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_plain_one_line<R: Receiver>(state: &mut State<R>, c: Context) -> Result<(), ()> {
    state.token(Token::Scalar, |state| {
        ns_plain_first(state, c)?;
        nb_ns_plain_in_line(state, c)
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_ns_plain_next_line<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    s_flow_folded(state, n)?;
    state.token(Token::Scalar, |state| {
        ns_plain_char(state, c)?;
        nb_ns_plain_in_line(state, c)
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_plain_multi_line<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    ns_plain_one_line(state, c)?;
    star!(state, s_ns_plain_next_line(state, n, c));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_flow_sequence<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    c_sequence_start(state)?;
    question!(state, s_separate(state, n, c));
    question!(state, ns_s_flow_seq_entries(state, n, c.in_flow()));
    c_sequence_end(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_s_flow_seq_entries<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    fn entry<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        c_collect_entry(state)?;
        question!(state, s_separate(state, n, c));
        // todo unroll recursion
        ns_s_flow_seq_entries(state, n, c)
    }

    ns_flow_seq_entry(state, n, c)?;
    question!(state, s_separate(state, n, c));
    question_fast!(state, entry(state, n, c));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_seq_entry<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(state, ns_flow_pair(state, n, c), ns_flow_node(state, n, c))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_flow_mapping<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    c_mapping_start(state)?;
    question!(state, s_separate(state, n, c));
    question!(state, ns_s_flow_map_entries(state, n, c.in_flow()));
    c_mapping_end(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_s_flow_map_entries<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    fn entry<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        c_collect_entry(state)?;
        question!(state, s_separate(state, n, c));
        question!(state, ns_s_flow_map_entries(state, n, c));
        Ok(())
    }

    ns_flow_map_entry(state, n, c)?;
    question!(state, s_separate(state, n, c));
    question_fast!(state, entry(state, n, c));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_map_entry<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    if state.is_char('?') && !state.next_is(char::non_space) {
        c_mapping_key(state)?;
        s_separate(state, n, c)?;
        ns_flow_map_explicit_entry(state, n, c)
    } else {
        ns_flow_map_implicit_entry(state, n, c)
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_map_explicit_entry<R: Receiver>(
    state: &mut State<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn empty<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
        e_node(state)?;
        e_node(state)
    }

    alt!(state, ns_flow_map_implicit_entry(state, n, c), empty(state))
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_map_implicit_entry<R: Receiver>(
    state: &mut State<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    alt!(
        state,
        ns_flow_map_yaml_key_entry(state, n, c),
        c_ns_flow_map_empty_key_entry(state, n, c),
        c_ns_flow_map_json_key_entry(state, n, c)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_map_yaml_key_entry<R: Receiver>(
    state: &mut State<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn value<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        question!(state, s_separate(state, n, c));
        c_ns_flow_map_separate_value(state, n, c)
    }

    ns_flow_yaml_node(state, n, c)?;
    alt!(state, value(state, n, c), e_node(state))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_ns_flow_map_empty_key_entry<R: Receiver>(
    state: &mut State<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    e_node(state)?;
    c_ns_flow_map_separate_value(state, n, c)
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_ns_flow_map_separate_value<R: Receiver>(
    state: &mut State<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn node<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(state, n, c)?;
        ns_flow_node(state, n, c)
    }

    c_mapping_value(state)?;
    if state.is(|ch| char::plain_safe(ch, c)) {
        return Err(());
    }
    alt!(state, node(state, n, c), e_node(state))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_ns_flow_map_json_key_entry<R: Receiver>(
    state: &mut State<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn value<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        question!(state, s_separate(state, n, c));
        c_ns_flow_map_adjacent_value(state, n, c)
    }

    c_flow_json_node(state, n, c)?;
    alt!(state, value(state, n, c), e_node(state))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_ns_flow_map_adjacent_value<R: Receiver>(
    state: &mut State<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    fn node<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        question!(state, s_separate(state, n, c));
        ns_flow_node(state, n, c)
    }

    c_mapping_value(state)?;
    alt!(state, node(state, n, c), e_node(state))
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_pair<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    if state.is_char('?') && !state.next_is(char::non_space) {
        c_mapping_key(state)?;
        s_separate(state, n, c)?;
        ns_flow_map_explicit_entry(state, n, c)
    } else {
        ns_flow_pair_entry(state, n, c)
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_pair_entry<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        state,
        ns_flow_pair_yaml_key_entry(state, n, c),
        c_ns_flow_map_empty_key_entry(state, n, c),
        c_ns_flow_pair_json_key_entry(state, n, c)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_pair_yaml_key_entry<R: Receiver>(
    state: &mut State<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    ns_s_implicit_yaml_key(state, Context::FlowKey)?;
    c_ns_flow_map_separate_value(state, n, c)
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_ns_flow_pair_json_key_entry<R: Receiver>(
    state: &mut State<R>,
    n: i32,
    c: Context,
) -> Result<(), ()> {
    c_s_implicit_json_key(state, Context::FlowKey)?;
    c_ns_flow_map_adjacent_value(state, n, c)
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_s_implicit_yaml_key<R: Receiver>(state: &mut State<R>, c: Context) -> Result<(), ()> {
    state.with_length_limit(1024, |state| {
        ns_flow_yaml_node(state, 0, c)?;
        question!(state, s_separate_in_line(state));
        Ok(())
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_s_implicit_json_key<R: Receiver>(state: &mut State<R>, c: Context) -> Result<(), ()> {
    state.with_length_limit(1024, |state| {
        c_flow_json_node(state, 0, c)?;
        question!(state, s_separate_in_line(state));
        Ok(())
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_yaml_content<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    ns_plain(state, n, c)
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_flow_json_content<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    match state.peek() {
        Some('[') => c_flow_sequence(state, n, c),
        Some('{') => c_flow_mapping(state, n, c),
        Some('\'') => c_single_quoted(state, n, c),
        Some('"') => c_double_quoted(state, n, c),
        _ => Err(()),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_content<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        state,
        ns_flow_yaml_content(state, n, c),
        c_flow_json_content(state, n, c)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_yaml_node<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    fn content<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(state, n, c)?;
        ns_flow_content(state, n, c)
    }

    fn props<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        c_ns_properties(state, n, c)?;
        alt!(state, content(state, n, c), e_scalar(state))
    }

    alt!(
        state,
        c_ns_alias_node(state),
        ns_flow_yaml_content(state, n, c),
        props(state, n, c)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_flow_json_node<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    if state.is(|ch| matches!(ch, '!' | '&')) {
        c_ns_properties(state, n, c)?;
        s_separate(state, n, c)?;
    }
    c_flow_json_content(state, n, c)
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_flow_node<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    fn content<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(state, n, c)?;
        ns_flow_content(state, n, c)
    }

    fn props<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        c_ns_properties(state, n, c)?;
        alt!(state, content(state, n, c), e_scalar(state))
    }

    alt!(
        state,
        c_ns_alias_node(state),
        ns_flow_content(state, n, c),
        props(state, n, c)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_b_block_header<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(i32, Chomping), ()> {
    let (m, t) = match state.peek() {
        Some('1'..='9') => (
            c_indentation_indicator(state)?,
            c_chomping_indicator(state)?,
        ),
        Some('-' | '+') => {
            let t = c_chomping_indicator(state)?;
            let m = c_indentation_indicator(state)?;
            (m, t)
        }
        _ => (None, Chomping::Clip),
    };

    s_b_comment(state)?;

    let m = m.unwrap_or_else(|| state.detect_scalar_indent(n));
    Ok((m, t))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_indentation_indicator<R: Receiver>(state: &mut State<R>) -> Result<Option<i32>, ()> {
    match state.peek() {
        Some(ch @ '1'..='9') => {
            state.token(Token::IndentationIndicator, |state| {
                state.bump();
                Ok(())
            })?;
            Ok(Some(ch.to_digit(10).unwrap() as i32))
        }
        _ => Ok(None),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_chomping_indicator<R: Receiver>(state: &mut State<R>) -> Result<Chomping, ()> {
    if state
        .token(Token::ChompingIndicator, |state| state.eat_char('-'))
        .is_ok()
    {
        Ok(Chomping::Strip)
    } else if state
        .token(Token::ChompingIndicator, |state| state.eat_char('+'))
        .is_ok()
    {
        Ok(Chomping::Keep)
    } else {
        Ok(Chomping::Clip)
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn b_chomped_last<R: Receiver>(state: &mut State<R>, t: Chomping) -> Result<(), ()> {
    if state.is_end_of_input() {
        Ok(())
    } else {
        match t {
            Chomping::Strip => b_non_content(state),
            Chomping::Clip | Chomping::Keep => b_as_line_feed(state),
        }
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_chomped_empty<R: Receiver>(state: &mut State<R>, n: i32, t: Chomping) -> Result<(), ()> {
    match t {
        Chomping::Strip => l_strip_empty(state, n),
        Chomping::Clip => l_strip_empty(state, n),
        Chomping::Keep => l_keep_empty(state, n),
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_strip_empty<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        s_indent_less_or_equal(state, n)?;
        b_non_content(state)
    }

    star!(state, line(state, n));
    question!(state, l_trail_comments(state, n));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_keep_empty<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    star!(state, l_empty(state, n, Context::BlockIn));
    question!(state, l_trail_comments(state, n));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_trail_comments<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    s_indent_less_than(state, n)?;
    c_nb_comment_text(state)?;
    b_comment(state)?;
    star!(state, l_comment(state));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_l_literal<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    c_literal(state)?;
    let (m, t) = c_b_block_header(state, n)?;
    l_literal_content(state, n + m, t)
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_nb_literal_text<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    star!(state, l_empty(state, n, Context::BlockIn));
    s_indent(state, n)?;
    state.token(Token::Scalar, |state| plus_fast!(state, nb_char(state)))
}

#[tracing::instrument(level = "trace", skip(state))]
fn b_nb_literal_next<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    b_as_line_feed(state)?;
    l_nb_literal_text(state, n)
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_literal_content<R: Receiver>(state: &mut State<R>, n: i32, t: Chomping) -> Result<(), ()> {
    fn line<R: Receiver>(state: &mut State<R>, n: i32, t: Chomping) -> Result<(), ()> {
        l_nb_literal_text(state, n)?;
        star!(state, b_nb_literal_next(state, n));
        b_chomped_last(state, t)
    }

    question!(state, line(state, n, t));
    l_chomped_empty(state, n, t)
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_l_folded<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    c_folded(state)?;
    let (m, t) = c_b_block_header(state, n)?;
    l_folded_content(state, n + m, t)
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_nb_folded_text<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    s_indent(state, n)?;
    state.token(Token::Scalar, |state| {
        ns_char(state)?;
        star!(state, nb_char(state));
        Ok(())
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_nb_folded_lines<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        b_l_folded(state, n, Context::BlockIn)?;
        s_nb_folded_text(state, n)
    }

    s_nb_folded_text(state, n)?;
    star!(state, line(state, n));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_nb_spaced_text<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    s_indent(state, n)?;
    state.token(Token::Scalar, |state| {
        s_white(state)?;
        star!(state, nb_char(state));
        Ok(())
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn b_l_spaced<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    b_as_line_feed(state)?;
    star!(state, l_empty(state, n, Context::BlockIn));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_nb_spaced_lines<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        b_l_spaced(state, n)?;
        s_nb_spaced_text(state, n)
    }

    s_nb_spaced_text(state, n)?;
    star!(state, line(state, n));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_nb_same_lines<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    star!(state, l_empty(state, n, Context::BlockIn));
    alt!(
        state,
        l_nb_folded_lines(state, n),
        l_nb_spaced_lines(state, n)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_nb_diff_lines<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn line<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        b_as_line_feed(state)?;
        l_nb_same_lines(state, n)
    }

    l_nb_same_lines(state, n)?;
    star!(state, line(state, n));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_folded_content<R: Receiver>(state: &mut State<R>, n: i32, t: Chomping) -> Result<(), ()> {
    fn line<R: Receiver>(state: &mut State<R>, n: i32, t: Chomping) -> Result<(), ()> {
        l_nb_diff_lines(state, n)?;
        b_chomped_last(state, t)
    }

    question!(state, line(state, n, t));
    l_chomped_empty(state, n, t)
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_block_sequence<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        s_indent(state, n)?;
        c_l_block_seq_entry(state, n)
    }

    let m = state.detect_collection_indent(n);
    plus!(state, entry(state, n + m))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_l_block_seq_entry<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    c_sequence_entry(state)?;
    if state.is(char::non_space) {
        return Err(());
    }
    s_l_block_indented(state, n, Context::BlockIn)
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_l_block_indented<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    fn collection<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        let m = state.detect_entry_indent(n);
        s_indent(state, m)?;
        alt!(
            state,
            ns_l_compact_sequence(state, n + 1 + m),
            ns_l_compact_mapping(state, n + 1 + m)
        )
    }

    fn empty<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
        e_node(state)?;
        s_l_comments(state)
    }

    alt!(
        state,
        collection(state, n),
        s_l_block_node(state, n, c),
        empty(state)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_l_compact_sequence<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        s_indent(state, n)?;
        c_l_block_seq_entry(state, n)
    }

    c_l_block_seq_entry(state, n)?;
    star!(state, entry(state, n));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_block_mapping<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        state.location();
        s_indent(state, n)?;
        ns_l_block_map_entry(state, n)
    }

    let m = state.detect_collection_indent(n);
    plus!(state, entry(state, n + m))
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_l_block_map_entry<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    if state.is_char('?') {
        c_l_block_map_explicit_entry(state, n)
    } else {
        ns_l_block_map_implicit_entry(state, n)
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_l_block_map_explicit_entry<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    c_l_block_map_explicit_key(state, n)?;
    alt!(state, l_block_map_explicit_value(state, n), e_node(state))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_l_block_map_explicit_key<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    c_mapping_key(state)?;
    if !state.is(char::non_space) {
        return Err(());
    }
    s_l_block_indented(state, n, Context::BlockOut)
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_block_map_explicit_value<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    s_indent(state, n)?;
    c_mapping_value(state)?;
    s_l_block_indented(state, n, Context::BlockOut)
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_l_block_map_implicit_entry<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    alt!(state, ns_s_block_map_implicit_key(state), e_node(state))?;
    c_l_block_map_implicit_value(state, n)
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_s_block_map_implicit_key<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    alt!(
        state,
        c_s_implicit_json_key(state, Context::BlockKey),
        ns_s_implicit_yaml_key(state, Context::BlockKey)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_l_block_map_implicit_value<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn empty<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
        e_node(state)?;
        s_l_comments(state)
    }

    c_mapping_value(state)?;
    alt!(
        state,
        s_l_block_node(state, n, Context::BlockOut),
        empty(state)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn ns_l_compact_mapping<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    fn entry<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
        s_indent(state, n)?;
        ns_l_block_map_entry(state, n)
    }

    ns_l_block_map_entry(state, n)?;
    star!(state, entry(state, n));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_l_block_node<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        state,
        s_l_block_in_block(state, n, c),
        s_l_flow_in_block(state, n)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_l_flow_in_block<R: Receiver>(state: &mut State<R>, n: i32) -> Result<(), ()> {
    s_separate(state, n + 1, Context::FlowOut)?;
    ns_flow_node(state, n + 1, Context::FlowOut)?;
    s_l_comments(state)
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_l_block_in_block<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    alt!(
        state,
        s_l_block_scalar(state, n, c),
        s_l_block_collection(state, n, c)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_l_block_scalar<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    s_separate(state, n + 1, c)?;
    if state.is(|ch| matches!(ch, '!' | '&')) {
        c_ns_properties(state, n + 1, c)?;
        s_separate(state, n + 1, c)?;
    }
    if state.is_char('|') {
        c_l_literal(state, n)
    } else if state.is_char('>') {
        c_l_folded(state, n)
    } else {
        Err(())
    }
}

#[tracing::instrument(level = "trace", skip(state))]
fn s_l_block_collection<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
    fn props<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        c_ns_properties(state, n, c)?;
        s_l_comments(state)
    }

    fn tag<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
        c_ns_tag_property(state)?;
        s_l_comments(state)
    }

    fn anchor<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
        c_ns_anchor_property(state)?;
        s_l_comments(state)
    }

    fn separated_props<R: Receiver>(state: &mut State<R>, n: i32, c: Context) -> Result<(), ()> {
        s_separate(state, n, c)?;
        alt!(state, props(state, n, c), tag(state), anchor(state))
    }

    question!(state, separated_props(state, n + 1, c));
    s_l_comments(state)?;
    alt!(
        state,
        l_block_sequence(state, c.seq_spaces(n)),
        l_block_mapping(state, n)
    )
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_document_prefix<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_byte_order_mark(state)?;
    star!(state, l_comment(state));
    Ok(())
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_directives_end<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::DirectivesEnd, |state| state.eat_str("---"))
}

#[tracing::instrument(level = "trace", skip(state))]
fn c_document_end<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    state.token(Token::DocumentEnd, |state| {
        state.eat_str("---")?;
        if !state.is(char::non_space) {
            return Err(());
        }
        Ok(())
    })
}

#[tracing::instrument(level = "trace", skip(state))]
fn l_document_suffix<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    c_document_end(state)?;
    s_l_comments(state)
}

// #[tracing::instrument(level = "trace", skip(state))]
fn l_bare_document<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    s_l_block_node(state, -1, Context::BlockIn)
}

// #[tracing::instrument(level = "trace", skip(state))]
fn l_explicit_document<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    fn empty<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
        e_node(state)?;
        s_l_comments(state)
    }

    c_directives_end(state)?;
    alt!(state, l_bare_document(state), empty(state))
}

// #[tracing::instrument(level = "trace", skip(state))]
fn l_directive_document<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    while state.is_char('%') {
        l_directive(state)?;
    }
    l_explicit_document(state)
}

// #[tracing::instrument(level = "trace", skip(state))]
fn l_any_document<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    alt!(state, l_directive_document(state), l_bare_document(state))
}

// #[tracing::instrument(level = "trace", skip(state))]
pub(super) fn l_yaml_stream<R: Receiver>(state: &mut State<R>) -> Result<(), ()> {
    let mut terminated = true;
    loop {
        l_document_prefix(state)?;
        let read_document = if !terminated {
            question!(state, l_directive_document(state)).is_some()
        } else {
            question!(state, l_any_document(state)).is_some()
        };
        terminated = question!(state, l_document_suffix(state)).is_some();
        if !terminated && !read_document {
            if state.is_end_of_input() {
                return Ok(());
            } else {
                return Err(());
            }
        }
    }
}
