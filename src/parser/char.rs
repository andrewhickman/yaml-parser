use super::Context;

pub(super) const BYTE_ORDER_MARK: char = '\u{feff}';

pub(super) fn printable(ch: char) -> bool {
    matches!(
        ch,
            '\t'
            | '\n'
            | '\x20'..='\x7e'
            | '\u{85}'
            | '\u{a0}'..='\u{fffd}'
            | '\u{010000}'..,
    )
}

pub(super) fn indicator(ch: char) -> bool {
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
}

pub(super) fn flow_indicator(ch: char) -> bool {
    matches!(ch, ',' | '[' | ']' | '{' | '}')
}

pub(super) fn r#break(ch: char) -> bool {
    matches!(ch, '\r' | '\n')
}

pub(super) fn non_break(ch: char) -> bool {
    printable(ch) && !r#break(ch) && ch != BYTE_ORDER_MARK
}

pub(super) fn space(ch: char) -> bool {
    matches!(ch, '\t' | ' ')
}

pub(super) fn non_space(ch: char) -> bool {
    non_break(ch) && !space(ch)
}

pub(super) fn plain_safe(ch: char, c: Context) -> bool {
    match c {
        Context::BlockKey | Context::FlowOut => non_space(ch),
        Context::FlowIn | Context::FlowKey => non_space(ch) && !flow_indicator(ch),
        Context::BlockIn | Context::BlockOut => unimplemented!(),
    }
}
