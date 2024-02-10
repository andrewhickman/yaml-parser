use crate::grammar::Context;

pub(super) const BYTE_ORDER_MARK: char = '\u{feff}';
pub(super) const SEQUENCE_ENTRY: char = '-';
pub(super) const MAPPING_KEY: char = '?';
pub(super) const MAPPING_VALUE: char = ':';
pub(super) const COLLECTION_ENTRY: char = ',';
pub(super) const SEQUENCE_START: char = '[';
pub(super) const SEQUENCE_END: char = ']';
pub(super) const MAPPING_START: char = '{';
pub(super) const MAPPING_END: char = '}';
pub(super) const COMMENT: char = '#';
pub(super) const ANCHOR: char = '&';
pub(super) const ALIAS: char = '*';
pub(super) const TAG: char = '!';
pub(super) const LITERAL: char = '|';
pub(super) const FOLDED: char = '>';
pub(super) const SINGLE_QUOTE: char = '\'';
pub(super) const DOUBLE_QUOTE: char = '"';
pub(super) const DIRECTIVE: char = '%';
pub(super) const ESCAPE: char = '\\';

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

pub(super) fn json(ch: char) -> bool {
    matches!(ch, '\x09' | '\x20'..='\u{10ffff}')
}

pub(super) fn word(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '-'
}

pub(super) fn indicator(ch: char) -> bool {
    matches!(
        ch,
        SEQUENCE_ENTRY
            | MAPPING_KEY
            | MAPPING_VALUE
            | COLLECTION_ENTRY
            | SEQUENCE_START
            | SEQUENCE_END
            | MAPPING_START
            | MAPPING_END
            | COMMENT
            | ANCHOR
            | ALIAS
            | TAG
            | LITERAL
            | FOLDED
            | SINGLE_QUOTE
            | DOUBLE_QUOTE
            | DIRECTIVE
            | '@'
            | '`'
    )
}

pub(super) fn flow_indicator(ch: char) -> bool {
    matches!(
        ch,
        COLLECTION_ENTRY | SEQUENCE_START | SEQUENCE_END | MAPPING_START | MAPPING_END
    )
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
