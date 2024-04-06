use insta::assert_yaml_snapshot;

use crate::grammar::{
    tests::{parse, parse_unseparated},
    trivia::{comment_lines, non_content_break, trailing_lines},
};

#[test]
fn test_line_break() {
    assert_yaml_snapshot!(parse(non_content_break, "\n"));
    assert_yaml_snapshot!(parse(non_content_break, "\r"));
    assert_yaml_snapshot!(parse(non_content_break, "\r\n"));
    assert_yaml_snapshot!(parse(non_content_break, "\0"));
    assert_yaml_snapshot!(parse(non_content_break, "foo"));
}

#[test]
fn test_comment_lines() {
    assert_yaml_snapshot!(parse(comment_lines, ""));
    assert_yaml_snapshot!(parse(comment_lines, "# comment"));
    assert_yaml_snapshot!(parse(comment_lines, "\n"));
    assert_yaml_snapshot!(parse(comment_lines, "\r\n"));
    assert_yaml_snapshot!(parse(comment_lines, "# comment\n"));
    assert_yaml_snapshot!(parse(comment_lines, " # comment"));
    assert_yaml_snapshot!(parse(comment_lines, "\t# comment"));
    assert_yaml_snapshot!(parse(comment_lines, " \n"));
    assert_yaml_snapshot!(parse(comment_lines, "\t\r\n\n\r\r\n\r\r"));
    assert_yaml_snapshot!(parse(comment_lines, "# comment\n"));
    assert_yaml_snapshot!(parse(comment_lines, "# comment\nfoo"));
    assert_yaml_snapshot!(parse(comment_lines, "foo"));
    assert_yaml_snapshot!(parse(comment_lines, "  # one\r\n# two\n\t#three\r   "));
    assert_yaml_snapshot!(parse(comment_lines, "# comment\n\n# two"));
    assert_yaml_snapshot!(parse(comment_lines, "# comment\0"));
    assert_yaml_snapshot!(parse(comment_lines, "# comment\0\r\n"));
}

#[test]
fn test_separator_lines() {
    assert_yaml_snapshot!(parse(trailing_lines, ""));
    assert_yaml_snapshot!(parse(trailing_lines, "#one"));
    assert_yaml_snapshot!(parse(trailing_lines, "#one\n"));
    assert_yaml_snapshot!(parse(trailing_lines, "#one\n#two"));
    assert_yaml_snapshot!(parse(trailing_lines, "foo"));
    assert_yaml_snapshot!(parse(trailing_lines, "\nfoo"));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, ""));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, "#one"));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, "#one\n"));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, "#one\n#two"));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, "foo"));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, "\nfoo"));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, " "));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, " #one"));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, " #one\n"));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, " #one\n#two"));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, " foo"));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, " \nfoo"));
    assert_yaml_snapshot!(parse_unseparated(trailing_lines, " #comment\nfoo"));
}
