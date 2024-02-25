use insta::assert_yaml_snapshot;

use crate::grammar::{tests::parse, trivia::non_content_break};

#[test]
fn test_line_break() {
    assert_yaml_snapshot!(parse(non_content_break, "\n"));
    assert_yaml_snapshot!(parse(non_content_break, "\r"));
    assert_yaml_snapshot!(parse(non_content_break, "\r\n"));
    assert_yaml_snapshot!(parse(non_content_break, "\0"));
    assert_yaml_snapshot!(parse(non_content_break, "foo"));
}
