use insta::assert_yaml_snapshot;

use crate::{cursor::Cursor, grammar::tests::parse, Diagnostic, Receiver};

use super::Document;

fn prefix<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<Document<'s>, Diagnostic> {
    super::prefix(cursor, receiver, true)
}

fn prefix_unterminated<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized + 'static),
) -> Result<Document<'s>, Diagnostic> {
    super::prefix(cursor, receiver, false)
}

#[test]
fn test_prefix() {
    assert_yaml_snapshot!(parse(prefix, ""));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}"));
    assert_yaml_snapshot!(parse(prefix, " "));
    assert_yaml_snapshot!(parse(prefix, "\u{feff} "));
    assert_yaml_snapshot!(parse(prefix, "# comment"));
    assert_yaml_snapshot!(parse(prefix, "\t# comment"));
    assert_yaml_snapshot!(parse(prefix, " \t\t # comment"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}# comment"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff} # comment"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}\t# comment"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}\t \t# comment"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}\t \t# comment\r"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}\t \t# comment\n"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}\t \t# comment\r\n"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}\t \t# comment\n \n"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}\t \t# comment\n#line\n"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}\t \t# comment\n #line\n"));
}
