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
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<Document<'s>, Diagnostic> {
    super::prefix(cursor, receiver, false)
}

fn directives<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<Document<'s>, Diagnostic> {
    let mut document = Document::default();
    super::directives(cursor, receiver, &mut document)?;
    Ok(document)
}

#[test]
fn test_prefix() {
    assert_yaml_snapshot!(parse(prefix, ""));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}"));
    assert_yaml_snapshot!(parse(prefix, " "));
    assert_yaml_snapshot!(parse(prefix, "\u{feff} "));
    assert_yaml_snapshot!(parse(prefix, "# comment"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}# comment"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}  # comment\n\n#comment2\n"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2\n---"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2 # yaml version\n---"));
    assert_yaml_snapshot!(parse(
        prefix,
        "%TAG !yaml! tag:yaml.org,2002: # tag\n# directive\n---"
    ));
    assert_yaml_snapshot!(parse(prefix, "%FOO bar baz\n---"));
    assert_yaml_snapshot!(parse(prefix, "%FOO bar baz\n# reserved\n---"));
    assert_yaml_snapshot!(parse(
        prefix,
        "%YAML 1.2\n%TAG !yaml! tag:yaml.org,2002:\r\n%FOO bar baz\n---"
    ));
    assert_yaml_snapshot!(parse(
        prefix,
        "%YAML 1.2\n# yaml\n%TAG !yaml! tag:yaml.org,2002:\r\n\t#tag\n%FOO bar baz\n---"
    ));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}%YAML 1.2\n---"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}%TAG !yaml! tag:yaml.org,2002:\n---"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}%FOO bar baz\n---"));
    assert_yaml_snapshot!(parse(
        prefix,
        "\u{feff}%YAML 1.2\n%TAG !yaml! tag:yaml.org,2002:\r\n%FOO bar baz\n---"
    ));
    assert_yaml_snapshot!(parse(
        prefix,
        "\u{feff}%YAML 1.2\n# yaml\n%TAG !yaml! tag:yaml.org,2002:\r\n\t#tag\n%FOO bar baz\n---"
    ));
    assert_yaml_snapshot!(parse(prefix, "---"));
    assert_yaml_snapshot!(parse(prefix, "---foo"));
    assert_yaml_snapshot!(parse(prefix, "---\nfoo"));
    assert_yaml_snapshot!(parse(prefix, "--- foo"));
    assert_yaml_snapshot!(parse(prefix, "---\tfoo"));
    assert_yaml_snapshot!(parse(prefix, " ---"));
    assert_yaml_snapshot!(parse(prefix, " ---foo"));
    assert_yaml_snapshot!(parse(prefix, " ---\nfoo"));
    assert_yaml_snapshot!(parse(prefix, " --- foo"));
    assert_yaml_snapshot!(parse(prefix, " ---\tfoo"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2\nfoo"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2---"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2---foo"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2\n ---"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2\n\t---"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2\n---foo"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2\n--- foo"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2\n---\tfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "---"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "---foo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "---\nfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "--- foo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "---\tfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, " ---"));
    assert_yaml_snapshot!(parse(prefix_unterminated, " ---foo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, " ---\nfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, " --- foo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, " ---\tfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "%YAML 1.2\nfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "%YAML 1.2---"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "%YAML 1.2\n ---"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "%YAML 1.2\n\t---"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "%YAML 1.2\n---foo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, ""));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\u{feff}"));
    assert_yaml_snapshot!(parse(prefix_unterminated, " "));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\u{feff} "));
    assert_yaml_snapshot!(parse(prefix_unterminated, "# comment"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\u{feff}# comment"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "#comment\nfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\u{feff}---"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\u{feff}---foo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\u{feff}---\nfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\u{feff}--- foo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\u{feff}---\tfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\u{feff}#comment\nfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\u{feff}#comment\n---\nfoo"));
    assert_yaml_snapshot!(parse(
        prefix,
        "%YAML invalid\n# yaml\n%TAG !yaml! tag:yaml.org,2002:\n%FOO bar baz\n---"
    ));
    assert_yaml_snapshot!(parse(
        prefix,
        "%TAG invalid\n%YAML 1.2\n# yaml\n%FOO bar baz\n---"
    ));
    assert_yaml_snapshot!(parse(
        prefix,
        "%FOO bar invalid\0 \n%YAML 1.2\n# yaml\n%TAG !yaml! tag:yaml.org,2002:\n---"
    ));
    assert_yaml_snapshot!(parse(prefix, "foo"));
    assert_yaml_snapshot!(parse(prefix, " foo"));
    assert_yaml_snapshot!(parse(prefix, "\tfoo"));
    assert_yaml_snapshot!(parse(prefix, "\nfoo"));
    assert_yaml_snapshot!(parse(prefix, "\n foo"));
    assert_yaml_snapshot!(parse(prefix, "\n\tfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "foo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, " foo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\tfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\nfoo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\n foo"));
    assert_yaml_snapshot!(parse(prefix_unterminated, "\n\tfoo"));
}

#[test]
fn test_yaml_directive() {
    assert_yaml_snapshot!(parse(directives, "%YAML"));
    assert_yaml_snapshot!(parse(directives, "%YAML "));
    assert_yaml_snapshot!(parse(directives, "%YAML\n"));
    assert_yaml_snapshot!(parse(directives, "%YAML foo"));
    assert_yaml_snapshot!(parse(directives, "%YAML 0"));
    assert_yaml_snapshot!(parse(directives, "%YAML 0\n"));
    assert_yaml_snapshot!(parse(directives, "%YAML 0foo"));
    assert_yaml_snapshot!(parse(directives, "%YAML 0."));
    assert_yaml_snapshot!(parse(directives, "%YAML 0.\n"));
    assert_yaml_snapshot!(parse(directives, "%YAML 0.foo"));
    assert_yaml_snapshot!(parse(directives, "%YAML 0.0foo"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.2 foo"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.2 \n"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.2\t\n"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.2\n"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.2\r\n"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.2"));
    assert_yaml_snapshot!(parse(directives, "%YAML 0.0"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.0"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.1"));
    assert_yaml_snapshot!(parse(directives, "%YAML 9999999999999.1"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.9999999999999"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.2 \n%YAML 1.2"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.3"));
    assert_yaml_snapshot!(parse(directives, "%YAML 1.2\n---\nfoo"));
}

#[test]
fn test_tag_directive() {
    assert_yaml_snapshot!(parse(directives, "%TAG"));
    assert_yaml_snapshot!(parse(directives, "%TAG "));
    assert_yaml_snapshot!(parse(directives, "%TAG\n"));
    assert_yaml_snapshot!(parse(directives, "%TAG foo"));
    assert_yaml_snapshot!(parse(directives, "%TAG !"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! "));
    assert_yaml_snapshot!(parse(directives, "%TAG !f"));
    assert_yaml_snapshot!(parse(directives, "%TAG !f*"));
    assert_yaml_snapshot!(parse(directives, "%TAG !f\0"));
    assert_yaml_snapshot!(parse(directives, "%TAG !f! !"));
    assert_yaml_snapshot!(parse(directives, "%TAG !! !"));
    assert_yaml_snapshot!(parse(directives, "%TAG !"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! "));
    assert_yaml_snapshot!(parse(directives, "%TAG !\n"));
    assert_yaml_snapshot!(parse(directives, "%TAG !*"));
    assert_yaml_snapshot!(parse(directives, "%TAG !\u{feff}"));
    assert_yaml_snapshot!(parse(directives, "%TAG !\0"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! !"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! !foo"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! foo"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! [foo]"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! tag:example.com,2000:app/"));
    assert_yaml_snapshot!(parse(directives, "%TAG !m! !my-"));
    assert_yaml_snapshot!(parse(directives, "%TAG !! !foo"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! %20"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! !%20"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! !ta%C3%9F"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! !%f0%9d%95%8atring"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! %F0%9F%92%96"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! %00%9F%92%96"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! !a%00%9F%92%96b"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! ![]{}"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! !|"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! |"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! %"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! %\n"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! % "));
    assert_yaml_snapshot!(parse(directives, "%TAG ! %*x"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! %bar"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! %\0"));
    assert_yaml_snapshot!(parse(directives, "%TAG ! %foo"));
    assert_yaml_snapshot!(parse(
        directives,
        "%TAG ! tag:example.com,2000:app/\n%TAG !m! !my-"
    ));
    assert_yaml_snapshot!(parse(
        directives,
        "%TAG ! tag:example.com,2000:app/\n%TAG ! tag:example.com,2000:app2"
    ));
    assert_yaml_snapshot!(parse(
        directives,
        "%TAG ! tag:example.com,2000:app/\n%TAG !! tag:example.com,2000:app2"
    ));
    assert_yaml_snapshot!(parse(
        directives,
        "%TAG !! tag:example.com,2000:app/\n%TAG !! tag:example.com,2000:app2"
    ));
    assert_yaml_snapshot!(parse(
        directives,
        "%TAG ! tag:example.com,2000:app/\n%TAG !app! tag:example.com,2000:app2"
    ));
    assert_yaml_snapshot!(parse(
        directives,
        "%TAG !app! tag:example.com,2000:app/\n%TAG !app! tag:example.com,2000:app2"
    ));
}

#[test]
fn test_reserved_directive() {
    assert_yaml_snapshot!(parse(directives, "%FOO"));
    assert_yaml_snapshot!(parse(directives, "%FOO "));
    assert_yaml_snapshot!(parse(directives, "%FOO\n"));
    assert_yaml_snapshot!(parse(directives, "%FOO a b c"));
    assert_yaml_snapshot!(parse(directives, "%FOO a # comment"));
    assert_yaml_snapshot!(parse(directives, "%FOO a b# comment"));
}
