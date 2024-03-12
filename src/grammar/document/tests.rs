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
    assert_yaml_snapshot!(parse(prefix, "\u{feff}# comment"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}  # comment\n\n#comment2\n"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2 # yaml version"));
    assert_yaml_snapshot!(parse(
        prefix,
        "%TAG !yaml! tag:yaml.org,2002: # tag\n# directive"
    ));
    assert_yaml_snapshot!(parse(prefix, "%FOO bar baz"));
    assert_yaml_snapshot!(parse(prefix, "%FOO bar baz\n# reserved"));
    assert_yaml_snapshot!(parse(
        prefix,
        "%YAML 1.2\n%TAG !yaml! tag:yaml.org,2002:\r\n%FOO bar baz"
    ));
    assert_yaml_snapshot!(parse(
        prefix,
        "%YAML 1.2\n# yaml\n%TAG !yaml! tag:yaml.org,2002:\r\n\t#tag\n%FOO bar baz"
    ));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}%YAML 1.2"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}%TAG !yaml! tag:yaml.org,2002:"));
    assert_yaml_snapshot!(parse(prefix, "\u{feff}%FOO bar baz"));
    assert_yaml_snapshot!(parse(
        prefix,
        "\u{feff}%YAML 1.2\n%TAG !yaml! tag:yaml.org,2002:\r\n%FOO bar baz"
    ));
    assert_yaml_snapshot!(parse(
        prefix,
        "\u{feff}%YAML 1.2\n# yaml\n%TAG !yaml! tag:yaml.org,2002:\r\n\t#tag\n%FOO bar baz"
    ));
}

#[test]
fn test_yaml_directive() {
    assert_yaml_snapshot!(parse(prefix, "%YAML"));
    assert_yaml_snapshot!(parse(prefix, "%YAML "));
    assert_yaml_snapshot!(parse(prefix, "%YAML\n"));
    assert_yaml_snapshot!(parse(prefix, "%YAML foo"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 0"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 0\n"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 0foo"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 0."));
    assert_yaml_snapshot!(parse(prefix, "%YAML 0.\n"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 0.foo"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 0.0foo"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2 foo"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2 \n"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2\t\n"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2\n"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2\r\n"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 0.0"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.0"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.1"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 9999999999999.1"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.9999999999999"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2 \n%YAML 1.2"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.3"));
    assert_yaml_snapshot!(parse(prefix, "%YAML 1.2\n---\nfoo"));
}

#[test]
fn test_tag_directive() {
    assert_yaml_snapshot!(parse(prefix, "%TAG"));
    assert_yaml_snapshot!(parse(prefix, "%TAG "));
    assert_yaml_snapshot!(parse(prefix, "%TAG\n"));
    assert_yaml_snapshot!(parse(prefix, "%TAG foo"));
    assert_yaml_snapshot!(parse(prefix, "%TAG !"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! "));
    assert_yaml_snapshot!(parse(prefix, "%TAG !f"));
    assert_yaml_snapshot!(parse(prefix, "%TAG !f*"));
    assert_yaml_snapshot!(parse(prefix, "%TAG !f\0"));
    assert_yaml_snapshot!(parse(prefix, "%TAG !f! !"));
    assert_yaml_snapshot!(parse(prefix, "%TAG !! !"));
    assert_yaml_snapshot!(parse(prefix, "%TAG !"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! "));
    assert_yaml_snapshot!(parse(prefix, "%TAG !\n"));
    assert_yaml_snapshot!(parse(prefix, "%TAG !*"));
    assert_yaml_snapshot!(parse(prefix, "%TAG !\u{feff}"));
    assert_yaml_snapshot!(parse(prefix, "%TAG !\0"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! !"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! !foo"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! foo"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! [foo]"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! tag:example.com,2000:app/"));
    assert_yaml_snapshot!(parse(prefix, "%TAG !m! !my-"));
    assert_yaml_snapshot!(parse(prefix, "%TAG !! !foo"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! %20"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! !%20"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! !ta%C3%9F"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! !%f0%9d%95%8atring"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! %F0%9F%92%96"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! %00%9F%92%96"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! !a%00%9F%92%96b"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! ![]{}"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! !|"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! |"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! %"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! %\n"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! % "));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! %*x"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! %bar"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! %\0"));
    assert_yaml_snapshot!(parse(prefix, "%TAG ! %foo"));
    assert_yaml_snapshot!(parse(
        prefix,
        "%TAG ! tag:example.com,2000:app/\n%TAG !m! !my-"
    ));
    assert_yaml_snapshot!(parse(
        prefix,
        "%TAG ! tag:example.com,2000:app/\n%TAG ! tag:example.com,2000:app2"
    ));
    assert_yaml_snapshot!(parse(
        prefix,
        "%TAG ! tag:example.com,2000:app/\n%TAG !! tag:example.com,2000:app2"
    ));
    assert_yaml_snapshot!(parse(
        prefix,
        "%TAG !! tag:example.com,2000:app/\n%TAG !! tag:example.com,2000:app2"
    ));
    assert_yaml_snapshot!(parse(
        prefix,
        "%TAG ! tag:example.com,2000:app/\n%TAG !app! tag:example.com,2000:app2"
    ));
    assert_yaml_snapshot!(parse(
        prefix,
        "%TAG !app! tag:example.com,2000:app/\n%TAG !app! tag:example.com,2000:app2"
    ));
}
