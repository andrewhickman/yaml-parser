use std::{fs, path::PathBuf};

use insta::assert_yaml_snapshot;
use serde::Serialize;
use yaml_parser::{CollectionStyle, Event, Receiver, ScalarStyle, Span, Token};

#[derive(Default)]
struct TestReceiver<'t> {
    text: &'t str,
    events: Vec<String>,
    tokens: Vec<TokenSer>,
}

#[derive(Debug, Serialize)]
struct TokenSer {
    token: String,
    start: SpanSer,
    end: SpanSer,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
struct SpanSer {
    index: usize,
    line: usize,
    column: usize,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
struct DiagnosticSer {
    message: String,
    start: SpanSer,
    end: SpanSer,
}

impl<'t> Receiver for TestReceiver<'t> {
    fn event(&mut self, event: Event, span: Span) {
        match event {
            Event::StreamStart => self.events.push("+STR".to_owned()),
            Event::StreamEnd => self.events.push("-STR".to_owned()),
            Event::DocumentStart { .. } => {
                if span.is_empty() {
                    self.events.push("+DOC".to_string())
                } else {
                    self.events
                        .push(format!("+DOC {}", &self.text[span.range()]))
                }
            }
            Event::DocumentEnd => {
                if span.is_empty() {
                    self.events.push("-DOC".to_string())
                } else {
                    self.events
                        .push(format!("-DOC {}", &self.text[span.range()]))
                }
            }
            Event::MappingStart { style, anchor, tag } => {
                let mut event = "+MAP".to_owned();
                if style == CollectionStyle::Flow {
                    event.push_str(" {}");
                }
                if let Some(anchor) = anchor {
                    event.push_str(" &");
                    event.push_str(anchor.as_ref());
                }
                if let Some(tag) = tag {
                    event.push(' ');
                    event.push_str(tag.as_ref());
                }
                self.events.push(event);
            }
            Event::MappingEnd => self.events.push("-MAP".to_owned()),
            Event::SequenceStart { style, anchor, tag } => {
                let mut event = "+SEQ".to_owned();
                if style == CollectionStyle::Flow {
                    event.push_str(" []");
                }
                if let Some(anchor) = anchor {
                    event.push_str(" &");
                    event.push_str(anchor.as_ref());
                }
                if let Some(tag) = tag {
                    event.push(' ');
                    event.push_str(tag.as_ref());
                }
                self.events.push(event);
            }
            Event::SequenceEnd => self.events.push("-SEQ".to_owned()),
            Event::Alias { value } => {
                self.events.push(format!("=ALI *{}", value));
            }
            Event::Scalar {
                style,
                value,
                anchor,
                tag,
            } => {
                let mut event = "=VAL".to_owned();
                if let Some(anchor) = anchor {
                    event.push_str(" &");
                    event.push_str(anchor.as_ref());
                }
                if let Some(tag) = tag {
                    event.push(' ');
                    event.push_str(tag.as_ref());
                }
                match style {
                    ScalarStyle::Plain => event.push_str(" :"),
                    ScalarStyle::SingleQuoted => event.push_str(" '"),
                    ScalarStyle::DoubleQuoted => event.push_str(" \""),
                    ScalarStyle::Literal => event.push_str(" |"),
                    ScalarStyle::Folded => event.push_str(" >"),
                }
                for ch in value.chars() {
                    match ch {
                        '\0' => event.push_str("\\0"),
                        '\x07' => event.push_str("\\a"),
                        '\x08' => event.push_str("\\b"),
                        '\x09' => event.push_str("\\t"),
                        '\x0a' => event.push_str("\\n"),
                        '\x0b' => event.push_str("\\v"),
                        '\x0c' => event.push_str("\\f"),
                        '\x0d' => event.push_str("\\r"),
                        '\x1b' => event.push_str("\\e"),
                        '\\' => event.push_str("\\\\"),
                        _ => event.push(ch),
                    }
                }
                self.events.push(event);
            }
        }
    }

    fn token(&mut self, token: Token, span: Span) {
        let token = format!("{:?}", token);
        let start = SpanSer {
            index: span.start.index,
            line: span.start.line,
            column: span.start.column,
        };
        let end = SpanSer {
            index: span.end.index,
            line: span.end.line,
            column: span.end.column,
        };

        if let Some(prev) = self.tokens.last_mut() {
            debug_assert_eq!(prev.end, start);
            if prev.token == token {
                prev.end = end;
                return;
            }
        }

        self.tokens.push(TokenSer { token, start, end });
    }
}

macro_rules! case {
    ($name:ident, $file:literal) => {
        #[test]
        fn $name() {
            let (yaml, expected_events) = load($file);
            let (actual_events, tokens) = parse(&yaml).unwrap();
            assert_eq!(actual_events, expected_events);
            assert_yaml_snapshot!(tokens);
        }
    };
    ($name:ident, $file:literal, fail: true) => {
        #[test]
        fn $name() {
            let (yaml, _) = load($file);
            let diagnostics = parse(&yaml).unwrap_err();
            assert_yaml_snapshot!(diagnostics);
        }
    };
}

fn load(name: &str) -> (String, Vec<String>) {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("yaml-test-suite")
        .join(name);
    let yaml = fs::read_to_string(path.join("in.yaml")).unwrap();

    let expected_events: Vec<_> = fs::read_to_string(path.join("test.event"))
        .unwrap()
        .lines()
        .map(|line| line.to_owned())
        .collect();

    (yaml, expected_events)
}

fn parse(text: &str) -> Result<(Vec<String>, Vec<TokenSer>), Vec<DiagnosticSer>> {
    let mut receiver = TestReceiver {
        events: Vec::new(),
        tokens: Vec::new(),
        text,
    };
    match yaml_parser::parse(&mut receiver, text) {
        Ok(()) => Ok((receiver.events, receiver.tokens)),
        Err(errors) => Err(errors
            .into_iter()
            .map(|diag| DiagnosticSer {
                message: diag.message,
                start: SpanSer {
                    index: diag.span.start.index,
                    line: diag.span.start.line,
                    column: diag.span.start.column,
                },
                end: SpanSer {
                    index: diag.span.end.index,
                    line: diag.span.end.line,
                    column: diag.span.end.column,
                },
            })
            .collect()),
    }
}

include!("suite/cases.gen.rs");
