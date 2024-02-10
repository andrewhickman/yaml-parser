use std::{fs, path::PathBuf};

use insta::assert_yaml_snapshot;
use serde::Serialize;
use yaml_parser::{CollectionStyle, Event, Parser, Receiver, ScalarStyle, Span, Token};

#[derive(Default)]
struct TestReceiver {
    tokens: Vec<TokenSer>,
    diagnostics: Vec<DiagnosticSer>,
}

#[derive(Debug, Serialize)]
struct TokenSer {
    token: String,
    start: SpanSer,
    end: SpanSer,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
struct SpanSer {
    #[serde(skip)]
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

fn format_event(text: &str, event: Event, span: Span) -> String {
    match event {
        Event::StreamStart { .. } => "+STR".to_owned(),
        Event::StreamEnd => "-STR".to_owned(),
        Event::DocumentStart { .. } => {
            if span.is_empty() {
                "+DOC".to_owned()
            } else {
                "+DOC ---".to_owned()
            }
        }
        Event::DocumentEnd => {
            if span.is_empty() {
                "-DOC".to_owned()
            } else {
                format!("-DOC {}", &text[span.range()])
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
            event
        }
        Event::MappingEnd => "-MAP".to_owned(),
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
            event
        }
        Event::SequenceEnd => "-SEQ".to_owned(),
        Event::Alias { value } => {
            format!("=ALI *{}", value)
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
                    '\u{a0}' => event.push_str("\\_"),
                    '\u{85}' => event.push_str("\\N"),
                    '\u{2028}' => event.push_str("\\L"),
                    '\u{2029}' => event.push_str("\\P"),
                    _ => event.push(ch),
                }
            }
            event
        }
    }
}

impl Receiver for TestReceiver {
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

    fn diagnostic(&mut self, message: &dyn std::fmt::Display, span: Span) {
        self.diagnostics.push(DiagnosticSer {
            message: message.to_string(),
            start: SpanSer {
                index: span.start.index,
                line: span.start.line,
                column: span.start.column,
            },
            end: SpanSer {
                index: span.end.index,
                line: span.end.line,
                column: span.end.column,
            },
        })
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
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(name);
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
        diagnostics: Vec::new(),
        tokens: Vec::new(),
    };

    let mut events = Vec::new();
    for event in Parser::from_str(text).with_receiver(&mut receiver) {
        match event {
            Ok((event, span)) => {
                let event = format_event(text, event, span);
                println!("{} {}", span.len(), event);
                events.push(event);
            }
            Err(err) => return Err(receiver.diagnostics),
        }
    }

    if !receiver.tokens.is_empty() {
        assert_eq!(receiver.tokens.first().unwrap().start.index, 0);
        for window in receiver.tokens.windows(2) {
            assert_eq!(window[0].end, window[1].start);
        }
        assert_eq!(receiver.tokens.last().unwrap().end.index, text.len());
    }

    Ok((events, receiver.tokens))
}

include!("suite/cases.gen.rs");
