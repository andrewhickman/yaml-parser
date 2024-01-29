use std::{fs, path::PathBuf};

use yaml_parser::{Event, Receiver, Span, Token};

#[derive(Default)]
struct TestReceiver {
    source: String,
    events: Vec<String>,
    scalar: String,
    anchor: String,
}

impl Receiver for TestReceiver {
    #[cfg_attr(not(feature = "tracing"), allow(unused_variables))]
    fn event(&mut self, event: Event, span: Span) {
        match event {
            Event::StreamStart => self.events.push("+STR".to_owned()),
            Event::StreamEnd => self.events.push("-STR".to_owned()),
            Event::DocumentStart => {
                if span.is_empty() {
                    self.events.push("+DOC".to_string())
                } else {
                    self.events
                        .push(format!("+DOC {}", &self.source[span.range()]))
                }
            }
            Event::DocumentEnd => self.events.push("-DOC".to_owned()),
            Event::MappingStart => {
                if self.anchor.is_empty() {
                    self.events.push("+MAP".to_string())
                } else {
                    self.events.push(format!("+MAP &{}", self.anchor));
                    self.anchor.clear();
                }
            },
            Event::MappingEnd => self.events.push("-MAP".to_owned()),
            Event::SequenceStart => {
                if self.anchor.is_empty() {
                    self.events.push("+SEQ".to_string())
                } else {
                    self.events.push(format!("+SEQ &{}", self.anchor));
                    self.anchor.clear();
                }
            },
            Event::SequenceEnd => self.events.push("-SEQ".to_owned()),
            Event::Alias => {
                if self.anchor.is_empty() {
                    self.events.push("=ALI".to_string());
                    panic!();
                } else {
                    self.events.push(format!("=ALI *{}", self.anchor));
                    self.anchor.clear();
                }
            }
            Event::Scalar => {
                let mut event = "=VAL".to_string();
                if !self.anchor.is_empty() {
                    event.push_str(" &");
                    event.push_str(&self.anchor);
                    self.anchor.clear();
                }
                if !self.scalar.is_empty() {
                    event.push(' ');
                    event.push_str(&self.scalar);
                    self.scalar.clear();
                }
                self.events.push(event);
            }
        }
    }

    fn token(&mut self, token: Token, span: Span) {
        // tracing::info!("{:?} {:?}", token, &self.source[span.range()]);
        match token {
            Token::SingleQuote | Token::DoubleQuote | Token::Literal | Token::Folded if self.scalar.is_empty() => {
                self.scalar.push_str(&self.source[span.range()])
            }
            Token::SingleQuoted | Token::DoubleQuoted => {
                self.scalar.push_str(&self.source[span.range()])
            }
            Token::Scalar => {
                if self.scalar.is_empty() {
                    self.scalar.push(':');
                }
                self.scalar.push_str(&self.source[span.range()])
            }
            Token::QuotedQuote => self.scalar.push('\''),
            Token::EscapeCode => match self.scalar[span.range()].as_bytes() {
                [b'0'] => self.scalar.push('\x00'),
                [b'a'] => self.scalar.push('\x07'),
                [b'b'] => self.scalar.push('\x08'),
                [b't' | b'\x09'] => self.scalar.push('\x09'),
                [b'n'] => self.scalar.push('\x0a'),
                [b'v'] => self.scalar.push('\x0b'),
                [b'f'] => self.scalar.push('\x0c'),
                [b'r'] => self.scalar.push('\x0d'),
                [b'e'] => self.scalar.push('\x1b'),
                [b' '] => self.scalar.push('\x20'),
                [b'"'] => self.scalar.push('\x22'),
                [b'/'] => self.scalar.push('\x2f'),
                [b'\\'] => self.scalar.push('\x5c'),
                [b'N'] => self.scalar.push('\u{85}'),
                [b'_'] => self.scalar.push('\u{a0}'),
                [b'L'] => self.scalar.push('\u{2028}'),
                [b'P'] => self.scalar.push('\u{2029}'),
                // todo invalid char?
                [b'x' | b'u' | b'U', hex @ ..] => self.scalar.push(
                    char::from_u32(
                        u32::from_str_radix(std::str::from_utf8(hex).unwrap(), 16).unwrap(),
                    )
                    .unwrap(),
                ),
                _ => panic!("invalid escape"),
            },
            Token::AnchorName => self.anchor.push_str(&self.source[span.range()]),
            Token::ScalarBreak => self.scalar.push_str("\n"),
            Token::ScalarSpace => self.scalar.push(' '),
            _ => (),
        }
    }
}

macro_rules! case {
    ($name:ident, $file:literal) => {
        #[test]
        fn $name() {
            case($file, true);
        }
    };
    ($name:ident, $file:literal, fail: true) => {
        #[test]
        fn $name() {
            case($file, false);
        }
    };
    ($name:ident, $file:literal, skip: true) => {
        #[test]
        #[ignore]
        fn $name() {
            case($file, true);
        }
    };
    ($name:ident, $file:literal, skip: true, fail: true) => {
        #[test]
        #[ignore]
        fn $name() {
            case($file, false);
        }
    };
}

fn case(name: &str, success: bool) {
    let _ = tracing_subscriber::fmt::try_init();

    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("yaml-test-suite")
        .join(name);
    let yaml = fs::read_to_string(path.join("in.yaml")).unwrap();

    let events: Vec<_> = fs::read_to_string(path.join("test.event"))
        .unwrap()
        .lines()
        .map(|line| line.to_owned())
        .collect();

    let mut receiver = TestReceiver {
        source: yaml.clone(),
        events: Vec::new(),
        scalar: String::new(),
        anchor: String::new(),
    };
    if success {
        assert!(yaml_parser::parse(&mut receiver, &yaml).is_ok());
        assert_eq!(receiver.events, events);
    } else {
        assert!(yaml_parser::parse(&mut receiver, &yaml).is_err());
    }
}

include!("suite/cases.gen.rs");
