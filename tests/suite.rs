use std::{fs, path::PathBuf};

use yaml_parser::{CollectionStyle, Diagnostic, Event, Receiver, ScalarStyle, Span};

#[derive(Default)]
struct TestReceiver<'t> {
    text: &'t str,
    events: Vec<String>,
}

impl<'t> Receiver for TestReceiver<'t> {
    #[cfg_attr(not(feature = "tracing"), allow(unused_variables))]
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
                event.push_str(value.escape_default().to_string().as_ref());
                self.events.push(event);
            }
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

fn parse(text: &str) -> Result<Vec<String>, Vec<Diagnostic>> {
    let mut receiver = TestReceiver {
        events: Vec::new(),
        text,
    };
    match yaml_parser::parse(&mut receiver, text) {
        Ok(()) => Ok(receiver.events),
        Err(errors) => Err(errors),
    }
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

    if success {
        assert_eq!(parse(&yaml).unwrap(), events);
    } else {
        assert!(parse(&yaml).is_err());
    }
}

include!("suite/cases.gen.rs");
