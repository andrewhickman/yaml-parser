use std::{fs, path::PathBuf};

use yaml_parser::{CollectionStyle, Error, Event, ScalarStyle, Span};

fn write_event(text: &str, event: Event, span: Span) -> String {
    match event {
        Event::StreamStart { .. } => "+STR".to_owned(),
        Event::StreamEnd => "-STR".to_owned(),
        Event::DocumentStart { .. } => {
            if span.is_empty() {
                "+DOC".to_string()
            } else {
                format!("+DOC {}", &text[span.range()])
            }
        }
        Event::DocumentEnd => {
            if span.is_empty() {
                "-DOC".to_string()
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
        Event::Alias { value } => format!("=ALI *{}", value),
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
            event
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

fn parse(text: &str) -> Result<Vec<String>, Error> {
    let mut events = Vec::new();
    for event in yaml_parser::Parser::from_str(text) {
        let (event, span) = event?;
        events.push(write_event(text, event, span));
    }
    Ok(events)
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
        assert!(parse(&yaml).is_ok());
    } else {
        assert!(parse(&yaml).is_err());
    }
}

include!("suite/cases.gen.rs");
