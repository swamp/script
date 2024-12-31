/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use eira::{
    FileSpanMessage, Header, Kind, Label, Pos, PosSpan, Scope, SourceFileSection, SourceLines,
};
use std::io::stderr;
use yansi::{Color, Paint};

struct TestSource {
    lines: Vec<String>,
}

impl SourceLines for TestSource {
    fn get_line(&self, line_number: usize) -> Option<&str> {
        self.lines.get(line_number - 1).map(String::as_str)
    }
}

fn source_from_raw(raw: &str) -> TestSource {
    TestSource {
        lines: raw.lines().map(String::from).collect(),
    }
}
#[test]
fn one_label() {
    let source = source_from_raw(
        r#"1
fn main() {
    let x = undefined_value;
    println!("{}", x);
}
"#,
    );

    let mut l = SourceFileSection::new();
    l.labels.push(Label {
        start: Pos { x: 13, y: 3 },
        character_count: 15,
        color: Color::BrightGreen,
        text: "this variable is not defined".bold().to_string(),
    });

    l.draw(&source, stderr()).unwrap();
}

#[test]
fn two_labels() {
    let source = source_from_raw(
        r#"1
fn main() {
    let x = undefined_value;
    println!("{}", x);
}
"#,
    );

    let mut l = SourceFileSection::new();
    l.labels.push(Label {
        start: Pos { x: 13, y: 3 },
        character_count: 15,
        color: Color::BrightYellow,

        text: "this variable is not defined".to_string(),
    });
    l.labels.push(Label {
        start: Pos { x: 5, y: 4 },
        character_count: 8,
        color: Color::BrightMagenta,
        text: "function println! is unknown".to_string(),
    });

    eprintln!("---------------");
    l.draw(&source, stderr()).unwrap();
}

#[test]
fn two_labels_on_same_line() {
    let source = source_from_raw(
        r#"1
fn main() {
    let x = undefined_value;
    println!("{}", x);
}
"#,
    );

    let mut l = SourceFileSection::new();
    l.labels.push(Label {
        start: Pos { x: 13, y: 3 },
        character_count: 15,
        color: Color::BrightMagenta,
        text: "this variable is not defined".to_string(),
    });
    l.labels.push(Label {
        start: Pos { x: 9, y: 3 },
        character_count: 1,
        color: Color::BrightYellow,
        text: "not sure what 'x' is".to_string(),
    });
    l.labels.push(Label {
        start: Pos { x: 5, y: 4 },
        character_count: 8,
        color: Color::BrightCyan,
        text: "function println! is unknown".to_string(),
    });

    eprintln!("---------------");
    l.draw(&source, stderr()).unwrap();
}

#[test]
fn two_labels_on_same_line_scope() {
    let source = source_from_raw(
        r#"1
fn main() {
    let x = undefined_value;
    if x {
       skipped_line()
       println!("{}", x);
    }
    another line
    and_another
}
"#,
    );

    let header = Header {
        header_kind: Kind::Error,
        code: 2044,
        message: "Illegal symbol for the type".to_string(),
    };
    header.write(stderr()).expect("header should work");

    FileSpanMessage::write(
        "imaginary/path/render.swamp",
        &PosSpan {
            pos: Pos { x: 14, y: 1 },
            length: 13,
        },
        stderr(),
    )
    .expect("filespan message should work");

    let mut l = SourceFileSection::new();

    l.scopes.push(Scope {
        start: PosSpan {
            pos: Pos { x: 4, y: 4 },
            length: 8,
        },
        end: PosSpan {
            pos: Pos { x: 5, y: 7 },
            length: 1,
        },
        color: Color::Red,
        text: "If scope is here".bold().to_string(),
    });

    l.scopes.push(Scope {
        start: PosSpan {
            pos: Pos { x: 4, y: 2 },
            length: 8,
        },
        end: PosSpan {
            pos: Pos { x: 1, y: 10 },
            length: 1,
        },
        color: Color::Green,
        text: "this is the scope".bold().to_string(),
    });

    let label_color = Color::Rgb(154, 128, 255);

    let unknown_function_message = format!(
        "{}{}{}",
        "function '".bold(),
        "println!".fg(Color::BrightBlue),
        "' is unknown".bold()
    );

    l.labels.push(Label {
        start: Pos { x: 8, y: 6 },
        character_count: 8,
        color: Color::BrightBlue,
        text: unknown_function_message,
    });

    let variable_message = format!(
        "{}{}{}",
        "Variable '".bold(),
        "undefined_value".fg(label_color),
        "' defined".bold()
    );

    l.labels.push(Label {
        start: Pos { x: 13, y: 3 },
        character_count: 15,
        color: label_color,
        text: variable_message,
    });

    l.labels.push(Label {
        start: Pos { x: 9, y: 3 },
        character_count: 1,
        color: Color::BrightCyan,
        text: "not sure what 'x' is".bold().to_string(),
    });

    l.layout();
    l.draw(&source, stderr()).unwrap();
}
