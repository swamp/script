use eira::{LabelItem, Layout, Pos, ScopeItem, SourceLines};
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

    let mut l = Layout::new();
    l.labels.push(LabelItem {
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

    let mut l = Layout::new();
    l.labels.push(LabelItem {
        start: Pos { x: 13, y: 3 },
        character_count: 15,
        color: Color::BrightYellow,

        text: "this variable is not defined".to_string(),
    });
    l.labels.push(LabelItem {
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

    let mut l = Layout::new();
    l.labels.push(LabelItem {
        start: Pos { x: 13, y: 3 },
        character_count: 15,
        color: Color::BrightMagenta,
        text: "this variable is not defined".to_string(),
    });
    l.labels.push(LabelItem {
        start: Pos { x: 9, y: 3 },
        character_count: 1,
        color: Color::BrightYellow,
        text: "not sure what 'x' is".to_string(),
    });
    l.labels.push(LabelItem {
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
    println!("{}", x);
    another line
}
"#,
    );

    let mut l = Layout::new();

    l.scopes.push(ScopeItem {
        start_y: 3,
        end_y: 4,
        color: Color::Green,
        text: "this is the scope".bold().to_string(),
    });

    let label_color = Color::Rgb(154, 128, 255);
    l.labels.push(LabelItem {
        start: Pos { x: 13, y: 3 },
        character_count: 15,
        color: label_color,
        text: format!(
            "Variable '{}' is not defined",
            "undefined_value".fg(label_color)
        ),
    });

    l.labels.push(LabelItem {
        start: Pos { x: 9, y: 3 },
        character_count: 1,
        color: Color::BrightCyan,
        text: "not sure what 'x' is".bold().to_string(),
    });

    let message = format!(
        "{}{}{}",
        "function '".bold(),
        "println!".fg(Color::BrightBlue),
        "' is unknown".bold()
    );

    l.labels.push(LabelItem {
        start: Pos { x: 5, y: 4 },
        character_count: 8,
        color: Color::BrightBlue,
        text: message,
    });

    l.draw(&source, stderr()).unwrap();
}
