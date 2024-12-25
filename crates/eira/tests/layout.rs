use eira::{LabelItem, Layout, Pos, SourceLines};
use std::io::stderr;

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
        height: 0,
        text: "this variable is not defined".to_string(),
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
        height: 0,
        text: "this variable is not defined".to_string(),
    });
    l.labels.push(LabelItem {
        start: Pos { x: 5, y: 4 },
        character_count: 8,
        height: 0,
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
        height: 0,
        text: "this variable is not defined".to_string(),
    });
    l.labels.push(LabelItem {
        start: Pos { x: 9, y: 3 },
        character_count: 1,
        height: 0,
        text: "not sure what 'x' is".to_string(),
    });
    l.labels.push(LabelItem {
        start: Pos { x: 5, y: 4 },
        character_count: 8,
        height: 0,
        text: "function println! is unknown".to_string(),
    });

    eprintln!("---------------");
    l.draw(&source, stderr()).unwrap();
}
