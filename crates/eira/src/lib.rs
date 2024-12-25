use std::io;
use std::io::Write;
use yansi::{Color, Paint};

#[derive(PartialEq)]
pub struct Pos {
    pub x: usize,
    pub y: usize,
}

pub struct PosSpan {
    pub pos: Pos,
    pub length: usize,
}
pub struct ScopeItem {
    pub start_y: usize,
    pub end_y: usize,
    pub text: String,
    pub color: Color,
}

#[derive(PartialEq)]
pub struct LabelItem {
    pub start: Pos,
    pub character_count: usize,
    pub text: String,
    pub color: Color,
}

pub trait SourceLines {
    fn get_line(&self, line_number: usize) -> Option<&str>;
}

// Only one file is supported for a Layout
pub struct Layout {
    pub scopes: Vec<ScopeItem>,
    pub labels: Vec<LabelItem>,
}

impl Default for Layout {
    fn default() -> Self {
        Self::new()
    }
}

impl Layout {
    #[must_use]
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            labels: vec![],
        }
    }
    fn pad<W: Write>(count: usize, mut writer: W) -> io::Result<()> {
        for _ in 0..count {
            write!(writer, " ")?;
        }
        Ok(())
    }

    fn write_source_line<W: Write>(
        source_line: &str,
        line_labels: &[&LabelItem],
        mut writer: W,
    ) -> io::Result<()> {
        let chars: Vec<char> = source_line.chars().collect();
        let mut current_pos = 0;

        while current_pos < chars.len() {
            let matching_label = line_labels.iter().find(|label| {
                (current_pos + 1) >= label.start.x
                    && (current_pos + 1) < label.start.x + label.character_count
            });

            let mut region_end = current_pos;
            while region_end < chars.len() {
                let next_label = line_labels.iter().find(|label| {
                    (region_end + 1) >= label.start.x
                        && (region_end + 1) < label.start.x + label.character_count
                });
                if next_label != matching_label {
                    break;
                }
                region_end += 1;
            }

            let text: String = chars[current_pos..region_end].iter().collect();
            if let Some(label) = matching_label {
                write!(writer, "{}", text.fg(label.color))?;
            } else {
                write!(writer, "{text}")?;
            }

            current_pos = region_end;
        }

        writeln!(writer)?;
        Ok(())
    }

    pub fn calculate_lines_that_must_be_shown(&self) -> Vec<usize> {
        // Only filter out source code lines that will be referenced by scope or labels
        let mut lines_to_show: Vec<usize> = self
            .labels
            .iter()
            .map(|label| label.start.y)
            .chain(
                self.scopes
                    .iter()
                    .flat_map(|scope| vec![scope.start_y, scope.end_y]),
            )
            .collect();
        lines_to_show.sort_unstable();
        lines_to_show.dedup();
        lines_to_show
    }

    pub fn layout(&mut self) {
        // Sort scopes first by start_y and then end_y in reverse
        self.scopes.sort_by(|a, b| match a.start_y.cmp(&b.start_y) {
            std::cmp::Ordering::Equal => b.end_y.cmp(&a.end_y),
            other => other,
        });

        // Sort label positions by y first and then by x.
        self.labels.sort_by(|a, b| match a.start.y.cmp(&b.start.y) {
            std::cmp::Ordering::Equal => a.start.x.cmp(&b.start.x),
            other => other,
        });
    }

    fn write_scope_continuation<W: Write>(
        active_scopes: &[&ScopeItem],
        max_scopes: usize,
        mut writer: W,
    ) -> io::Result<()> {
        for i in 0..max_scopes {
            if let Some(scope) = active_scopes.get(i) {
                write!(writer, "{}   ", "│".fg(scope.color))?;
            } else {
                write!(writer, "    ")?;
            }
        }
        Ok(())
    }

    fn write_line_prefix<W: Write>(
        max_line_num_width: usize,
        line_number: Option<usize>,
        mut writer: W,
    ) -> io::Result<()> {
        match line_number {
            Some(num) => write!(writer, "{num:>max_line_num_width$} │ ")?,
            None => write!(writer, "{:>max_line_num_width$} . ", "")?,
        }
        Ok(())
    }

    /// Draws the lines
    /// # Errors
    ///
    pub fn draw<W: Write, S: SourceLines>(&self, source: &S, mut writer: W) -> io::Result<()> {
        let lines_to_show = self.calculate_lines_that_must_be_shown();

        // Calculate the maximum number of simultaneous scopes
        let max_scopes = self.scopes.iter().fold(0, |max_count, scope| {
            let overlapping = self
                .scopes
                .iter()
                .filter(|other| scope.start_y <= other.end_y && other.start_y <= scope.end_y)
                .count();
            max_count.max(overlapping)
        });

        let max_line_num_width = lines_to_show
            .iter()
            .max()
            .map_or(0, |&max_line| max_line.to_string().len());

        for &current_line in &lines_to_show {
            let source_line = source
                .get_line(current_line)
                .expect("Source code lines with the requested line number should exist");

            // Get active scopes for source line (includes end line)
            let active_scopes: Vec<_> = self
                .scopes
                .iter()
                .filter(|scope| scope.start_y <= current_line && current_line <= scope.end_y)
                .collect();

            // Get labels for the current line and sort labels by x position in reverse order (right to left)
            let mut line_labels: Vec<_> = self
                .labels
                .iter()
                .filter(|label| label.start.y == current_line)
                .collect();
            line_labels.sort_by_key(|label| std::cmp::Reverse(label.start.x));

            Self::write_line_prefix(max_line_num_width, Some(current_line), &mut writer)?;
            for i in 0..max_scopes {
                let prefix = active_scopes.get(i).map_or("   ".to_string(), |scope| {
                    let scope_line_prefix = if current_line == scope.start_y {
                        "╭─▶"
                    } else if current_line == scope.end_y {
                        "├─▶"
                    } else {
                        "│  "
                    };
                    scope_line_prefix.fg(scope.color).to_string()
                });
                write!(writer, "{prefix} ")?;
            }
            Self::write_source_line(source_line, &line_labels, &mut writer)?;

            // Underlines
            Self::write_line_prefix(max_line_num_width, None, &mut writer)?;
            Self::write_scope_continuation(&active_scopes, max_scopes, &mut writer)?;

            let mut current_pos = 0;

            for label in line_labels.iter().rev() {
                // Pad exactly to the label's x position
                if label.start.x > current_pos {
                    Self::pad(label.start.x - 1 - current_pos, &mut writer)?;
                }

                let middle = (label.character_count - 1) / 2;
                let underline: String = (0..label.character_count)
                    .map(|i| if i == middle { '┬' } else { '─' })
                    .collect();

                write!(writer, "{}", underline.fg(label.color))?;

                current_pos = label.start.x - 1 + label.character_count;
            }
            writeln!(writer)?;

            // Write labels with scope continuation
            for (idx, label) in line_labels.iter().enumerate() {
                Self::write_line_prefix(max_line_num_width, None, &mut writer)?;
                Self::write_scope_continuation(&active_scopes, max_scopes, &mut writer)?;

                let mut current_pos = 0;

                // Draw vertical bars for all labels that will come after this one
                for future_label in line_labels.iter().skip(idx + 1) {
                    let middle =
                        (future_label.start.x - 1) + (future_label.character_count - 1) / 2;
                    Self::pad(middle - current_pos, &mut writer)?;
                    write!(writer, "{}", "│".fg(future_label.color))?;
                    current_pos = middle + 1;
                }

                // TODO: Save the aligned position so it doesn't have to be calculated again.
                let middle = (label.start.x - 1) + (label.character_count - 1) / 2;
                if middle > current_pos {
                    Self::pad(middle - current_pos, &mut writer)?;
                }

                // line length somewhat proportional to the span so it looks nicer
                let dash_count = (label.character_count / 4).clamp(2, 8);
                let connector = format!("╰{}", "─".repeat(dash_count));
                let label_line = format!("{} {}", connector.fg(label.color), label.text);
                write!(writer, "{label_line}")?;
                writeln!(writer)?;
            }

            // Write scope text for any scopes that have ended
            for scope in &active_scopes {
                if scope.end_y == current_line {
                    Self::write_line_prefix(max_line_num_width, None, &mut writer)?;
                    Self::write_scope_continuation(&active_scopes, max_scopes, &mut writer)?;
                    writeln!(writer)?;

                    Self::write_line_prefix(max_line_num_width, None, &mut writer)?;
                    write!(writer, "{} {}", "╰───".fg(scope.color), scope.text,)?;
                    writeln!(writer)?;
                }
            }
        }

        Ok(())
    }
}
