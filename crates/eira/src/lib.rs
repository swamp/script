use std::io;
use std::io::Write;
use yansi::Paint;

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
    pub width: usize,
    pub text: String,
}

pub struct LabelItem {
    pub start: Pos,
    pub character_count: usize,
    pub height: usize,
    pub text: String,
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
            let should_highlight = line_labels.iter().any(|label| {
                (current_pos + 1) >= label.start.x
                    && (current_pos + 1) < label.start.x + label.character_count
            });

            let mut region_end = current_pos;
            while region_end < chars.len() {
                let next_highlighted = line_labels.iter().any(|label| {
                    (region_end + 1) >= label.start.x
                        && (region_end + 1) < label.start.x + label.character_count
                });
                if next_highlighted != should_highlight {
                    break;
                }
                region_end += 1;
            }

            let text: String = chars[current_pos..region_end].iter().collect();
            if should_highlight {
                write!(writer, "{}", Paint::blue(&text))?;
            } else {
                write!(writer, "{text}")?;
            }

            current_pos = region_end;
        }

        writeln!(writer)?;
        Ok(())
    }

    fn write_scope_lines<W: Write>(
        current_line: usize,
        active_scopes: &[&ScopeItem],
        mut writer: W,
    ) -> io::Result<()> {
        let mut last_pos = 0;

        for scope in active_scopes {
            Self::pad(scope.width - last_pos, &mut writer)?;

            let char = if current_line == scope.end_y {
                '└'
            } else if current_line == scope.start_y {
                '┌'
            } else {
                '│'
            };

            write!(writer, "{}", Paint::blue(&char))?;
            last_pos = scope.width + 1;
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

    /// Draws the lines
    /// # Errors
    ///
    pub fn draw<W: Write, S: SourceLines>(&self, source: &S, mut writer: W) -> io::Result<()> {
        let lines_to_show = self.calculate_lines_that_must_be_shown();

        let max_line_num_width = lines_to_show
            .iter()
            .max()
            .map_or(0, |&max_line| max_line.to_string().len());

        for &current_line in &lines_to_show {
            let source_line = source
                .get_line(current_line)
                .expect("Source code lines with the requested line number should exist");

            let line_number = current_line.to_string();

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

            // Write source line with line number
            write!(writer, "{line_number:>max_line_num_width$} │ ")?;
            Self::write_source_line(source_line, &line_labels, &mut writer)?;

            // Write scope and label lines with dot separator
            if !active_scopes.is_empty() {
                write!(writer, "{:>width$} . ", "", width = max_line_num_width)?;
                Self::write_scope_lines(current_line, &active_scopes, &mut writer)?;
            }

            // Write labels from right to left (to make it look nicer with no bars "cutting" through labels.
            for (idx, label) in line_labels.iter().enumerate() {
                write!(writer, "{:>width$} . ", "", width = max_line_num_width)?;

                let mut current_pos = 0;

                // Draw vertical bars for all labels that will come after this one
                for future_label in line_labels.iter().skip(idx + 1) {
                    Self::pad(future_label.start.x - 1 - current_pos, &mut writer)?;
                    write!(writer, "│")?;
                    current_pos = future_label.start.x;
                }

                if label.start.x > current_pos {
                    Self::pad(label.start.x - 1 - current_pos, &mut writer)?;
                }

                write!(writer, "└{}", Paint::blue(&label.text))?;
                writeln!(writer)?;
            }
        }

        Ok(())
    }
}
