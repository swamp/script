use std::io;
use std::io::Write;
use yansi::{Color, Paint};

#[derive(Clone, PartialEq, Eq)]
pub struct Pos {
    pub x: usize,
    pub y: usize,
}

#[derive(Clone, PartialEq, Eq)]
pub struct PosSpan {
    pub pos: Pos,
    pub length: usize,
}

#[derive(PartialEq, Eq)]
pub struct ColoredSpan {
    pub pos: PosSpan,
    pub color: Color,
}

#[derive(PartialEq)]
pub struct ScopeItem {
    pub start: PosSpan,
    pub end: PosSpan,
    pub text: String,
    pub color: Color,
}

#[derive(Clone, PartialEq, Eq)]
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

    fn get_colored_spans_for_line(
        line_labels: &[&LabelItem],
        scopes: &[&ScopeItem],
        current_line: usize,
    ) -> Vec<ColoredSpan> {
        let mut spans = Vec::new();

        // Add label spans
        spans.extend(line_labels.iter().map(|label| ColoredSpan {
            pos: PosSpan {
                pos: label.start.clone(),
                length: label.character_count,
            },
            color: label.color,
        }));

        // Add scope start spans
        spans.extend(scopes.iter().filter_map(|scope| {
            if scope.start.pos.y == current_line {
                Some(ColoredSpan {
                    pos: scope.start.clone(),
                    color: scope.color,
                })
            } else {
                None
            }
        }));

        // Add scope end spans
        spans.extend(scopes.iter().filter_map(|scope| {
            if scope.end.pos.y == current_line {
                Some(ColoredSpan {
                    pos: scope.end.clone(),
                    color: scope.color,
                })
            } else {
                None
            }
        }));

        spans
    }

    fn write_source_line<W: Write>(
        source_line: &str,
        colored_spans: &[ColoredSpan],
        mut writer: W,
    ) -> io::Result<()> {
        let chars: Vec<char> = source_line.chars().collect();
        let mut current_pos = 0;

        while current_pos < chars.len() {
            let matching_span = colored_spans.iter().find(|span| {
                (current_pos + 1) >= span.pos.pos.x
                    && (current_pos + 1) < span.pos.pos.x + span.pos.length
            });

            let mut region_end = current_pos;
            while region_end < chars.len() {
                let next_span = colored_spans.iter().find(|span| {
                    (region_end + 1) >= span.pos.pos.x
                        && (region_end + 1) < span.pos.pos.x + span.pos.length
                });
                if next_span != matching_span {
                    break;
                }
                region_end += 1;
            }

            let text: String = chars[current_pos..region_end].iter().collect();
            if let Some(span) = matching_span {
                write!(writer, "{}", text.fg(span.color))?;
            } else {
                write!(writer, "{text}")?;
            }

            current_pos = region_end;
        }

        writeln!(writer)?;
        Ok(())
    }

    #[must_use]
    pub fn calculate_lines_that_must_be_shown(&self) -> Vec<usize> {
        // Only filter out source code lines that will be referenced by scope or labels
        let mut lines_to_show: Vec<usize> = self
            .labels
            .iter()
            .map(|label| label.start.y)
            .chain(
                self.scopes
                    .iter()
                    .flat_map(|scope| vec![scope.start.pos.y, scope.end.pos.y]),
            )
            .collect();
        lines_to_show.sort_unstable();
        lines_to_show.dedup();
        lines_to_show
    }

    pub fn layout(&mut self) {
        // Sort scopes by x position first, then by y position
        self.scopes
            .sort_by(|a, b| match a.start.pos.x.cmp(&b.start.pos.x) {
                std::cmp::Ordering::Equal => a.start.pos.y.cmp(&b.start.pos.y),
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
        let mut sorted_scopes = active_scopes.to_vec();
        sorted_scopes.sort_by_key(|scope| scope.start.pos.x);

        for i in 0..max_scopes {
            if let Some(scope) = sorted_scopes.get(i) {
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
        let prefix = line_number.map_or_else(
            || format!("{:>max_line_num_width$} . ", ""),
            |num| format!("{num:>max_line_num_width$} │ "),
        );
        write!(writer, "{}", prefix.fg(Color::BrightBlack))?;
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
                .filter(|other| {
                    scope.start.pos.y <= other.end.pos.y && other.start.pos.y <= scope.end.pos.y
                })
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
                .filter(|scope| {
                    scope.start.pos.y <= current_line && current_line <= scope.end.pos.y
                })
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
                    let scope_line_prefix = if current_line == scope.start.pos.y {
                        "╭─▶"
                    } else if current_line == scope.end.pos.y {
                        "├─▶"
                    } else {
                        "│  "
                    };
                    scope_line_prefix.fg(scope.color).to_string()
                });
                write!(writer, "{prefix} ")?;
            }
            let colored_spans =
                Self::get_colored_spans_for_line(&line_labels, &active_scopes, current_line);
            Self::write_source_line(source_line, &colored_spans, &mut writer)?;

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
                if scope.end.pos.y == current_line {
                    Self::write_line_prefix(max_line_num_width, None, &mut writer)?;
                    Self::write_scope_continuation(&active_scopes, max_scopes, &mut writer)?;
                    writeln!(writer)?;

                    // Write the scope text with continuation lines
                    Self::write_line_prefix(max_line_num_width, None, &mut writer)?;
                    for i in 0..max_scopes {
                        if let Some(s) = active_scopes.get(i) {
                            if s == scope {
                                write!(writer, "{}", "╰─── ".fg(s.color))?;
                                break; // stop writing since we are on the scope text we should print
                            } else {
                                write!(writer, "{}   ", "│".fg(s.color))?;
                            }
                        } else {
                            write!(writer, "    ")?;
                        }
                    }
                    writeln!(writer, "{}", scope.text.fg(scope.color))?;
                }
            }
        }

        Ok(())
    }
}
