/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use std::fmt::{Display, Formatter};
use std::io;
use std::io::Write;
use yansi::Paint;

pub use yansi::Color;

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

#[derive(PartialEq, Eq)]
pub struct Scope {
    pub start: PosSpan,
    pub end: PosSpan,
    pub text: String,
    pub color: Color,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Label {
    pub start: Pos,
    pub character_count: usize,
    pub text: String,
    pub color: Color,
}

pub trait SourceLines {
    fn get_line(&self, line_number: usize) -> Option<&str>;
}

// Scopes and Labels for a section of a source code file
pub struct SourceFileSection {
    pub scopes: Vec<Scope>,
    pub labels: Vec<Label>,
}

impl Default for SourceFileSection {
    fn default() -> Self {
        Self::new()
    }
}

pub struct PrefixInfo<'a> {
    pub maximum_overlapping_scope_count: usize,
    pub active_scopes: &'a [&'a Scope],
    pub max_number_string_size: usize,
    line_number: Option<usize>,
}

impl SourceFileSection {
    #[must_use]
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            labels: vec![],
        }
    }

    /// Writes the number of specified spaces
    fn source_code_pad<W: Write>(count: usize, mut writer: W) -> io::Result<()> {
        write!(writer, "{}", " ".repeat(count))
    }

    /// Writes the number of specified spaces
    fn scope_margin_pad<W: Write>(count: usize, mut writer: W) -> io::Result<()> {
        write!(writer, "{}", " ".repeat(count))
    }

    /// Writes the number of specified spaces
    fn line_number_margin_pad<W: Write>(count: usize, mut writer: W) -> io::Result<()> {
        write!(writer, "{}", " ".repeat(count))
    }

    /// Calculates which spans that are active for the specified source line.
    fn get_colored_spans_for_line(
        line_labels: &[&Label],
        scopes: &[&Scope],
        line_number: usize,
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
            if scope.start.pos.y == line_number {
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
            if scope.end.pos.y == line_number {
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

    /// Writes a source line exactly as it is in the source file, but the colors are used from the `ColoredSpan`.
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
    pub fn calculate_source_lines_that_must_be_shown(&self) -> Vec<usize> {
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

    /// Sorts the labels and scopes
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

    /// Write bars for active scopes
    fn write_scope_continuation<W: Write>(
        active_scopes: &[&Scope],
        max_scopes: usize,
        mut writer: W,
    ) -> io::Result<()> {
        let mut sorted_scopes = active_scopes.to_vec();
        sorted_scopes.sort_by_key(|scope| scope.start.pos.x);

        for i in 0..max_scopes {
            if let Some(scope) = sorted_scopes.get(i) {
                write!(writer, "{}", "│".fg(scope.color))?;
                Self::scope_margin_pad(3, &mut writer)?;
            } else {
                Self::scope_margin_pad(4, &mut writer)?;
            }
        }
        Ok(())
    }

    /// Writes the mandatory margin for a source code block with description items
    fn write_line_prefix<W: Write>(
        max_line_num_width: usize,
        line_number: Option<usize>,
        mut writer: W,
    ) -> io::Result<()> {
        let number_string =
            line_number.map_or_else(String::new, |found_number| found_number.to_string());

        let padding = max_line_num_width - number_string.len();

        Self::line_number_margin_pad(padding, &mut writer)?;
        write!(writer, "{}", number_string.fg(Color::BrightBlack))?;
        Self::line_number_margin_pad(1, &mut writer)?;
        let separator = if line_number.is_some() { "|" } else { "·" };

        write!(writer, "{}", separator.fg(Color::BrightBlack))?;
        Self::line_number_margin_pad(1, &mut writer)?;

        Ok(())
    }

    /// Calculates the maximum number of overlapping scope items for the source block
    /// Needed for the padding
    #[must_use]
    pub fn calculate_max_overlapping_scopes(scopes: &[Scope]) -> usize {
        scopes.iter().fold(0, |max_count, scope| {
            let overlapping = scopes
                .iter()
                .filter(|other| {
                    scope.start.pos.y <= other.end.pos.y && other.start.pos.y <= scope.end.pos.y
                })
                .count();
            max_count.max(overlapping)
        })
    }

    /// # Errors
    ///
    /// # Panics
    /// if the line number was not provided in `PrefixInfo`.
    pub fn write_source_line_with_prefixes(
        prefix_info: &PrefixInfo,
        labels: &[&Label],
        source_line: &str,
        mut writer: impl Write,
    ) -> io::Result<()> {
        let current_line_number = prefix_info
            .line_number
            .expect("a source line was missing a line number");
        Self::write_line_prefix(
            prefix_info.max_number_string_size,
            Some(current_line_number),
            &mut writer,
        )?;
        for i in 0..prefix_info.maximum_overlapping_scope_count {
            if let Some(scope) = prefix_info.active_scopes.get(i) {
                let is_start = current_line_number == scope.start.pos.y;
                let is_end = current_line_number == scope.end.pos.y;
                let scope_line_prefix = if is_start {
                    "╭─▶"
                } else if is_end {
                    "├─▶"
                } else {
                    "│"
                };
                let prefix = scope_line_prefix.fg(scope.color).to_string();
                write!(writer, "{prefix}")?;
                let padding = if is_start || is_end { 1 } else { 3 };

                Self::scope_margin_pad(padding, &mut writer)?;
            } else {
                Self::scope_margin_pad(4, &mut writer)?;
            }
        }
        let colored_spans = Self::get_colored_spans_for_line(
            labels,
            prefix_info.active_scopes,
            current_line_number,
        );
        Self::write_source_line(source_line, &colored_spans, &mut writer)
    }

    /// The mandatory prefix for each line. The line number and active scopes.
    /// # Errors
    ///
    pub fn write_start_of_line_prefix(
        prefix: &PrefixInfo,
        mut writer: impl Write,
    ) -> io::Result<()> {
        Self::write_line_prefix(
            prefix.max_number_string_size,
            prefix.line_number,
            &mut writer,
        )?;

        Self::write_scope_continuation(
            prefix.active_scopes,
            prefix.maximum_overlapping_scope_count,
            &mut writer,
        )
    }

    /// Underlines spans for upcoming labels to reference.
    /// # Errors
    ///
    pub fn write_underlines_for_upcoming_labels(
        prefix_info: &PrefixInfo,
        line_labels: &[&Label],
        mut writer: impl Write,
    ) -> io::Result<()> {
        Self::write_start_of_line_prefix(prefix_info, &mut writer)?;

        let mut current_pos = 0;

        for label in line_labels.iter().rev() {
            if label.start.x > current_pos {
                Self::source_code_pad(label.start.x - 1 - current_pos, &mut writer)?;
            }

            let middle = (label.character_count - 1) / 2;
            let underline: String = (0..label.character_count)
                .map(|i| if i == middle { '┬' } else { '─' })
                .collect();

            write!(writer, "{}", underline.fg(label.color))?;

            current_pos = label.start.x - 1 + label.character_count;
        }

        writeln!(writer)
    }

    /// Writes the line labels
    /// # Errors
    ///
    pub fn write_labels(
        prefix_info: &PrefixInfo,
        line_labels: &[&Label],
        mut writer: impl Write,
    ) -> io::Result<()> {
        for (idx, label) in line_labels.iter().enumerate() {
            Self::write_start_of_line_prefix(prefix_info, &mut writer)?;

            let mut current_pos = 0;

            // Draw vertical bars for all labels that will come after this one
            for future_label in line_labels.iter().skip(idx + 1) {
                let middle = (future_label.start.x - 1) + (future_label.character_count - 1) / 2;
                Self::source_code_pad(middle - current_pos, &mut writer)?;
                write!(writer, "{}", "│".fg(future_label.color))?;
                current_pos = middle + 1;
            }

            // TODO: Store the aligned position so it doesn't have to be calculated again.
            let middle = (label.start.x - 1) + (label.character_count - 1) / 2;
            if middle > current_pos {
                Self::source_code_pad(middle - current_pos, &mut writer)?;
            }

            // line length somewhat proportional to the span so it looks nicer
            let dash_count = (label.character_count / 4).clamp(2, 8);
            let connector = format!("╰{}", "─".repeat(dash_count));
            let label_line = format!("{} {}", connector.fg(label.color), label.text);
            write!(writer, "{label_line}")?;

            writeln!(writer)?;
        }
        Ok(())
    }

    /// Writes the description for scopes that have ended
    /// # Errors
    ///
    pub fn write_text_for_ending_scopes(
        prefix_info: &PrefixInfo,
        active_scopes: &[&Scope],
        line_number: usize, // Line number is provided, since the prefix_info line_number is `None`.
        mut writer: impl Write,
    ) -> io::Result<()> {
        for scope in active_scopes {
            if scope.end.pos.y == line_number {
                Self::write_start_of_line_prefix(prefix_info, &mut writer)?;
                writeln!(writer)?;

                Self::write_line_prefix(prefix_info.max_number_string_size, None, &mut writer)?;

                for i in 0..prefix_info.maximum_overlapping_scope_count {
                    if let Some(s) = active_scopes.get(i) {
                        if s == scope {
                            write!(writer, "{}", "╰─── ".fg(s.color))?;
                            break; // stop writing since we are on the scope text we should print
                        }
                        write!(writer, "{}", "│".fg(s.color))?;
                        Self::scope_margin_pad(3, &mut writer)?;
                    } else {
                        write!(writer, "    ")?;
                    }
                }
                writeln!(writer, "{}", scope.text.fg(scope.color))?;
            }
        }

        Ok(())
    }

    /// Draws the source file section
    /// # Errors
    ///
    /// # Panics
    /// If a source line can not be provided for the line number
    pub fn draw<W: Write, S: SourceLines>(&self, source: &S, mut writer: W) -> io::Result<()> {
        let line_numbers_to_show = self.calculate_source_lines_that_must_be_shown();

        let max_overlapping_scopes_count = Self::calculate_max_overlapping_scopes(&self.scopes);

        let max_line_number_width = line_numbers_to_show
            .iter()
            .max()
            .map_or(0, |&max_line| max_line.to_string().len());

        for &line_number in &line_numbers_to_show {
            let source_line = source
                .get_line(line_number)
                .expect("Source code lines with the requested line number should exist");

            // Get active scopes for source line (includes end line)
            let active_scopes: Vec<_> = self
                .scopes
                .iter()
                .filter(|scope| scope.start.pos.y <= line_number && line_number <= scope.end.pos.y)
                .collect();

            // Get labels for the current line and sort labels by x position in reverse order (right to left)
            let mut line_labels: Vec<_> = self
                .labels
                .iter()
                .filter(|label| label.start.y == line_number)
                .collect();
            line_labels.sort_by_key(|label| std::cmp::Reverse(label.start.x));

            let mut prefix_info = PrefixInfo {
                maximum_overlapping_scope_count: max_overlapping_scopes_count,
                active_scopes: &active_scopes,
                max_number_string_size: max_line_number_width,
                line_number: Some(line_number),
            };

            Self::write_source_line_with_prefixes(
                &prefix_info,
                &line_labels,
                source_line,
                &mut writer,
            )?;

            prefix_info.line_number = None; // only use line number when writing source lines

            Self::write_underlines_for_upcoming_labels(&prefix_info, &line_labels, &mut writer)?;

            Self::write_labels(&prefix_info, &line_labels, &mut writer)?;

            Self::write_text_for_ending_scopes(
                &prefix_info,
                &active_scopes,
                line_number,
                &mut writer,
            )?;
        }

        Ok(())
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Kind {
    Help, // Give extra information about an error. Maybe should be included in each warning and error?
    Note, // Extra context. Maybe should be included in each warning and error?
    Warning,
    Error,
}

impl Display for Kind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let prefix = match self {
            Self::Help => "help",
            Self::Note => "note",
            Self::Warning => "warning",
            Self::Error => "error",
        };
        write!(f, "{prefix}")
    }
}

pub struct Header<C: Display> {
    pub header_kind: Kind,
    pub code: C,
    pub message: String,
}

impl<C: Display> Header<C> {
    const fn color_for_kind(kind: Kind) -> Color {
        match kind {
            Kind::Help => Color::BrightBlue,
            Kind::Note => Color::BrightMagenta,
            Kind::Warning => Color::BrightYellow,
            Kind::Error => Color::BrightRed,
        }
    }

    /// # Errors
    ///
    pub fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        write!(
            writer,
            "{}",
            self.header_kind.fg(Self::color_for_kind(self.header_kind))
        )?;
        write!(writer, "[{}]", self.code.fg(Color::Blue))?;
        write!(writer, ": ")?;
        write!(writer, "{}", self.message.bold())?;
        writeln!(writer)
    }
}

pub struct FileSpanMessage;

impl FileSpanMessage {
    /// # Errors
    ///
    /// # Panics
    ///
    pub fn write<W: Write>(
        relative_file_name: &str,
        pos_span: &PosSpan,
        mut writer: W,
    ) -> io::Result<()> {
        write!(writer, "  --> ")?;
        write!(writer, "{}", relative_file_name.bright_cyan(),)?;
        write!(
            writer,
            ":{}:{}",
            pos_span.pos.y.fg(Color::BrightMagenta),
            pos_span.pos.x.fg(Color::BrightMagenta),
        )?;
        writeln!(writer)?;
        writeln!(writer)
    }
}
