/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use std::time::{Duration, Instant};

#[allow(dead_code)]
fn format_duration(nanos: f64) -> String {
    if nanos < 1_000.0 {
        format!("{nanos:.2} ns")
    } else if nanos < 100_000.0 {
        format!("{:.2} μs", nanos / 1_000.0)
    } else if nanos < 1_000_000_000.0 {
        format!("{:.2} ms", nanos / 1_000_000.0)
    } else {
        format!("{:.2} s", nanos / 1_000_000_000.0)
    }
}

pub enum Grade {
    Great,
    Good,
    Warning,
    Bad,
}

fn grade_from_nano(nanos: f64) -> Grade {
    let ms = (nanos / 1_000_000.0) as u64;
    if ms < 10 {
        Grade::Great
    } else if ms < 40 {
        Grade::Good
    } else if ms < 70 {
        Grade::Warning
    } else {
        Grade::Bad
    }
}

const RESET: &str = "\x1B[0m";
const RED: &str = "\x1B[31m";
const GREEN: &str = "\x1B[32m";
const YELLOW_BOLD: &str = "\x1B[1;33m";
const CYAN: &str = "\x1B[36m";
const BLUE: &str = "\x1B[34m";

#[cfg(feature = "enable_summary")]
pub fn summary(name: &str, total_execution_time: Duration) {
    let duration = total_execution_time;
    let total_nanos = duration.as_nanos() as f64;

    let grade = grade_from_nano(total_nanos);
    let grade_color_string = match grade {
        Grade::Great => GREEN,
        Grade::Good => CYAN,
        Grade::Warning => YELLOW_BOLD,
        Grade::Bad => RED,
    };

    println!(
        "timer '{}{}{}' completed in {}{}{}",
        BLUE,
        name,
        RESET,
        grade_color_string,
        format_duration(total_nanos),
        RESET,
    );
}

/// A simple timer that records elapsed time from its creation until it's dropped.
///
/// If the `enable_summary` feature is active, it prints the elapsed time
/// when dropped.
#[derive(Debug)]
pub struct ScopedTimer<'a> {
    start: Instant,
    #[allow(dead_code)]
    name: &'a str,
}

impl<'a> ScopedTimer<'a> {
    /// Creates a new timer and records the start time.
    ///
    /// # Arguments
    ///
    /// * `name` - A static string slice used to identify the timer in the summary output.
    #[must_use]
    pub fn new(name: &'a str) -> Self {
        Self {
            start: Instant::now(),
            name,
        }
    }

    /// Manually get the elapsed duration without dropping the timer.
    #[must_use]
    pub fn elapsed(&self) -> Duration {
        self.start.elapsed()
    }
}

impl Drop for ScopedTimer<'_> {
    fn drop(&mut self) {
        #[cfg(feature = "enable_summary")]
        {
            let elapsed = self.start.elapsed();
            summary(self.name, elapsed);
        }
    }
}
