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

#[cfg(feature = "enable_summary")]
pub fn summary(name: &str, total_execution_time: Duration) {
    let duration = total_execution_time;
    let total_nanos = duration.as_nanos() as f64;

    println!(
        "timer '{}' completed in {}",
        name,
        format_duration(total_nanos),
    );
}

/// A simple timer that records elapsed time from its creation until it's dropped.
///
/// If the `enable_summary` feature is active, it prints the elapsed time
/// when dropped.
#[derive(Debug)]
pub struct ScopedTimer {
    start: Instant,
    #[allow(dead_code)]
    name: &'static str,
}

impl ScopedTimer {
    /// Creates a new timer and records the start time.
    ///
    /// # Arguments
    ///
    /// * `name` - A static string slice used to identify the timer in the summary output.
    #[must_use]
    pub fn new(name: &'static str) -> Self {
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

impl Drop for ScopedTimer {
    fn drop(&mut self) {
        #[cfg(feature = "enable_summary")]
        {
            let elapsed = self.start.elapsed();
            summary(self.name, elapsed);
        }
    }
}
