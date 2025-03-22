use std::time::{Duration, Instant};
use swamp_script_vm_test::util::exec_internal;
use swamp_vm::Vm;

fn setup_vm() -> Vm {
    exec_internal(
        "
        mut a = 1

        while a < 32000 {
            a += 1
        }
    ",
    )
}

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
fn main() {
    println!("Starting VM performance benchmark");

    println!("\nWarm-up phase...");
    {
        let warmup_iterations = 5_000;
        let mut vm = setup_vm();

        for i in 0..warmup_iterations {
            if i % 1_000 == 0 {
                println!("  Warm-up iteration {i}");
            }
            vm.execute();
            vm.reset();
        }

        println!("  Warm-up complete (CPU cache and branch prediction should be primed)");
        println!("  Waiting for system to stabilize...");
        std::thread::sleep(std::time::Duration::from_millis(700));
    }

    println!("-- Single VM, multiple executions, should be the fastest");

    let execution_count = 10_000;

    let mut vm = setup_vm();

    let start = Instant::now();
    for _ in 0..execution_count {
        vm.execute();
        vm.reset();
    }
    let duration = start.elapsed();

    summary(duration, execution_count, None);

    // ---------------
    println!("-- Fresh VM executions (compile not timed), should be similar in performance");
    let iterations = 1_000;

    let mut total_execution_time = std::time::Duration::new(0, 0);

    let start_test_time = Instant::now();
    for _ in 0..iterations {
        let mut vm = setup_vm();

        let start = Instant::now();
        vm.execute();
        let execution_time = start.elapsed();
        total_execution_time += execution_time;
    }
    let test_time_duration = start_test_time.elapsed();
    summary(total_execution_time, iterations, Some(test_time_duration));

    println!("-- Multiple VMs executed sequentially, includes setup_vm (compile) time");
    let vm_count = 1_000;

    let start = Instant::now();
    for _ in 0..vm_count {
        let mut vm = setup_vm();
        vm.execute();
    }
    let duration = start.elapsed();
    summary(duration, vm_count, None);

    println!("\nAll benchmarks completed successfully!");
}

fn summary(total_execution_time: Duration, iteration_count: i32, test_time: Option<Duration>) {
    let duration = total_execution_time;
    let total_nanos = duration.as_nanos() as f64;
    let per_op_nanos = total_nanos / iteration_count as f64;
    print!(
        "  Completed {} executions in {} ({}/execution)",
        iteration_count,
        format_duration(total_nanos),
        format_duration(per_op_nanos)
    );

    if let Some(found_test_time) = test_time {
        print!(
            " (test_time {})",
            format_duration(found_test_time.as_nanos() as f64)
        );
    }
    println!();
}
