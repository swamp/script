use swamp_script_vm::instr_bldr::InstructionBuilder;
use swamp_script_vm::{INT_SIZE, Vm};

#[test]
fn add() {
    let mut mem = 0;

    let mut builder = InstructionBuilder::new();
    builder.add_ld_imm_i32(mem, 42);
    let a = mem;
    mem += INT_SIZE;

    builder.add_ld_imm_i32(mem, 96);
    let b = mem;
    mem += INT_SIZE;

    builder.add_add_i32(mem, a, b);

    builder.add_end();

    let mut vm = Vm::new(builder.instructions, 1024);

    vm.execute();

    assert_eq!(vm.get_i32(vm.frame_offset() + mem as usize), 42 + 96);
}

#[test]
fn lt() {
    let mut mem = 0;

    let mut builder = InstructionBuilder::new();
    builder.add_ld_imm_i32(mem, 42);
    let a = mem;
    mem += INT_SIZE;

    builder.add_ld_imm_i32(mem, 96);
    let b = mem;
    mem += INT_SIZE;

    builder.add_add_i32(mem, a, b);
    let result = mem;
    mem += INT_SIZE;

    builder.add_lt_i32(mem, a, b);

    builder.add_end();

    let mut vm = Vm::new(builder.instructions, 1024);

    vm.execute();

    assert_eq!(vm.get_i32(vm.frame_offset() + result as usize), 42 + 96);
    assert_eq!(vm.get_i32(vm.frame_offset() + mem as usize), 1);
}

#[test]
fn jmp_if() {
    let mut mem = 0;

    let mut builder = InstructionBuilder::new();
    builder.add_ld_imm_i32(mem, 42);
    let a = mem;
    mem += INT_SIZE;

    builder.add_ld_imm_i32(mem, 96);
    let b = mem;
    mem += INT_SIZE;

    builder.add_add_i32(mem, a, b);
    let result = mem;
    mem += INT_SIZE;

    builder.add_lt_i32(mem, a, b);
    let lt_result = mem;
    mem += INT_SIZE;

    builder.add_jmp_if(lt_result, 1);

    builder.add_ld_imm_i32(result, 255);
    mem += INT_SIZE;

    builder.add_end();

    let mut vm = Vm::new(builder.instructions, 1024);

    vm.execute();

    assert_eq!(vm.get_i32(vm.frame_offset() + result as usize), 42 + 96);
    assert_eq!(vm.get_i32(vm.frame_offset() + lt_result as usize), 1);
}
