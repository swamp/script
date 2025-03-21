use std::cell::RefCell;
use std::rc::Rc;
use swamp_vm::Vm;
use swamp_vm::host::HostArgs;

pub fn register_context_aware<T: 'static, F>(
    vm_ref: &mut Vm,
    id: u16,
    context: &Rc<RefCell<T>>,
    mut callback: F,
) where
    F: 'static + FnMut(&mut T, HostArgs),
{
    let context_clone = context.clone();
    let wrapper = move |arg: HostArgs| {
        let mut ctx = context_clone.borrow_mut();
        callback(&mut *ctx, arg);
    };

    vm_ref.add_host_function(id, wrapper);
}
