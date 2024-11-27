use crate::{ExecuteError, Interpreter, ScopeType, ValueWithSignal};
use crate::value::Value;

type FunctionFn = Box<dyn Fn(&[Value]) -> Result<Value, ExecuteError>>;

impl FunctionRef {
    pub fn execute(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> Result<Value, ExecuteError> {
        match self {
            ResolvedFunctionReference::External(_, (_, _), f) => {
                let v = f(&args)?;
                Ok(v)
            }
            ResolvedFunctionReference::Internal(_, (params, _), body) => {
                interpreter.push_scope(ScopeType::Function);

                // Bind parameters before executing body
                interpreter.bind_parameters(params, args)?;
                let result = interpreter.execute_statements(body)?;

                interpreter.pop_scope();

                // Since signals can not propagate from the function call, we just return a normal Value
                let v = match result {
                    ValueWithSignal::Value(v) => v,
                    ValueWithSignal::Return(v) => v,
                    ValueWithSignal::Break => Value::Unit,
                    ValueWithSignal::Continue => Value::Unit,
                };

                Ok(v)
            }
        }
    }
}
