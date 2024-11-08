// Implementations of operations.

use core::str;
use std::collections::HashMap;

use super::Operation;
use super::Type;
use super::Value;

macro_rules! arithmetic_binary_operation {
    ($ArithmeticBinaryOp:ident) => {
        #[non_exhaustive]
        pub struct $ArithmeticBinaryOp {
            operands: [Value; 2],
            result: Value,
        }

        impl $ArithmeticBinaryOp {
            pub fn new(operands: [Value; 2], result: Value) -> Self {
                Self { operands, result }
            }
        }

        impl Operation for $ArithmeticBinaryOp {
            fn name(&self) -> &str {
                stringify!($ArithmeticBinaryOp)
            }

            fn validate(&self) -> Result<(), String> {
                if self.operands[0].ttype != self.operands[1].ttype {
                    return Err("Type mismatch".to_string());
                }
                if self.operands[0].ttype != self.result.ttype {
                    return Err("Type mismatch".to_string());
                }
                Ok(())
            }

            fn operands(&self) -> &[Value] {
                &self.operands
            }

            fn result(&self) -> Option<Value> {
                Some(self.result)
            }

            fn as_any(&self) -> &dyn std::any::Any {
                self
            }
        }
    };
}

arithmetic_binary_operation!(AddOperation);
arithmetic_binary_operation!(SubtractOperation);
arithmetic_binary_operation!(MultiplyOperation);
arithmetic_binary_operation!(DivideOperation);

macro_rules! boolean_binary_operation {
    ($BooleanBinaryOp:ident) => {
        #[non_exhaustive]
        pub struct $BooleanBinaryOp {
            operands: [Value; 2],
            result: Value,
        }

        impl $BooleanBinaryOp {
            pub fn new(operands: [Value; 2], result: Value) -> Self {
                Self { operands, result }
            }
        }

        impl Operation for $BooleanBinaryOp {
            fn name(&self) -> &str {
                stringify!($BooleanBinaryOp)
            }

            fn validate(&self) -> Result<(), String> {
                if self.operands[0].ttype != self.operands[1].ttype {
                    return Err("Type mismatch".to_string());
                }
                if self.result.ttype != Type::Bool {
                    return Err("Type mismatch".to_string());
                }
                Ok(())
            }

            fn operands(&self) -> &[Value] {
                &self.operands
            }

            fn result(&self) -> Option<Value> {
                Some(self.result)
            }

            fn as_any(&self) -> &dyn std::any::Any {
                self
            }
        }
    };
}

boolean_binary_operation!(LessOperation);
boolean_binary_operation!(LessOrEqualOperation);
boolean_binary_operation!(GreaterOperation);
boolean_binary_operation!(GreaterOrEqualOperation);
boolean_binary_operation!(EqualOperation);
boolean_binary_operation!(NotEqualOperation);

pub struct ReturnOperation {
    operands: [Value; 1],
}

impl ReturnOperation {
    pub fn new(operand: Value) -> Self {
        Self {
            operands: [operand],
        }
    }
}

impl Operation for ReturnOperation {
    fn name(&self) -> &str {
        "ReturnOperation"
    }

    fn validate(&self) -> Result<(), String> {
        Ok(())
    }

    fn operands(&self) -> &[Value] {
        &self.operands
    }

    fn result(&self) -> Option<Value> {
        None
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub struct ConstantOperation {
    _data: Vec<u8>,
    result: Value,
}

impl ConstantOperation {
    pub fn new(result: Value) -> Self {
        Self {
            _data: vec![],
            result,
        }
    }
}

impl Operation for ConstantOperation {
    fn name(&self) -> &str {
        "ConstantOperation"
    }

    fn validate(&self) -> Result<(), String> {
        Ok(())
    }

    fn operands(&self) -> &[Value] {
        &[]
    }

    fn result(&self) -> Option<Value> {
        Some(self.result)
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub struct FunctionCallOperation {
    function: String,
    arguments: Vec<Value>,
    result: Value,
    metadata: HashMap<String, String>,
}

impl FunctionCallOperation {
    pub fn new(function: &str, arguments: Vec<Value>, result: Value) -> Self {
        Self {
            function: function.to_string(),
            arguments,
            result,
            metadata: HashMap::from([("fn".to_string(), function.to_string())]),
        }
    }

    pub fn function(&self) -> &str {
        &self.function
    }
}

impl Operation for FunctionCallOperation {
    fn name(&self) -> &str {
        "FunctionCallOperation"
    }

    fn validate(&self) -> Result<(), String> {
        Ok(())
    }

    fn operands(&self) -> &[Value] {
        &self.arguments
    }

    fn result(&self) -> Option<Value> {
        Some(self.result)
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn metadata(&self) -> Option<&HashMap<String, String>> {
        Some(&self.metadata)
    }
}

#[cfg(test)]
mod tests {

    use crate::ir::ops::*;
    use crate::ir::*;

    #[test]
    fn format_add_operation() {
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let arg1 = Value {
            id: 1,
            ttype: Type::I32,
        };
        let result = Value {
            id: 2,
            ttype: Type::I32,
        };
        let op = AddOperation::new([arg0, arg1], result);
        assert!(op.fmt() == "AddOperation(%0:I32, %1:I32) -> %2:I32");
    }

    #[test]
    fn format_multiply_operation() {
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let arg1 = Value {
            id: 1,
            ttype: Type::I32,
        };
        let result = Value {
            id: 2,
            ttype: Type::I32,
        };
        let op = MultiplyOperation::new([arg0, arg1], result);
        assert!(op.fmt() == "MultiplyOperation(%0:I32, %1:I32) -> %2:I32");
    }

    #[test]
    fn format_subtraction_operation() {
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let arg1 = Value {
            id: 1,
            ttype: Type::I32,
        };
        let result = Value {
            id: 2,
            ttype: Type::I32,
        };
        let op = SubtractOperation::new([arg0, arg1], result);
        assert!(op.fmt() == "SubtractOperation(%0:I32, %1:I32) -> %2:I32");
    }

    #[test]
    fn format_division_operation() {
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let arg1 = Value {
            id: 1,
            ttype: Type::I32,
        };
        let result = Value {
            id: 2,
            ttype: Type::I32,
        };
        let op: DivideOperation = DivideOperation::new([arg0, arg1], result);
        assert!(op.fmt() == "DivideOperation(%0:I32, %1:I32) -> %2:I32");
    }

    #[test]
    fn format_less_operation() {
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let arg1 = Value {
            id: 1,
            ttype: Type::I32,
        };
        let result = Value {
            id: 2,
            ttype: Type::I32,
        };
        let op = LessOperation::new([arg0, arg1], result);
        assert!(op.fmt() == "LessOperation(%0:I32, %1:I32) -> %2:I32");
    }

    #[test]
    fn format_less_or_equal_operation() {
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let arg1 = Value {
            id: 1,
            ttype: Type::I32,
        };
        let result = Value {
            id: 2,
            ttype: Type::I32,
        };
        let op = LessOrEqualOperation::new([arg0, arg1], result);
        assert!(op.fmt() == "LessOrEqualOperation(%0:I32, %1:I32) -> %2:I32");
    }

    #[test]
    fn format_greater_operation() {
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let arg1 = Value {
            id: 1,
            ttype: Type::I32,
        };
        let result = Value {
            id: 2,
            ttype: Type::I32,
        };
        let op = GreaterOperation::new([arg0, arg1], result);
        assert!(op.fmt() == "GreaterOperation(%0:I32, %1:I32) -> %2:I32");
    }

    #[test]
    fn format_greater_or_equal_operation() {
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let arg1 = Value {
            id: 1,
            ttype: Type::I32,
        };
        let result = Value {
            id: 2,
            ttype: Type::I32,
        };
        let op = GreaterOrEqualOperation::new([arg0, arg1], result);
        assert!(op.fmt() == "GreaterOrEqualOperation(%0:I32, %1:I32) -> %2:I32");
    }

    #[test]
    fn format_equal_operation() {
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let arg1 = Value {
            id: 1,
            ttype: Type::I32,
        };
        let result = Value {
            id: 2,
            ttype: Type::I32,
        };
        let op = EqualOperation::new([arg0, arg1], result);
        assert!(op.fmt() == "EqualOperation(%0:I32, %1:I32) -> %2:I32");
    }

    #[test]
    fn format_not_equal_operation() {
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let arg1 = Value {
            id: 1,
            ttype: Type::I32,
        };
        let result = Value {
            id: 2,
            ttype: Type::I32,
        };
        let op = NotEqualOperation::new([arg0, arg1], result);
        assert!(op.fmt() == "NotEqualOperation(%0:I32, %1:I32) -> %2:I32");
    }

    #[test]
    fn format_return_operation() {
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let op = ReturnOperation::new(arg0);
        assert!(op.fmt() == "ReturnOperation(%0:I32)");
    }

    #[test]
    fn format_constant_operation() {
        let result = Value {
            id: 0,
            ttype: Type::I32,
        };
        let op = ConstantOperation::new(result);
        assert!(op.fmt() == "ConstantOperation() -> %0:I32");
    }

    #[test]
    fn format_function_call_operation() {
        let function = "foo";
        let arg0 = Value {
            id: 0,
            ttype: Type::I32,
        };
        let arg1 = Value {
            id: 1,
            ttype: Type::I32,
        };
        let result = Value {
            id: 2,
            ttype: Type::I32,
        };
        let op = FunctionCallOperation::new(function, vec![arg0, arg1], result);
        assert!(op.fmt() == "FunctionCallOperation(fn:foo, %0:I32, %1:I32) -> %2:I32");
    }
}
