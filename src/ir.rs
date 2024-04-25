// An SSA-based Intermediate Representation inspired by
// https://mlir.llvm.org/docs/LangRef

use std::collections::HashSet;
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
#[non_exhaustive]
pub enum Type {
    I32,
    F32,
    F64,
    Bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
#[non_exhaustive]
pub struct Value {
    pub id: usize,
    pub ttype: Type,
}

impl Value {
    // Create a test-only value that does not belong to any Function.
    #[cfg(test)]
    pub(crate) fn create_orphan(ttype: Type) -> Self {
        Self {
            id: usize::MAX,
            ttype,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "%{:?}:{:?}", self.id, self.ttype)
    }
}

pub trait Operation {
    fn name(&self) -> &str;
    fn validate(&self) -> Result<(), String>;
    fn operands(&self) -> &[Value];
    fn result(&self) -> Option<Value>;
    fn fmt(&self) -> String {
        let comma_separated_operands = itertools::join(self.operands(), ", ");
        if let Some(result) = self.result() {
            format!(
                "{}({}) -> {}",
                self.name(),
                comma_separated_operands,
                result
            )
        } else {
            format!("{}({})", self.name(), comma_separated_operands)
        }
    }
    fn as_any(&self) -> &dyn std::any::Any;
}

pub fn cast<T: Operation + 'static>(operation: &dyn Operation) -> Option<&T> {
    operation.as_any().downcast_ref::<T>()
}

#[non_exhaustive]
pub struct AddOperation {
    operands: [Value; 2],
    result: Value,
}

impl AddOperation {
    pub fn new(operands: [Value; 2], result: Value) -> Self {
        Self { operands, result }
    }
}

impl Operation for AddOperation {
    fn name(&self) -> &str {
        "AddOperation"
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

#[non_exhaustive]
pub struct MultiplyOperation {
    operands: [Value; 2],
    result: Value,
}

impl MultiplyOperation {
    pub fn new(operands: [Value; 2], result: Value) -> Self {
        Self { operands, result }
    }
}

impl Operation for MultiplyOperation {
    fn name(&self) -> &str {
        "MultiplyOperation"
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

#[non_exhaustive]
pub struct Block {
    pub id: usize,
    pub arguments: Vec<Value>,
    pub operations: Vec<Box<dyn Operation>>,
}

impl Block {
    pub fn new() -> Self {
        Self {
            id: usize::MAX,
            arguments: Vec::new(),
            operations: Vec::new(),
        }
    }

    pub fn add_operation(&mut self, operation: Box<dyn Operation>) {
        self.operations.push(operation);
    }

    pub fn add_argument(&mut self, value: Value) {
        self.arguments.push(value);
    }

    pub fn validate(&self) -> Result<(), String> {
        for operation in &self.operations {
            operation.validate()?;
        }
        // TODO: Implement
        Ok(())
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Function {
    pub name: String,
    pub blocks: Vec<Block>,
    pub values: Vec<Value>,
}

impl Function {
    pub fn new(name: String) -> Self {
        Self {
            name,
            blocks: Vec::new(),
            values: Vec::new(),
        }
    }

    pub fn add_block(&mut self, mut block: Block) {
        block.id = self.blocks.len();
        self.blocks.push(block);
    }

    pub fn new_value(&mut self, ttype: Type) -> Value {
        self.values.push(Value {
            id: self.values.len(),
            ttype,
        });
        self.values[self.values.len() - 1]
    }

    pub fn validate(&self) -> Result<(), String> {
        for block in &self.blocks {
            block.validate()?;
        }
        let owned_values: HashSet<Value> = self.values.iter().cloned().collect();

        // Ensure that all block arguments are owned by the Function.
        for (block_idx, block) in self.blocks.iter().enumerate() {
            for arg in &block.arguments {
                if !owned_values.contains(arg) {
                    return Err(format!(
                        "Block {:?} argument {:?} not owned by Function",
                        block_idx, arg
                    ));
                }
            }
        }

        // Ensure that all operation arguments and results are owned by the Function.
        for (block_idx, block) in self.blocks.iter().enumerate() {
            for (operation_idx, operation) in block.operations.iter().enumerate() {
                for arg in operation.operands() {
                    if !owned_values.contains(arg) {
                        return Err(format!(
                            "Block {:?} Operation {:?} argument {:?} not owned by Function",
                            block_idx, operation_idx, arg
                        ));
                    }
                }
                if let Some(result) = operation.result() {
                    if !owned_values.contains(&result) {
                        return Err(format!(
                            "Block {:?} Operation {:?} result {:?} not owned by Function",
                            block_idx, operation_idx, result
                        ));
                    }
                }
            }
        }

        // TODO: Implement
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use crate::ir::{
        cast, AddOperation, Block, Function, MultiplyOperation, Operation, Type, Value,
    };

    #[test]
    fn check_api_create_single_function_within_block() {
        let mut function = Function::new("main".to_string());

        // Add two block arguments
        let arg0 = function.new_value(Type::I32);
        let arg1 = function.new_value(Type::I32);

        let mut block = Block::default();
        block.add_argument(arg0);
        block.add_argument(arg1);

        let arg2 = function.new_value(Type::I32);
        let add_op = Box::new(AddOperation::new([arg0, arg1], arg2));
        block.add_operation(add_op);

        function.add_block(block);

        assert!(function.name == "main");
        assert!(function.values.len() == 3);
        assert!(function.blocks.len() == 1);
        assert!(function.blocks[0].arguments.len() == 2);
        assert!(function.blocks[0].operations.len() == 1);
        let operation: &dyn Operation = &*function.blocks[0].operations[0];
        assert!(operation.operands().len() == 2);
        assert!(operation.result().is_some());

        assert!(function.validate().is_ok());
    }

    #[test]
    fn function_validation_fails_with_orphan_value_as_block_argument() {
        let mut function = Function::new("main".to_string());

        // Add two block arguments
        let arg0 = function.new_value(Type::I32);
        let arg1 = Value::create_orphan(Type::I32);

        let mut block = Block::default();
        block.add_argument(arg0);
        block.add_argument(arg1);

        let arg2 = function.new_value(Type::I32);
        let add_op = Box::new(AddOperation::new([arg0, arg1], arg2));
        block.add_operation(add_op);

        function.add_block(block);

        assert!(function.validate().is_err());
    }

    #[test]
    fn function_validation_fails_with_orphan_value_as_operation_operand() {
        let mut function = Function::new("main".to_string());

        // Add two block arguments
        let arg0 = function.new_value(Type::I32);
        let arg1 = Value::create_orphan(Type::I32);

        let mut block = Block::default();
        block.add_argument(arg0);
        block.add_argument(arg1);

        let arg2 = function.new_value(Type::I32);
        let add_op = Box::new(AddOperation::new(
            [arg0, Value::create_orphan(Type::I32)],
            arg2,
        ));
        block.add_operation(add_op);

        function.add_block(block);

        assert!(function.validate().is_err());
    }

    #[test]
    fn function_validation_fails_with_orphan_value_as_operation_return_value() {
        let mut function = Function::new("main".to_string());

        // Add two block arguments
        let arg0 = function.new_value(Type::I32);
        let arg1 = function.new_value(Type::I32);

        let mut block = Block::default();
        block.add_argument(arg0);
        block.add_argument(arg1);

        let arg2 = Value::create_orphan(Type::I32);
        let add_op = Box::new(AddOperation::new([arg0, arg1], arg2));
        block.add_operation(add_op);

        function.add_block(block);

        assert!(function.validate().is_err());
    }

    #[test]
    fn function_validation_fails_with_malformed_operation() {
        let mut function = Function::new("main".to_string());

        // Add two block arguments
        let arg0 = function.new_value(Type::I32);
        let arg1 = function.new_value(Type::I32);

        let mut block = Block::default();
        block.add_argument(arg0);
        block.add_argument(arg1);

        let arg2 = Value::create_orphan(Type::I32);
        let add_op = Box::new(AddOperation::new([arg0, arg1], arg2));
        block.add_operation(add_op);

        function.add_block(block);

        assert!(function.validate().is_err());
    }

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
        let add_op = AddOperation::new([arg0, arg1], result);
        assert!(add_op.fmt() == "AddOperation(%0:I32, %1:I32) -> %2:I32");
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
        let add_op = MultiplyOperation::new([arg0, arg1], result);
        assert!(add_op.fmt() == "MultiplyOperation(%0:I32, %1:I32) -> %2:I32");
    }

    #[test]
    fn check_api_create_multiply_add_within_block() {
        let mut function = Function::new("main".to_string());

        // Add three block arguments
        let arg0 = function.new_value(Type::I32);
        let arg1 = function.new_value(Type::I32);
        let arg2 = function.new_value(Type::I32);

        let mut block = Block::default();
        block.add_argument(arg0);
        block.add_argument(arg1);

        let multiply_result = function.new_value(Type::I32);
        let mul_op = Box::new(MultiplyOperation::new([arg0, arg1], multiply_result));
        block.add_operation(mul_op);
        let add_result = function.new_value(Type::I32);
        let add_op = Box::new(AddOperation::new([multiply_result, arg2], add_result));
        block.add_operation(add_op);
        function.add_block(block);

        assert!(function.name == "main");
        assert!(function.validate().is_ok());
    }

    #[test]
    fn check_cast() {
        let add_op = AddOperation::new(
            [
                Value {
                    id: 0,
                    ttype: Type::I32,
                },
                Value {
                    id: 1,
                    ttype: Type::I32,
                },
            ],
            Value {
                id: 2,
                ttype: Type::I32,
            },
        );

        let op: &dyn Operation = &add_op;

        if cast::<AddOperation>(op).is_none() {
            panic!("Failed to cast AddOperation to AddOperation")
        }

        if cast::<MultiplyOperation>(op).is_some() {
            panic!("Erroneously cast AddOperation to MultiplyOperation")
        }
    }
}
