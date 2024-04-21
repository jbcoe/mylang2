// An SSA-based Intermediate Representation inspired by
// https://mlir.llvm.org/docs/LangRef

pub struct Block {
    pub id: String,
    pub arguments: Vec<Value>,
    pub operations: Vec<Box<dyn Operation>>,
}

impl Block {
    pub fn new(id: &str) -> Block {
        Block {
            id: id.to_string(),
            arguments: Vec::new(),
            operations: Vec::new(),
        }
    }
}

impl core::fmt::Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("id", &self.id)
            .field("arguments", &self.arguments)
            .field("operations", &self.operations)
            .finish()
    }
}

pub trait Operation {
    fn validate(&self) -> Result<(), String>;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

impl core::fmt::Debug for dyn Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt(f)
    }
}

pub struct AddOperation {
    pub lhs: Value,
    pub rhs: Value,
    pub result: Value,
}

impl AddOperation {
    pub fn new(lhs: Value, rhs: Value, result: Value) -> AddOperation {
        AddOperation { lhs, rhs, result }
    }
}

impl Operation for AddOperation {
    fn validate(&self) -> Result<(), String> {
        if self.lhs.ttype != self.rhs.ttype {
            return Err("lhs and rhs must have the same type".to_string());
        }
        if self.lhs.ttype != self.result.ttype {
            return Err("lhs and result must have the same type".to_string());
        }
        Ok(())
    }

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AddOperation")
            .field("lhs", &self.lhs)
            .field("rhs", &self.rhs)
            .field("result", &self.result)
            .finish()
    }
}

#[derive(Clone)]
pub struct Value {
    pub name: String,
    pub ttype: String,
}

impl core::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Value")
            .field("name", &self.name)
            .field("ttype", &self.ttype)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::{AddOperation, Block, Value};

    #[test]
    fn debug_format_empty_block() {
        assert_eq!(
            "Block { id: \"Foo\", arguments: [], operations: [] }",
            format!("{:?}", Block::new("Foo"))
        );
    }

    #[test]
    fn debug_format_block_with_args() {
        let mut block = Block::new("Foo");
        block.arguments.push(Value {
            name: "bar".to_string(),
            ttype: "i32".to_string(),
        });
        assert_eq!(
            "Block { id: \"Foo\", arguments: [Value { name: \"bar\", ttype: \"i32\" }], operations: [] }",
            format!("{:?}", block)
        );
    }

    #[test]
    fn debug_format_block_with_operation() {
        let mut block = Block::new("Foo");
        block.arguments.push(Value {
            name: "lhs".to_string(),
            ttype: "i32".to_string(),
        });
        block.arguments.push(Value {
            name: "rhs".to_string(),
            ttype: "i32".to_string(),
        });
        let add = Box::new(AddOperation::new(
            block.arguments[0].clone(),
            block.arguments[1].clone(),
            Value {
                name: "result".to_string(),
                ttype: "i32".to_string(),
            },
        ));
        block.operations.push(add);
        assert_eq!(
            "Block { id: \"Foo\", arguments: [Value { name: \"lhs\", ttype: \"i32\" }, Value { name: \"rhs\", ttype: \"i32\" }], operations: [AddOperation { lhs: Value { name: \"lhs\", ttype: \"i32\" }, rhs: Value { name: \"rhs\", ttype: \"i32\" }, result: Value { name: \"result\", ttype: \"i32\" } }] }",
            format!("{:?}", block)
        );
    }
}
