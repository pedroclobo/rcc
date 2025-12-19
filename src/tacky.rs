use crate::ast;
use ast::AstVisitor;
use ast::Expression;
use std::fmt;

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<FunctionDefinition<'a>>,
}

impl std::fmt::Display for Program<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for function in &self.functions {
            write!(f, "{}", function)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub body: Vec<Instruction>,
}

impl std::fmt::Display for FunctionDefinition<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.name)?;
        for instruction in &self.body {
            writeln!(f, "\t{}", instruction)?;
        }
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Return(Value),
    Unary(UnaryOperator, Box<Value>, Box<Value>),
    Binary(BinaryOperator, Box<Value>, Box<Value>, Box<Value>),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Return(val) => write!(f, "return {}", val),
            Instruction::Unary(op, src, dst) => write!(f, "{} {} = {}", op, src, dst),
            Instruction::Binary(op, lhs, rhs, dst) => {
                write!(f, "{} = {} {} {}", dst, op, lhs, rhs)
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Tilde,
    Minus,
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Tilde => write!(f, "not"),
            UnaryOperator::Minus => write!(f, "neg"),
        }
    }
}

impl From<ast::UnaryOperator> for UnaryOperator {
    fn from(op: ast::UnaryOperator) -> Self {
        match op {
            ast::UnaryOperator::Tilde => UnaryOperator::Tilde,
            ast::UnaryOperator::Minus => UnaryOperator::Minus,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "add"),
            BinaryOperator::Sub => write!(f, "sub"),
            BinaryOperator::Mul => write!(f, "mul"),
            BinaryOperator::Div => write!(f, "div"),
            BinaryOperator::Mod => write!(f, "mod"),
            BinaryOperator::And => write!(f, "and"),
            BinaryOperator::Or => write!(f, "or"),
            BinaryOperator::Xor => write!(f, "xor"),
            BinaryOperator::Shl => write!(f, "shl"),
            BinaryOperator::Shr => write!(f, "shr"),
        }
    }
}

impl From<ast::BinaryOperator> for BinaryOperator {
    fn from(op: ast::BinaryOperator) -> Self {
        match op {
            ast::BinaryOperator::Add => BinaryOperator::Add,
            ast::BinaryOperator::Sub => BinaryOperator::Sub,
            ast::BinaryOperator::Mul => BinaryOperator::Mul,
            ast::BinaryOperator::Div => BinaryOperator::Div,
            ast::BinaryOperator::Mod => BinaryOperator::Mod,
            ast::BinaryOperator::And => BinaryOperator::And,
            ast::BinaryOperator::Or => BinaryOperator::Or,
            ast::BinaryOperator::Xor => BinaryOperator::Xor,
            ast::BinaryOperator::LShift => BinaryOperator::Shl,
            ast::BinaryOperator::RShift => BinaryOperator::Shr,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    Constant(i32),
    Var(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Constant(val) => write!(f, "{}", val),
            Value::Var(name) => write!(f, "%{}", name),
        }
    }
}

pub struct TackyEmitter<'a> {
    program: Option<Program<'a>>,
    function: Option<FunctionDefinition<'a>>,

    instructions: Vec<Instruction>,
    values: Vec<Value>,

    counter: usize,
}

impl Default for TackyEmitter<'_> {
    fn default() -> Self {
        TackyEmitter::new()
    }
}

impl TackyEmitter<'_> {
    pub fn new() -> Self {
        TackyEmitter {
            program: None,
            function: None,

            instructions: Vec::new(),
            values: Vec::new(),

            counter: 0,
        }
    }

    pub fn get_program(&mut self) -> Option<Program<'_>> {
        self.program.take()
    }

    fn make_tmp(&mut self) -> Value {
        let tmp = format!("tmp.{}", self.counter);
        self.counter += 1;
        Value::Var(tmp)
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    fn push_value(&mut self, expression: Value) {
        self.values.push(expression);
    }

    fn pop_value(&mut self) -> Option<Value> {
        self.values.pop()
    }
}

impl<'a> AstVisitor<'a> for TackyEmitter<'a> {
    type Error = TackyError;

    fn visit_program(&mut self, program: ast::Program<'a>) -> Result<(), Self::Error> {
        let mut functions = Vec::new();

        for function in program.functions {
            self.visit_function_definition(function)?;
            if let Some(function) = self.function.take() {
                functions.push(function);
            }
        }

        self.program = Some(Program { functions });

        Ok(())
    }

    fn visit_function_definition(
        &mut self,
        function_definition: ast::FunctionDefinition<'a>,
    ) -> Result<(), Self::Error> {
        self.instructions.clear();

        self.visit_statement(function_definition.body)?;

        self.function = Some(FunctionDefinition {
            name: function_definition.name,
            body: self.instructions.drain(..).collect(),
        });

        Ok(())
    }

    fn visit_statement(&mut self, statement: ast::Statement) -> Result<(), Self::Error> {
        match statement {
            ast::Statement::Return(expression) => {
                self.visit_expression(expression)?;

                if let Some(val) = self.pop_value() {
                    self.add_instruction(Instruction::Return(val));
                } else {
                    return Err(TackyError::MissingValue);
                }
            }
        };

        Ok(())
    }

    fn visit_expression(&mut self, expression: Expression) -> Result<(), Self::Error> {
        match expression {
            Expression::Constant(n) => {
                self.values.push(Value::Constant(n));
            }
            Expression::Unary(op, expr) => {
                self.visit_expression(*expr)?;

                let src = self.values.pop().ok_or(TackyError::MissingValue)?;
                let dst = self.make_tmp();
                self.add_instruction(Instruction::Unary(
                    op.into(),
                    Box::new(src),
                    Box::new(dst.clone()),
                ));

                self.push_value(dst);
            }
            Expression::Binary(op, lhs, rhs) => {
                self.visit_expression(*lhs)?;
                let lhs = self.values.pop().ok_or(TackyError::MissingValue)?;
                self.visit_expression(*rhs)?;
                let rhs = self.values.pop().ok_or(TackyError::MissingValue)?;

                let dst = self.make_tmp();
                self.add_instruction(Instruction::Binary(
                    op.into(),
                    Box::new(lhs),
                    Box::new(rhs),
                    Box::new(dst.clone()),
                ));

                self.push_value(dst);
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum TackyError {
    MissingValue,
}

impl fmt::Display for TackyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TackyError::MissingValue => {
                write!(f, "Missing value in statement")
            }
        }
    }
}

pub trait TackyVisitor<'a> {
    type Error;

    fn visit_program(&mut self, program: Program<'a>) -> Result<(), Self::Error>;
    fn visit_function_definition(
        &mut self,
        function_definition: FunctionDefinition<'a>,
    ) -> Result<(), Self::Error>;
    fn visit_instruction(&mut self, statement: Instruction) -> Result<(), Self::Error>;
    fn visit_value(&mut self, value: Value) -> Result<(), Self::Error>;
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn return_neg_minus_2() {
        /*
        int main(void) {
          return ~(-2);
        }
        */
        let inner_expr = ast::Expression::Constant(-2);
        let minus_expr = ast::Expression::Unary(ast::UnaryOperator::Minus, Box::new(inner_expr));
        let tilde_expr = ast::Expression::Unary(ast::UnaryOperator::Tilde, Box::new(minus_expr));
        let return_stmt = ast::Statement::Return(tilde_expr);
        let main_function = ast::FunctionDefinition {
            name: "main",
            body: return_stmt,
        };
        let ast_program = ast::Program {
            functions: vec![main_function],
        };

        let mut generator = TackyEmitter::new();
        let _ = generator.visit_program(ast_program);

        let program = generator.get_program().unwrap();
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].name, "main");

        // Expected instructions:
        // 1. Unary(Minus, Constant(-2), tmp.0)
        // 2. Unary(Tilde, tmp.0, tmp.1)
        // 3. Return(tmp.1)
        assert_eq!(program.functions[0].body.len(), 3);

        // 1. Unary(Minus, Constant(-2), tmp.0)
        match &program.functions[0].body[0] {
            Instruction::Unary(UnaryOperator::Minus, src, dst) => {
                assert_eq!(**src, Value::Constant(-2));
                assert_eq!(**dst, Value::Var("tmp.0".to_string()));
            }
            _ => panic!("Expected Unary(Minus, ...) instruction"),
        }

        // 2. Unary(Tilde, tmp.0, tmp.1)
        match &program.functions[0].body[1] {
            Instruction::Unary(UnaryOperator::Tilde, src, dst) => {
                assert_eq!(**src, Value::Var("tmp.0".to_string()));
                assert_eq!(**dst, Value::Var("tmp.1".to_string()));
            }
            _ => panic!("Expected Unary(Tilde, ...) instruction"),
        }

        // 3. Return(tmp.1)
        match &program.functions[0].body[2] {
            Instruction::Return(val) => {
                assert_eq!(*val, Value::Var("tmp.1".to_string()));
            }
            _ => panic!("Expected Return instruction"),
        }
    }

    #[test]
    fn return_triple_nested_unary() {
        /*
        int main(void) {
          return -(~(-8));
        }
        */

        let constant_expr = ast::Expression::Constant(8);
        let inner_negate =
            ast::Expression::Unary(ast::UnaryOperator::Minus, Box::new(constant_expr));
        let complement = ast::Expression::Unary(ast::UnaryOperator::Tilde, Box::new(inner_negate));
        let outer_negate = ast::Expression::Unary(ast::UnaryOperator::Minus, Box::new(complement));

        let return_stmt = ast::Statement::Return(outer_negate);
        let main_function = ast::FunctionDefinition {
            name: "main",
            body: return_stmt,
        };
        let ast_program = ast::Program {
            functions: vec![main_function],
        };

        let mut generator = TackyEmitter::new();
        let _ = generator.visit_program(ast_program);

        let program = generator.get_program().unwrap();
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].name, "main");

        // Expected instructions:
        // 1. Unary(Minus, Constant(8), tmp.0)      -> tmp.0 = -8
        // 2. Unary(Tilde, tmp.0, tmp.1)            -> tmp.1 = ~(-8) = ~(-8) = 7
        // 3. Unary(Minus, tmp.1, tmp.2)            -> tmp.2 = -(~(-8)) = -7
        // 4. Return(tmp.2)
        assert_eq!(program.functions[0].body.len(), 4);

        // 1. Unary(Minus, Constant(8), tmp.0)
        match &program.functions[0].body[0] {
            Instruction::Unary(UnaryOperator::Minus, src, dst) => {
                assert_eq!(**src, Value::Constant(8));
                assert_eq!(**dst, Value::Var("tmp.0".to_string()));
            }
            _ => panic!("Expected Unary(Minus, ...) instruction at position 0"),
        }

        // 2. Unary(Tilde, tmp.0, tmp.1)
        match &program.functions[0].body[1] {
            Instruction::Unary(UnaryOperator::Tilde, src, dst) => {
                assert_eq!(**src, Value::Var("tmp.0".to_string()));
                assert_eq!(**dst, Value::Var("tmp.1".to_string()));
            }
            _ => panic!("Expected Unary(Tilde, ...) instruction at position 1"),
        }

        // 3. Unary(Minus, tmp.1, tmp.2)
        match &program.functions[0].body[2] {
            Instruction::Unary(UnaryOperator::Minus, src, dst) => {
                assert_eq!(**src, Value::Var("tmp.1".to_string()));
                assert_eq!(**dst, Value::Var("tmp.2".to_string()));
            }
            _ => panic!("Expected Unary(Minus, ...) instruction at position 2"),
        }

        // 4. Return(tmp.2)
        match &program.functions[0].body[3] {
            Instruction::Return(val) => {
                assert_eq!(*val, Value::Var("tmp.2".to_string()));
            }
            _ => panic!("Expected Return instruction at position 3"),
        }
    }

    #[test]
    fn return_bitwise_binop() {
        /*
        int main(void) {
          return (1 & 2) | (3 ^ (4 << 1)) >> 1;
        }
        */

        let one = ast::Expression::Constant(1);
        let two = ast::Expression::Constant(2);
        let three = ast::Expression::Constant(3);
        let four = ast::Expression::Constant(4);

        // 4 << 1
        let shl = ast::Expression::Binary(
            ast::BinaryOperator::LShift,
            Box::new(four),
            Box::new(one.clone()),
        );

        // 3 ^ (4 << 1)
        let xor = ast::Expression::Binary(ast::BinaryOperator::Xor, Box::new(three), Box::new(shl));

        // (1 & 2)
        let and = ast::Expression::Binary(
            ast::BinaryOperator::And,
            Box::new(one.clone()),
            Box::new(two),
        );

        // (3 ^ (4 << 1)) >> 1
        let shr =
            ast::Expression::Binary(ast::BinaryOperator::RShift, Box::new(xor), Box::new(one));

        // (1 & 2) | ((3 ^ (4 << 1)) >> 1)
        let or = ast::Expression::Binary(ast::BinaryOperator::Or, Box::new(and), Box::new(shr));

        let return_stmt = ast::Statement::Return(or);
        let main_function = ast::FunctionDefinition {
            name: "main",
            body: return_stmt,
        };
        let ast_program = ast::Program {
            functions: vec![main_function],
        };

        let mut generator = TackyEmitter::new();
        let _ = generator.visit_program(ast_program);

        let program = generator.get_program().unwrap();
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].name, "main");

        // Expected instruction sequence:
        // tmp.0 = 1 & 2
        // tmp.1 = 4 << 1
        // tmp.2 = 3 ^ tmp.1
        // tmp.3 = tmp.2 >> 1
        // tmp.4 = tmp.0 | tmp.3
        // return tmp.4
        assert_eq!(program.functions[0].body.len(), 6);
        println!("{}", program);

        // 1. 1 & 2
        match &program.functions[0].body[0] {
            Instruction::Binary(BinaryOperator::And, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Constant(1));
                assert_eq!(**rhs, Value::Constant(2));
                assert_eq!(**dst, Value::Var("tmp.0".to_string()));
            }
            _ => panic!("Expected Binary(And) at position 0"),
        }

        // 2. 4 << 1
        match &program.functions[0].body[1] {
            Instruction::Binary(BinaryOperator::Shl, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Constant(4));
                assert_eq!(**rhs, Value::Constant(1));
                assert_eq!(**dst, Value::Var("tmp.1".to_string()));
            }
            _ => panic!("Expected Binary(Shl) at position 1"),
        }

        // 3. 3 ^ tmp.1
        match &program.functions[0].body[2] {
            Instruction::Binary(BinaryOperator::Xor, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Constant(3));
                assert_eq!(**rhs, Value::Var("tmp.1".to_string()));
                assert_eq!(**dst, Value::Var("tmp.2".to_string()));
            }
            _ => panic!("Expected Binary(Xor) at position 2"),
        }

        // 4. tmp.2 >> 1
        match &program.functions[0].body[3] {
            Instruction::Binary(BinaryOperator::Shr, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Var("tmp.2".to_string()));
                assert_eq!(**rhs, Value::Constant(1));
                assert_eq!(**dst, Value::Var("tmp.3".to_string()));
            }
            _ => panic!("Expected Binary(Shr) at position 3"),
        }

        // 5. tmp.0 | tmp.3
        match &program.functions[0].body[4] {
            Instruction::Binary(BinaryOperator::Or, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Var("tmp.0".to_string()));
                assert_eq!(**rhs, Value::Var("tmp.3".to_string()));
                assert_eq!(**dst, Value::Var("tmp.4".to_string()));
            }
            _ => panic!("Expected Binary(Or) at position 4"),
        }

        // 6. return tmp.4
        match &program.functions[0].body[5] {
            Instruction::Return(val) => {
                assert_eq!(*val, Value::Var("tmp.4".to_string()));
            }
            _ => panic!("Expected Return at position 5"),
        }
    }
}
