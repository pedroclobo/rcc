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
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Return(Value),
    Unary(UnaryOperator, Box<Value>, Box<Value>),
    Binary(BinaryOperator, Box<Value>, Box<Value>, Box<Value>),
    Copy(Box<Value>, Box<Value>),
    Label(String),
    Jump(String),
    JumpIfZero(Box<Value>, String),
    JumpIfNotZero(Box<Value>, String),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Return(val) => write!(f, "\treturn {}", val),
            Instruction::Unary(op, src, dst) => write!(f, "\t{} {} = {}", op, src, dst),
            Instruction::Binary(op, lhs, rhs, dst) => {
                write!(f, "\t{} = {} {} {}", dst, op, lhs, rhs)
            }
            Instruction::Copy(src, dst) => write!(f, "\tid {} = {}", dst, src),
            Instruction::Label(label) => write!(f, "{}: ", label),
            Instruction::Jump(label) => write!(f, "\tjmp {}", label),
            Instruction::JumpIfZero(value, label) => write!(f, "\tjz {} {}", value, label),
            Instruction::JumpIfNotZero(value, label) => write!(f, "\tjnz {} {}", value, label),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Tilde,
    Minus,
    Not,
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Tilde => write!(f, "not"),
            UnaryOperator::Minus => write!(f, "neg"),
            UnaryOperator::Not => write!(f, "not"),
        }
    }
}

impl From<ast::UnaryOperator> for UnaryOperator {
    fn from(op: ast::UnaryOperator) -> Self {
        match op {
            ast::UnaryOperator::Tilde => UnaryOperator::Tilde,
            ast::UnaryOperator::Minus => UnaryOperator::Minus,
            ast::UnaryOperator::Not => UnaryOperator::Not,
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
    BAnd,
    BOr,
    Xor,
    Shl,
    Shr,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "add"),
            BinaryOperator::Sub => write!(f, "sub"),
            BinaryOperator::Mul => write!(f, "mul"),
            BinaryOperator::Div => write!(f, "div"),
            BinaryOperator::Mod => write!(f, "mod"),
            BinaryOperator::BAnd => write!(f, "and"),
            BinaryOperator::BOr => write!(f, "or"),
            BinaryOperator::Xor => write!(f, "xor"),
            BinaryOperator::Shl => write!(f, "shl"),
            BinaryOperator::Shr => write!(f, "shr"),
            BinaryOperator::Eq => write!(f, "eq"),
            BinaryOperator::Neq => write!(f, "neq"),
            BinaryOperator::Lt => write!(f, "lt"),
            BinaryOperator::Gt => write!(f, "gt"),
            BinaryOperator::Le => write!(f, "le"),
            BinaryOperator::Ge => write!(f, "ge"),
        }
    }
}

// TODO: try from?
impl From<ast::BinaryOperator> for BinaryOperator {
    fn from(op: ast::BinaryOperator) -> Self {
        match op {
            ast::BinaryOperator::Add => BinaryOperator::Add,
            ast::BinaryOperator::Sub => BinaryOperator::Sub,
            ast::BinaryOperator::Mul => BinaryOperator::Mul,
            ast::BinaryOperator::Div => BinaryOperator::Div,
            ast::BinaryOperator::Mod => BinaryOperator::Mod,
            ast::BinaryOperator::BAnd => BinaryOperator::BAnd,
            ast::BinaryOperator::BOr => BinaryOperator::BOr,
            ast::BinaryOperator::Xor => BinaryOperator::Xor,
            ast::BinaryOperator::LShift => BinaryOperator::Shl,
            ast::BinaryOperator::RShift => BinaryOperator::Shr,
            ast::BinaryOperator::And => panic!("And operator not supported"),
            ast::BinaryOperator::Or => panic!("Or operator not supported"),
            ast::BinaryOperator::Eq => BinaryOperator::Eq,
            ast::BinaryOperator::Neq => BinaryOperator::Neq,
            ast::BinaryOperator::Lt => BinaryOperator::Lt,
            ast::BinaryOperator::Gt => BinaryOperator::Gt,
            ast::BinaryOperator::Le => BinaryOperator::Le,
            ast::BinaryOperator::Ge => BinaryOperator::Ge,
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

    fn make_label(&mut self) -> String {
        let tmp = format!("{}", self.counter);
        self.counter += 1;
        tmp
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
                if matches!(op, ast::BinaryOperator::And) {
                    self.visit_expression(*lhs)?;
                    let lhs = self.values.pop().ok_or(TackyError::MissingValue)?;

                    let false_label = self.make_label();
                    let v1 = self.make_tmp();
                    self.add_instruction(Instruction::Copy(Box::new(lhs.clone()), Box::new(v1)));
                    self.add_instruction(Instruction::JumpIfZero(
                        Box::new(lhs.clone()),
                        false_label.clone(),
                    ));

                    self.visit_expression(*rhs)?;
                    let rhs = self.values.pop().ok_or(TackyError::MissingValue)?;

                    let v2 = self.make_tmp();
                    self.add_instruction(Instruction::Copy(Box::new(rhs.clone()), Box::new(v2)));
                    self.add_instruction(Instruction::JumpIfZero(
                        Box::new(rhs.clone()),
                        false_label.clone(),
                    ));

                    let dst = self.make_tmp();
                    self.add_instruction(Instruction::Copy(
                        Box::new(Value::Constant(1)),
                        Box::new(dst.clone()),
                    ));
                    let end_label = self.make_label();
                    self.add_instruction(Instruction::Jump(end_label.clone()));

                    self.add_instruction(Instruction::Label(false_label.clone()));
                    self.add_instruction(Instruction::Copy(
                        Box::new(Value::Constant(0)),
                        Box::new(dst.clone()),
                    ));
                    self.add_instruction(Instruction::Label(end_label.clone()));

                    self.push_value(dst.clone());
                } else if matches!(op, ast::BinaryOperator::Or) {
                    self.visit_expression(*lhs)?;
                    let lhs = self.values.pop().ok_or(TackyError::MissingValue)?;

                    let true_label = self.make_label();
                    let v1 = self.make_tmp();
                    self.add_instruction(Instruction::Copy(Box::new(lhs.clone()), Box::new(v1)));
                    self.add_instruction(Instruction::JumpIfNotZero(
                        Box::new(lhs.clone()),
                        true_label.clone(),
                    ));

                    self.visit_expression(*rhs)?;
                    let rhs = self.values.pop().ok_or(TackyError::MissingValue)?;
                    let v2 = self.make_tmp();
                    self.add_instruction(Instruction::Copy(Box::new(rhs.clone()), Box::new(v2)));
                    self.add_instruction(Instruction::JumpIfNotZero(
                        Box::new(rhs.clone()),
                        true_label.clone(),
                    ));

                    let dst = self.make_tmp();
                    self.add_instruction(Instruction::Copy(
                        Box::new(Value::Constant(0)),
                        Box::new(dst.clone()),
                    ));
                    let end_label = self.make_label();
                    self.add_instruction(Instruction::Jump(end_label.clone()));

                    self.add_instruction(Instruction::Label(true_label.clone()));
                    self.add_instruction(Instruction::Copy(
                        Box::new(Value::Constant(1)),
                        Box::new(dst.clone()),
                    ));
                    self.add_instruction(Instruction::Label(end_label.clone()));

                    self.push_value(dst.clone());
                } else {
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
            ast::BinaryOperator::BAnd,
            Box::new(one.clone()),
            Box::new(two),
        );

        // (3 ^ (4 << 1)) >> 1
        let shr =
            ast::Expression::Binary(ast::BinaryOperator::RShift, Box::new(xor), Box::new(one));

        // (1 & 2) | ((3 ^ (4 << 1)) >> 1)
        let or = ast::Expression::Binary(ast::BinaryOperator::BOr, Box::new(and), Box::new(shr));

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
            Instruction::Binary(BinaryOperator::BAnd, lhs, rhs, dst) => {
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
            Instruction::Binary(BinaryOperator::BOr, lhs, rhs, dst) => {
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

    #[test]
    fn return_comparison_binop() {
        /*
        int main(void) {
          return (1 < 2) == (3 != 4) && (5 <= 6) >= (7 > 8);
        }
        */

        let one = ast::Expression::Constant(1);
        let two = ast::Expression::Constant(2);
        let three = ast::Expression::Constant(3);
        let four = ast::Expression::Constant(4);
        let five = ast::Expression::Constant(5);
        let six = ast::Expression::Constant(6);
        let seven = ast::Expression::Constant(7);
        let eight = ast::Expression::Constant(8);

        // 1 < 2
        let lt = ast::Expression::Binary(ast::BinaryOperator::Lt, Box::new(one), Box::new(two));

        // 3 != 4
        let neq =
            ast::Expression::Binary(ast::BinaryOperator::Neq, Box::new(three), Box::new(four));

        // (1 < 2) == (3 != 4)
        let eq = ast::Expression::Binary(ast::BinaryOperator::Eq, Box::new(lt), Box::new(neq));

        // 5 <= 6
        let le = ast::Expression::Binary(ast::BinaryOperator::Le, Box::new(five), Box::new(six));

        // 7 > 8
        let gt = ast::Expression::Binary(ast::BinaryOperator::Gt, Box::new(seven), Box::new(eight));

        // (5 <= 6) >= (7 > 8)
        let ge = ast::Expression::Binary(ast::BinaryOperator::Ge, Box::new(le), Box::new(gt));

        // ((1 < 2) == (3 != 4)) && ((5 <= 6) >= (7 > 8))
        let and = ast::Expression::Binary(ast::BinaryOperator::And, Box::new(eq), Box::new(ge));

        let return_stmt = ast::Statement::Return(and);
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
        // tmp.0 = 1 < 2
        // tmp.1 = 3 != 4
        // tmp.2 = tmp.0 == tmp.1
        // tmp.3 = 5 <= 6
        // tmp.4 = 7 > 8
        // tmp.5 = tmp.3 >= tmp.4
        // tmp.6 = tmp.2 && tmp.5
        // return tmp.6
        assert_eq!(program.functions[0].body.len(), 8);

        // 1. 1 < 2
        match &program.functions[0].body[0] {
            Instruction::Binary(BinaryOperator::Lt, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Constant(1));
                assert_eq!(**rhs, Value::Constant(2));
                assert_eq!(**dst, Value::Var("tmp.0".to_string()));
            }
            _ => panic!("Expected Binary(Lt) at position 0"),
        }

        // 2. 3 != 4
        match &program.functions[0].body[1] {
            Instruction::Binary(BinaryOperator::Neq, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Constant(3));
                assert_eq!(**rhs, Value::Constant(4));
                assert_eq!(**dst, Value::Var("tmp.1".to_string()));
            }
            _ => panic!("Expected Binary(Neq) at position 1"),
        }

        // 3. tmp.0 == tmp.1
        match &program.functions[0].body[2] {
            Instruction::Binary(BinaryOperator::Eq, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Var("tmp.0".to_string()));
                assert_eq!(**rhs, Value::Var("tmp.1".to_string()));
                assert_eq!(**dst, Value::Var("tmp.2".to_string()));
            }
            _ => panic!("Expected Binary(Eq) at position 2"),
        }

        // 4. 5 <= 6
        match &program.functions[0].body[3] {
            Instruction::Binary(BinaryOperator::Le, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Constant(5));
                assert_eq!(**rhs, Value::Constant(6));
                assert_eq!(**dst, Value::Var("tmp.3".to_string()));
            }
            _ => panic!("Expected Binary(Le) at position 3"),
        }

        // 5. 7 > 8
        match &program.functions[0].body[4] {
            Instruction::Binary(BinaryOperator::Gt, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Constant(7));
                assert_eq!(**rhs, Value::Constant(8));
                assert_eq!(**dst, Value::Var("tmp.4".to_string()));
            }
            _ => panic!("Expected Binary(Gt) at position 4"),
        }

        // 6. tmp.3 >= tmp.4
        match &program.functions[0].body[5] {
            Instruction::Binary(BinaryOperator::Ge, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Var("tmp.3".to_string()));
                assert_eq!(**rhs, Value::Var("tmp.4".to_string()));
                assert_eq!(**dst, Value::Var("tmp.5".to_string()));
            }
            _ => panic!("Expected Binary(Ge) at position 5"),
        }

        // 7. tmp.2 && tmp.5
        match &program.functions[0].body[6] {
            Instruction::Binary(BinaryOperator::And, lhs, rhs, dst) => {
                assert_eq!(**lhs, Value::Var("tmp.2".to_string()));
                assert_eq!(**rhs, Value::Var("tmp.5".to_string()));
                assert_eq!(**dst, Value::Var("tmp.6".to_string()));
            }
            _ => panic!("Expected Binary(And) at position 6"),
        }

        // 8. return tmp.6
        match &program.functions[0].body[7] {
            Instruction::Return(val) => {
                assert_eq!(*val, Value::Var("tmp.6".to_string()));
            }
            _ => panic!("Expected Return at position 7"),
        }
    }
}
