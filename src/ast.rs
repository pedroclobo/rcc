#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<FunctionDefinition<'a>>,
}

#[derive(Debug)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub body: Statement,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Tilde,
    Minus,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
}

pub trait Visitor {
    type Error;

    fn visit_program(&mut self, program: Program) -> Result<(), Self::Error>;
    fn visit_function_definition(
        &mut self,
        function_definition: FunctionDefinition,
    ) -> Result<(), Self::Error>;
    fn visit_statement(&mut self, statement: Statement) -> Result<(), Self::Error>;
    fn visit_expression(&mut self, expression: Expression) -> Result<(), Self::Error>;
}
