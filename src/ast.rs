use slotmap::{SlotMap, new_key_type};

use crate::{interning::InternedStr, lexing::SourceLocation};

new_key_type! {
    pub struct TypeId;
}

#[derive(Debug, Clone)]
pub struct Type {
    pub location: SourceLocation,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Unresolved,
    Type,
    Struct {
        name: InternedStr,
        inferred_parameters: Vec<Parameter>,
        members: Vec<Member>,
    },
    Enum {
        name: InternedStr,
        inferred_parameters: Vec<Parameter>,
        members: Vec<Member>,
    },
}

#[derive(Debug, Clone)]
pub struct Member {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub type_: Box<EvalContext>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub location: SourceLocation,
    pub const_: bool,
    pub name: InternedStr,
    pub type_: Box<EvalContext>,
}

new_key_type! {
    pub struct FunctionId;
}

#[derive(Debug, Clone)]
pub struct Function {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub inferred_parameters: Vec<Parameter>,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<EvalContext>,
    pub body: Option<Box<EvalContext>>,
}

#[derive(Debug, Clone)]
pub struct EvalContext {
    pub variables: SlotMap<VariableId, Variable>,
    pub expression: Expression,
}

new_key_type! {
    pub struct VariableId;
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub type_: Option<Box<EvalContext>>,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub location: SourceLocation,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Name(Name),
    Block {
        statements: Vec<Statement>,
        final_expression: Box<Expression>,
    },
    Match {
        condition: Box<Expression>,
        arms: Vec<MatchArmExpression>,
    },
    Call {
        operand: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct MatchArmExpression {
    pub pattern: Pattern,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub location: SourceLocation,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Block {
        statements: Vec<Statement>,
    },
    Match {
        condition: Box<Expression>,
        arms: Vec<MatchArmStatement>,
    },
    Assignment {
        pattern: Box<Pattern>,
        value: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct MatchArmStatement {
    pub pattern: Pattern,
    pub statement: Statement,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub location: SourceLocation,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Name(Name),
    Discard,
    Let(VariableId),
}

#[derive(Debug, Clone)]
pub enum Name {
    Type(TypeId),
    Function(FunctionId),
    ImplicitParameter { index: usize },
    Parameter { index: usize },
    Variable(VariableId),
}
