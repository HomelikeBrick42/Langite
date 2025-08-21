use crate::{
    interning::InternedStr,
    lexing::{SourceLocation, Token},
};

#[derive(Debug, Clone)]
pub struct Item {
    pub location: SourceLocation,
    pub kind: ItemKind,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Struct {
        struct_token: Token,
        name_token: Token,
        name: InternedStr,
        inferred_parameters: Option<Box<InferredParameters>>,
        open_brace_token: Token,
        members: Vec<Member>,
        close_brace_token: Token,
    },
    Enum {
        enum_token: Token,
        name_token: Token,
        name: InternedStr,
        inferred_parameters: Option<Box<InferredParameters>>,
        open_brace_token: Token,
        members: Vec<Member>,
        close_brace_token: Token,
    },
    Function {
        fn_token: Token,
        name_token: Token,
        name: InternedStr,
        inferred_parameters: Option<Box<InferredParameters>>,
        parameters: Box<Parameters>,
        right_arrow_token: Token,
        return_type: Box<Expression>,
        body: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct Member {
    pub name_token: Token,
    pub name: InternedStr,
    pub colon_token: Token,
    pub type_: Expression,
}

#[derive(Debug, Clone)]
pub struct InferredParameters {
    pub open_bracket_token: Token,
    pub inferred_parameters: Vec<Parameter>,
    pub close_bracket_token: Token,
}

#[derive(Debug, Clone)]
pub struct Parameters {
    pub open_parenthesis_token: Token,
    pub parameters: Vec<Parameter>,
    pub close_parenthesis_token: Token,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub const_token: Option<Token>,
    pub name_token: Token,
    pub name: InternedStr,
    pub colon_token: Token,
    pub type_: Expression,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub location: SourceLocation,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    ParenthesisedExpression {
        open_parenthesis_token: Token,
        expression: Box<Expression>,
        close_parenthesis_token: Token,
    },
    Name {
        name_token: Token,
        name: InternedStr,
    },
    Discard {
        discard_token: Token,
    },
    Let {
        let_token: Token,
        name_token: Token,
        name: InternedStr,
        colon_token: Option<Token>,
        type_: Option<Box<Expression>>,
    },
    Block {
        open_brace_token: Token,
        statements: Vec<Statement>,
        close_brace_token: Token,
    },
    Match {
        match_token: Token,
        condition: Box<Expression>,
        open_brace_token: Token,
        arms: Vec<MatchArm>,
        close_brace_token: Token,
    },
    Call {
        operand: Box<Expression>,
        open_parenthesis_token: Token,
        arguments: Vec<Expression>,
        close_parenthesis_token: Token,
    },
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Expression,
    pub fat_right_arrow_token: Token,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub location: SourceLocation,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Item(Box<Item>),
    Expression(Box<Expression>),
    Assignment {
        pattern: Box<Expression>,
        equals_token: Token,
        value: Box<Expression>,
    },
}
