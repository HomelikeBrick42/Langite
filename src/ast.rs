use crate::{interning::InternedStr, lexing::SourceLocation};
use slotmap::{SlotMap, new_key_type};

new_key_type! {
    pub struct ConstId;
}

#[derive(Debug, Clone)]
pub struct Const {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub type_: Option<Box<EvalContext>>,
    pub value: Option<Box<EvalContext>>,
}

new_key_type! {
    pub struct FunctionId;
}

#[derive(Debug, Clone)]
pub struct Function {
    pub location: SourceLocation,
    pub inferred_parameters: Vec<Parameter>,
    pub parameters: Vec<Parameter>,
    pub body: FunctionBody,
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Defintion {
        return_type: Box<EvalContext>,
        body: Box<EvalContext>,
    },
    NakedDefintion {
        body: Box<EvalContext>,
    },
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
    Struct {
        members: Vec<Member>,
    },
    Enum {
        members: Vec<Member>,
    },
    Function(FunctionId),
    FunctionType {
        inferred_parameters: Vec<Parameter>,
        parameters: Vec<Parameter>,
        return_type: Box<EvalContext>,
    },
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
    Distinct {
        keys: Vec<EvalContext>,
        expression: Box<EvalContext>,
    },
    TypeOf {
        expression: Box<EvalContext>,
    },
    Constructor {
        type_: Box<EvalContext>,
        members: Vec<ConstructorMember>,
    },
}

#[derive(Debug, Clone)]
pub struct Member {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub type_: Box<EvalContext>,
}

#[derive(Debug, Clone)]
pub struct ConstructorMember {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub location: SourceLocation,
    pub const_: bool,
    pub name: InternedStr,
    pub type_: Box<EvalContext>,
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
    Destructor {
        type_: Box<EvalContext>,
        members: Vec<DestructorMember>,
    },
}

#[derive(Debug, Clone)]
pub struct DestructorMember {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub pattern: Pattern,
}

#[derive(Debug, Clone)]
pub enum Builtin {
    Type,
}

#[derive(Debug, Clone)]
pub enum Name {
    Builtin(Builtin),
    Const(ConstId),
    ImplicitParameter { index: usize },
    Parameter { index: usize },
    Variable(VariableId),
}
