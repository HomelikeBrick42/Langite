use std::{
    cell::{Cell, RefCell},
    ffi::c_void,
    rc::Rc,
};

use derive_more::{Display, IsVariant};
use enum_as_inner::EnumAsInner;

use crate::{SourceLocation, SourceSpan, Type};

#[derive(Clone, Debug, IsVariant, EnumAsInner)]
pub enum Ast {
    File(Rc<AstFile>),
    Procedure(Rc<AstProcedure>),
    ProcedureType(Rc<AstProcedureType>),
    Parameter(Rc<AstParameter>),
    Scope(Rc<AstScope>),
    LetDeclaration(Rc<AstLet>),
    VarDeclaration(Rc<AstVar>),
    Name(Rc<AstName>),
    Integer(Rc<AstInteger>),
    Call(Rc<AstCall>),
    Return(Rc<AstReturn>),
    Unary(Rc<AstUnary>),
    Binary(Rc<AstBinary>),
    If(Rc<AstIf>),
    While(Rc<AstWhile>),
    Cast(Rc<AstCast>),
    Assign(Rc<AstAssign>),
    Builtin(Rc<AstBuiltin>),
}

impl PartialEq for Ast {
    fn eq(&self, other: &Self) -> bool {
        self.get_ptr() == other.get_ptr()
    }
}

impl Ast {
    pub fn get_type(&self) -> Option<Rc<Type>> {
        match self {
            Ast::File(file) => file.resolved_type.borrow().clone(),
            Ast::Procedure(procedure) => procedure.resolved_type.borrow().clone(),
            Ast::ProcedureType(procedure_type) => procedure_type.resolved_type.borrow().clone(),
            Ast::Parameter(parameter) => parameter.resolved_type.borrow().clone(),
            Ast::Scope(scope) => scope.resolved_type.borrow().clone(),
            Ast::LetDeclaration(declaration) => declaration.resolved_type.borrow().clone(),
            Ast::VarDeclaration(declaration) => declaration.resolved_type.borrow().clone(),
            Ast::Name(name) => name
                .resolved_declaration
                .borrow()
                .as_ref()
                .map(Ast::get_type)
                .flatten(),
            Ast::Integer(integer) => integer.resolved_type.borrow().clone(),
            Ast::Call(call) => call.resolved_type.borrow().clone(),
            Ast::Return(returnn) => returnn.resolved_type.borrow().clone(),
            Ast::Unary(unary) => unary.resolved_type.borrow().clone(),
            Ast::Binary(binary) => binary.resolved_type.borrow().clone(),
            Ast::If(iff) => iff.resolved_type.borrow().clone(),
            Ast::While(whilee) => whilee.resolved_type.borrow().clone(),
            Ast::Cast(cast) => cast.resolved_type.borrow().clone(),
            Ast::Assign(assign) => assign.resolved_type.borrow().clone(),
            Ast::Builtin(builtin) => match &builtin.kind {
                AstBuiltinKind::Bool
                | AstBuiltinKind::Type
                | AstBuiltinKind::Void
                | AstBuiltinKind::IntegerType { size: _, signed: _ } => {
                    builtin.resolved_type.borrow().clone()
                }
            },
        }
    }

    pub fn set_resolving(&self, value: bool) {
        match self {
            Ast::File(file) => file.resolving.set(value),
            Ast::Procedure(procedure) => procedure.resolving.set(value),
            Ast::ProcedureType(procedure_type) => procedure_type.resolving.set(value),
            Ast::Parameter(parameter) => parameter.resolving.set(value),
            Ast::Scope(scope) => scope.resolving.set(value),
            Ast::LetDeclaration(declaration) => declaration.resolving.set(value),
            Ast::VarDeclaration(declaration) => declaration.resolving.set(value),
            Ast::Name(name) => name.resolving.set(value),
            Ast::Integer(integer) => integer.resolving.set(value),
            Ast::Call(call) => call.resolving.set(value),
            Ast::Return(returnn) => returnn.resolving.set(value),
            Ast::Unary(unary) => unary.resolving.set(value),
            Ast::Binary(binary) => binary.resolving.set(value),
            Ast::If(iff) => iff.resolving.set(value),
            Ast::While(whilee) => whilee.resolving.set(value),
            Ast::Cast(cast) => cast.resolving.set(value),
            Ast::Assign(assign) => assign.resolving.set(value),
            Ast::Builtin(builtin) => match &builtin.kind {
                AstBuiltinKind::Type => (),
                AstBuiltinKind::Void => (),
                AstBuiltinKind::Bool => (),
                AstBuiltinKind::IntegerType { size: _, signed: _ } => (),
            },
        }
    }

    pub fn get_resolving(&self) -> bool {
        match self {
            Ast::File(file) => file.resolving.get(),
            Ast::Procedure(procedure) => procedure.resolving.get(),
            Ast::ProcedureType(procedure_type) => procedure_type.resolving.get(),
            Ast::Parameter(parameter) => parameter.resolving.get(),
            Ast::Scope(scope) => scope.resolving.get(),
            Ast::LetDeclaration(declaration) => declaration.resolving.get(),
            Ast::VarDeclaration(declaration) => declaration.resolving.get(),
            Ast::Name(name) => name.resolving.get(),
            Ast::Integer(integer) => integer.resolving.get(),
            Ast::Call(call) => call.resolving.get(),
            Ast::Return(returnn) => returnn.resolving.get(),
            Ast::Unary(unary) => unary.resolving.get(),
            Ast::Binary(binary) => binary.resolving.get(),
            Ast::If(iff) => iff.resolving.get(),
            Ast::While(whilee) => whilee.resolving.get(),
            Ast::Cast(cast) => cast.resolving.get(),
            Ast::Assign(assign) => assign.resolving.get(),
            Ast::Builtin(builtin) => match &builtin.kind {
                AstBuiltinKind::Type => false,
                AstBuiltinKind::Void => false,
                AstBuiltinKind::Bool => false,
                AstBuiltinKind::IntegerType { size: _, signed: _ } => false,
            },
        }
    }

    pub fn get_location(&self) -> SourceSpan {
        match self {
            Ast::File(file) => file.location.clone(),
            Ast::Procedure(procedure) => procedure.location.clone(),
            Ast::ProcedureType(procedure_type) => procedure_type.location.clone(),
            Ast::Parameter(parameter) => parameter.location.clone(),
            Ast::Scope(scope) => scope.location.clone(),
            Ast::LetDeclaration(declaration) => declaration.location.clone(),
            Ast::VarDeclaration(declaration) => declaration.location.clone(),
            Ast::Name(name) => name.location.clone(),
            Ast::Integer(integer) => integer.location.clone(),
            Ast::Call(call) => call.location.clone(),
            Ast::Return(returnn) => returnn.location.clone(),
            Ast::Unary(unary) => unary.location.clone(),
            Ast::Binary(binary) => binary.location.clone(),
            Ast::If(iff) => iff.location.clone(),
            Ast::While(whilee) => whilee.location.clone(),
            Ast::Cast(cast) => cast.location.clone(),
            Ast::Assign(assign) => assign.location.clone(),
            Ast::Builtin(_) => SourceSpan {
                filepath: "builtin.lang".into(),
                start: SourceLocation {
                    position: 0,
                    line: 1,
                    column: 1,
                },
                end: SourceLocation {
                    position: 0,
                    line: 1,
                    column: 1,
                },
            },
        }
    }

    pub(crate) fn get_ptr(&self) -> *const c_void {
        match self {
            Ast::File(file) => Rc::as_ptr(file) as *const _,
            Ast::Procedure(procedure) => Rc::as_ptr(procedure) as *const _,
            Ast::ProcedureType(procedure_type) => Rc::as_ptr(procedure_type) as *const _,
            Ast::Parameter(parameter) => Rc::as_ptr(parameter) as *const _,
            Ast::Scope(scope) => Rc::as_ptr(scope) as *const _,
            Ast::LetDeclaration(declaration) => Rc::as_ptr(declaration) as *const _,
            Ast::VarDeclaration(declaration) => Rc::as_ptr(declaration) as *const _,
            Ast::Name(name) => Rc::as_ptr(name) as *const _,
            Ast::Integer(integer) => Rc::as_ptr(integer) as *const _,
            Ast::Call(call) => Rc::as_ptr(call) as *const _,
            Ast::Return(returnn) => Rc::as_ptr(returnn) as *const _,
            Ast::Unary(unary) => Rc::as_ptr(unary) as *const _,
            Ast::Binary(binary) => Rc::as_ptr(binary) as *const _,
            Ast::If(iff) => Rc::as_ptr(iff) as *const _,
            Ast::While(whilee) => Rc::as_ptr(whilee) as *const _,
            Ast::Cast(cast) => Rc::as_ptr(cast) as *const _,
            Ast::Assign(assign) => Rc::as_ptr(assign) as *const _,
            Ast::Builtin(builtin) => Rc::as_ptr(builtin) as *const _,
        }
    }
}

type ResolvedType = RefCell<Option<Rc<Type>>>;

#[derive(Clone, Debug, PartialEq)]
pub struct AstFile {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub expressions: Vec<Ast>,
}

#[derive(Clone, Debug, Display, PartialEq, EnumAsInner)]
pub enum CallingConvention {
    #[display(fmt = "#cdecl")]
    CDecl,
    #[display(fmt = "#stdcall")]
    StdCall,
    #[display(fmt = "#fastcall")]
    FastCall,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstProcedure {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub name: String,
    pub parameters: Vec<Rc<AstParameter>>,
    pub return_type: Ast,
    pub calling_convention: CallingConvention,
    pub body: AstProcedureBody,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstProcedureType {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub parameter_types: Vec<Ast>,
    pub calling_convention: CallingConvention,
    pub return_type: Ast,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstParameter {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub mutable: bool,
    pub name: String,
    pub typ: Ast,
}

#[derive(Clone, Debug, PartialEq, IsVariant, EnumAsInner)]
pub enum AstProcedureBody {
    ExternName(String),
    Scope(Rc<AstScope>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstScope {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub expressions: Vec<Ast>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstLet {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub name: String,
    pub typ: Option<Ast>,
    pub value: Ast,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstVar {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub name: String,
    pub typ: Option<Ast>,
    pub value: Ast,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstName {
    pub resolving: Cell<bool>,
    pub location: SourceSpan,
    pub name: String,
    pub resolved_declaration: RefCell<Option<Ast>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstInteger {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub value: u128,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstCall {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub operand: Ast,
    pub arguments: Vec<Ast>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstReturn {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub value: Option<Ast>,
}

#[derive(Clone, Debug, PartialEq, EnumAsInner)]
pub enum UnaryOperator {
    Identity,
    Negation,
    LogicalNot,
    PointerType,
    AddressOf,
    Dereference,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstUnary {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub operator: UnaryOperator,
    pub operand: Ast,
}

#[derive(Clone, Debug, PartialEq, EnumAsInner)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstBinary {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub left: Ast,
    pub operator: BinaryOperator,
    pub right: Ast,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstIf {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub condition: Ast,
    pub then_expression: Ast,
    pub else_expression: Option<Ast>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstWhile {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub condition: Ast,
    pub then_expression: Ast,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstCast {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub typ: Ast,
    pub operand: Ast,
}

#[derive(Clone, Debug, PartialEq, EnumAsInner)]
pub enum AstAssignDirection {
    Left,
    Right,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstAssign {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub location: SourceSpan,
    pub direction: AstAssignDirection,
    pub operand: Ast,
    pub value: Ast,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstBuiltin {
    pub resolving: Cell<bool>,
    pub resolved_type: ResolvedType,
    pub typ: RefCell<Option<Rc<Type>>>,
    pub kind: AstBuiltinKind,
}

#[derive(Clone, Debug, PartialEq, EnumAsInner)]
pub enum AstBuiltinKind {
    Type,
    Void,
    Bool,
    IntegerType { size: usize, signed: bool },
}
