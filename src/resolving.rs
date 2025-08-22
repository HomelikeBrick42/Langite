use crate::{ast, interning::InternedStr, lexing::SourceLocation, syntax_tree as st};
use derive_more::Display;
use rustc_hash::{FxHashMap, FxHashSet};
use slotmap::SlotMap;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[error("{location}: {kind}")]
pub struct ResolvingError {
    pub location: SourceLocation,
    pub kind: ResolvingErrorKind,
}

#[derive(Debug, Display, Clone)]
pub enum ResolvingErrorKind {
    #[display("Unknown name '{_0}'")]
    UnknownName(InternedStr),
    #[display("Cannot redeclare name '{_0}'")]
    NameRedeclaration(InternedStr),
    #[display("Cannot redeclare member '{_0}'")]
    MemberRedeclaration(InternedStr),
    #[display("Cannot use let pattern in an expression")]
    LetInExpression,
    #[display("Cannot use discard pattern in an expression")]
    DiscardInExpression,
    #[display("Expected a statement")]
    ExpectedStatement,
    #[display("Expected an expression")]
    ExpectedExpression,
    #[display("Expected a pattern")]
    ExpectedPattern,
}

pub struct ResolvingOutput {
    pub consts: SlotMap<ast::ConstId, ast::Const>,
    pub functions: SlotMap<ast::FunctionId, ast::Function>,
}

pub fn resolve_program(
    items: &[st::Item],
) -> Result<(ResolvingOutput, FxHashMap<InternedStr, ast::Name>), ResolvingError> {
    let mut output = ResolvingOutput {
        consts: SlotMap::with_key(),
        functions: SlotMap::with_key(),
    };
    let mut names = FxHashMap::<InternedStr, ast::Name>::default();

    names.insert("Type".into(), ast::Name::Builtin(ast::Builtin::Type));

    resolve_items(items.iter(), &mut output, &mut names)?;

    Ok((output, names))
}

fn resolve_items<'a>(
    items: impl Iterator<Item = &'a st::Item>,
    output: &mut ResolvingOutput,
    names: &mut FxHashMap<InternedStr, ast::Name>,
) -> Result<(), ResolvingError> {
    let mut consts = Vec::<(ast::ConstId, Option<&st::Expression>, &st::Expression)>::new();

    for item in items {
        match &item.kind {
            st::ItemKind::Const {
                const_token: _,
                name_token,
                name,
                colon_token: _,
                type_,
                equals_token: _,
                value,
            } => {
                if names.contains_key(name) {
                    return Err(ResolvingError {
                        location: name_token.location,
                        kind: ResolvingErrorKind::NameRedeclaration(*name),
                    });
                }

                let id = output.consts.insert(ast::Const {
                    location: name_token.location,
                    name: *name,
                    type_: None,
                    value: None,
                });

                names.insert(*name, ast::Name::Const(id));

                consts.push((id, type_.as_deref(), value));
            }
        }
    }

    for (const_id, type_, value) in consts {
        let mut names = names.clone();
        names.retain(|_, name| match name {
            ast::Name::Builtin(_) => true,
            ast::Name::Const(_) => true,
            ast::Name::ImplicitParameter { index: _ } => false,
            ast::Name::Parameter { index: _ } => false,
            ast::Name::Variable(_) => false,
        });

        if let Some(type_) = type_ {
            output.consts[const_id].type_ =
                Some(Box::new(resolve_type_annotation(type_, output, &names)?));
        }
        {
            let mut variables = SlotMap::with_key();
            let value = resolve_expression(value, output, &names, &mut variables)?;
            output.consts[const_id].value = Some(Box::new(ast::EvalContext {
                variables,
                expression: value,
            }));
        }
    }

    Ok(())
}

fn resolve_type_annotation(
    type_: &st::Expression,
    output: &mut ResolvingOutput,
    names: &FxHashMap<InternedStr, ast::Name>,
) -> Result<ast::EvalContext, ResolvingError> {
    let mut names = names.clone();
    names.retain(|_, name| match name {
        ast::Name::Builtin(_) => true,
        ast::Name::Const(_) => true,
        ast::Name::ImplicitParameter { index: _ } => true,
        ast::Name::Parameter { index: _ } => true,
        ast::Name::Variable(_) => false,
    });
    let mut variables = SlotMap::with_key();
    let type_ = resolve_expression(type_, output, &names, &mut variables)?;
    Ok(ast::EvalContext {
        variables,
        expression: type_,
    })
}

fn resolve_parameter(
    st::Parameter {
        const_token,
        name_token,
        name,
        colon_token: _,
        type_,
    }: &st::Parameter,
    output: &mut ResolvingOutput,
    names: &FxHashMap<InternedStr, ast::Name>,
) -> Result<ast::Parameter, ResolvingError> {
    Ok(ast::Parameter {
        location: name_token.location,
        const_: const_token.is_some(),
        name: *name,
        type_: Box::new(resolve_type_annotation(type_, output, names)?),
    })
}

fn resolve_member(
    st::Member {
        name_token,
        name,
        colon_token: _,
        type_,
    }: &st::Member,
    output: &mut ResolvingOutput,
    names: &FxHashMap<InternedStr, ast::Name>,
) -> Result<ast::Member, ResolvingError> {
    Ok(ast::Member {
        location: name_token.location,
        name: *name,
        type_: Box::new(resolve_type_annotation(type_, output, names)?),
    })
}

fn resolve_expression(
    expression: &st::Expression,
    output: &mut ResolvingOutput,
    names: &FxHashMap<InternedStr, ast::Name>,
    variables: &mut SlotMap<ast::VariableId, ast::Variable>,
) -> Result<ast::Expression, ResolvingError> {
    Ok(match &expression.kind {
        st::ExpressionKind::Struct {
            struct_token,
            open_brace_token: _,
            members,
            close_brace_token: _,
        } => {
            let mut names = names.clone();
            names.retain(|_, name| match name {
                ast::Name::Builtin(_) => true,
                ast::Name::Const(_) => true,
                ast::Name::ImplicitParameter { index: _ } => true,
                ast::Name::Parameter { index: _ } => true,
                ast::Name::Variable(_) => false,
            });

            let members = {
                let mut member_names = FxHashSet::default();
                members
                    .iter()
                    .map(|member| {
                        let member = resolve_member(member, output, &names)?;
                        if member_names.contains(&member.name) {
                            return Err(ResolvingError {
                                location: member.location,
                                kind: ResolvingErrorKind::MemberRedeclaration(member.name),
                            });
                        }
                        member_names.insert(member.name);
                        Ok(member)
                    })
                    .collect::<Result<Vec<_>, ResolvingError>>()?
            };

            ast::Expression {
                location: struct_token.location,
                kind: ast::ExpressionKind::Struct { members },
            }
        }

        st::ExpressionKind::Enum {
            enum_token,
            open_brace_token: _,
            members,
            close_brace_token: _,
        } => {
            let mut names = names.clone();
            names.retain(|_, name| match name {
                ast::Name::Builtin(_) => true,
                ast::Name::Const(_) => true,
                ast::Name::ImplicitParameter { index: _ } => true,
                ast::Name::Parameter { index: _ } => true,
                ast::Name::Variable(_) => false,
            });

            let members = {
                let mut member_names = FxHashSet::default();
                members
                    .iter()
                    .map(|member| {
                        let member = resolve_member(member, output, &names)?;
                        if member_names.contains(&member.name) {
                            return Err(ResolvingError {
                                location: member.location,
                                kind: ResolvingErrorKind::MemberRedeclaration(member.name),
                            });
                        }
                        member_names.insert(member.name);
                        Ok(member)
                    })
                    .collect::<Result<Vec<_>, ResolvingError>>()?
            };

            ast::Expression {
                location: enum_token.location,
                kind: ast::ExpressionKind::Enum { members },
            }
        }

        st::ExpressionKind::Function {
            fn_token,
            inferred_parameters,
            parameters,
            right_arrow_token: _,
            return_type,
            body,
        } => {
            let mut names = names.clone();

            match body {
                st::FunctionBody::Type => {}

                st::FunctionBody::Defintion(_) => {
                    names.retain(|_, name| match name {
                        ast::Name::Builtin(_) => true,
                        ast::Name::Const(_) => true,
                        ast::Name::ImplicitParameter { index: _ } => false,
                        ast::Name::Parameter { index: _ } => false,
                        ast::Name::Variable(_) => false,
                    });
                }
            }

            let inferred_parameters = if let Some(inferred_parameters) = inferred_parameters {
                inferred_parameters
                    .inferred_parameters
                    .iter()
                    .enumerate()
                    .map(|(index, inferred_parameter)| {
                        let inferred_parameter =
                            resolve_parameter(inferred_parameter, output, &names)?;
                        if names.contains_key(&inferred_parameter.name) {
                            return Err(ResolvingError {
                                location: inferred_parameter.location,
                                kind: ResolvingErrorKind::NameRedeclaration(
                                    inferred_parameter.name,
                                ),
                            });
                        }
                        names.insert(
                            inferred_parameter.name,
                            ast::Name::ImplicitParameter { index },
                        );
                        Ok(inferred_parameter)
                    })
                    .collect::<Result<Vec<_>, ResolvingError>>()?
            } else {
                vec![]
            };

            let parameters = parameters
                .parameters
                .iter()
                .enumerate()
                .map(|(index, parameter)| {
                    let parameter = resolve_parameter(parameter, output, &names)?;
                    if names.contains_key(&parameter.name) {
                        return Err(ResolvingError {
                            location: parameter.location,
                            kind: ResolvingErrorKind::NameRedeclaration(parameter.name),
                        });
                    }
                    names.insert(parameter.name, ast::Name::Parameter { index });
                    Ok(parameter)
                })
                .collect::<Result<Vec<_>, ResolvingError>>()?;

            let return_type = Box::new(resolve_type_annotation(return_type, output, &names)?);

            ast::Expression {
                location: fn_token.location,
                kind: match body {
                    st::FunctionBody::Type => ast::ExpressionKind::FunctionType {
                        inferred_parameters,
                        parameters,
                        return_type,
                    },

                    st::FunctionBody::Defintion(body) => {
                        let body = {
                            let mut variables = SlotMap::with_key();
                            let body = resolve_expression(body, output, &names, &mut variables)?;
                            Box::new(ast::EvalContext {
                                variables,
                                expression: body,
                            })
                        };

                        let id = output.functions.insert(ast::Function {
                            location: fn_token.location,
                            inferred_parameters,
                            parameters,
                            return_type,
                            body,
                        });

                        ast::ExpressionKind::Function(id)
                    }
                },
            }
        }

        st::ExpressionKind::ParenthesisedExpression {
            open_parenthesis_token: _,
            expression,
            close_parenthesis_token: _,
        } => resolve_expression(expression, output, names, variables)?,

        st::ExpressionKind::Name { name_token, name } => match names.get(name) {
            Some(name) => ast::Expression {
                location: name_token.location,
                kind: ast::ExpressionKind::Name(name.clone()),
            },
            None => {
                return Err(ResolvingError {
                    location: name_token.location,
                    kind: ResolvingErrorKind::UnknownName(*name),
                });
            }
        },

        st::ExpressionKind::Discard { .. } => {
            return Err(ResolvingError {
                location: expression.location,
                kind: ResolvingErrorKind::DiscardInExpression,
            });
        }

        st::ExpressionKind::Let { .. } => {
            return Err(ResolvingError {
                location: expression.location,
                kind: ResolvingErrorKind::LetInExpression,
            });
        }

        st::ExpressionKind::Block {
            open_brace_token,
            statements,
            close_brace_token: _,
        } => {
            let Some(final_statement) = statements.last() else {
                return Err(ResolvingError {
                    location: open_brace_token.location,
                    kind: ResolvingErrorKind::ExpectedExpression,
                });
            };

            let mut names = names.clone();

            resolve_items(
                statements.iter().flat_map(|statement| {
                    if let st::StatementKind::Item(item) = &statement.kind {
                        Some(&**item)
                    } else {
                        None
                    }
                }),
                output,
                &mut names,
            )?;

            let statements = statements[..statements.len() - 1]
                .iter()
                .flat_map(|statement| {
                    resolve_statement(statement, output, &mut names, variables).transpose()
                })
                .collect::<Result<Vec<_>, ResolvingError>>()?;

            let st::StatementKind::Expression(final_expression) = &final_statement.kind else {
                return Err(ResolvingError {
                    location: final_statement.location,
                    kind: ResolvingErrorKind::ExpectedExpression,
                });
            };

            let final_expression = Box::new(resolve_expression(
                final_expression,
                output,
                &names,
                variables,
            )?);

            ast::Expression {
                location: open_brace_token.location,
                kind: ast::ExpressionKind::Block {
                    statements,
                    final_expression,
                },
            }
        }

        st::ExpressionKind::Match {
            match_token,
            condition,
            open_brace_token: _,
            arms,
            close_brace_token: _,
        } => {
            let condition = Box::new(resolve_expression(condition, output, names, variables)?);
            let arms = arms
                .iter()
                .map(|arm| {
                    let mut names = names.clone();
                    let pattern = resolve_pattern(&arm.pattern, output, &mut names, variables)?;
                    let value = resolve_expression(&arm.value, output, &names, variables)?;
                    Ok(ast::MatchArmExpression { pattern, value })
                })
                .collect::<Result<Vec<_>, ResolvingError>>()?;
            ast::Expression {
                location: match_token.location,
                kind: ast::ExpressionKind::Match { condition, arms },
            }
        }

        st::ExpressionKind::Call {
            operand,
            open_parenthesis_token,
            arguments,
            close_parenthesis_token: _,
        } => {
            let operand = Box::new(resolve_expression(operand, output, names, variables)?);
            let arguments = arguments
                .iter()
                .map(|argument| resolve_expression(argument, output, names, variables))
                .collect::<Result<Vec<_>, ResolvingError>>()?;
            ast::Expression {
                location: open_parenthesis_token.location,
                kind: ast::ExpressionKind::Call { operand, arguments },
            }
        }

        st::ExpressionKind::Distinct {
            distinct_token,
            keys,
            expression,
        } => {
            let keys = if let Some(keys) = keys {
                keys.keys
                    .iter()
                    .map(|key| resolve_type_annotation(key, output, names))
                    .collect::<Result<Vec<_>, ResolvingError>>()?
            } else {
                vec![]
            };
            let expression = Box::new(resolve_type_annotation(expression, output, names)?);
            ast::Expression {
                location: distinct_token.location,
                kind: ast::ExpressionKind::Distinct { keys, expression },
            }
        }

        st::ExpressionKind::TypeOf {
            type_of_token,
            open_parenthesis_token: _,
            expression,
            close_parenthesis_token: _,
        } => {
            let expression = Box::new(resolve_type_annotation(expression, output, names)?);
            ast::Expression {
                location: type_of_token.location,
                kind: ast::ExpressionKind::TypeOf { expression },
            }
        }
    })
}

fn resolve_statement(
    statement: &st::Statement,
    output: &mut ResolvingOutput,
    names: &mut FxHashMap<InternedStr, ast::Name>,
    variables: &mut SlotMap<ast::VariableId, ast::Variable>,
) -> Result<Option<ast::Statement>, ResolvingError> {
    Ok(match &statement.kind {
        st::StatementKind::Item(_item) => None,

        st::StatementKind::Expression(expression) => {
            let statement = resolve_statement_expression(expression, output, names, variables)?;
            Some(statement)
        }

        st::StatementKind::Assignment {
            pattern,
            equals_token,
            value,
        } => {
            let value = Box::new(resolve_expression(value, output, names, variables)?);
            let pattern = Box::new(resolve_pattern(pattern, output, names, variables)?);
            Some(ast::Statement {
                location: equals_token.location,
                kind: ast::StatementKind::Assignment { pattern, value },
            })
        }
    })
}

fn resolve_statement_expression(
    statement: &st::Expression,
    output: &mut ResolvingOutput,
    names: &mut FxHashMap<InternedStr, ast::Name>,
    variables: &mut SlotMap<ast::VariableId, ast::Variable>,
) -> Result<ast::Statement, ResolvingError> {
    Ok(match &statement.kind {
        st::ExpressionKind::Block {
            open_brace_token,
            statements,
            close_brace_token: _,
        } => {
            let mut names = names.clone();

            resolve_items(
                statements.iter().flat_map(|statement| {
                    if let st::StatementKind::Item(item) = &statement.kind {
                        Some(&**item)
                    } else {
                        None
                    }
                }),
                output,
                &mut names,
            )?;

            let statements = statements
                .iter()
                .flat_map(|statement| {
                    resolve_statement(statement, output, &mut names, variables).transpose()
                })
                .collect::<Result<Vec<_>, ResolvingError>>()?;

            ast::Statement {
                location: open_brace_token.location,
                kind: ast::StatementKind::Block { statements },
            }
        }

        st::ExpressionKind::Match {
            match_token,
            condition,
            open_brace_token: _,
            arms,
            close_brace_token: _,
        } => {
            let condition = Box::new(resolve_expression(condition, output, names, variables)?);
            let arms = arms
                .iter()
                .map(|arm| {
                    let mut names = names.clone();
                    let pattern = resolve_pattern(&arm.pattern, output, &mut names, variables)?;
                    let statement =
                        resolve_statement_expression(&arm.value, output, &mut names, variables)?;
                    Ok(ast::MatchArmStatement { pattern, statement })
                })
                .collect::<Result<Vec<_>, ResolvingError>>()?;
            ast::Statement {
                location: match_token.location,
                kind: ast::StatementKind::Match { condition, arms },
            }
        }

        _ => {
            return Err(ResolvingError {
                location: statement.location,
                kind: ResolvingErrorKind::ExpectedStatement,
            });
        }
    })
}

fn resolve_pattern(
    expression: &st::Expression,
    output: &mut ResolvingOutput,
    names: &mut FxHashMap<InternedStr, ast::Name>,
    variables: &mut SlotMap<ast::VariableId, ast::Variable>,
) -> Result<ast::Pattern, ResolvingError> {
    Ok(match &expression.kind {
        st::ExpressionKind::ParenthesisedExpression {
            open_parenthesis_token: _,
            expression,
            close_parenthesis_token: _,
        } => resolve_pattern(expression, output, names, variables)?,

        st::ExpressionKind::Name { name_token, name } => match names.get(name) {
            Some(name) => ast::Pattern {
                location: name_token.location,
                kind: ast::PatternKind::Name(name.clone()),
            },
            None => {
                return Err(ResolvingError {
                    location: name_token.location,
                    kind: ResolvingErrorKind::UnknownName(*name),
                });
            }
        },

        st::ExpressionKind::Discard { discard_token } => ast::Pattern {
            location: discard_token.location,
            kind: ast::PatternKind::Discard,
        },

        st::ExpressionKind::Let {
            let_token: _,
            name_token,
            name,
            colon_token: _,
            type_,
        } => {
            let type_ = type_
                .as_ref()
                .map(|type_| Ok(Box::new(resolve_type_annotation(type_, output, names)?)))
                .transpose()?;
            let variable_id = variables.insert(ast::Variable {
                location: name_token.location,
                name: *name,
                type_,
            });
            names.insert(*name, ast::Name::Variable(variable_id));
            ast::Pattern {
                location: name_token.location,
                kind: ast::PatternKind::Let(variable_id),
            }
        }

        _ => {
            return Err(ResolvingError {
                location: expression.location,
                kind: ResolvingErrorKind::ExpectedPattern,
            });
        }
    })
}
