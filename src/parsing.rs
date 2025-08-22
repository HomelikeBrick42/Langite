use crate::{
    interning::InternedStr,
    lexing::{Lexer, LexerErrorKind, LexingError, SourceLocation, Token, TokenKind},
    syntax_tree::{
        DistinctKeys, Expression, ExpressionKind, FunctionBody, InferredParameters, Item, ItemKind,
        MatchArm, Member, Parameter, Parameters, Statement, StatementKind,
    },
};
use derive_more::Display;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[error("{location}: {kind}")]
pub struct ParsingError {
    pub location: SourceLocation,
    pub kind: ParsingErrorKind,
}

#[derive(Debug, Display, Clone)]
pub enum ParsingErrorKind {
    #[display("{_0}")]
    LexingError(LexerErrorKind),
    #[display("Expected {name} but found {token}")]
    UnexpectedToken { name: &'static str, token: Token },
}

impl From<LexingError> for ParsingError {
    fn from(LexingError { location, kind }: LexingError) -> Self {
        Self {
            location,
            kind: ParsingErrorKind::LexingError(kind),
        }
    }
}

macro_rules! expect_token {
    ($lexer:ident, $name:literal, $kind:pat $(, $names:expr)*) => {
        match $lexer.next_token() {
            Ok(token @ Token { kind: $kind, .. }) => Ok((token $(, $names)*)),
            Ok(token) => Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::UnexpectedToken {
                    name: $name,
                    token,
                },
            }),
            Err(error) => Err(error.into()),
        }
    };
}

macro_rules! eat_token {
    ($lexer:expr, $kind:pat $(, $names:expr)*) => {{
        let mut peeked_lexer: Lexer<'_> = $lexer.clone();
        match peeked_lexer.next_token() {
            Ok(token @ Token { kind: $kind, .. }) => {
                *$lexer = peeked_lexer;
                Some((token $(, $names)*))
            }
            _ => None,
        }
    }};
}

pub fn parse_file(filepath: InternedStr, source: &str) -> Result<Vec<Item>, ParsingError> {
    let lexer = &mut Lexer::new(filepath, source);

    let mut items = vec![];
    while !lexer.is_empty() {
        while eat_token!(lexer, TokenKind::Newline).is_some() {}
        if lexer.is_empty() {
            break;
        }
        items.push(parse_item(lexer)?);
        expect_token!(lexer, "newline", TokenKind::Newline)?;
    }
    Ok(items)
}

pub fn parse_item(lexer: &mut Lexer<'_>) -> Result<Item, ParsingError> {
    Ok(match lexer.next_token()? {
        const_token @ Token {
            location,
            kind: TokenKind::ConstKeyword,
        } => {
            let (name_token, name) = expect_token!(lexer, "name", TokenKind::Name(name), name)?;
            let (colon_token, type_) =
                if let Some(colon_token) = eat_token!(lexer, TokenKind::Colon) {
                    eat_token!(lexer, TokenKind::Newline);
                    let type_ = Box::new(parse_expression(lexer)?);
                    (Some(colon_token), Some(type_))
                } else {
                    (None, None)
                };
            let equals_token = expect_token!(lexer, "=", TokenKind::Equals)?;
            eat_token!(lexer, TokenKind::Newline);
            let value = Box::new(parse_expression(lexer)?);
            Item {
                location,
                kind: ItemKind::Const {
                    const_token,
                    name_token,
                    name,
                    colon_token,
                    type_,
                    equals_token,
                    value,
                },
            }
        }

        token => {
            return Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::UnexpectedToken {
                    name: "item",
                    token,
                },
            });
        }
    })
}

pub fn parse_parameter(lexer: &mut Lexer<'_>) -> Result<Parameter, ParsingError> {
    let const_token = eat_token!(lexer, TokenKind::ConstKeyword);
    let (name_token, name) = expect_token!(lexer, "parameter name", TokenKind::Name(name), name)?;
    let colon_token = expect_token!(lexer, ":", TokenKind::Colon)?;
    eat_token!(lexer, TokenKind::Newline);
    let type_ = parse_expression(lexer)?;
    Ok(Parameter {
        const_token,
        name_token,
        name,
        colon_token,
        type_,
    })
}

pub fn parse_parameters(
    lexer: &mut Lexer<'_>,
    open_parenthesis_token: Token,
) -> Result<Parameters, ParsingError> {
    let mut parameters = vec![];
    loop {
        while eat_token!(lexer, TokenKind::Newline).is_some() {}
        if matches!(
            lexer.peek_token(),
            Ok(Token {
                kind: TokenKind::CloseParenthesis,
                ..
            })
        ) {
            break;
        }
        parameters.push(parse_parameter(lexer)?);
        if matches!(
            lexer.peek_token(),
            Ok(Token {
                kind: TokenKind::CloseParenthesis,
                ..
            })
        ) {
            break;
        }
        expect_token!(lexer, ",", TokenKind::Comma)?;
    }
    let close_parenthesis_token = expect_token!(lexer, ")", TokenKind::CloseParenthesis)?;
    Ok(Parameters {
        open_parenthesis_token,
        parameters,
        close_parenthesis_token,
    })
}

pub fn parse_inferred_parameters(
    lexer: &mut Lexer<'_>,
    open_bracket_token: Token,
) -> Result<InferredParameters, ParsingError> {
    let mut inferred_parameters = vec![];
    loop {
        while eat_token!(lexer, TokenKind::Newline).is_some() {}
        if matches!(
            lexer.peek_token(),
            Ok(Token {
                kind: TokenKind::CloseBracket,
                ..
            })
        ) {
            break;
        }
        inferred_parameters.push(parse_parameter(lexer)?);
        if matches!(
            lexer.peek_token(),
            Ok(Token {
                kind: TokenKind::CloseBracket,
                ..
            })
        ) {
            break;
        }
        expect_token!(lexer, ",", TokenKind::Comma)?;
    }
    let close_bracket_token = expect_token!(lexer, "]", TokenKind::CloseBracket)?;
    Ok(InferredParameters {
        open_bracket_token,
        inferred_parameters,
        close_bracket_token,
    })
}

pub fn parse_member(lexer: &mut Lexer<'_>) -> Result<Member, ParsingError> {
    let (name_token, name) = expect_token!(lexer, "member name", TokenKind::Name(name), name)?;
    let colon_token = expect_token!(lexer, ":", TokenKind::Colon)?;
    eat_token!(lexer, TokenKind::Newline);
    let type_ = parse_expression(lexer)?;
    Ok(Member {
        name_token,
        name,
        colon_token,
        type_,
    })
}

pub fn parse_members(lexer: &mut Lexer<'_>) -> Result<(Token, Vec<Member>, Token), ParsingError> {
    let open_brace_token = expect_token!(lexer, "{", TokenKind::OpenBrace)?;
    let mut members = vec![];
    loop {
        while eat_token!(lexer, TokenKind::Newline).is_some() {}
        if matches!(
            lexer.peek_token(),
            Ok(Token {
                kind: TokenKind::CloseBrace,
                ..
            })
        ) {
            break;
        }
        members.push(parse_member(lexer)?);
        if matches!(
            lexer.peek_token(),
            Ok(Token {
                kind: TokenKind::CloseBrace,
                ..
            })
        ) {
            break;
        }
        expect_token!(lexer, ",", TokenKind::Comma)?;
    }
    let close_brace_token = expect_token!(lexer, "}", TokenKind::CloseBrace)?;
    Ok((open_brace_token, members, close_brace_token))
}

pub fn parse_expression(lexer: &mut Lexer<'_>) -> Result<Expression, ParsingError> {
    parse_binary_expression(lexer)
}

pub fn parse_primary_expression(lexer: &mut Lexer<'_>) -> Result<Expression, ParsingError> {
    Ok(match lexer.next_token()? {
        struct_token @ Token {
            location,
            kind: TokenKind::StructKeyword,
        } => {
            let (open_brace_token, members, close_brace_token) = parse_members(lexer)?;

            Expression {
                location,
                kind: ExpressionKind::Struct {
                    struct_token,
                    open_brace_token,
                    members,
                    close_brace_token,
                },
            }
        }

        enum_token @ Token {
            location,
            kind: TokenKind::EnumKeyword,
        } => {
            let (open_brace_token, members, close_brace_token) = parse_members(lexer)?;

            Expression {
                location,
                kind: ExpressionKind::Enum {
                    enum_token,
                    open_brace_token,
                    members,
                    close_brace_token,
                },
            }
        }

        fn_token @ Token {
            location,
            kind: TokenKind::FnKeyword,
        } => {
            let inferred_parameters =
                if let Some(open_bracket_token) = eat_token!(lexer, TokenKind::OpenBracket) {
                    Some(Box::new(parse_inferred_parameters(
                        lexer,
                        open_bracket_token,
                    )?))
                } else {
                    None
                };

            let open_parenthesis_token = expect_token!(lexer, "(", TokenKind::OpenParenthesis)?;
            let parameters = Box::new(parse_parameters(lexer, open_parenthesis_token)?);

            eat_token!(lexer, TokenKind::Newline);

            let (right_arrow_token, return_type, body) = if let Some(right_arrow_token) =
                eat_token!(lexer, TokenKind::FatRightArrow)
            {
                eat_token!(lexer, TokenKind::Newline);
                let expression = Box::new(parse_expression(lexer)?);
                (
                    right_arrow_token,
                    None,
                    FunctionBody::NakedDefintion(expression),
                )
            } else {
                let right_arrow_token = expect_token!(lexer, "->", TokenKind::RightArrow)?;
                eat_token!(lexer, TokenKind::Newline);
                let return_type = Box::new(parse_expression(lexer)?);

                let body = if let Some(open_brace_token) = eat_token!(lexer, TokenKind::OpenBrace) {
                    FunctionBody::Defintion(Box::new(parse_block(lexer, open_brace_token)?))
                } else {
                    FunctionBody::Type
                };

                (right_arrow_token, Some(return_type), body)
            };

            Expression {
                location,
                kind: ExpressionKind::Function {
                    fn_token,
                    inferred_parameters,
                    parameters,
                    right_arrow_token,
                    return_type,
                    body,
                },
            }
        }

        open_parenthesis_token @ Token {
            location,
            kind: TokenKind::OpenParenthesis,
        } => {
            eat_token!(lexer, TokenKind::Newline);
            let expression = Box::new(parse_expression(lexer)?);
            eat_token!(lexer, TokenKind::Newline);
            let close_parenthesis_token = expect_token!(lexer, ")", TokenKind::CloseParenthesis)?;
            Expression {
                location,
                kind: ExpressionKind::ParenthesisedExpression {
                    open_parenthesis_token,
                    expression,
                    close_parenthesis_token,
                },
            }
        }

        open_brace_token @ Token {
            location: _,
            kind: TokenKind::OpenBrace,
        } => parse_block(lexer, open_brace_token)?,

        name_token @ Token {
            location,
            kind: TokenKind::Name(name),
        } => Expression {
            location,
            kind: ExpressionKind::Name { name_token, name },
        },

        discard_token @ Token {
            location,
            kind: TokenKind::Discard,
        } => Expression {
            location,
            kind: ExpressionKind::Discard { discard_token },
        },

        let_token @ Token {
            location,
            kind: TokenKind::LetKeyword,
        } => {
            let (name_token, name) = expect_token!(lexer, "name", TokenKind::Name(name), name)?;
            let (colon_token, type_) =
                if let Some(colon_token) = eat_token!(lexer, TokenKind::Colon) {
                    eat_token!(lexer, TokenKind::Newline);
                    let type_ = Box::new(parse_expression(lexer)?);
                    (Some(colon_token), Some(type_))
                } else {
                    (None, None)
                };
            Expression {
                location,
                kind: ExpressionKind::Let {
                    let_token,
                    name_token,
                    name,
                    colon_token,
                    type_,
                },
            }
        }

        match_token @ Token {
            location,
            kind: TokenKind::MatchKeyword,
        } => {
            let condition = Box::new(parse_expression(lexer)?);
            let open_brace_token = expect_token!(lexer, "}", TokenKind::OpenBrace)?;
            let mut arms = vec![];
            loop {
                while eat_token!(lexer, TokenKind::Newline).is_some() {}
                if matches!(
                    lexer.peek_token(),
                    Ok(Token {
                        kind: TokenKind::CloseBrace,
                        ..
                    })
                ) {
                    break;
                }
                arms.push(parse_match_arm(lexer)?);
                if matches!(
                    lexer.peek_token(),
                    Ok(Token {
                        kind: TokenKind::CloseBrace,
                        ..
                    })
                ) {
                    break;
                }
                expect_token!(lexer, ",", TokenKind::Comma)?;
            }
            let close_brace_token = expect_token!(lexer, "}", TokenKind::CloseBrace)?;
            Expression {
                location,
                kind: ExpressionKind::Match {
                    match_token,
                    condition,
                    open_brace_token,
                    arms,
                    close_brace_token,
                },
            }
        }

        distinct_token @ Token {
            location,
            kind: TokenKind::DistinctKeyword,
        } => {
            let keys = if let Some(open_parenthesis_token) =
                eat_token!(lexer, TokenKind::OpenParenthesis)
            {
                let mut keys = vec![];
                loop {
                    while eat_token!(lexer, TokenKind::Newline).is_some() {}
                    if matches!(
                        lexer.peek_token(),
                        Ok(Token {
                            kind: TokenKind::CloseParenthesis,
                            ..
                        })
                    ) {
                        break;
                    }
                    keys.push(parse_expression(lexer)?);
                    if matches!(
                        lexer.peek_token(),
                        Ok(Token {
                            kind: TokenKind::CloseParenthesis,
                            ..
                        })
                    ) {
                        break;
                    }
                    expect_token!(lexer, ",", TokenKind::Comma)?;
                }
                let close_parenthesis_token =
                    expect_token!(lexer, ")", TokenKind::CloseParenthesis)?;
                Some(DistinctKeys {
                    open_parenthesis_token,
                    keys,
                    close_parenthesis_token,
                })
            } else {
                None
            };

            let expression = Box::new(parse_expression(lexer)?);

            Expression {
                location,
                kind: ExpressionKind::Distinct {
                    distinct_token,
                    keys,
                    expression,
                },
            }
        }

        type_of_token @ Token {
            location,
            kind: TokenKind::TypeOfKeyword,
        } => {
            let open_parenthesis_token = expect_token!(lexer, "(", TokenKind::OpenParenthesis)?;
            let expression = Box::new(parse_expression(lexer)?);
            let close_parenthesis_token = expect_token!(lexer, ")", TokenKind::CloseParenthesis)?;
            Expression {
                location,
                kind: ExpressionKind::TypeOf {
                    type_of_token,
                    open_parenthesis_token,
                    expression,
                    close_parenthesis_token,
                },
            }
        }

        token => {
            return Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::UnexpectedToken {
                    name: "expression",
                    token,
                },
            });
        }
    })
}

pub fn parse_binary_expression(lexer: &mut Lexer<'_>) -> Result<Expression, ParsingError> {
    let mut left = parse_primary_expression(lexer)?;

    #[expect(clippy::while_let_loop)]
    loop {
        if let Some(open_parenthesis_token) = eat_token!(lexer, TokenKind::OpenParenthesis) {
            let mut arguments = vec![];
            loop {
                while eat_token!(lexer, TokenKind::Newline).is_some() {}
                if matches!(
                    lexer.peek_token(),
                    Ok(Token {
                        kind: TokenKind::CloseParenthesis,
                        ..
                    })
                ) {
                    break;
                }
                arguments.push(parse_expression(lexer)?);
                if matches!(
                    lexer.peek_token(),
                    Ok(Token {
                        kind: TokenKind::CloseParenthesis,
                        ..
                    })
                ) {
                    break;
                }
                expect_token!(lexer, ",", TokenKind::Comma)?;
            }
            let close_parenthesis_token = expect_token!(lexer, ")", TokenKind::CloseParenthesis)?;
            left = Expression {
                location: open_parenthesis_token.location,
                kind: ExpressionKind::Call {
                    operand: Box::new(left),
                    open_parenthesis_token,
                    arguments,
                    close_parenthesis_token,
                },
            };
        } else {
            break;
        }
    }

    Ok(left)
}

pub fn parse_match_arm(lexer: &mut Lexer<'_>) -> Result<MatchArm, ParsingError> {
    let pattern = parse_expression(lexer)?;
    eat_token!(lexer, TokenKind::Newline);
    let fat_right_arrow_token = expect_token!(lexer, "=>", TokenKind::FatRightArrow)?;
    eat_token!(lexer, TokenKind::Newline);
    let value = parse_expression(lexer)?;
    Ok(MatchArm {
        pattern,
        fat_right_arrow_token,
        value,
    })
}

pub fn parse_block(
    lexer: &mut Lexer<'_>,
    open_brace_token: Token,
) -> Result<Expression, ParsingError> {
    let mut statements = vec![];
    loop {
        while eat_token!(lexer, TokenKind::Newline).is_some() {}
        if matches!(
            lexer.peek_token(),
            Ok(Token {
                kind: TokenKind::CloseBrace,
                ..
            })
        ) {
            break;
        }
        statements.push(parse_statement(lexer)?);
        if matches!(
            lexer.peek_token(),
            Ok(Token {
                kind: TokenKind::CloseBrace,
                ..
            })
        ) {
            break;
        }
        expect_token!(lexer, "newline", TokenKind::Newline)?;
    }
    let close_brace_token = expect_token!(lexer, "}", TokenKind::CloseBrace)?;
    Ok(Expression {
        location: open_brace_token.location,
        kind: ExpressionKind::Block {
            open_brace_token,
            statements,
            close_brace_token,
        },
    })
}

pub fn parse_statement(lexer: &mut Lexer<'_>) -> Result<Statement, ParsingError> {
    Ok(match lexer.peek_token()?.kind {
        TokenKind::ConstKeyword => {
            let item = Box::new(parse_item(lexer)?);
            Statement {
                location: item.location,
                kind: StatementKind::Item(item),
            }
        }

        _ => {
            let expression = Box::new(parse_expression(lexer)?);
            if let Some(equals_token) = eat_token!(lexer, TokenKind::Equals) {
                eat_token!(lexer, TokenKind::Newline);
                let value = Box::new(parse_expression(lexer)?);
                Statement {
                    location: equals_token.location,
                    kind: StatementKind::Assignment {
                        pattern: expression,
                        equals_token,
                        value,
                    },
                }
            } else {
                Statement {
                    location: expression.location,
                    kind: StatementKind::Expression(expression),
                }
            }
        }
    })
}
