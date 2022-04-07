﻿namespace Langite.Syntax
{
    public enum TokenKind
    {
        EndOfFile,
        Newline,
        Name,
        Integer,
        Float,
        String,
        Wildcard,
        Const,
        Func,
        Proc,
        Return,
        If,
        Else,
        Builtin,
        OpenParenthesis,
        CloseParenthesis,
        OpenBrace,
        CloseBrace,
        OpenSquareBracket,
        CloseSquareBracket,
        Colon,
        Comma,
        Period,
        At,
        Plus,
        Minus,
        Asterisk,
        Slash,
        Percent,
        LessThan,
        LessThanEqual,
        GreaterThan,
        GreaterThanEqual,
        Equal,
        EqualEqual,
        ExclamationMark,
        ExclamationMarkEqual,
        LeftArrow,
        RightArrow,
        BuiltinArray,
    }
}