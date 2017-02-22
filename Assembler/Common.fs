namespace Assembler

module Common =

    type Token =
        | TokIdentifier of string
        | TokReg of int
        | TokConst of int
        | TokComma
        | TokExclam
        | TokSquareLeft
        | TokSquareRight
        | TokCurlyLeft
        | TokCurlyRight
        | TokNewLine
        | TokNull