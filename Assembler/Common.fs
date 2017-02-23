namespace Assembler

module Common =

    type Token =
        | TokIdentifier of string   //includes MOV, ADD, etc. and labels
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