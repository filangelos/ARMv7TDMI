namespace Assembler

module Common =

    type Token =
        | TokInstruction of string
        | TokReg of int
        | TokLabel of string
        | TokConst of int
        | TokComma
        | TokExclam
        | TokSquareLeft
        | TokSquareRight
        | TokCurlyLeft
        | TokCurlyRight
        | TokNewLine
        | TokNull