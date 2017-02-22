namespace Assembler

module Common =

    type Token =
        | TokName of string
        | TokConst of int
        | TokComma
        | TokNewLine
        | TokNull