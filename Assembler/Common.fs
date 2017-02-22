namespace Assembler

module Common =
    type Token =
        | TokName
        | TokConst
        | TokComma
        | TokNewLine