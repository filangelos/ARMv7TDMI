namespace Assembler

module Tokeniser =
    open Common
    open System.Text.RegularExpressions

    let (|MatchToken|_|) pattern input =
        let m = Regex.Match(input,pattern) 
        if (m.Success) then Some m.Groups.[1].Value else None

    let tokenise (input:string) =
        let matchToken (lst:Token list) (str:string) =
            match str with
            | MatchToken "[rR]([0-9][0-6]?)" reg ->
                lst @ [TokReg(reg |> int)]
            | MatchToken "#0[xX]([0-9A-F]+)" hexVal ->
                printfn "hex: %A" hexVal
                let intVal = System.Convert.ToInt32(hexVal, 16)
                lst @ [TokConst(intVal)]
            | MatchToken "#0[bB]([01]+)(?!.)" binVal ->
                printfn "bin: %A" binVal
                let intVal = System.Convert.ToInt32(binVal, 2)
                lst @ [TokConst(intVal)]
            | MatchToken "([A-Za-z]+)" name ->
                lst @ [TokIdentifier(name)]
            | MatchToken "#([0-9]+)" value ->
                printfn "int: %A" value
                lst @ [TokConst(value |> int)]
            | MatchToken "," _ ->
                lst @ [TokComma]
            | MatchToken "!" _ ->
                lst @ [TokExclam]
            | MatchToken "\[" _ ->
                lst @ [TokSquareLeft]
            | MatchToken "\]" _ ->
                lst @ [TokSquareRight]
            | MatchToken "\{" _ ->
                lst @ [TokCurlyLeft]
            | MatchToken "\}" _ ->
                lst @ [TokCurlyRight]
            | MatchToken "\n" _ ->
                lst @ [TokNewLine]
            | _ -> failwithf "Unidentified character(s) in %A" str

        let strList = input.Split([|' '; '\t'|])

        Array.fold matchToken [] strList

    let tokeniseTest =
        printfn "%A" (tokenise "MOV r1, r2 \n ADD r1, r1, #0b101010")