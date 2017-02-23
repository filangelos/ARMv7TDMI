namespace Assembler

module Tokeniser =
    open Common
    open System.Text.RegularExpressions

    
    //return the first matched group of a regex and the leftovers of input
    let (|MatchToken|_|) pattern input =
        let m = Regex.Match(input,pattern) 
        if (m.Success)
        then Some (m.Groups.[1].Value, (new Regex(pattern)).Replace(input, "", 1))
        else None

    let tokenise (input:string) =

        //breaks down a string into a list of tokens and appends them onto lst
        let rec strToToken (lst:Token list) (str:string) =
            match str with
            | MatchToken "[rR]([0-9][0-6]?)(?![^,])" (reg, newStr) ->
                strToToken (lst @ [TokReg(reg |> int)]) newStr
            | MatchToken "#0[xX]([0-9A-F]+(?![^0-9A-F,]))" (hexVal, newStr) ->
                //printfn "hex: %A" hexVal
                let intVal = System.Convert.ToInt32(hexVal, 16)
                strToToken (lst @ [TokConst intVal]) newStr
            | MatchToken "#0[bB]([01]+(?![^01,]))" (binVal, newStr) ->
                //printfn "bin: %A" binVal
                let intVal = System.Convert.ToInt32(binVal, 2)
                strToToken (lst @ [TokConst intVal]) newStr
            | MatchToken "([A-Za-z][A-Za-z0-9_]*)" (name, newStr) ->
                strToToken (lst @ [TokIdentifier name]) newStr
            | MatchToken "#([0-9]+)" (value, newStr) ->
                printfn "int: %A" value
                strToToken (lst @ [TokConst(value |> int)]) newStr
            | MatchToken "," (_, newStr) ->
                strToToken (lst @ [TokComma]) newStr
            | MatchToken "!" (_, newStr) ->
                strToToken (lst @ [TokExclam]) newStr
            | MatchToken "\[" (_, newStr) ->
                strToToken (lst @ [TokSquareLeft]) newStr
            | MatchToken "\]" (_, newStr) ->
                strToToken (lst @ [TokSquareRight]) newStr
            | MatchToken "\{" (_, newStr) ->
                strToToken (lst @ [TokCurlyLeft]) newStr
            | MatchToken "\}" (_, newStr) ->
                strToToken (lst @ [TokCurlyRight]) newStr
            | MatchToken "\n" (_,newStr) ->
                strToToken (lst @ [TokNewLine]) newStr
            | "" -> lst
            | _ -> failwithf "Unidentified character(s) in %A" str

        let strList = input.Split([|' '; '\t'|])

        Array.fold strToToken [] strList

    let tokeniseTest =
        printfn "%A" (tokenise "MOV r1, r2 \n LABEL \n ADD r1, r14, #0XF000")