namespace Assembler

module Tokeniser =
    open Common
    open System.Text.RegularExpressions

    
    //returns the first matched group of a regex and the leftovers from the input
    let (|MatchToken|_|) pattern input =
        let m = Regex.Match(input,pattern) 
        if (m.Success)
        then Some (m.Groups.[1].Value, (new Regex(pattern)).Replace(input, "", 1))
        else None

    //returns a list of tokens from a string input
    let tokenise (input:string) =

        //breaks down a string into a list of tokens and appends them onto lst
        let rec strToToken (lst:Token list) (str:string) =
            match str with
            | MatchToken "[rR]([0-9][0-6]?)(?![^,])" (reg, leftovers) ->
                strToToken (lst @ [TokReg(reg |> int)]) leftovers
            | MatchToken "#0[xX]([0-9A-Fa-f]+(?![^0-9A-Fa-f,]))" (hexVal, leftovers) ->
                //printfn "hex: %A" hexVal
                let intVal = System.Convert.ToInt32(hexVal, 16)
                strToToken (lst @ [TokConst intVal]) leftovers
            | MatchToken "#0[bB]([01]+(?![^01,]))" (binVal, leftovers) ->
                //printfn "bin: %A" binVal
                let intVal = System.Convert.ToInt32(binVal, 2)
                strToToken (lst @ [TokConst intVal]) leftovers
            | MatchToken "#([0-9]+)(?![^0-9,])" (value, leftovers) ->
                //printfn "dec: %A" value
                strToToken (lst @ [TokConst(value |> int)]) leftovers
            | MatchToken "((?<![0-9]+)[A-Za-z][A-Za-z0-9_]*)" (name, leftovers) ->
                strToToken (lst @ [TokIdentifier name]) leftovers
            | MatchToken "," (_, leftovers) ->
                strToToken (lst @ [TokComma]) leftovers
            | MatchToken "!" (_, leftovers) ->
                strToToken (lst @ [TokExclam]) leftovers
            | MatchToken "\[" (_, leftovers) ->
                strToToken (lst @ [TokSquareLeft]) leftovers
            | MatchToken "\]" (_, leftovers) ->
                strToToken (lst @ [TokSquareRight]) leftovers
            | MatchToken "\{" (_, leftovers) ->
                strToToken (lst @ [TokCurlyLeft]) leftovers
            | MatchToken "\}" (_, leftovers) ->
                strToToken (lst @ [TokCurlyRight]) leftovers
            | MatchToken "\n" (_,leftovers) ->
                strToToken (lst @ [TokNewLine]) leftovers
            | "" -> lst
            | _ -> failwithf "Unidentified character(s) in %A" str

        let strList = input.Split([|' '; '\t'|])

        Array.fold strToToken [] strList

    let tokeniseTest =
        printfn "%A" (tokenise "MOV R1, r2 \n LABEL \n ADD r1, r14, #0xF")