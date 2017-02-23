namespace Assembler

module Tokeniser =
    open Common
    open System.Text.RegularExpressions
    open FsCheck
    
    //NB: handle exceptions from tokenise() externally for web GUI (e.g. in main)

    ///returns the first matched group of a regex and the leftovers from the input
    let (|MatchToken|_|) pattern input =
        let m = Regex.Match(input, "(?<![\s\S]+)" + pattern) //pattern must start at beginning of string
        if (m.Success)
        then Some (m.Groups.[1].Value, (new Regex(pattern)).Replace(input, "", 1))
        else None

    ///returns a list of tokens from a string input
    let tokenise (input:string) =

        ///breaks down a string into a list of tokens and appends them onto lst
        let rec strToToken (lst:Token list) (str:string) =
            match str with
            //str may contain several tokens, so recursively call MatchToken until str is empty 
            | MatchToken "[rR]([0-9]|1[0-6])(?![^,])" (reg, leftovers) ->                           //register
                strToToken (lst @ [TokReg(reg |> int)]) leftovers
            | MatchToken "#(0[xX][0-9A-Fa-f]+(?![^0-9A-Fa-f,\[\]\{\}\!]))" (hexVal, leftovers) ->   //hex const
                //printfn "hex: %A" hexVal
                strToToken (lst @ [TokConst (int hexVal)]) leftovers
            | MatchToken "#(0[bB][01]+(?![^01,\[\]\{\}\!]))" (binVal, leftovers) ->                 //bin const
                //printfn "bin: %A" binVal
                strToToken (lst @ [TokConst (int binVal)]) leftovers
            | MatchToken "#([0-9]+)(?![^0-9,\[\]\{\}\!])" (value, leftovers) ->                     //dec const
                //printfn "dec: %A" value
                strToToken (lst @ [TokConst(value |> int)]) leftovers
            | MatchToken "((?<![0-9]+)[A-Za-z][A-Za-z0-9_]*)" (name, leftovers) ->                  //identifier (MOV, ADD, label)
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
            | MatchToken "\n|\r|\f" (_,leftovers) ->
                strToToken (lst @ [TokNewLine]) leftovers
            | "" -> lst
            | _ -> failwithf "Unidentified character(s) in %A" str

        let strList = input.Split([|' '; '\t'|])
        //printfn "%A" strList
        Array.fold strToToken [] strList

    ///prints the results for the tokenise function against a set of good and bad inputs
    let tokeniseTest =
        let goodTests = [|  "MOV R1, #24";
                            "MOV r12 ,R4 , #0x45";
                            "aDd r0, r2 ,#0B101100";
                            "LABEL123_ABC MOV r1, R16";
                            "MOV R1 ,r16 \n LABEL \n ADD r1, r14 ,#0b101 \n LDR r0!, [r1, #0x5]"
                        |]

        //test for syntax errors
        let badTests = [|  "1MOV r1, r2";
                            "add rr1, r22, 1";
                            "mov r1, #abc123";
                            "aDd r0, r2 ,#0B474";
                            "adc r1, r1, #0xh";
                            "1LABEL";
                            "LABEL MOV r1, 0b0102";
                            "LDr r0!, [r3 ,#3];";
                            "MOV r1, r^2";
                            "MOV r1, #ab0c45"
                        |]

        let rec tryGoodTests testList count = 
            if count < (Array.length testList) then
                try     
                    tokenise testList.[count] |> ignore
                    tryGoodTests testList (count+1)
                with
                    | Failure msg ->
                        printfn "%A" msg
                        printfn "Test %A is bad input, expected good input" count
                        count
            else
                count

        let rec tryBadTests testList count = 
            if count < (Array.length testList) then
                try     
                    tokenise testList.[count] |> ignore
                    //if exception is not raised by tokenise due to bad input:
                    printfn "Test %A is good input, expected bad input" count
                    count
                with
                    //if exception is raised, test is passed
                    | Failure msg ->
                        tryBadTests testList (count+1)
            else
                count

        printfn "Running goodTests..."
        printfn "goodTests: passed %A/%A" (tryGoodTests goodTests 0) (Array.length goodTests)
        printfn "Running badTests..."
        printfn "badTests: passed %A/%A" (tryBadTests badTests 0) (Array.length badTests)
        
        //Perform property-based testing to check for correct number of tokens
        let strWords = ["MOV"; "ADC"; "r1"; "R16"; "["; "]"; "{"; "}"; "\n" ; "LABEL"; "#0xFF"; "#2"; "#0b101"]

        let checkTokenListLength = 
            //http://stackoverflow.com/questions/1123958/get-a-random-subset-from-a-set-in-f
            let rnd = new System.Random()
            let rec subset xs = 
                let removeAt n xs = ( Seq.nth (n-1) xs, Seq.append (Seq.take (n-1) xs) (Seq.skip n xs) )
                match xs with 
                | [] -> []
                | _ -> let (rem, left) = removeAt (rnd.Next( List.length xs ) + 1) xs
                       let next = subset (List.ofSeq left)
                       if rnd.Next(2) = 0 then rem :: next else next
            let subList = subset strWords
            let subStr = String.concat " " subList
            let tokList = tokenise subStr
            tokList.Length = subList.Length

        printfn "Running FSCheck..."
        Check.Quick checkTokenListLength