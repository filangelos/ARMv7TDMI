namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Baron Khan

    Module: Tokeniser
    Description: Takes a string input representing the programs and produces a list of tokens.
*)

module Tokeniser =

    open System.Text.RegularExpressions
    open FsCheck

    ///returns the first matched group of a regex and the leftovers from the input
    let (|MatchToken|_|) pattern input =
        let m = Regex.Match(input, "(?<![\s\S]+)" + pattern) //pattern must start at beginning of string
        if (m.Success)
        then Some (m.Groups.[1].Value, (new Regex(pattern)).Replace(input, "", 1))
        else None

    ///remove comments from an input string
    let rec removeComments (input:string) =
        let newInput = (new Regex(";[\s\s0-9\w\W]*\n")).Replace(input, "\n", 1)
        let newInput2 = (new Regex(";[\s\s0-9\w\W]*$")).Replace(newInput, "", 1)
        if newInput2 = input then newInput2
        else removeComments newInput2

    ///turns an integer into a TokReg token (feel free to change this mess of code)
    let getTokenRegisterFromID (id:int) = 
        match id with
        | 0 -> TokReg(R0)
        | 1 -> TokReg(R1)
        | 2 -> TokReg(R2)
        | 3 -> TokReg(R3)
        | 4 -> TokReg(R4)
        | 5 -> TokReg(R5)
        | 6 -> TokReg(R6)
        | 7 -> TokReg(R7)
        | 8 -> TokReg(R8)
        | 9 -> TokReg(R9)
        | 10 -> TokReg(R10)
        | 11 -> TokReg(R11)
        | 12 -> TokReg(R12)
        | 13 -> TokReg(R13)
        | 14 -> TokReg(R14)
        | 15 -> TokReg(R15)
        | _ -> TokError("R"+id.ToString())

    ///returns a list of tokens from a string input
    let tokenise (input:string) =

        ///breaks down a string into a list of tokens and appends them onto lst
        let rec strToToken (lst:Token list) (str:string) =
            match str with
            //str may contain several tokens, so recursively call MatchToken until str is empty 
            | MatchToken "[rR]([0-9]|1[0-6])(?![^,\[\]\{\}\!\n])" (reg, leftovers) ->                   //register
                strToToken (lst @ [getTokenRegisterFromID(reg |> int)]) leftovers
            | MatchToken "#(0[xX][0-9A-Fa-f]+(?![^0-9A-Fa-f,\[\]\{\}\!\n]))" (hexVal, leftovers) ->     //hex const
                //printfn "hex: %A" hexVal
                strToToken (lst @ [TokConst (int hexVal)]) leftovers
            | MatchToken "#(0[bB][01]+(?![^01,\[\]\{\}\!\n]))" (binVal, leftovers) ->                   //bin const
                //printfn "bin: %A" binVal
                strToToken (lst @ [TokConst (int binVal)]) leftovers
            | MatchToken "#([0-9]+)(?![^0-9,\[\]\{\}\!\n])" (value, leftovers) ->                       //dec const
                //printfn "dec: %A" value
                strToToken (lst @ [TokConst(value |> int)]) leftovers
            | MatchToken "((?<![0-9]+)[A-Za-z][A-Za-z0-9_]*(?![^,\[\]\{\}\!\n]))" (name, leftovers) ->  //identifier (MOV, ADD, label)
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
            | _ -> (lst @ [TokError(str)])

        //remove comments from input
        let inputNoComments = removeComments input

        let strList = inputNoComments.Split([|' '; '\t'|])
        //printfn "%A" strList
        Array.fold strToToken [] strList


(*--------------------------------------------------------TESTING--------------------------------------------------------*)

    ///prints the results for the tokenise function against a set of good, bad and random inputs
    let tokeniseTest =
        ///list of correct syntax
        let goodTests = [|  "MOV R1, #24";
                            "MOV r12 ,R4 , #0x45";
                            "aDd r0, r2 ,#0B101100";
                            "LABEL123_ABC MOV r1, R16      ; end of line";
                            "MOV R1 ,r16 \n LABEL ; My comment\n ADD r1, r14 ,#0b101 \n LDR r0!, [r1, #0x5]"
                        |]

        ///list of incorrect syntax
        let badTests = [|   "1MOV r1, r2";
                            "add rr1, r22, 1";
                            "mov r1, #abc123";
                            "aDd r0, r2 ,#0B474";
                            "adc r1, r1, #0xh";
                            "1LABEL";
                            "LABEL MOV r1, 0b0102";
                            "LDr r0!, [r3 ,#!3]";
                            "MOV r1, r^2";
                            "MOV r1, #ab0c45"
                        |]

        ///test for good syntax
        (*let rec tryGoodTests testList count = 
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
        
        ///test for bad syntax
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
        *)

        let rec tryGoodTests testList count = 
            if count < (Array.length testList) then  
                let tokList = tokenise testList.[count]
                let containsError = List.exists (fun a -> match a with | TokError _ -> true | _ -> false ) tokList
                if containsError then 
                    printfn "Test %A (\n%A\n) is bad input, expected good input" count testList.[count]
                    count
                else
                    tryGoodTests testList (count+1)
            else
                count
        
        let rec tryBadTests testList count = 
            if count < (Array.length testList) then    
                let tokList = tokenise testList.[count]
                let containsError = List.exists (fun a -> match a with | TokError _ -> true | _ -> false ) tokList
                if containsError then 
                    tryBadTests testList (count+1)
                else
                    printfn "Test %A (\n%A\n) is good input, expected bad input" count testList.[count]
                    count
            else
                count

        let strWords = ["MOV"; "ADC"; "r1"; "R16"; "["; "]"; "{"; "}"; "\n" ; "LABEL"; "#0xFF"; "#2"; "#0b101"]

        let checkTokenListLength separator = 
            let isSeparatorAToken = ((tokenise separator).Length > 0)
            //http://stackoverflow.com/questions/1123958/get-a-random-subset-from-a-set-in-f
            let rnd = new System.Random()
            let rec subset xs = 
                let removeAt n xs = ( Seq.item (n-1) xs, Seq.append (Seq.take (n-1) xs) (Seq.skip n xs) )
                match xs with 
                | [] -> []
                | _ -> let (rem, left) = removeAt (rnd.Next( List.length xs ) + 1) xs
                       let next = subset (List.ofSeq left)
                       if rnd.Next(2) = 0 then rem :: next else next
            let subList = subset strWords
            let subStr = String.concat separator subList
            let tokList = tokenise subStr
            if isSeparatorAToken then 
                tokList.Length = (subList.Length * 2) - 1
            else
                tokList.Length = subList.Length


        //perform valid input tests
        printfn "Running goodTests..."
        printfn "goodTests: passed %A/%A" (tryGoodTests goodTests 0) (Array.length goodTests)
        printfn "Running badTests..."
        printfn "badTests: passed %A/%A" (tryBadTests badTests 0) (Array.length badTests)
        
        //perform property-based testing to check for correct number of tokens
        printfn "Running FSCheck for token list length..."
        Check.Quick (checkTokenListLength " ")
        Check.Quick (checkTokenListLength " , ")
        Check.Quick (checkTokenListLength ", ")
        Check.Quick (checkTokenListLength " ,")
        Check.Quick (checkTokenListLength " \n ")
        Check.Quick (checkTokenListLength " \n")
        Check.Quick (checkTokenListLength "\n ")


        //temporary testing, will add robust testing later
        printfn "Testing Comments Removal... (will improve testing later)\ninput -> output:"
        let test = "; This is a comment"
        printfn "1. %A -> %A" (test) (removeComments test)
        let test2 = "MOV r1, r2 ;End of line comment"
        printfn "2. %A -> %A" (test2) (removeComments test2)
        let test3 = "MOV r1, r2 ; remove comment and instruction: MOV r1, r1"
        printfn "3. %A -> %A" (test3) (removeComments test3)
        let test4 = "MOV r1, r2 ; remove comment but not instruction: \n MOV r1, r1"
        printfn "4. %A -> %A" (test4) (removeComments test4)
        let test5 = ";comment with random chars 354 245 ! [ ] £ # // %$ 65"
        printfn "5. %A -> %A" (test5) (removeComments test5)