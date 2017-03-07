namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Pranav Prabhu

    Module: Parser
    Description: Parse individual instruction and initiate correct function call
*)

module Parser =

    open System
    open AST
    open Tokeniser
    open Instructions
    open Common

    type FlagSet =
        | Twoflags of bool*bool 
        | Oneflag of bool

    type Expr = 
        | Instruction of string*RegisterID*RegisterID*Operand*FlagSet
        
    (*IGNORE BELOW - temporary implementation for pipeline*)

    //http://stackoverflow.com/questions/2071056/splitting-a-list-into-list-of-lists-based-on-predicate
    ///divides a list L into chunks for which all elements match pred
    let divide pred L =
        let rec aux buf acc L =
            match L,buf with
            //no more input and an empty buffer -> return acc
            | [],[] -> List.rev acc 
            //no more input and a non-empty buffer -> return acc + rest of buffer
            | [],buf -> List.rev (List.rev buf :: acc) 
            //found something that matches pred: put it in the buffer and go to next in list
            | h::t,buf when pred h -> aux (h::buf) acc t
            //found something that doesn't match pred. Continue but don't add an empty buffer to acc
            | h::t,[] -> aux [] acc t
            //found input that doesn't match pred. Add buffer to acc and continue with an empty buffer
            | h::t,buf -> aux [] (List.rev buf :: acc) t
        aux [] [] L


//    let splitLst = divide (fun x -> match x with | TokNewLine -> false | _ -> true) lst

/// Type that represents Success/Failure in parsing
    type Result<'a> =
        | Success of 'a
        | Failure of string 

/// Type that wraps a parsing function
    type Parser<'T> = Parser of (Token List -> Result<'T * Token List>)

/// Parse the Instruction Keyword
    let pToken tokenToMatch = 
        // define a nested inner function
        let innerFn (tokenLst: Token List) =
            match tokenLst with 
                | [] -> Failure "Token List Parsed"
                | h::tl -> 
                    let token = h
                    if token = tokenToMatch then
                        let remaining = tl
                        Success (tokenToMatch,remaining)
                    else
                        let msg = sprintf "Expecting '%A'. Got '%A'" tokenToMatch token
                        Failure msg
        // return the "wrapped" inner function
        Parser innerFn 

    /// Run a parser with some input
    let run parser input = 
        // unwrap parser to get inner function
        let (Parser innerFn) = parser 
        // call inner function with input
        innerFn input

    /// Combine two parsers after each other (to achieve parser pipeline)
    let ( >>> ) parser1 parser2 =
        let innerFn input =
            // run parser1 with the input
            let result1 = run parser1 input
            
            // test the result for Failure/Success
            match result1 with
            | Failure err -> 
                // return error from parser1
                Failure err  

            | Success (value1,remaining1) -> 
                // run parser2 with the remaining input
                let result2 =  run parser2 remaining1
                
                // test the result for Failure/Success
                match result2 with 
                | Failure err ->
                    // return error from parser2 
                    Failure err 
                
                | Success (value2,remaining2) -> 
                    // combine both values as a pair
                    let newValue = (value1,value2)
                    // return remaining input after parser2
                    Success (newValue,remaining2)

        // return the inner function
        Parser innerFn 

    /// Combine two parsers as "A orElse B"
    let ( <|> ) parser1 parser2 =
        let innerFn input =
            // run parser1 with the input
            let result1 = run parser1 input

            // test the result for Failure/Success
            match result1 with
            | Success result -> 
                // if success, return the original result
                result1

            | Failure err -> 
                // if failed, run parser2 with the input
                let result2 = run parser2 input

                // return parser2's result
                result2 

        // return the inner function
        Parser innerFn 

    /// Choose any of a list of parsers
    let choice listOfParsers = 
        List.reduce ( <|> ) listOfParsers 

    //////////////////Testing//////////////
    let tokenInstrList = enumerator<InstructionKeyword> |> Array.map (fun x -> TokInstr(x)) |> Array.toList
    let tokenRegList = enumerator<RegisterID> |> Array.map (fun x -> TokOperand(ID(x))) |> Array.toList

   // let tokenOpList = enumerator<Operand> |> Array.map (fun x -> Token(x)) |> Array.toList
    let testTokenList = [TokInstr(ADD); TokOperand(ID(R0)); TokOperand(ID(R1))]
    let parseGroup =
        let anyOf listOftokens = 
            listOftokens
            |> List.map pToken // convert into parsers
            |> choice
        anyOf 
    let parseEverything =
        parseGroup tokenInstrList >>> parseGroup tokenRegList >>> pToken TokComma >>> (
           (parseGroup tokenRegList >>> pToken TokComma >>> parseGroup tokenInstrList) <|>  (parseGroup tokenInstrList)
        ) 

    printf "%A" (run parseEverything testTokenList)