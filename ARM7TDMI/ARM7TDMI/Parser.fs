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
    type PLabel = string
    type PError = string

    type Result<'a> =
        | Success of 'a
        | Failure of PLabel * PError

    /// Type that wraps a parsing function
    type Parser<'T> = {
        parseFunc : (List<Token> -> Result<'T * List<Token>>)
        pLabel:  PLabel
        }

    type Position = {
        line : int
        column : int
    }

    /// define an initial position
    let initialPos = {line=0; column=0}

    /// increment the column number
    let incrCol pos = 
        {pos with column=pos.column + 1}

    /// increment the line number and set the column to 0
    let incrLine pos = 
        {line=pos.line + 1; column=0}

    type InputState = {
        lines : List<Token>[]
        position : Position
    }

    // Create a new InputState from a Token List for a specific Line
    let rec convTokList tokenLst = 
         match tokenLst with 
                | [] -> {lines = [||]; position=initialPos}
            //    | h::tl -> if h = TokNewLine then 
            //                {lines=h; position=initialPos}
            //                convTokList tl
            //                else
            //                convTokList tl

            


    let printOutcome result =
        match result with
        | Success (value,input) -> 
            printfn "%A" value
        | Failure (label,error) -> 
            printfn "Error parsing %s\n%s" label error

    let setLabel parser newLabel = 
        // change the inner function to use the new label
        let newInnerFn input = 
            let result = parser.parseFunc input
            match result with
            | Success s ->
                // if Success, do nothing
                Success s 
            | Failure (oldLabel,err) -> 
                // if Failure, return new label
                Failure (newLabel,err)        // <====== use newLabel here
        // return the Parser
        {parseFunc=newInnerFn; pLabel=newLabel}

    
    let ( <?> ) = setLabel
    
    let satisfy predicate label =
        let innerFn (tokenLst: Token List) =
            match tokenLst with 
                | [] -> Failure ("Token List Parsed", "Finished")
                | h::tl -> 
                    let token = h
                    if predicate token then
                        let remaining = tl
                        Success (token,remaining)
                     else
                        let err = sprintf "Unexpected %A" token
                        Failure (label, err)
        // return the parser
        {parseFunc=innerFn; pLabel=label}

    // TODO: Split this up into important subtoken structures
    let pToken tokenToMatch = 
        let predicate tk = (tk = tokenToMatch) 
        let label = sprintf "%A" tokenToMatch 
        satisfy predicate label 

    /// Run a parser with some input
    let run parser input = 
        // unwrap parser to get inner function
        let {parseFunc =innerFn; pLabel=label} = parser
        // call inner function with input
        innerFn input

    /// "bindP" takes a parser-producing function f, and a parser p
    /// and passes the output of p into f, to create a new parser
    let bindP f p =
        let label = "Empty"
        let innerFn input =
            let result1 = run p input 
            match result1 with
            | Failure (label, err) -> 
                // return error from parser1
                Failure (label, err) 
            | Success (value1,remainingInput) ->
                // apply f to get a new parser
                let p2 = f value1
                // run parser with remaining input
                run p2 remainingInput
        {parseFunc =innerFn; pLabel=label }

    /// Infix version of bindP
    let ( >>= ) p f = bindP f p

    /// Lift a value to a Parser
    let returnP x = 
        let innerFn input =
            // ignore the input and return x
            Success (x,input)
        // return the inner function
        {parseFunc =innerFn; pLabel= "Success"} 

    /// apply a function to the value inside a parser
    let mapP f = 
        bindP (f >> returnP)

    /// infix version of mapP
    let ( <!> ) = mapP

    /// "piping" version of mapP
    let ( |>> ) x f = mapP f x

    /// apply a wrapped function to a wrapped value
    let applyP fP xP =         
        fP >>= (fun f -> xP >>= (fun x -> returnP (f x) ))

    /// infix version of apply
    let ( <*> ) = applyP

    /// lift a two parameter function to Parser World
    let lift2 f xP yP =
        returnP f <*> xP <*> yP

    /// Combine two parsers as "A andThen B"
    let andThen p1 p2 =   
        let label = sprintf "%A andThen %A" (setLabel p1) (setLabel p2)      
        p1 >>= (fun p1Result -> 
        p2 >>= (fun p2Result -> 
            returnP (p1Result,p2Result) ))
        <?> label

    /// Infix version of andThen
    let ( .>>. ) = andThen

    /// Combine two parsers as "A orElse B"
    let orElse p1 p2 =
        let innerFn input =
            // run parser1 with the input
            let result1 = run p1 input

            // test the result for Failure/Success
            match result1 with
            | Success result -> 
                // if success, return the original result
                result1

            | Failure (err, label) -> 
                // if failed, run parser2 with the input
                let result2 = run p2 input

                // return parser2's result
                result2 

        // return the inner function
        {parseFunc =innerFn; pLabel = p1.pLabel;}

    /// Infix version of orElse
    let ( <|> ) = orElse

    /// Choose any of a list of parsers
    let choice listOfParsers = 
        List.reduce ( <|> ) listOfParsers 

    /// Choose any of a list of characters
    let anyOf listOfTokens = 
        listOfTokens
        |> List.map pToken // convert into parsers
        |> choice
    /// Convert a list of Parsers into a Parser of a list
    let rec sequence parserList =
        // define the "cons" function, which is a two parameter function
        let cons head tail = head::tail

        // lift it to Parser World
        let consP = lift2 cons

        // process the list of parsers recursively
        match parserList with
        | [] -> 
            returnP []
        | head::tail ->
            consP head (sequence tail)

    /// (helper) match zero or more occurences of the specified parser
    let rec parseZeroOrMore parser input =
        // run parser with the input
        let firstResult = run parser input 
        // test the result for Failure/Success
        match firstResult with
        | Failure (err, label) -> 
            // if parse fails, return empty list
            ([],input)  
        | Success (firstValue,inputAfterFirstParse) -> 
            // if parse succeeds, call recursively
            // to get the subsequent values
            let (subsequentValues,remainingInput) = 
                parseZeroOrMore parser inputAfterFirstParse
            let values = firstValue::subsequentValues
            (values,remainingInput)  

    /// matches zero or more occurences of the specified parser
    let many parser = 
        let rec innerFn input =
            // parse the input -- wrap in Success as it always succeeds
            Success (parseZeroOrMore parser input)

        {parseFunc =innerFn; pLabel="Success"}

    /// matches one or more occurences of the specified parser
    let many1 p =         
        p      >>= (fun head -> 
        many p >>= (fun tail -> 
            returnP (head::tail) ))

    /// Parses an optional occurrence of p and returns an option value.
    let opt p = 
        let some = p |>> Some
        let none = returnP None
        some <|> none

    /// Keep only the result of the left side parser
    let (.>>) p1 p2 = 
        // create a pair
        p1 .>>. p2 
        // then only keep the first value
        |> mapP (fun (a,b) -> a) 

    /// Keep only the result of the right side parser
    let (>>.) p1 p2 = 
        // create a pair
        p1 .>>. p2 
        // then only keep the second value
        |> mapP (fun (a,b) -> b) 
    
 /// Keep only the result of the middle parser
    let between p1 p2 p3 = 
        p1 >>. p2 .>> p3 

    /// Parses one or more occurrences of p separated by sep
    let sepBy1 p sep =
        let sepThenP = sep >>. p            
        p .>>. many sepThenP 
        |>> fun (p,pList) -> p::pList

    /// Parses zero or more occurrences of p separated by sep
    let sepBy p sep =
        sepBy1 p sep <|> returnP []

    let (>>%) p x =
        p |>> (fun _ -> x)

    type Instr =
        |  JInstr1 of InstrType1*Option<Token>*Option<ConditionCode>*RegisterID*RegisterID
        |  JInstr2
        |  JInstr3
        |  JInstr4

        
    let instType1 = 
        let tokenInstrList = enumerator<InstrType1> |> Array.map TokInstr1 |> Array.toList
        let tokenCondList = enumerator<ConditionCode> |> Array.map TokCond |> Array.toList
        let tokenRegList = enumerator<RegisterID> |> Array.map (ID >> TokOperand) |> Array.toList
        let pInstr1 = anyOf tokenInstrList <?> "Type 1 Instruction"
        let pCond = anyOf tokenCondList <?> "Conditional Code"
        let pReg = anyOf tokenRegList <?> "Register"
        



    //  let tokenInstrList = enumerator<InstructionKeyword> |> Array.map TokInstr |> Array.toList
    //   let tokenRegList = enumerator<RegisterID> |> Array.map (ID >> TokOperand) |> Array.toList
    //   let tokenOpList = enumerator<Input> |> Array.map TokOperand |> Array.toList

    // let finalPipeline =
    //   parseGroup tokenInstrList >>> parseGroup tokenRegList >>> pToken TokComma 
           

    //////////////////Testing//////////////

    let testTokenList = [TokInstr(ADD); TokOperand(ID(R0)); TokOperand(ID(R1))]

//    printf "%A" (run finalPipeline testTokenList)