namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Pranav Prabhu

    Module: Parser
    Description: Parse individual instruction and pass parsed list of Result Types to AST. 
*)

module Parser =

    open System
    open AST
    open Tokeniser
    open Instructions
    open Common
    
    /// Type that represents Success/Failure in parsing
    type PLabel = string
    type PError = string

    type Result<'a> =
        | Success of 'a
        | Failure of PLabel * PError
    let printResult result =
        match result with
        | Success (value,input) -> 
            printfn "%A" value
        | Failure (label,error) -> 
            printfn "Error parsing %s\n%s" label error
    // Prints result of parsing

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
    let fromToken tokenLst = 
        let totalLst = []
        let rec innerfn remTokenLst = 
            let innerLst = []
            match remTokenLst with 
                | [] -> {lines = [||]; position=initialPos}
                | h::tl -> match h with 
                            | TokNewLine -> innerfn tl
                            | _ -> let x = innerLst::h
        innerfn tokenLst 
                            
    
        
        //    {lines=lines; position=initialPos}


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
    let anyOf tokenList = 
        let label = sprintf "anyOf %A" tokenList
        tokenList
        |> List.map pToken // convert into parsers
        |> choice
        <?> label
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

<<<<<<< HEAD
=======
    
     //   |  JInstr8 of InstrType8*StackDirection*Option<ConditionCode>
     // Need to Sort out Instructions 7 and 8 because of weird Source Dest Thing
>>>>>>> 2f624185bb3cf931c7415669ee20ad3ffb85476d
    let tokenCondList = enumerator<ConditionCode> |> Array.map TokCond |> Array.toList
    let tokenRegList = enumerator<RegisterID> |> Array.map (ID >> TokOperand) |> Array.toList
    let pS = pToken (TokS S) <?> "Set Flag Variable"
    let pCond = anyOf tokenCondList <?> "Conditional Code"
    let pReg = anyOf tokenRegList <?> "Register"
   
    let pInput = pReg
    // fix to include pInt literal 
    let instType1 = 
        let tokenInstrList1 = enumerator<InstrType1> |> Array.map TokInstr1 |> Array.toList
        let pInstr1 = anyOf tokenInstrList1 <?> "Type 1 Opcode"
        let label = "Instruction Type 1"
        ( pInstr1 .>>. opt pS .>>. opt pCond .>>. pReg .>>. pReg >>% JInstr1 )<?> label

    let instType2 = 
        let tokenInstrList2 = enumerator<InstrType2> |> Array.map TokInstr2 |> Array.toList
        let pInstr2 = anyOf tokenInstrList2 <?> "Type 2 Opcode"
        let label = "Instruction Type 2"
        (pInstr2 .>>. opt pS .>>. opt pCond .>>. pReg  >>% JInstr2) <?> label

    let instType3 = 
        let tokenInstrList3 = enumerator<InstrType3> |> Array.map TokInstr3 |> Array.toList
        let pInstr3 = anyOf tokenInstrList3 <?> "Type 3 Opcode"
        let label = "Instruction Type 3"
        (pInstr3 .>>. opt pS .>>. opt pCond .>>. pReg .>>. pReg .>>. pInput >>% JInstr3) <?> label

    let instType4 = 
        let tokenInstrList4 = enumerator<InstrType4> |> Array.map TokInstr4 |> Array.toList
        let pInstr4 = anyOf tokenInstrList4 <?> "Type 4 Opcode"
        let label = "Instruction Type 4"
        (pInstr4 .>>. opt pS .>>. opt pCond .>>. pReg .>>. pReg .>>. pInput >>% JInstr4) <?> label

    let instType5 = 
        let tokenInstrList5 = enumerator<InstrType5> |> Array.map TokInstr5 |> Array.toList
        let pInstr4 = anyOf tokenInstrList5 <?> "Type 5 Opcode"
        let label = "Instruction Type 5"
        (pInstr4 .>>. opt pS .>>. opt pCond .>>. pReg .>>. pInput >>% JInstr5) <?> label
    

    //////////////////Testing//////////////

    let testInstrType1List1 = [TokInstr1(MOV); TokOperand(ID(R0)); TokOperand(ID(R1))]
    let testInstrType1List2 = [TokInstr1(MOV); TokOperand(ID(R0)); TokOperand(Literal(10))]

    let testInstrType1List3 = [TokInstr1(MVN); TokS(S); TokOperand(ID(R0)); TokOperand(ID(R1))]

    let testInstrType1List4 = [TokInstr1(MVN); TokS(S); TokCond(EQ); TokOperand(ID(R0)); TokOperand(Literal(10))]

    let testInstrType1ListFail1 = [TokInstr1(MVN); TokError("R20"); TokOperand(Literal(10))]
    let testInstrType1ListFail2 = [TokInstr1(MVN); TokError("R16"); TokError("R20");]

    let testInstrType1ListFail3 = [TokInstr1(MOV); TokError("B"); TokOperand(ID(R0)); TokOperand(ID(R1))]

    let testInstrType1ListFail4 = [TokInstr1(MOV); TokS(S); TokError("ER"); TokOperand(ID(R0)); TokOperand(Literal(10))]

   // printf "%A" (run instType1 testTokenList) 