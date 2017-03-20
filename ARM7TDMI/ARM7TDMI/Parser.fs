namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Pranav Prabhu

    Module: Parser

    Description: Take in a Token List, Parse and return a List of InstrType or Error (and associated Error information)
    to AST for processing onwards to actual implementation. Parsing done using monadic parser combinators. 
    

    Sources: fsharpforfunandprofit.com, 
*)

module Parser =

    open System
    open Common
    open Toolkit
    open FsCheck

    type Pos = {
        lineNo : int
        tokenNo : int
    }

    type InitState = {
        lineList : List<Token>[]
        position : Pos
    }
    let initPos = {lineNo=0; tokenNo=0;}

    /// increment the tokenNo number
    let incrTok pos =  {pos with tokenNo=pos.tokenNo + 1}

    /// increment the lineNo number and set tokenNo to 0
    let incrLine pos = {lineNo=pos.lineNo + 1; tokenNo=0}

    // Create a new InitState from a Token List for a specific Line

    /// Split a list to a list of lists at the delimiter (del)
    let splitBy (del: 'a) (lst: 'a list) : ('a list) list =

        // reverse non-empty list
        let yieldRevNonEmpty lst = 
            match List.isEmpty lst with
            | true -> []
            | false -> [List.rev lst]
        
        // tail recursive accummulation of list using computational expressions
        let rec loop acc lst = seq {
            match lst with 
            | [] -> yield! yieldRevNonEmpty acc
            | h::t when h = del ->
            yield! yieldRevNonEmpty acc
            yield! loop [] t
            | h::t -> yield! loop (h::acc) t }
        
        loop [] lst |> List.ofSeq
    let tokenToInit tokenLst = 
        let y = splitBy TokNewLine tokenLst   
        match y with
            | [] -> {lineList = [||]; position = initPos;}
            | _ ->  {lineList = List.toArray y; position=initPos;}

    // return the current line
    let currLine inputState = 
        let linePos = inputState.position.lineNo
        if linePos < inputState.lineList.Length then
            inputState.lineList.[linePos]
        else
            [TokEOF]

    /// Get the next token from the input, if any
    /// else return None. Also return the updated InputState
    /// Signature: InputState -> InputState * char option 
    let nextToken input =
        let linePos = input.position.lineNo
        let tokPos = input.position.tokenNo
        // three cases
        // 1) if line >= maxLine -> 
        //       return EOF
        // 2) if col less than line length -> 
        //       return char at colPos, increment colPos
        // 3) if col at line length -> 
        //       return NewLine, increment linePos

        if linePos >= input.lineList.Length then
            input, None
        else
            let currLine = currLine input
            if tokPos < currLine.Length then
                let token = currLine.[tokPos]
                let newPos = incrTok input.position 
                let newState = {input with position=newPos}
                newState, Some token
            else 
                // end of line, so return LF and move to next line
                let token = TokNewLine
                let newPos = incrLine input.position 
                let newState = {input with position=newPos}
                newState, Some token

    ///////// Inpit State required for tracking position errors /////////////


    type Inpt = InitState

    type PLabel = string
    type PError = string

    type PPosition = {
        currLine : List<Token>
        lineNo : int
        tokenNo : int
    }

    type Outcome<'a> =
        | Success of 'a
        | Failure of PLabel * PError * PPosition

    type Parser<'T> = {
        parseFunc : (InitState -> Outcome<'T * InitState>)
        pLabel:  PLabel
        }
        
    let parserPosfromInitState(initState:Inpt) = {
        currLine =  currLine initState
        lineNo = initState.position.lineNo
        tokenNo = initState.position.tokenNo
    }

    let printOutcome result =
        match result with
        | Success (value,input) -> 
            printfn "%A" value
        | Failure (label, err, parPos) -> 
            let errorLine = parPos.currLine
            let tokPos = parPos.tokenNo
            let linePos = parPos.lineNo
            let failureLine = sprintf "%*s^%s" tokPos "" err
            printfn "Line:%i - TokenNo:%i Error parsing %A\n %A\n %s" linePos tokPos label errorLine failureLine
    //convert Error Line to String....
    let rec readAllTokens input =
        [
            let remainingInput,tokenOpt = nextToken input 
            match tokenOpt with
            | None -> 
                // end of input
                ()
            | Some tk -> 
                // return first character
                yield tk
                // return the remaining characters
                yield! readAllTokens remainingInput
        ]

    let setLabel parser newLabel = 
        // change the inner function to use the new label
        let newInnerFn input = 
            let result = parser.parseFunc input
            match result with
            | Success s ->
                // if Success, do nothing
                Success s 
            | Failure (oldLabel,err, parPos) -> 
                // if Failure, return new label
                Failure (newLabel,err, parPos)        // <====== use newLabel here
        // return the Parser
        {parseFunc=newInnerFn; pLabel=newLabel}


    let ( <?> ) = setLabel

    let satisfy predicate label =
        let innerFn tokenLst=
            let remainInput, tokenOpt = nextToken tokenLst
            match tokenOpt with 
                | None -> let err = "No more input"
                          let pos = parserPosfromInitState tokenLst
                          printf "%A" (nextToken tokenLst)                                                              
                          Failure (label,err,pos)
                        
                | Some first -> 
                    if predicate first then
                        Success (first,remainInput)
                    else
                        let err = sprintf "Unexpected '%A'" first
                        let pos = parserPosfromInitState tokenLst
                        Failure (label,err,pos)
        // return the parser
        {parseFunc=innerFn; pLabel=label}

    let pToken tokenToMatch = 
        let predicate tk = (tk = tokenToMatch) 
        let label = sprintf "%A" tokenToMatch 
        satisfy predicate label 

    let pRegToken regToMatch = 
        let predicate rg = (rg = regToMatch) 
        let label = sprintf "%A" regToMatch 
        satisfy predicate label 
    let runInput parser input = 
        parser.parseFunc input
    /// Run a parser with some input

    let run parser inputTokenLst = 
        runInput parser (tokenToInit inputTokenLst)

    /// "bindP" takes a parser-producing function f, and a parser p
    /// and passes the output of p into f, to create a new parser
    let bindP f p =
        let label = "Empty"
        let innerFn input =
            let res1 = runInput p input 
            match res1 with
            | Failure (label, err, pos) -> 
                // return error from parser1
                Failure (label, err, pos) 
            | Success (value1,remainingInput) ->
                // apply f to get a new parser
                let p2 = f value1
                // run parser with remaining input
                runInput p2 remainingInput
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
            let result1 = runInput p1 input

            // test the result for Failure/Success
            match result1 with
                | Success result -> 
                // if success, return the original result
                    result1

                | Failure (label, err, pos) -> 
                // if failed, run parser2 with the input
                    let result2 = runInput p2 input

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
    let anyOfReg regList = 
        let label = sprintf "anyOf %A" regList
        regList
        |> List.map pRegToken // convert into parsers
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
        | h::tl ->
            consP h (sequence tl)
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



    let (>>%) p x =
        p |>> (fun _ -> x)
    (*let createParserForwardedToRef<'a>() =

        let dummyParser= 
            let innerFn input : Outcome<'a * Inpt> = failwith "unfixed forwarded parser"
            {parseFunc=innerFn; pLabel="Unknown"}
        
        // ref to placeholder Parser
        let parserRef = ref dummyParser 

        // wrapper Parser
        let innerFn input = 
            // forward input to the placeholder
            runInput !parserRef input 
        let outParser = {parseFunc=innerFn; pLabel="Unknown"}

        outParser, parserRef

    let parseInstr,parseInstrForRef = createParserForwardedToRef<Instr>()
    *)

    /////////////////////////////////// Object Lists TO BE DISCARDED DUE TO FABLE NOT WORKING WITH ENUM ////////////////////////////////////    
    let tokenCondList = [TokCond(EQ); TokCond(NE); TokCond(CS); TokCond(HS); TokCond(CC); TokCond(LO); TokCond(MI); TokCond(PL);
                            TokCond(VS); TokCond(VC); TokCond(HI); TokCond(LS); TokCond(GE); TokCond(LT); TokCond(GT); TokCond(LE);
                            TokCond(AL); TokCond(NV);]
    let regList = [R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10; R11; R12; R13; R14; R15;]

    let tokenRegList = List.map TokReg regList
    let tokenInstrList1 = [TokInstr1(MOV); TokInstr1(MVN)]
    let tokenInstrList2 = [TokInstr2(ADR)]
    let instrList3 = [ADD ; ADC ; SUB ; SBC ; RSB ; RSC ; AND
        ; EOR ; BIC ; ORR;]
    let tokenInstrList3 = List.map TokInstr3 instrList3

    let instrList4 = [LSL; LSR; ASR; ROR_;]
    let tokenInstrList4 = List.map TokInstr4 instrList4
    let tokenInstrList5 = [TokInstr5(RRX_)]
    let instrList6 = [CMP ; CMN ; TST ; TEQ;]
    let tokenInstrList6 = List.map TokInstr6 instrList6

    let tokenInstrList7 = [TokInstr7(LDR); TokInstr7(STR);] 
    let tokenInstrList8 = [TokInstr8(LDM); TokInstr8(STM);]

    let tokenInstrList9 = [TokInstr9(B_); TokInstr9(BL);]

    ////////////////////////////////// Parsers /////////////////////////////////////////////////////////////////
    let pInstr1 =
        let parseTuple = anyOf tokenInstrList1 <?> "Type 1 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr1 a -> a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple

    let pInstr2 =  
        let parseTuple = anyOf tokenInstrList2 <?> "Type 2 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr2 a -> a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple
    let pInstr3 = 
        let parseTuple = anyOf tokenInstrList3 <?> "Type 3 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr3 a -> a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple

    let pInstr4 = 
        let parseTuple = anyOf tokenInstrList4 <?> "Type 4 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr4 a -> a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple

    let pInstr5 =
        let parseTuple = anyOf tokenInstrList5 <?> "Type 5 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr5 a -> a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple

    let pInstr6 =
        let parseTuple = anyOf tokenInstrList6 <?> "Type 6 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr6 a -> a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple

    let pInstr7 = 
        let parseTuple = anyOf tokenInstrList7 <?> "Type 7 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr7 a -> a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple
    let pInstr8 = 
        let parseTuple = anyOf tokenInstrList8 <?> "Type 8 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr5 a -> a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple

    let pInstr9 = 
        let parseTuple = anyOf tokenInstrList9 <?> "Type 9 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr9 a -> a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple

    let pS = 
        let parseTuple = pToken (TokS S) <?> "S Type"
        let tupleTransform(x) =
            match x with 
            | TokS a -> a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple

    let pB = 
        let parseTuple = pToken (TokB B) <?> "S Type"
        let tupleTransform(x) =
            match x with 
            | TokB a -> a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple
    let pComma =
        let parseTuple = pToken TokComma <?> "Comma"
        parseTuple >>% TokComma
    let pCond = 
        let parseTuple = anyOf tokenCondList <?> "Conditional Code"
        let tupleTransform(x) =
            match x with 
            | TokCond a -> a 
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple
    let pReg = 
        let parseTuple = anyOf tokenRegList <?> "Register"
        let tupleTransform(x) =
            match x with 
            | TokReg a -> a 
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple
    let pRegComma = 
            let parseTuple = pReg .>>. pComma <?> "Reg followed by Comma"
            let tupleTransform (t1,t2) = 
                match t1, t2 with  
                | a, TokComma-> a
                | _ -> failwith "Impossible"
            mapP tupleTransform parseTuple

    ///////////////////////////////////////// INPUT ATTEMPT /////////////////////////////////////////////////////////////////
    let pInt =
        let predicate y = 
            match y with
            | TokLiteral a -> true
            | _ -> false
        let label = sprintf "Integer" 
        satisfy predicate label 

    let pLiteral =  
        let parseTuple = pInt <?> "Integer"
        let tupleTransform(t) = 
            match t with
            | TokLiteral a -> Literal a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple

    let pRegtoInput = 
        let parserTuple = pReg 
        let tupleTransform (t) = 
            match t with
            | a -> ID a 
        mapP tupleTransform parserTuple

    let pInput = 
        let parseTuple = pLiteral <|> pRegtoInput <?> "Register or Literal Int"
        let tupleTransform (t1) = 
            match t1 with  
            |  ID a -> ID a
            | Literal x -> Literal x
        mapP tupleTransform parseTuple
        
        
     ///////////////////////////////////////////////// OPERAND ATTEMPT //////////////////////////////////////////////

    let pShiftDirection4 =
        let parseTuple = pInstr4 .>>. pInt <?> "Shift Direction (Int)"
        let tupleTransform (t1, t2) = 
            match t1, t2 with
            | LSL, TokLiteral a -> Left a 
            | LSR, TokLiteral a-> RightL a
            | ASR, TokLiteral a -> RightA a
            | ROR_,TokLiteral a-> ROR a
        mapP tupleTransform parseTuple 

    let pShiftDirection5 =
        let parseTuple = pInstr5 <?> "Shift Direction"
        let tupleTransform (t1) = 
            match t1 with
            | RRX_ -> RRX
        mapP tupleTransform parseTuple 
    let pOp =
        let parseTuple = pInput .>>. pComma .>>. opt (pShiftDirection5 <|> pShiftDirection4)  <?> "Operand"
        let tupleTransform ((t1, t2), t3) = 
            match t1, t2, t3 with
            | ID x, _, y  -> match y with 
                                | None -> Operand (ID x, NoShift)
                                | Some y -> Operand(ID x, y)
            | Literal a, _, b ->  match b with 
                                    | None -> Operand (Literal a, NoShift)
                                    | Some b -> Operand(Literal a, b)
        mapP tupleTransform parseTuple

    /////////////////////////////// AddressRegister Type /////////////////////////////////////////////////////////////
    let pLBracket = pToken TokSquareLeft <?> "LeftBracket"

    let pRBracket = pToken TokSquareRight <?> "RightBracket"
    let pExclam = pToken TokExclam <?> "Exclamation Mark"
    let pCommaOffset =     
        let parseTuple = pComma .>>. pInt <?> "Offset Integer"
        let tupleTransform (t1,t2) = 
            match t2 with  
            | TokLiteral a -> a
        mapP tupleTransform parseTuple

    let pOffsetAddress = 
        let parseTuple =  opt pCommaOffset .>> pRBracket <?> "Offset Addressing"
        let tupleTransform t1 =
            match t1 with
            | Some a -> TempOffset a  
            | None -> NoOffset
        mapP tupleTransform parseTuple

    let pOffsetPre = 
        let parseTuple =  pCommaOffset .>> pComma .>> pRBracket .>> pExclam <?> "Pre-indexed Offset Addressing"
        let tupleTransform t1 = PreIndex t1
        mapP tupleTransform parseTuple

    let pOffsetPost = 
        let parseTuple =  pRBracket .>>. pComma .>>. pInt <?> "Pre-indexed Offset Addressing"
        let tupleTransform ((t1, t2), t3) = 
            match t3 with
            | TokLiteral a -> PostIndex a
        mapP tupleTransform parseTuple

    let pAddressRegister =
        let parseTuple =  pLBracket >>. pReg .>>. choice [pOffsetAddress; pOffsetPre; pOffsetPost]  <?> "Shift Direction"
        let tupleTransform (t1, t2) = {register=t1;offset = t2;}
        mapP tupleTransform parseTuple          
          

////////////////////////////////////////// String/Expression/Labels Parsers //////////////////////////////////////////////////////////
    let pString =   
        let predicate y = 
            match y with
            | TokLabel a -> true
            | _ -> false
        let label = sprintf "String" 
        satisfy predicate label 

    let pLabel =
        let parseTuple = pString <?> "String"
        let tupleTransform(x) =
            match x with 
            | TokLabel a ->  a
            | _ -> failwith "Impossible"
        mapP tupleTransform parseTuple

    let instTypeLabel = 
        let label = "Instruction Type Label"
        let tupleTransform = function
            | x -> JLabel(x)
        let instrLabelHold = pLabel <?> label
        mapP tupleTransform instrLabelHold

    let pExpr = 
        let parseTuple = (pInt <|> pString) <?> "Shift Direction"
        let tupleTransform (t1) = 
            match t1 with
            | TokLiteral a -> Number a
            | TokLabel a -> Lab a
        mapP tupleTransform parseTuple          

    /////////////////////////Final Instruction Types ////////////////////////////////////////////////////////////////
    let instType1 = 
        let label = "Instruction Type 1"
        let tupleTransform = function
            | x -> JInstr1(x)
        let instr1Hold = pInstr1 .>>. opt pS .>>. opt pCond .>>. pRegComma .>>. pOp <?> label
        mapP tupleTransform instr1Hold

    let instType2 = 
        let label = "Instruction Type 2"
        let tupleTransform = function
            | x -> JInstr2(x)
        let instr2Hold = pInstr2 .>>. opt pCond .>>. pRegComma .>>. pExpr <?> label
        mapP tupleTransform instr2Hold

    let instType3 =
        let label = "Instruction Type 3"
        let tupleTransform = function
            | x -> JInstr3(x)
        let instr3Hold = pInstr3 .>>. opt pS .>>. opt pCond .>>. pRegComma .>>. pRegComma .>>. pOp  <?> label
        mapP tupleTransform instr3Hold

    let instType4 = 
        let label = "Instruction Type 4"
        let tupleTransform = function
            | x -> JInstr4(x)
        let instr4Hold = pInstr4 .>>. opt pS .>>. opt pCond .>>. pRegComma .>>. pRegComma .>>. pInput <?> label
        mapP tupleTransform instr4Hold

    let instType5 = 
        let label = "Instruction Type 5"
        let tupleTransform = function
            | x -> JInstr5(x)
        let instr5Hold = pInstr5 .>>. opt pS .>>. opt pCond .>>. pRegComma .>>. pInput <?> label
        mapP tupleTransform instr5Hold

    let instType6 = 
        let label = "Instruction Type 6"
        let tupleTransform = function
            | x -> JInstr6(x)
        let instr6Hold = pInstr6 .>>. opt pCond .>>. pRegComma .>>. pOp <?> label
        mapP tupleTransform instr6Hold

    let instType7 = 
        let label = "Instruction Type 7"
        let tupleTransform = function
            | x -> JInstr7(x)
        let instr7Hold = pInstr7 .>>. opt pB .>>. opt pCond .>>. pReg .>>. pAddressRegister <?> label
        mapP tupleTransform instr7Hold




    //////////////// Final Choice ////////////////////////////////////////
    let parseInstr = choice [
                            instType1;
                            instType2;
                            instType3;
                            instType4;
                            instType5;
                            instType6;
                            instType7;
                            instTypeLabel;
                            ]
                            

    let Parse (tokenLstLst: Token List) : Instr List = 
        let z outcome = match outcome with 
                        | Success(value, input) -> value
                        | Failure(label,err,parPos) -> JLabel ("Failed Line")
        let y = splitBy TokNewLine tokenLstLst                            
        let x = List.map (fun j -> run parseInstr j) 
        let u = List.map z 
        y |> x |> u



    (**************************************************TESTING***********************************************************)

    let testInstrType1List2 = [TokInstr1(MOV); TokReg(R0); TokLiteral(10);]

    let testInstrType1List3 = [TokInstr1(MVN); TokS(S); TokReg(R0); TokReg(R1)]

    let testInstrType1List4 = [TokInstr1(MVN); TokS(S); TokCond(EQ); TokReg(R0); TokLiteral(10)]

    let testInstrType1ListFail1 = [TokInstr1(MVN); TokError("R20"); TokLiteral(10)]
    let testInstrType1ListFail2 = [TokInstr1(MVN); TokError("R16"); TokError("R20");]

    let testInstrType1ListFail3 = [TokInstr1(MOV); TokError("B"); TokReg(R0); TokReg(R1)]

    let testInstrType1ListFail4 = [TokInstr1(MOV); TokS(S); TokError("ER"); TokReg(R0); TokLiteral(10)]
    let testInstrType1List1 = [TokInstr1(MOV); TokReg(R0); TokComma; TokLiteral(10);]
    let t1 = [TokReg(R0); TokError("1$");]


    let testParser () = 
        printfn "Running testParser...\n"

        let parseGoodTokenList =
                                    [|
                                        [TokInstr3 ADD; TokReg R1; TokComma; TokReg R2; TokComma; TokReg R3; TokNewLine; TokInstr1 MOV; TokReg R2; TokComma; TokReg R1; TokEOF];
                                        [TokInstr1 MVN; TokReg R0; TokComma; TokLiteral 5; TokEOF];
                                        [TokInstr3 ADC; TokS S; TokReg R0; TokComma; TokReg R1; TokComma; TokLiteral 5; TokComma; TokInstr4 LSL; TokLiteral 5; TokEOF];
                                        [TokInstr4 LSL; TokS S; TokCond EQ; TokReg R0; TokComma; TokReg R0; TokComma; TokLiteral 11; TokEOF];
                                        [TokInstr5 RRX_; TokS S; TokCond NE; TokReg R10; TokComma; TokReg R1; TokEOF];
                                        [TokInstr6 TST; TokCond PL; TokReg R0; TokComma; TokReg R4; TokComma; TokInstr4 ROR_; TokLiteral 1; TokEOF];
                                        [TokInstr1 MOV; TokReg R0; TokComma; TokReg R1; TokComma; TokInstr5 RRX_; TokEOF]
                                    |]

        let parseBadTokenList =
                                    [|
                                        [TokNewLine; TokInstr1 MOV; TokLiteral 5; TokComma; TokReg R1; TokEOF];
                                        [TokReg R0; TokInstr1 MOV; TokComma; TokReg R1; TokEOF];
                                        [TokInstr3 ADD; TokReg R1; TokComma; TokReg R2; TokReg R3; TokEOF];
                                        [TokReg R14; TokEOF];
                                        [TokInstr3 ADC; TokS S; TokReg R0; TokComma; TokReg R1; TokComma; TokInstr4 LSL; TokLiteral 5; TokEOF];
                                        [TokInstr4 LSL; TokCond EQ; TokS S; TokReg R0; TokComma; TokLiteral 11; TokComma; TokLiteral 6; TokEOF];
                                        [TokInstr5 RRX_; TokS S; TokCond NE; TokReg R10; TokComma; TokReg R1; TokComma; TokCond NE; TokEOF];
                                        [TokInstr6 TST; TokCond PL; TokReg R0; TokComma; TokReg R4; TokComma; TokInstr5 RRX_; TokLiteral 1; TokEOF];
                                        [TokInstr3 ADD; TokReg R0; TokComma; TokReg R1; TokEOF]
                                    |]
        
        let rec tryGoodTests testList count = 
            if count < (Array.length testList) then  
                let outList = Parse testList.[count]
                let containsError = List.exists (fun a -> match a with | JLabel ("Failed Line") -> true | _ -> false ) outList
                if containsError then 
                    printfn "Test %A (\n%A\n) is bad input, expected good input. Instructions list = %A" count testList.[count] outList
                    count
                else
                    tryGoodTests testList (count+1)
            else
                count
        
        let rec tryBadTests testList count = 
            if count < (Array.length testList) then    
                let outList = Parse testList.[count]
                let containsError = List.exists (fun a -> match a with | JLabel ("Failed Line") -> true | _ -> false ) outList
                if containsError then 
                    tryBadTests testList (count+1)
                else
                    printfn "Test %A (\n%A\n) is good input, expected bad input. Instructions List = %A" count testList.[count] outList
                    count
            else
                count

        //perform valid input tests
        printfn "Running goodTests..."
        printfn "goodTests: passed %A/%A" (tryGoodTests parseGoodTokenList 0) (Array.length parseGoodTokenList)
        printfn "Running badTests..."
        printfn "badTests: passed %A/%A" (tryBadTests parseBadTokenList 0) (Array.length parseBadTokenList)

        let testList =  [
                            [TokInstr3 ADD; TokReg R1; TokComma; TokReg R2; TokComma; TokReg R3; TokNewLine];
                            [TokInstr1 MVN; TokReg R0; TokComma; TokLiteral 5; TokNewLine];
                            [TokInstr3 ADC; TokS S; TokReg R0; TokComma; TokReg R1; TokComma; TokLiteral 5; TokComma; TokNewLine];
                            [TokInstr4 LSL; TokS S; TokCond EQ; TokReg R0; TokComma; TokReg R0; TokComma; TokLiteral 11; TokNewLine];
                            [TokInstr5 RRX_; TokS S; TokCond NE; TokReg R10; TokComma; TokReg R1; TokNewLine];
                            [TokInstr6 TST; TokCond PL; TokReg R0; TokComma; TokReg R4; TokComma; TokInstr4 ROR_; TokLiteral 1; TokNewLine];
                            [TokInstr1 MOV; TokReg R0; TokComma; TokReg R1; TokComma; TokInstr5 RRX_; TokNewLine];
                            [TokLabel "Label"; TokNewLine]
                        ]

        let checkInstructionCount () = 
            //http://stackoverflow.com/questions/1123958/get-a-random-subset-from-a-set-in-f
            let rnd = new System.Random()
            let rec subset xs = 
                let removeAt n xs = ( Seq.item (n-1) xs, Seq.append (Seq.take (n-1) xs) (Seq.skip n xs) )
                match xs with 
                | [] -> []
                | _ -> let (rem, left) = removeAt (rnd.Next( List.length xs ) + 1) xs
                       let next = subset (List.ofSeq left)
                       if rnd.Next(2) = 0 then rem :: next else next
            let subList = subset testList
            let subTok = (List.concat subList) @ [TokEOF]
            let parseList = Parse subTok
            parseList.Length = subList.Length + 1

        printfn "\nChecking Instruction List lengths with FsCheck..."
        Check.Quick (checkInstructionCount())

        printfn "This is not working. No idea why:"
        printfn "%A" (Tokeniser.tokenise "ADD R1, R2, R3 \n MOV R2, R1")
        printfn "%A" ((Tokeniser.tokenise "ADD R1, R2, R3 \n MOV R2, R1") |> Parse)
        printfn "%A" (Parse [TokInstr3 ADD; TokReg R1; TokComma; TokReg R2; TokComma; TokReg R3; TokNewLine; TokInstr1 MOV; TokReg R2; TokComma; TokReg R1; TokEOF])
        printfn "Finished testParser..."