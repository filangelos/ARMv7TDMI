namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Pranav Prabhu

    Module: Parser

    Description: Take in a Token List from Baron Khan's Tokeniser, Parses and return a List of InstrType or Error (and associated Error information)
    to AST for processing onwards to actual implementation. Parsing done using monadic parser combinators. 
    
    Sources: fsharpforfunandprofit.com, quanttec.com/fparsec/, vimeo.com/113707214, 
    
    Notes: 
    1) Initially tried to use FParsec Library, not compatible with Fable, 
       so self-implemented suite of required parsing tools and combinators

    2) Self-implemented (through research and guidance from above Sources) 
       parser structure not compatible with Fable (due to lambda in Parser<'T>), 
       but maintained for Command Line testing as too far progressed 
       
    3) Infix operators match those used in FParsec for implemented combinator functions thus making future reference easy.
*)

module Parser =

    open System
    open Common
    // Toolkit used for splitBy function
    open Toolkit
    open FsCheck
    
    // Position in the Array of Token Lists - InitState.lineList
    type Pos = {
        lineNo : int
        tokenNo : int
    }

    //The state of the Parser at Input
    type InitState = {
        lineList : List<Token>[]
        position : Pos
    }

    // Initialise Position
    let initPos = {lineNo=0; tokenNo=0;}

    /// Increment the tokenNo number
    let incrTok pos =  {pos with tokenNo=pos.tokenNo + 1}

    /// Increment the lineNo number and set tokenNo to 0
    let incrLine pos = {lineNo=pos.lineNo + 1; tokenNo=0}

    // Create a new InitState from a Token List
    let tokenToInit tokenLst = 
        //Use splitBy to split by TokNewLine Token
        // and return the an array of token lists
        let y = splitBy TokNewLine tokenLst true
        match y with
            | [] -> {lineList = [||]; position = initPos;}
            | _ ->  {lineList = List.toArray y; position=initPos;}

    // Return the current line or token signifying end of file (TokEOF)
    let currLine initState = 
        let linePos = initState.position.lineNo
        if linePos < initState.lineList.Length then
            initState.lineList.[linePos]
        else
            [TokEOF]

    // Decision function for nextToken return
    // 1) If lineNo >= lastLine -> return TokEOF
    // 2) If tokenNo < line length -> return token at tokPos and tokPos++
    // 3) If tokenNo = line length -> return TokNewLine, linePos++
    let decisionFunc (linePos, tokPos, input)= 
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
                let token = TokNewLine
                let newPos = incrLine input.position 
                let newState = {input with position=newPos}
                newState, Some token

    // Get the next token from the Initlist or return 
    // None if there are any else return End Options.
    // Then return the updated InitState
    let nextToken input =
        let linePos = input.position.lineNo
        let tokPos = input.position.tokenNo
        decisionFunc (linePos, tokPos, input)

    // String signifying that specific parser's components    
    type PLabel = string
    // String specifying the Error
    type PError = string

    // Position in the file being Parsed at any one time
    type PPosition = {
        currLine : List<Token>
        lineNo : int
        tokenNo : int
    }

    // Outcome of a Parser - either success or failure (with error message)
    type Outcome<'a> =
        | Success of 'a
        | Failure of PLabel * PError * PPosition

    // Parser Type
    type Parser<'T> = {
        //Parses InitState and returns success or failure
        parseFunc : (InitState -> Outcome<'T * InitState>)
        //Label of individual parser in pipeline
        pLabel:  PLabel
        }
        
    // Returns a PPosition for the next parser in the pipeline  
    let parserPosfromInitState initState = {
        currLine =  currLine initState
        lineNo = initState.position.lineNo
        tokenNo = initState.position.tokenNo
    }

    // Set Label for Parser
    let setLabel parser newLab = 
        // Inner function to update to change label to next in pipeline
        let innerFunc input = 
            let result = parser.parseFunc input
            match result with
            | Success s -> Success s 
            // If we fail, return label for new parser in pipeline
            | Failure (oldLab,err, parPos) -> Failure (newLab,err, parPos)
        // return new Parser
        {parseFunc=innerFunc; pLabel=newLab}

    // Infix operator for setting label
    let ( <?> ) = setLabel

    // Outer function that gets predicate to be matched and its label
    let satisfy pred lab =
        // inner function consumes next token in token list
        let innerFunc tokenLst=
            let remainInput, tokenOpt = nextToken tokenLst
            match tokenOpt with 
                //No token in list - return Failure
                | None -> let err = "No more input"
                          let pos = parserPosfromInitState tokenLst
                          printf "%A" (nextToken tokenLst)                                                              
                          Failure (lab,err,pos) 
                //Check if token matches predicate
                | Some first -> 
                    if pred first then
                        Success (first,remainInput)
                    else
                        let err = sprintf "Unexpected '%A'" first
                        let pos = parserPosfromInitState tokenLst
                        Failure (lab,err,pos)
        // return the parser
        {parseFunc=innerFunc; pLabel=lab}

    
    // Run a specific parser on a Token
    let runInput parser input = 
        parser.parseFunc input

    // Run successice parsers on a Token List
    let run parser inputTokenLst = 
        runInput parser (tokenToInit inputTokenLst)

    // Parse a specific Token defined by tokentoMatch
    let pToken tokenToMatch = 
        let pred tk = (tk = tokenToMatch) 
        let label = sprintf "%A" tokenToMatch 
        satisfy pred label 
 
    /// This is akin to a standard compose function >>, but takes a 
    /// function which produces a parser (func) , and a parser
    /// and then passes the output of parser into func, to create a new parser
    let bindParse func parser =
        let label = "Nothing"
        let innerFn input =
            let res1 = runInput parser input 
            match res1 with
            | Failure (label, err, pos) -> 
                // return error from parser1
                Failure (label, err, pos) 
            | Success (val1,remInput) ->
                // apply the function f to get a new a parser value
                let p2 = func val1
                // run parser with remaining input
                runInput p2 remInput
        {parseFunc =innerFn; pLabel=label}

    //Infix operator for binding
    let ( >>= ) parser func = bindParse func parser

    //Returns a function 
    let returnP x = 
        let innerFn input =
            // ignore the input and return x
            Success (x,input)
        // return the inner function
        {parseFunc =innerFn; pLabel= "Success"} 

    /// apply a function to the value inside a parser
    let mapParse f = bindParse (f >> returnP)

    /// Combine two parsers as "A andThen B"
    let (.>>.) p1 p2 =   
        let label = sprintf "%A andThen %A" (p1.pLabel) (p2.pLabel)      
        p1 >>= (fun p1Result -> p2 >>= (fun p2Result -> returnP (p1Result,p2Result) )) <?> label



    /// Combine two parsers as "A orElse B"
    let ( <|> )p1 p2 =
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

    /// Optional instance of parser P
    let opt p = 
        let some = mapParse Some p
        let none = returnP None
        some <|> none

    /// Return only the result of the left side parser
    let (.>>) p1 p2 = 
        // create a pair
        p1 .>>. p2 
        // then only keep the first value
        |> mapParse (fun (a,b) -> a) 

    /// Return result of the right side parser
    let (>>.) p1 p2 = 
        // create a pair
        p1 .>>. p2 
        // then only keep the second value
        |> mapParse (fun (a,b) -> b) 

    /// Return result of parser in middle
    let inBetween p1 p2 p3 = 
        p1 >>. p2 .>> p3
    let (>>%) parse t = mapParse (fun _ -> t) parse

    /// matches 0+ instances of Parser<'a>
    let zeroPlus parser = 
        let label = sprintf "0+ %s" (parser.pLabel)
        let rec pZeroPlus parser input =
            let initResult = runInput parser input 
            // match outcome with Failure or Success
            match initResult with
            // parse failure - return empty list
            | Failure (_) ->  ([],input)  
            // parse success - recursive call to get subsequent value
            | Success (initValue, firstParseOutcome) -> 
                let (nextValues,remainingInput) = 
                    pZeroPlus parser firstParseOutcome
                let values = initValue::nextValues
                (values,remainingInput) 
        let rec innerFn input =
            // parse the input -- always succeeds, so can be wrapped in Success
            Success (pZeroPlus parser input)
        {parseFunc=innerFn; pLabel=label}

    /// matches 1+ instances of the specified Parser<'a> - built using zeroPlus parser
    let onePlus p =         
        let label = sprintf "1+ %s" (p.pLabel)
        p >>= (fun h -> zeroPlus p >>= (fun tl -> returnP(h::tl))) <?> label

/////////////////////////////////////// Token Lists ////////////////////////////////////////////////////    
    let tokenCondList = [TokCond(EQ); TokCond(NE); TokCond(CS); TokCond(HS); TokCond(CC); TokCond(LO); TokCond(MI); TokCond(PL);
                            TokCond(VS); TokCond(VC); TokCond(HI); TokCond(LS); TokCond(GE); TokCond(LT); TokCond(GT); TokCond(LE);
                            TokCond(AL); TokCond(NV);]
    let regList = [R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10; R11; R12; R13; R14; R15;]

    let tokenRegList = List.map TokReg regList
    let tokenInstrList1 = [TokInstr1(MOV); TokInstr1(MVN)]
    let tokenInstrList2 = [TokInstr2(ADR)]
    let instrList3 = [ADD ; ADC ; SUB ; SBC ; RSB ; RSC ; AND; EOR ; BIC ; ORR;]
    let tokenInstrList3 = List.map TokInstr3 instrList3

    let instrList4 = [LSL; LSR; ASR; ROR_;]
    let tokenInstrList4 = List.map TokInstr4 instrList4
    let tokenInstrList5 = [TokInstr5(RRX_)]
    let instrList6 = [CMP ; CMN ; TST ; TEQ;]
    let tokenInstrList6 = List.map TokInstr6 instrList6

    let tokenInstrList7 = [TokInstr7(LDR); TokInstr7(STR);] 
    let tokenInstrList8 = [TokInstr8(LDM); TokInstr8(STM);]

    let tokenInstrList9 = [TokInstr9(B_); TokInstr9(BL);]

    let stackDirList = [IA ; IB ; DA ; DB ; ED ; FD ; EA ; FA;]

    let tokenStackDirList = List.map TokStackDir stackDirList

    ////////////////////////////////// Primitive (+Near-Primitive) Token Parsers /////////////////////////////////////////////////////////////////
    let pInstr1 =
        let parseTuple = anyOf tokenInstrList1 <?> "Type 1 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr1 a -> a
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple

    let pInstr2 =  
        let parseTuple = anyOf tokenInstrList2 <?> "Type 2 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr2 a -> a
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple
    let pInstr3 = 
        let parseTuple = anyOf tokenInstrList3 <?> "Type 3 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr3 a -> a
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple

    let pInstr4 = 
        let parseTuple = anyOf tokenInstrList4 <?> "Type 4 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr4 a -> a
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple

    let pInstr5 =
        let parseTuple = anyOf tokenInstrList5 <?> "Type 5 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr5 a -> a
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple

    let pInstr6 =
        let parseTuple = anyOf tokenInstrList6 <?> "Type 6 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr6 a -> a
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple

    let pInstr7 = 
        let parseTuple = anyOf tokenInstrList7 <?> "Type 7 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr7 a -> a
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple
    let pInstr8 = 
        let parseTuple = anyOf tokenInstrList8 <?> "Type 8 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr8 a -> a
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple

    let pInstr9 = 
        let parseTuple = anyOf tokenInstrList9 <?> "Type 9 Opcode"
        let tupleTransform(x) =
            match x with 
            | TokInstr9 a -> a
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple

    let pS = 
        let parseTuple = pToken (TokS S) <?> "S Type"
        let tupleTransform(x) =
            match x with 
            | TokS a -> a
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple

    let pB = 
        let parseTuple = pToken (TokB B) <?> "B Type"
        let tupleTransform(x) =
            match x with 
            | TokB a -> a
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple
    let pComma =
        let parseTuple = pToken TokComma <?> "Comma"
        parseTuple >>% TokComma
    let pCond = 
        let parseTuple = anyOf tokenCondList <?> "Conditional Code"
        let tupleTransform(x) =
            match x with 
            | TokCond a -> a 
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple
    let pReg = 
        let parseTuple = anyOf tokenRegList <?> "Register"
        let tupleTransform(x) =
            match x with 
            | TokReg a -> a 
            | _ -> failwith "Impossible"
        mapParse tupleTransform parseTuple
    let pRegComma = 
            let parseTuple = pReg .>>. pComma <?> "Register followed by Comma"
            let tupleTransform (t1,t2) = 
                match t1, t2 with  
                | a, TokComma-> a
                | _ -> failwith "Impossible"
            mapParse tupleTransform parseTuple

    let pStackDir = 
            let parseTuple = anyOf tokenStackDirList <?> "Stack Direction"
            let tupleTransform t1 =  
                match t1 with 
                | TokStackDir a -> a
            mapParse tupleTransform parseTuple

    ///////////////////////////////////////// Input Type /////////////////////////////////////////////////////////////
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
        mapParse tupleTransform parseTuple

    let pRegtoInput = 
        let parserTuple = pReg <?> "Register" 
        let tupleTransform (t) = 
            match t with
            | a -> ID a 
        mapParse tupleTransform parserTuple

    let pInput = 
        let parseTuple = pLiteral <|> pRegtoInput <?> "Register or Literal Int"
        let tupleTransform (t1) = 
            match t1 with  
            |  ID a -> ID a
            | Literal x -> Literal x
        mapParse tupleTransform parseTuple
        
        
     ///////////////////////////////////////////////// OPERAND ATTEMPT //////////////////////////////////////////////

    let pShiftDirection4 =
        let parseTuple = pComma >>. pInstr4 .>>. pInt <?> "Shift Direction (Int)"
        let tupleTransform (t1, t2) = 
            match t1, t2 with
            | LSL, TokLiteral a -> Left a 
            | LSR, TokLiteral a-> RightL a
            | ASR, TokLiteral a -> RightA a
            | ROR_,TokLiteral a-> ROR a
        mapParse tupleTransform parseTuple 

    let pShiftDirection5 =
        let parseTuple = pComma >>. pInstr5 <?> "Shift Direction RRX"
        let tupleTransform (t1) = 
            match t1 with
            | RRX_ -> RRX
        mapParse tupleTransform parseTuple 

    let pOp =
        let parseTuple = pInput .>>. opt(pShiftDirection5 <|> pShiftDirection4)  <?> "Operand"
        let tupleTransform (t1, t2) = 
            match t1, t2 with
            | ID x, y  -> match y with 
                                | None -> Operand (ID x, NoShift)
                                | Some y -> Operand(ID x, y)
            | Literal a, b ->  match b with 
                                    | None -> Operand (Literal a, NoShift)
                                    | Some b -> Operand(Literal a, b)
        mapParse tupleTransform parseTuple

////////////////////////////////////////////////// AddressRegister Type ////////////////////////////////////////////////
    let pLBracket = pToken TokSquareLeft <?> "LeftBracket"
    let pRBracket = pToken TokSquareRight <?> "RightBracket"
    let pExclam = pToken TokExclam <?> "Exclamation Mark"
    let pExclamBool = 
        let parseTuple = opt pExclam <?> "Optional Exclamation Mark"
        let tupleTransform t = 
            match t with
            | Some x -> true
            | None -> false
        mapParse tupleTransform parseTuple

    let pCommaOffset =     
        let parseTuple = pComma .>>. pInput <?> "Offset Integer"
        let tupleTransform (t1,t2) = t2 
        mapParse tupleTransform parseTuple

    let pOffsetAddress = 
        let parseTuple =  opt pCommaOffset .>> pRBracket <?> "Offset Addressing"
        let tupleTransform t1 =
            match t1 with
            | Some a -> TempOffset a  
            | None -> NoOffset
        mapParse tupleTransform parseTuple

    let pOffsetPre = 
        let parseTuple =  pCommaOffset .>> pComma .>> pRBracket .>> pExclam <?> "Pre-indexed Offset Addressing"
        let tupleTransform t1 = PreIndex t1
        mapParse tupleTransform parseTuple

    let pOffsetPost = 
        let parseTuple =  pRBracket >>. pCommaOffset <?> "Post-indexed Offset Addressing"
        let tupleTransform t1 = PostIndex t1
        mapParse tupleTransform parseTuple

    let pAddressRegister =
        let parseTuple =  pLBracket >>. pReg .>>. choice [pOffsetPost; pOffsetPre; pOffsetAddress;]  <?> "Address Register"
        let tupleTransform (t1, t2) = {register=t1;offset = t2;}
        mapParse tupleTransform parseTuple          
          
// TokReg(R10); TokComma; TokLiteral(10); TokComma; TokSquareRight; TokExclam;
//////////////////////////////////////// String/Expression/Labels Parsers ////////////////////////////////////////////////
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
        mapParse tupleTransform parseTuple

    let pExpr = 
        let parseTuple = (pInt <|> pString) <?> "Shift Direction"
        let tupleTransform (t1) = 
            match t1 with
            | TokLiteral a -> Number a
            | TokLabel a -> Lab a
        mapParse tupleTransform parseTuple          

 //////////////////////////////////////////// Final Instruction Types /////////////////////////////////////////////////////
    let instType1 = 
        let label = "Instruction Type 1"
        let tupleTransform = function
            | x -> JInstr1(x)
        let instr1Hold = pInstr1 .>>. opt pS .>>. opt pCond .>>. pRegComma .>>. pOp <?> label
        mapParse tupleTransform instr1Hold

    let instType2 = 
        let label = "Instruction Type 2"
        let tupleTransform = function
            | x -> JInstr2(x)
        let instr2Hold = pInstr2 .>>. opt pCond .>>. pRegComma .>>. pExpr <?> label
        mapParse tupleTransform instr2Hold

    let instType3 =
        let label = "Instruction Type 3"
        let tupleTransform = function
            | x -> JInstr3(x)
        let instr3Hold = pInstr3 .>>. opt pS .>>. opt pCond .>>. pRegComma .>>. pRegComma .>>. pOp  <?> label
        mapParse tupleTransform instr3Hold

    let instType4 = 
        let label = "Instruction Type 4"
        let tupleTransform = function
            | x -> JInstr4(x)
        let instr4Hold = pInstr4 .>>. opt pS .>>. opt pCond .>>. pRegComma .>>. pRegComma .>>. pInput <?> label
        mapParse tupleTransform instr4Hold

    let instType5 = 
        let label = "Instruction Type 5"
        let tupleTransform = function
            | x -> JInstr5(x)
        let instr5Hold = pInstr5 .>>. opt pS .>>. opt pCond .>>. pRegComma .>>. pInput <?> label
        mapParse tupleTransform instr5Hold

    let instType6 = 
        let label = "Instruction Type 6"
        let tupleTransform = function
            | x -> JInstr6(x)
        let instr6Hold = pInstr6 .>>. opt pCond .>>. pRegComma .>>. pOp <?> label
        mapParse tupleTransform instr6Hold

    let instType7 = 
        let label = "Instruction Type 7"
        let tupleTransform = function
            | x -> JInstr7(x)
        let instr7Hold = pInstr7 .>>. opt pB .>>. opt pCond .>>. pRegComma .>>. pAddressRegister <?> label
        mapParse tupleTransform instr7Hold
    let instType8 = 
            let label = "Instruction Type 8"
            let tupleTransform = function
                | x -> JInstr8(x)
            let instr8Hold = pInstr8 .>>. pStackDir .>>. opt pCond .>>. pReg .>>. pExclamBool .>>. onePlus pReg <?> label
            mapParse tupleTransform instr8Hold

    let instType9 = 
            let label = "Instruction Type 9"
            let tupleTransform = function
                | x -> JInstr9(x)
            let instr9Hold = pInstr9 .>>. opt pCond .>>. pLabel <?> label
            mapParse tupleTransform instr9Hold

    let instTypeLabel = 
        let label = "Instruction Type Label"
        let tupleTransform = function
            | x -> JLabel(x)
        let instrLabelHold = pLabel <?> label
        mapParse tupleTransform instrLabelHold

//////////////////////////////////////// Final Choice + External Parse Instruction////////////////////////////////
    let parseInstr = choice [
                            instType1;
                            instType2;
                            instType3;
                            instType4;
                            instType5;
                            instType6;
                            instType7;
                            instType8;
                            instType9;
                            instTypeLabel;
                            ]
                            

    let Parse (tokenLstLst: Token List) : Instr List = 
        let z outcome = match outcome with 
                        | Success(value, input) -> value
                        | Failure(label,err,parPos) -> let errorLine = parPos.currLine
                                                       let tokPos = parPos.tokenNo
                                                       let failureLine = sprintf "%*s^%s" tokPos "" err
                                                       JError(sprintf "TokenNo:%i Error parsing %A\n %A\n %s" tokPos label errorLine failureLine)
        let y = splitBy TokNewLine tokenLstLst true                           
        let x = List.map (fun j -> run parseInstr j) 
        let u = List.map z 
        y |> x |> u

(**************************************************TESTING***********************************************************)

    // Helper function to print Outcome for testing
    let printOutcome outcome =
        match outcome with
        | Success (value,input) -> 
            printfn "%A" value
        | Failure (label, err, parPos) -> 
            let errorLine = parPos.currLine
            let tokPos = parPos.tokenNo
            let linePos = parPos.lineNo
            let failureLine = sprintf "%*s^%s" tokPos "" err
            //convert Error Line to printable solution for testing
            printfn "Line:%i - TokenNo:%i Error parsing %A\n %A\n %s" linePos tokPos label errorLine failureLine

    // Helper function to ensure all token's are read for testing 
    let rec readAllTokens input =
        [
            let remainingInput,tokenOpt = nextToken input 
            match tokenOpt with
            | None -> 
                // end of input
                ()
            | Some tk -> 
                // return first token
                yield tk
                // return the remaining token
                yield! readAllTokens remainingInput
        ]

    let testInstrType1List4 = [TokInstr1(MVN); TokS(S); TokCond(EQ); TokReg(R0); TokLiteral(10); TokEOF;]

    let testInstrType1ListFail1 = [TokInstr1(MVN); TokError("R20"); TokLiteral(10); TokNewLine; TokInstr1(MVN); TokError("R16"); TokError("R20");TokEOF;]

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

        let parseGoodTokenList2 =   [|
                                        [TokInstr1(MOV); TokReg(R0); TokLiteral(10);TokNewLine;TokInstr1(MVN); TokS(S); TokReg(R0); TokReg(R1);TokEOF]
                                        [TokInstr2(ADR); TokCond(EQ); TokReg(R12); TokComma; TokLabel("LAB");]
                                        [TokInstr7(LDR); TokB(B); TokCond(EQ);TokReg(R10);TokSquareLeft;  TokReg(R10); TokComma; TokLiteral(10); TokComma; TokSquareRight; TokExclam;]
                                        [TokInstr7(STR); TokB(B); TokCond(EQ);TokReg(R10);TokSquareLeft;  TokReg(R10); TokComma; TokLiteral(10); TokComma; TokSquareRight; TokExclam;]
                                        [TokInstr1(MVN); TokS(S); TokCond(EQ); TokReg(R0); TokLiteral(10); TokEOF;]      
                                    |]

        let parseBadTokenList =
                                    [|
                                        [TokNewLine; TokInstr1 MOV; TokLiteral 5; TokComma; TokReg R1; TokEOF];
                                        [TokReg R0; TokInstr1 MOV; TokComma; TokReg R1; TokEOF];
                                        [TokInstr3 ADD; TokReg R1; TokComma; TokReg R2; TokReg R3; TokEOF];
                                        [TokReg R14; TokEOF];
                                        [TokInstr3 ADC; TokS S; TokReg R0; TokComma; TokReg R1; TokComma; TokInstr4 LSL; TokLiteral 5; TokEOF];
                                        [TokInstr4 LSL; TokCond EQ; TokS S; TokReg R0; TokComma; TokLiteral 11; TokComma; TokLiteral 6; TokEOF];
                                        [TokInstr3 ADD; TokReg R0; TokComma; TokReg R1; TokEOF]
                                    |]
    
        
        let rec tryGoodTests testList count = 
            if count < (Array.length testList) then  
                let outList = Parse testList.[count]
                let containsError = List.exists (fun a -> match a with | JError _ -> true | _ -> false ) outList
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
                let containsError = List.exists (fun a -> match a with | JError _ -> true | _ -> false ) outList
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


        printfn "Finished testParser..."

