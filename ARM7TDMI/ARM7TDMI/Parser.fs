namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Pranav Prabhu

    Module: Parser

    Description: Parser which accepts a Token List from the Tokeniser and return an Instruction List to the AST,
                 Implements Monadic Parser Combinators and Railway Oriented Error Handling from ParserMonad.fs,
                 Testing of entire Parser included at the end.
    
    Guidance Sources: fsharpforfunandprofit.com, quanttec.com/fparsec/, vimeo.com/113707214, stackoverflow.com
   
*)

module Parser =

    open System
    open Common
    // Toolkit used for splitBy function
    open Toolkit
    open FsCheck
    open ParserMonad

/////////////////////////////////////// Token Lists ////////////////////////////////////////////////////   

    // A series of Token lists as the enumerator function given to us by TC failed in Fable 
    // Used with anyOf function to rapidly parse various token types
    // For some types included base type and a List.map function to map D.U.types to Token types
    // Effective dictionary for Parser Combinators 

    let tokenCondList = [TokCond(EQ); TokCond(NE); TokCond(CS); TokCond(HS); TokCond(CC); TokCond(LO); 
                         TokCond(MI); TokCond(PL); TokCond(VS); TokCond(VC); TokCond(HI); TokCond(LS); 
                         TokCond(GE); TokCond(LT); TokCond(GT); TokCond(LE); TokCond(AL); TokCond(NV);]

    let regList = [R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10; R11; R12; R13; R14; R15;]
    let tokenRegList = List.map TokReg regList

    let tokenInstrList1 = [TokInstr1(MOV); TokInstr1(MVN)]
    let tokenInstrList2 = [TokInstr2(ADR)]

    let instrList3 = [ADD; ADC; SUB; SBC; RSB; RSC; AND; EOR; BIC; ORR;]
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

    
    ////////////////////////////////// Primitive (+Near-Primitive) Token Parsers /////////////////////////////////////
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
        
    let pInstrDCD = 
        let parseTuple = pToken (TokDCD DCD) <?> "DCD Token"
        let tupleTransform t1 =  
            match t1 with 
            | TokDCD a -> a
        mapParse tupleTransform parseTuple

    ///////////////////////////////////////// Input Parser /////////////////////////////////////////////////////////////
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
        
        
     //////////////////////////////////////////////////// Operand Type ///////////////////////////////////////////////////

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

////////////////////////////////////////////////// AddressRegister Type ////////////////////////////////////////////
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
        let parseTuple =  pLBracket >>. pReg .>>. choice[pOffsetPost; pOffsetPre; pOffsetAddress;] <?> "Address Register"
        let tupleTransform (t1, t2) = {register=t1;offset = t2;}
        mapParse tupleTransform parseTuple          
          
////////////////////////////////////// String/Expression/Labels Types /////////////////////////////////////////////
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

 //////////////////////////////////////////// Final Instruction Parsers  ////////////////////////////////////////////////
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

    let instEOF = 
        let label = "End Of File Instruction"
        let tupleTransform = function
            | x -> JInstrEOF
        let instrLabelHold = pToken TokEOF <?> label
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
                            instEOF;
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

