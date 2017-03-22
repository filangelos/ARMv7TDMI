namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: Parser

    Description: Match Parser - Alternative for compatability with Fable.
                 Due to Github issue #756 (https://github.com/fable-compiler/Fable/issues/756)
                 this parser is implemented in a day to overcome transpilation issues.
*)

module Parser =

    open System
    open Common
    open Toolkit
    open Fable.Core.JsInterop

    let Parse (tokenLstLst: Token List) : Instr List =
        let instructions = splitBy TokNewLine tokenLstLst
        let lines = [1 .. (List.length instructions)]
        List.zip lines instructions
        |> List.map ( fun (line,tokList) -> 
            match tokList with
            // when end of file do a dummy instruction
            | [TokEOF] -> JInstr1 ((((MOV,None),None),R0),Operand(ID R0, NoShift) )
            // add label
            | [TokLabel label] -> JLabel label
            // TokInstr1
            | (TokInstr1 instr)::t ->
                match t with
                // TokInstr1 |> TokS
                | (TokS s)::t ->
                    match t with
                    // TokInstr1 |> TokS |> TokCond
                    | (TokCond cond)::t ->
                        match t with
                        // TokInstr1 |> TokS |> TokCond |> TokReg
                        | (TokReg rd)::t ->
                            match t with
                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma
                            | TokComma::t ->
                                match t with
                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg
                                | (TokReg rn)::t ->
                                    match t with
                                    | [] -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn, NoShift) )
                                    // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma
                                    | TokComma::t ->
                                        match t with
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4
                                        | (TokInstr4 shift)::t ->
                                            match t with
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokReg
                                            | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                            | [(TokLiteral exp)] -> 
                                                match shift with
                                                | LSL  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn,Left exp))
                                                | LSR  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn,RightL exp))
                                                | ASR  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn,RightA exp))
                                                | ROR_ -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn,ROR exp))
                                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                        | [(TokInstr5 shift)] -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn,RRX))
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral
                                | (TokLiteral l)::t ->
                                    match t with
                                    | [] -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l, NoShift) )
                                    // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma
                                    | TokComma::t ->
                                        match t with
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4
                                        | (TokInstr4 shift)::t ->
                                            match t with
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokReg
                                            | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                            | (TokLiteral exp)::t -> 
                                                match shift with
                                                | LSL  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l,Left exp))
                                                | LSR  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l,RightL exp))
                                                | ASR  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l,RightA exp))
                                                | ROR_ -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l,ROR exp))
                                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                        | [(TokInstr5 shift)] -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l,RRX))
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                    // TokInstr1 |> TokS |> TokReg
                    | (TokReg rd)::t ->
                        match t with
                        // TokInstr1 |> TokS |> TokReg |> TokComma
                        | TokComma::t ->
                            match t with
                            // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg
                            | (TokReg rn)::t ->
                                match t with
                                | [] -> JInstr1 ((((instr,Some s),None),rd),Operand(ID rn, NoShift) )
                                // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma
                                | TokComma::t ->
                                    match t with
                                    // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4
                                    | (TokInstr4 shift)::t ->
                                        match t with
                                        // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokReg
                                        | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                        // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                        | [(TokLiteral exp)] -> 
                                            match shift with
                                            | LSL  -> JInstr1 ((((instr,Some s),None),rd),Operand(ID rn,Left exp))
                                            | LSR  -> JInstr1 ((((instr,Some s),None),rd),Operand(ID rn,RightL exp))
                                            | ASR  -> JInstr1 ((((instr,Some s),None),rd),Operand(ID rn,RightA exp))
                                            | ROR_ -> JInstr1 ((((instr,Some s),None),rd),Operand(ID rn,ROR exp))
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                    // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                    | [(TokInstr5 shift)] -> JInstr1 ((((instr,Some s),None),rd),Operand(ID rn,RRX))
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                            // TokInstr1 |> TokS |> TokReg |> TokComma |> TokLiteral
                            | (TokLiteral l)::t ->
                                match t with
                                | [] -> JInstr1 ((((instr,Some s),None),rd),Operand(Literal l, NoShift) )
                                // TokInstr1 |> TokS |> TokReg |> TokComma |> TokLiteral |> TokComma
                                | TokComma::t ->
                                    match t with
                                    // TokInstr1 |> TokS |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4
                                    | (TokInstr4 shift)::t ->
                                        match t with
                                        // TokInstr1 |> TokS |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokReg
                                        | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                        // TokInstr1 |> TokS |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                        | (TokLiteral exp)::t -> 
                                            match shift with
                                            | LSL  -> JInstr1 ((((instr,Some s),None),rd),Operand(Literal l,Left exp))
                                            | LSR  -> JInstr1 ((((instr,Some s),None),rd),Operand(Literal l,RightL exp))
                                            | ASR  -> JInstr1 ((((instr,Some s),None),rd),Operand(Literal l,RightA exp))
                                            | ROR_ -> JInstr1 ((((instr,Some s),None),rd),Operand(Literal l,ROR exp))
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                    // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                    | [(TokInstr5 shift)] -> JInstr1 ((((instr,Some s),None),rd),Operand(Literal l,RRX))
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                // TokInstr1 |> TokCond
                | (TokCond cond)::t -> 
                    match t with
                    // TokInstr1 |> TokCond |> TokReg
                    | (TokReg rd)::t ->
                        match t with
                        // TokInstr1 |> TokCond |> TokReg |> TokComma
                        | TokComma::t ->
                            match t with
                            // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokReg
                            | (TokReg rn)::t ->
                                match t with
                                | [] -> JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn, NoShift) )
                                // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma
                                | TokComma::t ->
                                    match t with
                                    // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4
                                    | (TokInstr4 shift)::t ->
                                        match t with
                                        // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokReg
                                        | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                        // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                        | [(TokLiteral exp)] -> 
                                            match shift with
                                            | LSL  -> JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn,Left exp))
                                            | LSR  -> JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn,RightL exp))
                                            | ASR  -> JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn,RightA exp))
                                            | ROR_ -> JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn,ROR exp))
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                    // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                    | [(TokInstr5 shift)] -> JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn,RRX))
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                            // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokLiteral
                            | (TokLiteral l)::t ->
                                match t with
                                | [] -> JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l, NoShift) )
                                // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma
                                | TokComma::t ->
                                    match t with
                                    // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4
                                    | (TokInstr4 shift)::t ->
                                        match t with
                                        // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokReg
                                        | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                        // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                        | (TokLiteral exp)::t -> 
                                            match shift with
                                            | LSL  -> JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l,Left exp))
                                            | LSR  -> JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l,RightL exp))
                                            | ASR  -> JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l,RightA exp))
                                            | ROR_ -> JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l,ROR exp))
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                    // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                    | [(TokInstr5 shift)] -> JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l,RRX))
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                // TokInstr1 |> TokReg
                | (TokReg rd)::t -> 
                    match t with
                    // TokInstr1 |> TokReg |> TokComma
                    | TokComma::t ->
                        match t with
                        // TokInstr1 |> TokReg |> TokComma |> TokReg
                        | (TokReg rn)::t ->
                            match t with
                            | [] -> JInstr1 ((((instr,None),None),rd),Operand(ID rn, NoShift) )
                            // TokInstr1 |> TokReg |> TokComma |> TokReg |> TokComma
                            | TokComma::t ->
                                match t with
                                // TokInstr1 > TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4
                                | (TokInstr4 shift)::t ->
                                    match t with
                                    // TokInstr1 > TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokReg
                                    | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                    // TokInstr1 > TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                    | [(TokLiteral exp)] -> 
                                        match shift with
                                        | LSL  -> JInstr1 ((((instr,None),None),rd),Operand(ID rn,Left exp))
                                        | LSR  -> JInstr1 ((((instr,None),None),rd),Operand(ID rn,RightL exp))
                                        | ASR  -> JInstr1 ((((instr,None),None),rd),Operand(ID rn,RightA exp))
                                        | ROR_ -> JInstr1 ((((instr,None),None),rd),Operand(ID rn,ROR exp))
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                // TokInstr1 |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                | [(TokInstr5 shift)] -> JInstr1 ((((instr,None),None),rd),Operand(ID rn,RRX))
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                        // TokInstr1 |> TokReg |> TokComma |> TokLiteral
                        | (TokLiteral l)::t ->
                            match t with
                            | [] -> JInstr1 ((((instr,None),None),rd),Operand(Literal l, NoShift) )
                            // TokInstr1 |> TokReg |> TokComma |> TokLiteral |> TokComma
                            | TokComma::t ->
                                match t with
                                // TokInstr1 |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4
                                | (TokInstr4 shift)::t ->
                                    match t with
                                    // TokInstr1 > TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokReg
                                    | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                    // TokInstr1 > TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                    | (TokLiteral exp)::t -> 
                                        match shift with
                                        | LSL  -> JInstr1 ((((instr,None),None),rd),Operand(Literal l,Left exp))
                                        | LSR  -> JInstr1 ((((instr,None),None),rd),Operand(Literal l,RightL exp))
                                        | ASR  -> JInstr1 ((((instr,None),None),rd),Operand(Literal l,RightA exp))
                                        | ROR_ -> JInstr1 ((((instr,None),None),rd),Operand(Literal l,ROR exp))
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                // TokInstr1 > TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                | [(TokInstr5 shift)] -> JInstr1 ((((instr,None),None),rd),Operand(Literal l,RRX))
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                | _ -> JError (sprintf "Parsing Error: Unexpected token %A after %A at line %i" (List.head t)?Fields instr?Case line )
            // TokInstr2
            | (TokInstr2 instr)::t -> 
                match t with
                // TokInstr2 |> TokCond
                | (TokCond cond)::t -> 
                    match t with
                    // TokInstr2 |> TokCond |> TokReg
                    | (TokReg rd)::t ->
                        match t with
                        // TokInstr2 |> TokCond |> TokReg |> TokComma
                        | TokComma::t ->
                            match t with
                            // TokInstr2 |> TokCond |> TokReg |> TokComma |> TokLabel
                            | (TokLabel lab)::t ->
                                JInstr2 (((instr,Some cond),rd), Lab lab)
                            // TokInstr2 |> TokCond |> TokReg |> TokComma |> TokLiteral
                            | (TokLiteral num)::t ->
                                JInstr2 (((instr,Some cond),rd), Number num)
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an expression at line %i" (List.head t)?Fields line )
                        | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Fields line )
                // TokInstr2 |> TokReg
                | (TokReg rd)::t ->
                    match t with
                    // TokInstr2 |> TokReg |> TokComma
                    | TokComma::t ->
                        match t with
                        // TokInstr2 |> TokReg |> TokComma |> TokLabel
                        | (TokLabel lab)::t ->
                            JInstr2 (((instr,None),rd), Lab lab)
                        // TokInstr2 |> TokReg |> TokComma |> TokLiteral
                        | (TokLiteral num)::t ->
                            JInstr2 (((instr,None),rd), Number num)
                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an expression at line %i" (List.head t)?Fields line )
                    | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, after %A at line %i" (List.head t)?Fields instr?Case line )
            // TokInstr3
            | (TokInstr3 instr)::t -> 
                match t with
                // TokInstr3 |> TokS
                | (TokS s)::t ->
                    match t with
                    // TokInstr3 |> TokS |> TokCond
                    | (TokCond cond)::t ->
                        match t with
                        // TokInstr3 |> TokS |> TokCond |> TokReg
                        | (TokReg rd)::t ->
                            match t with
                            // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma
                            | TokComma::t ->
                                match t with
                                // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg
                                | (TokReg re)::t ->
                                    match t with
                                    | TokComma::t ->
                                        match t with
                                        // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg
                                        | (TokReg rn)::t ->
                                            match t with
                                            | [] -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(ID rn, NoShift) )
                                            // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma
                                            | TokComma::t ->
                                                match t with
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4
                                                | (TokInstr4 shift)::t ->
                                                    match t with
                                                    // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokReg
                                                    | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                                    // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                                    | [(TokLiteral exp)] -> 
                                                        match shift with
                                                        | LSL  -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(ID rn,Left exp))
                                                        | LSR  -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(ID rn,RightL exp))
                                                        | ASR  -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(ID rn,RightA exp))
                                                        | ROR_ -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(ID rn,ROR exp))
                                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                                | [(TokInstr5 shift)] -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(ID rn,RRX))
                                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral
                                        | (TokLiteral l)::t ->
                                            match t with
                                            | [] -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(Literal l, NoShift) )
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma
                                            | TokComma::t ->
                                                match t with
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4
                                                | (TokInstr4 shift)::t ->
                                                    match t with
                                                    // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokReg
                                                    | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                                    // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                                    | (TokLiteral exp)::t -> 
                                                        match shift with
                                                        | LSL  -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(Literal l,Left exp))
                                                        | LSR  -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(Literal l,RightL exp))
                                                        | ASR  -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(Literal l,RightA exp))
                                                        | ROR_ -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(Literal l,ROR exp))
                                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                                | [(TokInstr5 shift)] -> JInstr3 (((((instr,Some s),Some cond),rd),re),Operand(Literal l,RRX))
                                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                    // TokInstr3 |> TokS |> TokReg
                    | (TokReg rd)::t ->
                        match t with
                        // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma
                        | TokComma::t ->
                            match t with
                            // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg
                            | (TokReg re)::t ->
                                match t with
                                | TokComma::t ->
                                    match t with
                                    // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg
                                    | (TokReg rn)::t ->
                                        match t with
                                        | [] -> JInstr3 (((((instr,Some s),None),rd),re),Operand(ID rn, NoShift) )
                                        // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma
                                        | TokComma::t ->
                                            match t with
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4
                                            | (TokInstr4 shift)::t ->
                                                match t with
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokReg
                                                | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                                | [(TokLiteral exp)] -> 
                                                    match shift with
                                                    | LSL  -> JInstr3 (((((instr,Some s),None),rd),re),Operand(ID rn,Left exp))
                                                    | LSR  -> JInstr3 (((((instr,Some s),None),rd),re),Operand(ID rn,RightL exp))
                                                    | ASR  -> JInstr3 (((((instr,Some s),None),rd),re),Operand(ID rn,RightA exp))
                                                    | ROR_ -> JInstr3 (((((instr,Some s),None),rd),re),Operand(ID rn,ROR exp))
                                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                            | [(TokInstr5 shift)] -> JInstr3 (((((instr,Some s),None),rd),re),Operand(ID rn,RRX))
                                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                                    // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral
                                    | (TokLiteral l)::t ->
                                        match t with
                                        | [] -> JInstr3 (((((instr,Some s),None),rd),re),Operand(Literal l, NoShift) )
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma
                                        | TokComma::t ->
                                            match t with
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4
                                            | (TokInstr4 shift)::t ->
                                                match t with
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokReg
                                                | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                                | (TokLiteral exp)::t -> 
                                                    match shift with
                                                    | LSL  -> JInstr3 (((((instr,Some s),None),rd),re),Operand(Literal l,Left exp))
                                                    | LSR  -> JInstr3 (((((instr,Some s),None),rd),re),Operand(Literal l,RightL exp))
                                                    | ASR  -> JInstr3 (((((instr,Some s),None),rd),re),Operand(Literal l,RightA exp))
                                                    | ROR_ -> JInstr3 (((((instr,Some s),None),rd),re),Operand(Literal l,ROR exp))
                                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                            | [(TokInstr5 shift)] -> JInstr3 (((((instr,Some s),None),rd),re),Operand(Literal l,RRX))
                                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                // TokInstr1 |> TokCond
                | (TokCond cond)::t -> 
                    match t with
                    // TokInstr3 |> TokCond |> TokReg
                    | (TokReg rd)::t ->
                        match t with
                        // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma
                        | TokComma::t ->
                            match t with
                            // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg
                            | (TokReg re)::t ->
                                match t with
                                | TokComma::t ->
                                    match t with
                                    // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg
                                    | (TokReg rn)::t ->
                                        match t with
                                        | [] -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(ID rn, NoShift) )
                                        // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma
                                        | TokComma::t ->
                                            match t with
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4
                                            | (TokInstr4 shift)::t ->
                                                match t with
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokReg
                                                | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                                | [(TokLiteral exp)] -> 
                                                    match shift with
                                                    | LSL  -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(ID rn,Left exp))
                                                    | LSR  -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(ID rn,RightL exp))
                                                    | ASR  -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(ID rn,RightA exp))
                                                    | ROR_ -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(ID rn,ROR exp))
                                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                            | [(TokInstr5 shift)] -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(ID rn,RRX))
                                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                                    // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral
                                    | (TokLiteral l)::t ->
                                        match t with
                                        | [] -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(Literal l, NoShift) )
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma
                                        | TokComma::t ->
                                            match t with
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4
                                            | (TokInstr4 shift)::t ->
                                                match t with
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokReg
                                                | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                                | (TokLiteral exp)::t -> 
                                                    match shift with
                                                    | LSL  -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(Literal l,Left exp))
                                                    | LSR  -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(Literal l,RightL exp))
                                                    | ASR  -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(Literal l,RightA exp))
                                                    | ROR_ -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(Literal l,ROR exp))
                                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                            | [(TokInstr5 shift)] -> JInstr3 (((((instr,None),Some cond),rd),re),Operand(Literal l,RRX))
                                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                // TokInstr1 |> TokReg
                | (TokReg rd)::t ->
                    match t with
                    // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma
                    | TokComma::t ->
                        match t with
                        // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg
                        | (TokReg re)::t ->
                            match t with
                            | TokComma::t ->
                                match t with
                                // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg
                                | (TokReg rn)::t ->
                                    match t with
                                    | [] -> JInstr3 (((((instr,None),None),rd),re),Operand(ID rn, NoShift) )
                                    // TokInstr3 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma
                                    | TokComma::t ->
                                        match t with
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4
                                        | (TokInstr4 shift)::t ->
                                            match t with
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokReg
                                            | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                            | [(TokLiteral exp)] -> 
                                                match shift with
                                                | LSL  -> JInstr3 (((((instr,None),None),rd),re),Operand(ID rn,Left exp))
                                                | LSR  -> JInstr3 (((((instr,None),None),rd),re),Operand(ID rn,RightL exp))
                                                | ASR  -> JInstr3 (((((instr,None),None),rd),re),Operand(ID rn,RightA exp))
                                                | ROR_ -> JInstr3 (((((instr,None),None),rd),re),Operand(ID rn,ROR exp))
                                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                        | [(TokInstr5 shift)] -> JInstr3 (((((instr,None),None),rd),re),Operand(ID rn,RRX))
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral
                                | (TokLiteral l)::t ->
                                    match t with
                                    | [] -> JInstr3 (((((instr,None),None),rd),re),Operand(Literal l, NoShift) )
                                    // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma
                                    | TokComma::t ->
                                        match t with
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4
                                        | (TokInstr4 shift)::t ->
                                            match t with
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokReg
                                            | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                            | (TokLiteral exp)::t -> 
                                                match shift with
                                                | LSL  -> JInstr3 (((((instr,None),None),rd),re),Operand(Literal l,Left exp))
                                                | LSR  -> JInstr3 (((((instr,None),None),rd),re),Operand(Literal l,RightL exp))
                                                | ASR  -> JInstr3 (((((instr,None),None),rd),re),Operand(Literal l,RightA exp))
                                                | ROR_ -> JInstr3 (((((instr,None),None),rd),re),Operand(Literal l,ROR exp))
                                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                        | [(TokInstr5 shift)] -> JInstr3 (((((instr,None),None),rd),re),Operand(Literal l,RRX))
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
            // TokInstr4
            | (TokInstr4 instr)::t -> JError "unimplemented"
            // TokInstr5
            | (TokInstr5 instr)::t -> 
                match t with
                // TokInstr5 |> TokS
                | (TokS s)::t ->
                    match t with
                    // TokInstr5 |> TokS |> TokCond
                    | (TokCond cond)::t ->
                        match t with
                        // TokInstr5 |> TokS |> TokCond |> TokReg
                        | (TokReg rd)::t ->
                            match t with
                            // TokInstr5 |> TokS |> TokCond |> TokReg |> TokComma
                            | TokComma::t ->
                                match t with
                                // TokInstr5 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg
                                | (TokReg re)::t ->
                                    match t with
                                    | [] -> JInstr5 ((((instr,Some s),Some cond),rd),ID re)
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, at line %i" (List.head t)?Case line )
                                // TokInstr5 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral
                                | (TokLiteral l)::t ->
                                    match t with
                                    | [] -> JInstr5 ((((instr,Some s),Some cond),rd),Literal l)
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                    // TokInstr5 |> TokS |> TokReg
                    | (TokReg rd)::t ->
                        match t with
                        // TokInstr5 |> TokS |> TokReg |> TokComma
                        | TokComma::t ->
                            match t with
                            // TokInstr5 |> TokS |> TokReg |> TokComma |> TokReg
                            | (TokReg re)::t ->
                                match t with
                                | [] -> JInstr5 ((((instr,Some s),None),rd),ID re)
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, at line %i" (List.head t)?Case line )
                            // TokInstr5 |> TokS |> TokReg |> TokComma |> TokLiteral
                            | (TokLiteral l)::t ->
                                match t with
                                | [] -> JInstr5 ((((instr,Some s),None),rd),Literal l)
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                // TokInstr5 |> TokCond
                | (TokCond cond)::t -> 
                    match t with
                    // TokInstr5 |> TokCond |> TokReg
                    | (TokReg rd)::t ->
                        match t with
                        // TokInstr5 |> TokCond |> TokReg |> TokComma
                        | TokComma::t ->
                            match t with
                            // TokInstr5 |> TokCond |> TokReg |> TokComma |> TokReg
                            | (TokReg re)::t ->
                                match t with
                                | [] -> JInstr5 ((((instr,None),Some cond),rd),ID re)
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, at line %i" (List.head t)?Case line )
                            // TokInstr5 |> TokCond |> TokReg |> TokComma |> TokLiteral
                            | (TokLiteral l)::t ->
                                match t with
                                | [] -> JInstr5 ((((instr,None),Some cond),rd),Literal l )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                // TokInstr5 |> TokReg
                | (TokReg rd)::t -> 
                    match t with
                    // TokInstr5 |> TokReg |> TokComma
                    | TokComma::t ->
                        match t with
                        // TokInstr5 |> TokReg |> TokComma |> TokReg
                        | (TokReg re)::t ->
                            match t with
                            | [] -> JInstr5 ((((instr,None),None),rd),ID re)
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, at line %i" (List.head t)?Case line )
                        // TokInstr5 |> TokReg |> TokComma |> TokLiteral
                        | (TokLiteral l)::t ->
                            match t with
                            | [] -> JInstr5 ((((instr,None),None),rd),Literal l)
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while at line %i" (List.head t)?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                | _ -> JError (sprintf "Parsing Error: Unexpected token %A after %A at line %i" (List.head t)?Fields instr?Case line )
            // TokInstr6
            | (TokInstr6 instr)::t -> 
                match t with
                // TokInstr6 |> TokCond
                | (TokCond cond)::t -> 
                    match t with
                    // TokInstr6 |> TokCond |> TokReg
                    | (TokReg rd)::t ->
                        match t with
                        // TokInstr6 |> TokCond |> TokReg |> TokComma
                        | TokComma::t ->
                            match t with
                            // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokReg
                            | (TokReg rn)::t ->
                                match t with
                                | [] -> JInstr6 (((instr,Some cond),rd),Operand(ID rn, NoShift) )
                                // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma
                                | TokComma::t ->
                                    match t with
                                    // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4
                                    | (TokInstr4 shift)::t ->
                                        match t with
                                        // TokInstr6 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokReg
                                        | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                        // TokInstr6 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                        | [(TokLiteral exp)] -> 
                                            match shift with
                                            | LSL  -> JInstr6 (((instr,Some cond),rd),Operand(ID rn,Left exp))
                                            | LSR  -> JInstr6 (((instr,Some cond),rd),Operand(ID rn,RightL exp))
                                            | ASR  -> JInstr6 (((instr,Some cond),rd),Operand(ID rn,RightA exp))
                                            | ROR_ -> JInstr6 (((instr,Some cond),rd),Operand(ID rn,ROR exp))
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                    // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                    | [(TokInstr5 shift)] -> JInstr6 (((instr,Some cond),rd),Operand(ID rn,RRX))
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                            // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokLiteral
                            | (TokLiteral l)::t ->
                                match t with
                                | [] -> JInstr6 (((instr,Some cond),rd),Operand(Literal l, NoShift) )
                                // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma
                                | TokComma::t ->
                                    match t with
                                    // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4
                                    | (TokInstr4 shift)::t ->
                                        match t with
                                        // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokReg
                                        | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                        // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                        | (TokLiteral exp)::t -> 
                                            match shift with
                                            | LSL  -> JInstr6 (((instr,Some cond),rd),Operand(Literal l,Left exp))
                                            | LSR  -> JInstr6 (((instr,Some cond),rd),Operand(Literal l,RightL exp))
                                            | ASR  -> JInstr6 (((instr,Some cond),rd),Operand(Literal l,RightA exp))
                                            | ROR_ -> JInstr6 (((instr,Some cond),rd),Operand(Literal l,ROR exp))
                                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                    // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                    | [(TokInstr5 shift)] -> JInstr6 (((instr,Some cond),rd),Operand(Literal l,RRX))
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
                // TokInstr6 |> TokCond |> TokReg
                | (TokReg rd)::t ->
                    match t with
                    // TokInstr6 |> TokCond |> TokReg |> TokComma
                    | TokComma::t ->
                        match t with
                        // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokReg
                        | (TokReg rn)::t ->
                            match t with
                            | [] -> JInstr6 (((instr,None),rd),Operand(ID rn, NoShift) )
                            // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma
                            | TokComma::t ->
                                match t with
                                // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4
                                | (TokInstr4 shift)::t ->
                                    match t with
                                    // TokInstr6 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokReg
                                    | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                    // TokInstr6 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                    | [(TokLiteral exp)] -> 
                                        match shift with
                                        | LSL  -> JInstr6 (((instr,None),rd),Operand(ID rn,Left exp))
                                        | LSR  -> JInstr6 (((instr,None),rd),Operand(ID rn,RightL exp))
                                        | ASR  -> JInstr6 (((instr,None),rd),Operand(ID rn,RightA exp))
                                        | ROR_ -> JInstr6 (((instr,None),rd),Operand(ID rn,ROR exp))
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                | [(TokInstr5 shift)] -> JInstr6 (((instr,None),rd),Operand(ID rn,RRX))
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                        // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokLiteral
                        | (TokLiteral l)::t ->
                            match t with
                            | [] -> 
                                printfn "crap"
                                JInstr6 (((instr,None),rd),Operand(Literal l, NoShift) )
                            // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma
                            | TokComma::t ->
                                match t with
                                // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4
                                | (TokInstr4 shift)::t ->
                                    match t with
                                    // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokReg
                                    | (TokReg exp)::t -> JError ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                    // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                    | (TokLiteral exp)::t -> 
                                        match shift with
                                        | LSL  -> JInstr6 (((instr,None),rd),Operand(Literal l,Left exp))
                                        | LSR  -> JInstr6 (((instr,None),rd),Operand(Literal l,RightL exp))
                                        | ASR  -> JInstr6 (((instr,None),rd),Operand(Literal l,RightA exp))
                                        | ROR_ -> JInstr6 (((instr,None),rd),Operand(Literal l,ROR exp))
                                    | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                // TokInstr6 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                | [(TokInstr5 shift)] -> JInstr6 (((instr,None),rd),Operand(Literal l,RRX))
                                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                            | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                        | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                    | _ -> JError ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                | _ -> JError ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
            // TokInstr7
            | (TokInstr7 instr)::t -> JError "unimplemented"
            // TokInstr8
            | (TokInstr8 instr)::t -> JError "unimplemented"
            // TokInstr9
            | (TokInstr9 instr)::t ->
                match t with
                // TokCond
                | (TokCond cond)::t ->
                    match t with
                    | [(TokLabel lab)] ->
                        JInstr9 ((instr,Some cond),lab)
                    | _ -> JError (sprintf "Parsing Error: Unexpected token %A at line %i" (List.head tokList)?Fields line )
                | [(TokLabel lab)] -> 
                    JInstr9 ((instr,None),lab)
                | _ -> JError (sprintf "Parsing Error: Unexpected token %A at line %i" (List.head tokList)?Fields line )
            | _ -> JError (sprintf "Parsing Error: Unexpected token %A at line %i" (List.head tokList)?Fields line ) )