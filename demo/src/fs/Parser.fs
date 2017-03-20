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
                                            | (TokReg exp)::t -> failwith ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                            | [(TokLiteral exp)] -> 
                                                match shift with
                                                | LSL  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn,Left exp))
                                                | LSR  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn,RightL exp))
                                                | ASR  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn,RightA exp))
                                                | ROR_ -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn,ROR exp))
                                            | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                        | [(TokInstr5 shift)] -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn,RRX))
                                        | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                    | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
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
                                            | (TokReg exp)::t -> failwith ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                            // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                            | (TokLiteral exp)::t -> 
                                                match shift with
                                                | LSL  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l,Left exp))
                                                | LSR  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l,RightL exp))
                                                | ASR  -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l,RightA exp))
                                                | ROR_ -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l,ROR exp))
                                            | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                        // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                        | [(TokInstr5 shift)] -> JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l,RRX))
                                        | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                    | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                                | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                            | _ -> failwith ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                        | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
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
                                        | (TokReg exp)::t -> failwith ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                        // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                        | [(TokLiteral exp)] -> 
                                            match shift with
                                            | LSL  -> JInstr1 ((((instr,Some s),None),rd),Operand(ID rn,Left exp))
                                            | LSR  -> JInstr1 ((((instr,Some s),None),rd),Operand(ID rn,RightL exp))
                                            | ASR  -> JInstr1 ((((instr,Some s),None),rd),Operand(ID rn,RightA exp))
                                            | ROR_ -> JInstr1 ((((instr,Some s),None),rd),Operand(ID rn,ROR exp))
                                        | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                    // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                    | [(TokInstr5 shift)] -> JInstr1 ((((instr,Some s),None),rd),Operand(ID rn,RRX))
                                    | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
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
                                        | (TokReg exp)::t -> failwith ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                        // TokInstr1 |> TokS |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                        | (TokLiteral exp)::t -> 
                                            match shift with
                                            | LSL  -> JInstr1 ((((instr,Some s),None),rd),Operand(Literal l,Left exp))
                                            | LSR  -> JInstr1 ((((instr,Some s),None),rd),Operand(Literal l,RightL exp))
                                            | ASR  -> JInstr1 ((((instr,Some s),None),rd),Operand(Literal l,RightA exp))
                                            | ROR_ -> JInstr1 ((((instr,Some s),None),rd),Operand(Literal l,ROR exp))
                                        | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                    // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                    | [(TokInstr5 shift)] -> JInstr1 ((((instr,Some s),None),rd),Operand(Literal l,RRX))
                                    | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                            | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                        | _ -> failwith ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                    | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
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
                                        | (TokReg exp)::t -> failwith ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                        // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                        | [(TokLiteral exp)] -> 
                                            match shift with
                                            | LSL  -> JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn,Left exp))
                                            | LSR  -> JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn,RightL exp))
                                            | ASR  -> JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn,RightA exp))
                                            | ROR_ -> JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn,ROR exp))
                                        | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                    // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                    | [(TokInstr5 shift)] -> JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn,RRX))
                                    | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
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
                                        | (TokReg exp)::t -> failwith ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                        // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                        | (TokLiteral exp)::t -> 
                                            match shift with
                                            | LSL  -> JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l,Left exp))
                                            | LSR  -> JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l,RightL exp))
                                            | ASR  -> JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l,RightA exp))
                                            | ROR_ -> JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l,ROR exp))
                                        | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                    // TokInstr1 |> TokCond |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                    | [(TokInstr5 shift)] -> JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l,RRX))
                                    | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                                | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                            | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                        | _ -> failwith ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                    | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting a register at line %i" (List.head t)?Case line )
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
                                    | (TokReg exp)::t -> failwith ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                    // TokInstr1 > TokReg |> TokComma |> TokReg |> TokComma |> TokInstr4 |> TokLiteral
                                    | [(TokLiteral exp)] -> 
                                        match shift with
                                        | LSL  -> JInstr1 ((((instr,None),None),rd),Operand(ID rn,Left exp))
                                        | LSR  -> JInstr1 ((((instr,None),None),rd),Operand(ID rn,RightL exp))
                                        | ASR  -> JInstr1 ((((instr,None),None),rd),Operand(ID rn,RightA exp))
                                        | ROR_ -> JInstr1 ((((instr,None),None),rd),Operand(ID rn,ROR exp))
                                    | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                // TokInstr1 |> TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                | [(TokInstr5 shift)] -> JInstr1 ((((instr,None),None),rd),Operand(ID rn,RRX))
                                | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                            | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
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
                                    | (TokReg exp)::t -> failwith ( sprintf "Implementation Error: expressions accept only literals for shifting at line %A" line )
                                    // TokInstr1 > TokReg |> TokComma |> TokLiteral |> TokComma |> TokInstr4 |> TokLiteral
                                    | (TokLiteral exp)::t -> 
                                        match shift with
                                        | LSL  -> JInstr1 ((((instr,None),None),rd),Operand(Literal l,Left exp))
                                        | LSR  -> JInstr1 ((((instr,None),None),rd),Operand(Literal l,RightL exp))
                                        | ASR  -> JInstr1 ((((instr,None),None),rd),Operand(Literal l,RightA exp))
                                        | ROR_ -> JInstr1 ((((instr,None),None),rd),Operand(Literal l,ROR exp))
                                    | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                                // TokInstr1 > TokReg |> TokComma |> TokReg |> TokComma |> TokInstr5
                                | [(TokInstr5 shift)] -> JInstr1 ((((instr,None),None),rd),Operand(Literal l,RRX))
                                | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting a shift instruction at line %i" (List.head t)?Case line )
                            | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an optional expression at line %i" (List.head t)?Case line )
                        | _ -> failwith ( sprintf "Parsing Error: Unexpected token %A, while expecting an operand at line %i" (List.head t)?Case line )
                    | _ -> failwith ( sprintf "Parsing Error: Comma missing after %A at line %i" rd?Case line )
                | _ -> failwith (sprintf "Parsing Error: Unexpected token %A after %A at line %i" (List.head t)?Fields instr?Case line )
            | _ -> failwith (sprintf "Parsing Error: Unexpected token %A at line %i" (List.head tokList)?Fields line ) )
