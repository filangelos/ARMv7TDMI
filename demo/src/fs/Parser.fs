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

    let Parse (tokenLstLst: Token List) : Instr List =
        splitBy TokNewLine tokenLstLst
        |> List.map ( fun tokList -> 
            match tokList with
            | [TokEOF] -> JInstr1 ((((MOV,None),None),R0),Operand(ID R0, NoShift) )
            /// TokInstr1 | *[TokS] | *[TokCond] | TokReg | TokComma | <TokReg | TokLiteral>
            // TokInstr1
            | (TokInstr1 instr)::t ->
                match t with
                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokLiteral
                | [TokS s; TokCond cond; TokReg rd; TokComma; TokLiteral l] ->
                    JInstr1 ((((instr,Some s),Some cond),rd),Operand(Literal l, NoShift) )
                // TokInstr1 |> TokS |> TokCond |> TokReg |> TokComma |> TokReg
                | [TokS s; TokCond cond; TokReg rd; TokComma; TokReg rn] ->
                    JInstr1 ((((instr,Some s),Some cond),rd),Operand(ID rn, NoShift) )
                // TokInstr1 |> TokS |> TokReg |> TokComma |> TokLiteral
                | [TokS s; TokReg rd; TokComma; TokLiteral l] ->
                    JInstr1 ((((instr,Some s),None),rd),Operand(Literal l, NoShift) )
                // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg
                | [TokS s; TokReg rd; TokComma; TokReg rn] ->
                    JInstr1 ((((instr,Some s),None),rd),Operand(ID rn, NoShift) )
                // TokInstr1 |> TokS |> TokReg |> TokComma |> TokLiteral
                | [TokCond cond; TokReg rd; TokComma; TokLiteral l] ->
                    JInstr1 ((((instr,None),Some cond),rd),Operand(Literal l, NoShift) )
                // TokInstr1 |> TokS |> TokReg |> TokComma |> TokReg
                | [TokCond cond; TokReg rd; TokComma; TokReg rn] ->
                    JInstr1 ((((instr,None),Some cond),rd),Operand(ID rn, NoShift) )
                // TokInstr1 |> TokReg |> TokComma |> TokLiteral
                | [TokReg rd; TokComma; TokLiteral l] ->
                    JInstr1 ((((instr,None),None),rd),Operand(Literal l, NoShift) )
                // TokInstr1 |> TokReg |> TokComma |> TokReg
                | [TokReg rd; TokComma; TokReg rn] ->
                    JInstr1 ((((instr,None),None),rd),Operand(ID rn, NoShift) )
                | _ -> failwith (sprintf "fail at %A" tokList) )