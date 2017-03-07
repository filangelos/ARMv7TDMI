namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Pranav Prabhu

    Module: Parser
    Description: Parse individual instruction and initiate correct function call
*)
<<<<<<< HEAD
=======

open System
//open Instructions
//open Common
<<<<<<< HEAD
>>>>>>> a77e12d18eecd1bd7efd3ee99970408f8df9f5e1
=======
open AST
open Tokeniser
>>>>>>> 59d60e4d29d1181e020064eadb3b93b10fed5356

module Parser =

    open System
    open Instructions
    open Common

    type FlagSet =
        | Twoflags of bool*bool 
        | Oneflag of bool

<<<<<<< HEAD
    type Expr = 
        | Instruction of string*RegisterID*RegisterID*Operand*FlagSet
        
=======
  //  let (|AND|_|) rd rn op2 =

  //  let (|ANDS|_|) rd rn op2 =

 //   let (|ADD|_|) rd rn op2 =
    

 (*  let instructionParse str  :Instructions = 
        match str with
        | ADD rd rn op2  -> addWithCarryS rd rn op2 MachineState false false
        | ADDC rd rn op2  -> addWithCarryS rd rn op2 MachineState true false
        | ADDS rd rn op2  -> addWithCarryS rd rn op2 MachineState false true
        | ADDCS rd rn op2  -> addWithCarryS rd rn op2 MachineState true true
        | AND rd rn op2  -> andOp rd rn op2 MachineState false
        | ANDS rd rn op2  -> andOp rd rn op2 MachineState true
        | MOV rd op2 -> mov rd op2 MachineState false
        | MOVS rd op2 -> mov rd op2 MachineState true
        | MVN rd op2 -> mvn rd op2 MachineState false
        | MVNS rd op2 -> mvn rd op2 MachineState true
        | OR rd rn op2 -> orr rd rn op2 MachineState false
        | ORS rd rn op2 -> orr rd rn  op2 MachineState true
        | XOR rd op2 -> eOr rd rn op2 MachineState false
        | XORS rd op2 -> eOr rd rn op2 MachineState true
        | OR rd rn op2 -> orr rd rn op2 MachineState false
        | ORS rd rn op2 -> orr rd rn op2 MachineState true
        | BIC rd rn op2 -> orr rd rn op2 MachineState false
        | BICS rd rn op2 -> orr rd rn op2 MachineState true\

    let parseAndReturn (tokenList: Token List) = 
        tokenList |> instructionParse
        *)



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

    let parse (lst:Token list) (ast:AST) =
        let splitLst = divide (fun x -> match x with | TokNewLine -> false | _ -> true) lst
        printfn "%A" splitLst
        let rec parse1 (lst: Token list list) (ast:AST) addr =
            match lst with
            | line::t ->
                match line with
                | [TokInstr(instr); TokReg(rd); TokComma; TokConst(v)] ->
                    parse1 t (addInstruction ast MOV (Parameters1RegShift(rd, Literal(v), false, NoShift)) (None) addr) (addr+2)
                | [TokInstr(instr); TokReg(rd); TokComma; TokReg(rn)] ->
                    parse1 t (addInstruction ast MOV (Parameters1RegShift(rd, ID(rn), false, NoShift)) (None) addr) (addr+2)
                | _ -> failwithf "error"
            | [] -> ast

        let myAst = parse1 splitLst ast 0
        printfn "ast built:\n%A" myAst
        myAst


    let testParse =
        printfn "\nRunning testParse:\n"
        let myAst1 = ([], Map.empty<string, Address>)
        let tokens = tokenise "MOV r1, #12 \n MOV r2, r1"
        let myAst2 = parse tokens myAst1
        let result = reduce myAst2 (MachineState.make()) 0 10
        printfn "final result is %A" result
        printfn "\nFinished testParse.\n"
>>>>>>> 59d60e4d29d1181e020064eadb3b93b10fed5356
