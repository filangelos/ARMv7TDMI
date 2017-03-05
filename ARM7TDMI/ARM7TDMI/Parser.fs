namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Pranav Prabhu

    Module: Parser
    Description: Parse individual instruction and initiate correct function call
*)

open System
//open Instructions
//open Common

module Parser =
    let pchar(cToMatch,str) =
        if String.IsNullOrEmpty(str) then
            let msg = "No more input"
            (msg,"")
        else 
            let first = str.[0] 
            if first = cToMatch then
                let remaining = str.[1..]
                let msg = sprintf "Found %c" cToMatch
                (msg,remaining)
            else
                let msg = sprintf "Expecting '%c'. Got '%c'" cToMatch first
                (msg,str)
        
   // let (|ADD|_|) rd rn op2 =

  //  let (|ADDC|_|) rd rn op2 =

   //  let (|ADDS|_|) rd rn op2 =

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