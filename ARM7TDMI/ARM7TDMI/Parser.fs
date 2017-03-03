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
    open Instructions
    open Common
    let instructionParse fullOp = 
        match fullOp with
        | ("ADD", rd, rn, op2)  -> addWithCarryS (rd, rn, op2, MachineState, false, false)
        | ("ADDC", rd, rn, op2)  -> addWithCarryS (rd, rn, op2, MachineState, true, false)
        | ("ADDS", rd, rn, op2)  -> addWithCarryS (rd, rn, op2, MachineState, false, true)
        | ("ADDCS", rd, rn, op2)  -> addWithCarryS (rd, rn, op2, MachineState, true, true)
        (*| AND rd rn op2  -> andOp rd rn op2 MachineState false
        | ANDS rd rn op2  -> andOp rd rn op2 MachineState true
        | MOV rd op2 -> mov rd op2 MachineState false
        | MOVS rd op2 -> mov rd op2 MachineState true
        | MVN rd op2 -> mvn rd op2 MachineState false
        | MVNS rd op2 -> mvn rd op2 MachineState true
        | ORR rd rn op2 -> orr rd rn op2 MachineState false
        | ORRS rd rn op2 -> orr rd rn  op2 MachineState true
        | XOR rd op2 -> eOr rd rn op2 MachineState false
        | XORS rd op2 -> eOr rd rn op2 MachineState true
        | OR rd rn op2 -> orr rd rn op2 MachineState false
        | ORS rd rn op2 -> orr rd rn op2 MachineState true
        | BIC rd rn op2 -> orr rd rn op2 MachineState false
        | BICS rd rn op2 -> orr rd rn op2 MachineState true
        *)

    let (|CheckRegister|_|) reg = 
        try TokReg(reg) |> Some
        with _ -> None
    let (|CheckOp|_|) op = 
        try Literal(op) |> Some
        with _ -> None
    let tokenMatcher (tokenList :Token List) : Operation List = 
        let rec getOp tl =
            match tl with
            | [] -> []
            | ["ADD"; CheckRegister rd; rn; op2] :: tl -> when (CheckRegister rd && CheckRegister rn && CheckOp op2) -> instructionParse ("ADD", rd, rn, op2) :: getOp(tl)
            | ["ADDC"; rd; rn; op2] :: tl -> instructionParse ("ADDC", rd, rn, op2) :: getOp(tl)
            | ["ADDS"; rd; rn; op2] :: tl -> instructionParse ("ADDS", rd, rn, op2) :: getOp(tl)
            | ["ADDCS"; rd; rn; op2] :: tl -> instructionParse ("ADDCS", rd, rn, op2) :: getOp(tl)
            | _ -> hello
        getOp tokenList
   
    let parseAndReturn (tokenList: Token List) = 
        tokenMatcher (tokenList) |> instructionParse
         0