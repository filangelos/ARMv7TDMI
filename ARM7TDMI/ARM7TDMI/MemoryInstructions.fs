﻿namespace ARM7TDMI


(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Youssef Rizk

    Module: Memory Instructions
    Description: Memory Instructions for LDR/STR operations, etc.
*)

module MemoryInstructions =


    type Offset = 
        | TempOffset of int // LDR     R8, [R10, #4] 
        | PreIndex of int // LDR     R8, [R10, #4]!
        | PostIndex of int // LDR     R8, [R10], #4
        | NoOffset // syntax: LDR     R8, [R10] 


    type AddressMode = 
        | IA | IB | DA | DB 
        | ED | FD | EA | FA

    type AddressRegister = 
        {register: RegisterID; offset: Offset}

    type Expression = 
        | Label of string
        | Number of int

    open MachineState
    open Instructions
    //open AST

    //Memory-specific shortcuts
    let ( gb ) = Optics.get MachineState.Byte_
    let ( sb ) = Optics.set MachineState.Byte_
    let ( gw ) = Optics.get MachineState.Word_
    let ( sw ) = Optics.set MachineState.Word_
    //let ( ^. ) = Optics.get MachineState.Register_
    //let ( ^= ) = Optics.set MachineState.Register_
    //let ( ^* ) = Optics.get MachineState.Flag_
    //let ( ^- ) = Optics.set MachineState.Flag_

    let sortRegister regList =

        let numberRegister = 
            function
            | R0 -> 0
            | R1 -> 1
            | R2 -> 2
            | R3 -> 3
            | R4 -> 4
            | R5 -> 5
            | R6 -> 6
            | R7 -> 7
            | R8 -> 8
            | R9 -> 9
            | R10 -> 10
            | R11 -> 11
            | R12 -> 12
            | R13 -> 13
            | R14 -> 14
            | R15 -> 15


        let combinedList = List.map (fun a -> (a,numberRegister a)) regList

        let sortList = List.sortBy (fun a -> snd a) combinedList

        List.map (fun a -> fst a) sortList

    let addressWithOffset state = function
                                  | {register = R_; offset = PreIndex x} | {register = R_; offset = TempOffset x} -> ((^.) R_ state) + x
                                  | {register = R_; offset = _} -> (^.) R_ state


    let loadInstructionW ((regD: RegisterID), (regN: AddressRegister), (state: MachineState)) =

        let address = addressWithOffset state regN

        let state1 = (^=) regD ((gw) address state) state

        match regN with
        | {register = R_; offset = PreIndex x} | {register = R_; offset = PostIndex x} -> (^=) R_ (((^.) R_ state) + x) state1
        | _ -> state1



    let storeInstructionW ((result: Data), (regN: AddressRegister), (state: MachineState)) =

        let address = addressWithOffset state regN

        let state1 = (sw) address result state

        match regN with
        | {register = R_; offset = PreIndex x} | {register = R_; offset = PostIndex x} -> (^=) R_ (((^.) R_ state) + x) state1
        | _ -> state1


    let loadInstructionB ((regD: RegisterID), (regN: AddressRegister), (state: MachineState)) =

        let address = addressWithOffset state regN

        let state1 = (^=) regD (int ((gb) address state)) state

        match regN with
        | {register = R_; offset = PreIndex x} | {register = R_; offset = PostIndex x} -> (^=) R_ (((^.) R_ state) + x) state1
        | _ -> state1



    let storeInstructionB ((result: Data), (regN: AddressRegister), (state: MachineState)) =

        let address = addressWithOffset state regN

        let state1 = (sb) address (byte result) state

        match regN with
        | {register = R_; offset = PreIndex x} | {register = R_; offset = PostIndex x} -> (^=) R_ (((^.) R_ state) + x) state1
        | _ -> state1
  

    let loadMultiple ((addMode: AddressMode), (addRegister: RegisterID), (regList: RegisterID list), (state: MachineState), (writeBack: bool)) =

        let listSize = List.length regList

        let addVal = (^.) addRegister state

        let addressList = match addMode with
                          | ED | IB -> [(addVal + 4).. 4 ..(addVal + 4*listSize)]
                          | EA | DB -> [(addVal - 4*listSize).. 4 ..(addVal - 4)]
                          | FD | IA -> [(addVal).. 4 ..(addVal + 4*(listSize - 1))]
                          | FA | DA -> [(addVal - 4*(listSize - 1)).. 4 ..(addVal)]

        let combinedList = List.zip addressList (sortRegister regList)

        let finstate = List.fold (fun st (a,b) -> (^=) b ((gw) a st) st) state combinedList

        if writeBack then (^=) addRegister (List.last addressList) finstate else finstate

    let storeMultiple ((addMode: AddressMode), (addRegister: RegisterID), (regList: RegisterID list), (state: MachineState), (writeBack: bool)) =

        let listSize = List.length regList

        let addVal = (^.) addRegister state

        let addressList = match addMode with
                          | FA | IB -> [(addVal + 4).. 4 ..(addVal + 4*listSize)]
                          | FD | DB -> [(addVal - 4*listSize).. 4 ..(addVal - 4)]
                          | EA | IA -> [(addVal).. 4 ..(addVal + 4*(listSize - 1))]
                          | ED | DA -> [(addVal - 4*(listSize - 1)).. 4 ..(addVal)]

        let combinedList = List.zip addressList (sortRegister regList)

        let finstate = List.fold (fun st (a,b) -> (sw) a ((^.) b st) st) state combinedList

        if writeBack then (^=) addRegister (List.last addressList) finstate else finstate

    //not tested yet
    let ldrPseudo ((regD: RegisterID), (expr: Expression), (state: MachineState)) =

        let exprValue = match expr with
                        | Number x -> x
                        | Label x -> MachineState.getAddress x state

        mov (regD, Operand(Literal exprValue, NoShift), state, false)

    //let simpleLDRSTRtest = 
    ////testing loadInstruction
    //    let a3 = MachineState.make()
    //    let b3 = storeInstructionW (2, {register= R0; offset= TempOffset 4}, a3)
    //    printfn "%A" a3
    //    printfn "%A" b3
    //    let c3 = storeInstructionW (1, {register= R1; offset= PreIndex 8}, b3)
    //    printfn "%A" c3
    //    let d3 = loadInstructionW (R2, {register= R0; offset= TempOffset 4}, c3)
    //    printfn "%A" d3
    //    let e3 = loadInstructionW (R1, {register= R1; offset= NoOffset}, d3)
    //    printfn "%A" e3


    //let LDMtest = 
    //    printfn "Starting LDM Memory Instruction Testing"
    //    let a3 = MachineState.make()
    //    let b3 = storeInstruction (2, {register= R0; offset= TempOffset 4}, a3)
    //    printfn "%A" a3
    //    printfn "%A" b3
    //    let c3 = storeInstruction (1, {register= R0; offset= TempOffset 8}, b3)
    //    printfn "%A" c3
    //    let d3 = loadMultiple(IB, R0, [R5;R6], c3) 
    //    printfn "%A" d3  
    //    printfn "Memory Instruction Testing Done"    

    //let STMtest = 
    //    printfn "Starting STM Memory Instruction Testing" 
    //    let a3 = MachineState.make()
    //    let b3 = mov (R5, Operand(Literal(5),NoShift), a3, false)
    //    printfn "%A" a3
    //    printfn "%A" b3
    //    let c3 = mov (R6, Operand(Literal(6),NoShift), b3, false)
    //    printfn "%A" c3
    //    let d3 = storeMultiple(IB, R0, [R5;R6], c3, true) 
    //    printfn "%A" d3 
    //    let e3 = loadMultiple(DA, R0, [R3;R4], d3, true)
    //    printfn "%A" e3  
    //    printfn "Memory Instruction Testing Done" 

    let simpleLDRSTRtest = 
        printfn "%A" (sortRegister [R10; R12; R1; R3])
    ////testing loadInstruction
    //    let a3 = MachineState.make()
    //    let b3 = storeInstructionB (2, {register= R0; offset= TempOffset 3}, a3)
    //    printfn "%A" a3
    //    printfn "%A" b3
    //    let c3 = storeInstructionB (1, {register= R1; offset= PreIndex 7}, b3)
    //    printfn "%A" c3
    //    let d3 = loadInstructionB (R2, {register= R0; offset= TempOffset 3}, c3)
    //    printfn "%A" d3
    //    let e3 = loadInstructionB (R1, {register= R1; offset= NoOffset}, d3)
    //    printfn "%A" e3