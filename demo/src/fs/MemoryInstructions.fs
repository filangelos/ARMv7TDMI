namespace ARM7TDMI


(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Youssef Rizk

    Module: Memory Instructions
    Description: Memory Instructions for LDR/STR operations, etc.
*)

module MemoryInstructions =


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

    let sortRegister regList = // implemented like this because List.sort does not operate correctly under Fable

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



    let storeInstructionW ((result: RegisterID), (regN: AddressRegister), (state: MachineState)) =

        let resultVal = (^.) result state

        let address = addressWithOffset state regN

        let state1 = (sw) address resultVal state

        match regN with
        | {register = R_; offset = PreIndex x} | {register = R_; offset = PostIndex x} -> (^=) R_ (((^.) R_ state) + x) state1
        | _ -> state1


    let loadInstructionB ((regD: RegisterID), (regN: AddressRegister), (state: MachineState)) =

        let address = addressWithOffset state regN

        let state1 = (^=) regD (int ((gb) address state)) state

        match regN with
        | {register = R_; offset = PreIndex x} | {register = R_; offset = PostIndex x} -> (^=) R_ (((^.) R_ state) + x) state1
        | _ -> state1



    let storeInstructionB ((result: RegisterID), (regN: AddressRegister), (state: MachineState)) =

        let resultVal = (^.) result state

        let address = addressWithOffset state regN

        let state1 = (sb) address (byte resultVal) state

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

        if writeBack then match addMode with
                          | ED | IB -> (^=) addRegister (addVal + 4*listSize) finstate
                          | EA | DB -> (^=) addRegister (addVal - 4*listSize) finstate
                          | FD | IA -> (^=) addRegister (addVal + 4*listSize) finstate
                          | FA | DA -> (^=) addRegister (addVal - 4*listSize) finstate

        else finstate

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

        if writeBack then match addMode with
                          | FA | IB -> (^=) addRegister (addVal + 4*listSize) finstate
                          | FD | DB -> (^=) addRegister (addVal - 4*listSize) finstate
                          | EA | IA -> (^=) addRegister (addVal + 4*listSize) finstate
                          | ED | DA -> (^=) addRegister (addVal - 4*listSize) finstate

        else finstate

    //not tested yet
    let ldrPseudo ((regD: RegisterID), (expr: Expression), (state: MachineState)) =

        let exprValue = match expr with
                        | Number x -> x
                        | Lab x -> Optics.get MachineState.Label_ x state

        mov (regD, Operand(Literal exprValue, NoShift), state, false)

    //Not Tested!!! (Needs AST to be working)
    let fillInstruction ((label: string), (space: int), (state: MachineState)) =

        let labelAddress = Optics.get MachineState.Label_ label state

        let addressList = [labelAddress .. labelAddress + (space - 1)]

        let combinedList = List.zip addressList (List.map (fun a -> byte 0) addressList)

        List.fold (fun st (a,b) -> (sb) a b st) state combinedList

    //Not Tested!!! (Needs AST to be working)
    let dcdInstruction ((label: string), (data: int list), (state: MachineState)) =

        let labelAddress = Optics.get MachineState.Label_ label state

        let addressList = [labelAddress .. 4 .. labelAddress + 4*((List.length data) - 1)]

        let combinedList = List.zip addressList data

        List.fold (fun st (a,b) -> (sw) a b st) state combinedList

    //Not Tested!!! (Needs AST to be working)
    let branchInstruction ((label: string), (state: MachineState)) = 

        let branchAddress = Optics.get MachineState.Label_ label state

        (^=) R15 branchAddress state