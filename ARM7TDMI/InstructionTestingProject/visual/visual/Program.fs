namespace VisualInterface

module Program=

    open VisualInterface
    open Expecto
    open MachineState
    open InstructionsInterfaces
    open Optics
    open MemoryInstructions


    let ( ^. ) = Optics.get MachineState.Register_
    let ( ^= ) = Optics.set MachineState.Register_
    let ( ^* ) = Optics.get MachineState.Flag_
    let ( ^- ) = Optics.set MachineState.Flag_

    /// postlude which sets R1 bits to status bit values
    let NZCVToR12 =
       """
          MOV R1, #0
          ADDMI R1, R1, #8
          ADDEQ R1, R1, #4
          ADDCS R1, R1, #2
          ADDVS R1, R1, #1
       """ 

    let defaultParas = {
            Cached = true                  // true if results are stored in a cache on disk and reused to speed 
                                            // up future repeat simulations
            VisualPath =  @"..\..\..\visualapp\visual\" // the directory in which the downloaded VisUAL.exe can be found
            WorkFileDir = @"..\..\..\VisualWork\"      // the directory in which both temporary files and the persistent cache file are put
            MemDataStart = 0x10000            // start of VisUAL data section Memory
            MemLocs = [0x10000..4..0x100FF]                    // memory locations to be traced and data returned

        }

    type Flags = 
        {
            FN: bool
            FZ: bool
            FC: bool
            FV: bool
        }
   
    let defParasWithLocs locs = {defaultParas with MemLocs = locs}
    
    /// Adds postlude to assembly code to detect flags values.
    /// Returns registers (before flag detection code) * flags
    let RunVisualWithFlagsOut paras src =
        let asm = src + NZCVToR12
        let trace = VisualInterface.RunVisual defaultParas asm
        if Array.length trace < 5 then failwithf "Error: Trace \n%A\nfrom\n%s\n has length %d < 5." trace asm (Array.length trace)
        let regs = 
            [0..15] 
            |> List.map (fun n -> R n, trace.[5].ResOut.[R n]) // get reg values before postlude
            |> Map.ofList
        let flagsInt = trace.[0].ResOut.[R 1] //Postlude code sets R1(3:0) equal to NZCV
        printfn "flagsint=%x, trace=%A" flagsInt trace.[5]
        printfn "%A" src
        let flagBool n = (flagsInt &&& (1 <<< n)) > 0
        { 
          FN = flagBool 3
          FZ = flagBool 2
          FC = flagBool 1
          FV = flagBool 0
        }, regs

    /// Run Visual with specified source code and list of memory locations to trace
    /// src - source code
    /// memLocs - list of memory locations to trace
    let RunVisualWithFlagsOutLocs memLocs src =
        RunVisualWithFlagsOut {defaultParas with MemLocs = memLocs} src

    /// convenience function, convert 4 char string to NZCV status flag record
    let strToFlags s =
        let toBool = function | '0' -> false | '1' -> true | s -> failwithf "Bad character in flag specification '%c'" s
        match s |> Seq.toList |> List.map toBool with
        | [ a ; b ; c ; d] -> { FN=a; FZ=b;FC=c;FV=d}
        | _ -> failwithf "Wrong number of characters (should be 4) in flag specification %s" s
    
    
    /// run an expecto test of VisUAL
    /// name - name of test
    ///
    let VisualUnitTest name src (flagsExpected:string) (outExpected: (Out * int) list) =
        testCase name <| fun () ->
            let mems = outExpected |> List.collect (function | Mem n, x -> [n,x] | _ -> [])
            let memLocs = mems |> List.map fst
            let flags, outs = RunVisualWithFlagsOutLocs memLocs src
            Expecto.Expect.equal flags (flagsExpected |> strToFlags) (sprintf "Status flags don't match %A" src)
            let regs = outExpected |> List.filter (function | R _,_ -> true | _ -> false)
            let getOut (out, v) = 
                try
                    out, outs.[out]
                with
                | _ -> 
                failwithf "Can't find output %A in outs %A" out outs
            Expecto.Expect.sequenceEqual (outExpected |> List.map getOut) outExpected "Reg and Mem outputs don't match"

    
          
    let seqConfig = { Expecto.Tests.defaultConfig with parallel = false}

    
    //Setting up Arrays from which to generate random instructions

    let regStringArr = [|0..3|] |> Array.map (fun a -> "R"+ (string a))//only testing on registers R0-R3
    let litStringArr = [|0..16|] |> Array.map (fun a -> "#"+ (string a))//only testing literals 0-16
    let negLitStringArr = [|-2..16|] |> Array.map (fun a -> "#"+ (string a))//including negative literals for some instructions
    let operandStringArr = Array.concat [regStringArr ; negLitStringArr]//concatenating previous arrays into an array containing general operands

    //Array containing instructions to be tests along with the number of inputs that each instruction takes. Represented as string*int
    let instructionARR = [|("MOV",2); ("MOVS",2); ("MVN",2); ("MVNS",2);("CMP",2); ("CMN",2); ("TST",2); ("TEQ",2); ("ADD",3); ("ADDS",3);("ADC",3);("ADCS",3); ("SUB",3); ("SUBS",3);("SBC",3);("SBCS",3); ("RSB",3); ("RSBS",3);("RSC",3);("RSCS",3); ("ORR",3); ("ORRS",3);("AND",3);("ANDS",3); ("EOR",3); ("EORS",3);("BIC",3);("BICS",3); ("LSL",3); ("LSLS",3);("LSR",3);("LSRS",3); ("ASR",3); ("ASRS",3);("ROR",3);("RORS",3);("RRX",2);("RRXS",2)|]

    //creating a function to choose random elements from the arrays
    let rnd = System.Random()
    let makeRandomInstruction () =

        let getRandomInst () = //picks a random instruction from regStringArr
            fun (arr: (string*int) []) -> arr.[(rnd.Next(Array.length arr))]

        let rnd1 = new System.Random() 
        let getRandomOp () = //picks a random element from any Array
            fun (arr: (string) []) -> arr.[(rnd1.Next(Array.length arr))]

        let instr = getRandomInst () instructionARR

        match snd instr with
        | 2 when (fst instr) = "RRX" -> (fst instr) + " " + (getRandomOp () regStringArr) + "," + (getRandomOp () regStringArr)
        | 2 when (fst instr) = "RRXS" -> (fst instr) + " " + (getRandomOp () regStringArr) + "," + (getRandomOp () regStringArr)
        | 2 -> (fst instr) + " " + (getRandomOp () regStringArr) + "," + (getRandomOp () operandStringArr) 
        | 3 when (fst instr) = "ROR" || (fst instr) = "RORS"|| (fst instr) = "ASRS"|| (fst instr) = "ASR"|| (fst instr) = "LSL"|| (fst instr) = "LSLS"|| (fst instr) = "LSR"|| (fst instr) = "LSRS" -> (fst instr) + " " + (getRandomOp () regStringArr) + "," + (getRandomOp () regStringArr) + "," + (getRandomOp () litStringArr)        
        | 3  when (fst instr) = "AND" || (fst instr) = "ANDS"|| (fst instr) = "BICS"|| (fst instr) = "BIC" ||(fst instr) = "ORR" || (fst instr) = "ORRS" ||(fst instr) = "EOR" || (fst instr) = "EORS" -> (fst instr) + " " + (getRandomOp () regStringArr) + "," + (getRandomOp () regStringArr) + "," + (getRandomOp () litStringArr)
        | 3  -> (fst instr) + " " + (getRandomOp () regStringArr) + "," + (getRandomOp () regStringArr) + "," + (getRandomOp () operandStringArr)


    //converts state of a flag to a string   
    let stringFlag flag state = match (^*) flag state with
                                | true -> "1"
                                | false -> "0"

    //Calls stringFlag for each flag and concatenate the result
    let stringConcat state =
        let stringList = [stringFlag N state; stringFlag Z state; stringFlag C state; stringFlag V state]
        String.concat "" stringList

    //very basic parser to match instructions to functional implementations and execute them
    let parser (cmnd: string) state =
        let textSpliter = Array.toList (cmnd.Split [|' ';',';'#'|] |> Array.filter (fun a -> a <> "")) //splits the input string (i.e. instruction) into its important bits
        let regMatcher = function //matches Reg X to RX
                         |"R0" -> R0
                         |"R1" -> R1
                         |"R2" -> R2
                         |"R3" -> R3
                         |"R4" -> R4
                         |"R5" -> R5
                         |"R6" -> R6
                         |"R7" -> R7
                         |"R8" -> R8
                         |"R9" -> R9
                         |"R10" -> R10
                         |"R11" -> R11
                         |"R12" -> R12
                         |"R13" -> R13
                         |"R14" -> R14
                         |"R15" -> R15

        let registerOrLit (op: string) = match op.[0] with 
                               | 'R' -> ID (regMatcher op)
                               | _ -> Literal (int op)

        let shiftMatcher (shift: string) = match shift with //matches shift
                                           | "LSL" -> Left
                                           | "LSR" -> RightL
                                           | "ASR" -> RightA
                                           | "ROR" -> ROR

        let instructionMatcher (instr: string list) = match instr.[0] with //matches the strings to the corresponding instruction
                                       | "MOV" | "MOVS" -> mov_ ((regMatcher (instr.[1])), (registerOrLit (instr.[2])), state, (instr.[0].Length = 4), None)
                                       | "MVN" | "MVNS" -> mvn_ ((regMatcher (instr.[1])), (registerOrLit (instr.[2])), state, (instr.[0].Length = 4), None)
                                       | "ADD" | "ADDS" -> add_ ((regMatcher (instr.[1])), (regMatcher (instr.[2])), (registerOrLit (instr.[3])), state, (instr.[0].Length = 4), None)
                                       | "ADC" | "ADCS" -> adc_ ((regMatcher (instr.[1])), (regMatcher (instr.[2])), (registerOrLit (instr.[3])), state, (instr.[0].Length = 4), None)
                                       | "LSR" | "LSRS" -> lsr_ ((regMatcher (instr.[1])), ID(regMatcher (instr.[2])), int (instr.[3]), state, (instr.[0].Length = 4), None)
                                       | "LSL" | "LSLS" -> lsl_ ((regMatcher (instr.[1])), ID(regMatcher (instr.[2])), int (instr.[3]), state, (instr.[0].Length = 4), None)
                                       | "ASR" | "ASRS" -> asr_ ((regMatcher (instr.[1])), ID(regMatcher (instr.[2])), int (instr.[3]), state, (instr.[0].Length = 4), None)
                                       | "ROR" | "RORS" -> ror_ ((regMatcher (instr.[1])), ID(regMatcher (instr.[2])), int (instr.[3]), state, (instr.[0].Length = 4), None)
                                       | "RRX" | "RRXS" -> rrx_ ((regMatcher (instr.[1])),( registerOrLit (instr.[2])), state, (instr.[0].Length = 4), None)
                                       | "SUB" | "SUBS" -> sub_ ((regMatcher (instr.[1])), (regMatcher (instr.[2])), (registerOrLit (instr.[3])), state, (instr.[0].Length = 4), None)
                                       | "SBC" | "SBCS" -> sbc_ ((regMatcher (instr.[1])), (regMatcher (instr.[2])), (registerOrLit (instr.[3])), state, (instr.[0].Length = 4), None)
                                       | "RSB" | "RSBS" -> rsb_ ((regMatcher (instr.[1])), (regMatcher (instr.[2])), (registerOrLit (instr.[3])), state, (instr.[0].Length = 4), None)
                                       | "RSC" | "RSCS" -> rsc_ ((regMatcher (instr.[1])), (regMatcher (instr.[2])), (registerOrLit (instr.[3])), state, (instr.[0].Length = 4), None)
                                       | "CMP" when (List.length instr = 3) -> cmp_ ((regMatcher (instr.[1])), Operand((registerOrLit (instr.[2])),NoShift), state, None)
                                       | "CMP" when (List.length instr = 5) -> cmp_ ((regMatcher (instr.[1])), Operand((registerOrLit (instr.[2])),((shiftMatcher instr.[3]) (int instr.[4]))), state, None)
                                       | "CMP" -> cmp_ ((regMatcher (instr.[1])), Operand((registerOrLit (instr.[2])),RRX), state, None)                                       
                                       | "CMN" when (List.length instr = 3) -> cmn_ ((regMatcher (instr.[1])), Operand((registerOrLit (instr.[2])),NoShift), state, None)
                                       | "CMN" when (List.length instr = 5) -> cmn_ ((regMatcher (instr.[1])), Operand((registerOrLit (instr.[2])),((shiftMatcher instr.[3]) (int instr.[4]))), state, None)
                                       | "CMN" -> cmn_ ((regMatcher (instr.[1])), Operand((registerOrLit (instr.[2])),RRX), state, None)                                       
                                       | "TST" -> tst_ ((regMatcher (instr.[1])), (registerOrLit (instr.[2])), state, None)
                                       | "TEQ" -> teq_ ((regMatcher (instr.[1])), (registerOrLit (instr.[2])), state, None)
                                       | "AND" | "ANDS" -> and_ ((regMatcher (instr.[1])), (regMatcher (instr.[2])), (registerOrLit (instr.[3])), state, (instr.[0].Length = 4), None)
                                       | "ORR" | "ORRS" -> orr_ ((regMatcher (instr.[1])), (regMatcher (instr.[2])), (registerOrLit (instr.[3])), state, (instr.[0].Length = 4), None)
                                       | "EOR" | "EORS" -> eor_ ((regMatcher (instr.[1])), (regMatcher (instr.[2])), (registerOrLit (instr.[3])), state, (instr.[0].Length = 4), None)
                                       | "BIC" | "BICS" -> bic_ ((regMatcher (instr.[1])), (regMatcher (instr.[2])), (registerOrLit (instr.[3])), state, (instr.[0].Length = 4), None)

        textSpliter |> instructionMatcher

    let randFunc str a = //initializes everything to the correct state
        let inst = makeRandomInstruction ()
        let executableInst = (parser inst (MachineState.initWithFlags str))
        match str with
        | "0010" -> VisualUnitTest ("Test1 #"+(string a)) ("MOV R7, #3 \n LSRS R7, R7, #1 \n" + inst) (stringConcat executableInst) [R 0, ((^.) R0 executableInst); R 1, ((^.) R1 executableInst);R 2, ((^.) R2 executableInst);R 3, ((^.) R3 executableInst)]
        | _ -> VisualUnitTest ("Test1 #"+(string a)) inst (stringConcat executableInst) [R 0, ((^.) R0 executableInst); R 1, ((^.) R1 executableInst);R 2, ((^.) R2 executableInst);R 3, ((^.) R3 executableInst)]
        //only these states were considered since they are the most important, especially for instructions that involve carry

    //let testList1 = [1..50] |> List.map randFunc 
                                                  //VisualUnitTest ("Test1 #"+(string a)) inst (stringConcat (parser inst (MachineState.initWithFlags "0000"))) [R 0, ((^.) R0 (parser inst (MachineState.initWithFlags "0000")))]) 

    [<EntryPoint>]
    let main argv = 
        InitCache defaultParas.WorkFileDir // read the currently cached info from disk to speed things up
        let tests = 
            testList "Visual tests" ([
               // First 24 Instructions test memory instructions
                VisualUnitTest "LDR test" "LDR R0, =test \n LDR R1, [R0] \n test DCD 123" "0000" [R 1, (^.) R1 (loadInstructionW (R1, {register= R0; offset = NoOffset}, (MachineState.make() |> (^=) R0 65536 |> (sw) 65536 123)))]
                VisualUnitTest "LDR test (TempOffset)" "LDR R0, =test \n LDR R1, [R0,#4] \n test DCD 123,1234" "0000" [R 1, (^.) R1 (loadInstructionW (R1, {register= R0; offset = TempOffset 4}, (MachineState.make() |> (^=) R0 65536 |> (sw) 65536 123|> (sw) 65540 1234)))]
                VisualUnitTest "LDR test (PreOffset)" "LDR R0, =test \n LDR R1, [R0,#4]! \n test DCD 123,1234" "0000" [R 1, (^.) R1 (loadInstructionW (R1, {register= R0; offset = PreIndex 4}, (MachineState.make() |> (^=) R0 65536 |> (sw) 65536 123|> (sw) 65540 1234)))]
                VisualUnitTest "LDR test (PostOffset)" "LDR R0, =test \n LDR R1, [R0],#4 \n test DCD 123,1234" "0000" [R 1, (^.) R1 (loadInstructionW (R1, {register= R0; offset = PostIndex 4}, (MachineState.make() |> (^=) R0 65536 |> (sw) 65536 123|> (sw) 65540 1234)))]
                VisualUnitTest "LDM test FD" "LDR R0, =test \n LDMFD R0, {R1,R2,R3} \n test DCD 123,1234,12345,2345" "0000" [R 1, (^.) R1 (loadMultiple (FD, R0, [R1;R3;R2], (MachineState.make() |> (^=) R0 0x100 |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false));R 2, (^.) R2 (loadMultiple (FD, R0, [R1;R2;R3], (MachineState.make() |> (^=) R0 0x100 |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false));R 3, (^.) R3 (loadMultiple (FD, R0, [R1;R2;R3], (MachineState.make() |> (^=) R0 0x100 |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false))]
                VisualUnitTest "LDM test ED" "LDR R0, =test \n LDMED R0, {R1,R2,R3} \n test DCD 123,1234,12345,2345" "0000" [R 1, (^.) R1 (loadMultiple (ED, R0, [R1;R3;R2], (MachineState.make() |> (^=) R0 0x100 |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false));R 2, (^.) R2 (loadMultiple (ED, R0, [R1;R2;R3], (MachineState.make() |> (^=) R0 0x100 |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false));R 3, (^.) R3 (loadMultiple (ED, R0, [R1;R2;R3], (MachineState.make() |> (^=) R0 0x100 |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false))]
                VisualUnitTest "LDM test FA" "LDR R0, =test+12 \n LDMFA R0, {R1,R2,R3} \n test DCD 123,1234,12345,2345" "0000" [R 1, (^.) R1 (loadMultiple (FA, R0, [R1;R3;R2], (MachineState.make() |> (^=) R0 0x10C |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false));R 2, (^.) R2 (loadMultiple (FA, R0, [R1;R2;R3], (MachineState.make() |> (^=) R0 0x10C |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false));R 3, (^.) R3 (loadMultiple (FA, R0, [R1;R2;R3], (MachineState.make() |> (^=) R0 0x10C |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false))]
                VisualUnitTest "LDM test EA" "LDR R0, =test+12 \n LDMEA R0, {R1,R2,R3} \n test DCD 123,1234,12345,2345" "0000" [R 1, (^.) R1 (loadMultiple (EA, R0, [R1;R3;R2], (MachineState.make() |> (^=) R0 0x10C |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false));R 2, (^.) R2 (loadMultiple (EA, R0, [R1;R2;R3], (MachineState.make() |> (^=) R0 0x10C |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false));R 3, (^.) R3 (loadMultiple (EA, R0, [R1;R2;R3], (MachineState.make() |> (^=) R0 0x10C |> (sw) 0x100 123|> (sw) 0x104 1234 |> (sw) 0x108 12345|> (sw) 0x10C 2345),false))]
                VisualUnitTest "STR test by Proxy" "LDR R0, =test \n MOV R1, #3 \n STR R1, [R0] \n LDR R2, [R0] \n test FILL 16" "0000" [R 2, (^.) R2 (loadInstructionW (R2, {register= R0; offset = NoOffset}, (storeInstructionW (ID R1, {register = R0; offset = NoOffset},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536)))))]            
                VisualUnitTest "STR test by Proxy (TempOffset)" "LDR R0, =test \n MOV R1, #3 \n STR R1, [R0,#4] \n LDR R2, [R0] \n test FILL 16" "0000" [R 2, (^.) R2 (loadInstructionW (R2, {register= R0; offset = NoOffset}, (storeInstructionW (ID R1, {register = R0; offset = TempOffset 4},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536)))))]              
                VisualUnitTest "STR test by Proxy (PreOffset)" "LDR R0, =test \n MOV R1, #3 \n STR R1, [R0,#4]! \n LDR R2, [R0] \n test FILL 16" "0000" [R 2, (^.) R2 (loadInstructionW (R2, {register= R0; offset = NoOffset}, (storeInstructionW (ID R1, {register = R0; offset = PreIndex 4},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536))))); R 0, (^.) R0 (loadInstructionW (R2, {register= R0; offset = NoOffset}, (storeInstructionW (ID R1, {register = R0; offset = PreIndex 4},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536)))))]             
                VisualUnitTest "STR test by Proxy (PostOffset)" "LDR R0, =test \n MOV R1, #3 \n STR R1, [R0], #4 \n LDR R2, [R0] \n test FILL 16" "0000" [R 2, (^.) R2 (loadInstructionW (R2, {register= R0; offset = NoOffset}, (storeInstructionW (ID R1, {register = R0; offset = PostIndex 4},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536))))); R 0, (^.) R0 (loadInstructionW (R2, {register= R0; offset = NoOffset}, (storeInstructionW (ID R1, {register = R0; offset = PostIndex 4},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536)))))]
                VisualUnitTest "STM test EA by Proxy" "LDR R0, =test \n MOV R5, #3 \n MOV R6, #4 \n MOV R7, #6 \n STMEA R0, {R5,R6,R7} \n LDMFD R0, {R1,R2,R3} \n test FILL 16" "0000" [R 1, (^.) R1 (loadMultiple (FD, R0, [R1;R3;R2], storeMultiple (EA, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 65536 |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),false),false));R 2, (^.) R2 (loadMultiple (FD, R0, [R1;R2;R3], storeMultiple (EA, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 65536 |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),false),false));R 0, (^.) R0 (loadMultiple (FD, R0, [R1;R2;R3], storeMultiple (EA, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 65536 |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),false),false));R 3, (^.) R3 (loadMultiple (FD , R0, [R1;R2;R3], storeMultiple (EA, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 65536 |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),false),false))]
                VisualUnitTest "STM test FA by Proxy" "LDR R0, =test \n MOV R5, #3 \n MOV R6, #4 \n MOV R7, #6 \n STMFA R0, {R5,R6,R7} \n LDMED R0, {R1,R2,R3} \n test FILL 16" "0000" [R 1, (^.) R1 (loadMultiple (ED, R0, [R1;R3;R2], storeMultiple (FA, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 65536 |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),false),false));R 2, (^.) R2 (loadMultiple (ED, R0, [R1;R2;R3], storeMultiple (FA, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 65536 |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),false),false));R 0, (^.) R0 (loadMultiple (ED, R0, [R1;R2;R3], storeMultiple (FA, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 65536 |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),false),false));R 3, (^.) R3 (loadMultiple (ED , R0, [R1;R2;R3], storeMultiple (FA, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 65536 |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),false),false))]
                VisualUnitTest "STM test ED by Proxy" "LDR R0, =test+12 \n MOV R5, #3 \n MOV R6, #4 \n MOV R7, #6 \n STMED R0!, {R5,R6,R7} \n LDMED R0!, {R1,R2,R3} \n test FILL 16" "0000" [R 1, (^.) R1 (loadMultiple (ED, R0, [R1;R3;R2], storeMultiple (ED, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 (65536+12) |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),true),true));R 2, (^.) R2 (loadMultiple (ED, R0, [R1;R2;R3], storeMultiple (ED, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 (65536+12) |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),true),true));R 0, (^.) R0 (loadMultiple (ED, R0, [R1;R2;R3], storeMultiple (ED, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 (65536+12) |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),true),true));R 3, (^.) R3 (loadMultiple (ED , R0, [R1;R2;R3], storeMultiple (ED, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 (65536+12) |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),true),true))]
                VisualUnitTest "STM test FD by Proxy" "LDR R0, =test+12 \n MOV R5, #3 \n MOV R6, #4 \n MOV R7, #6 \n STMFD R0!, {R5,R6,R7} \n LDMFD R0!, {R1,R2,R3} \n test FILL 16" "0000" [R 1, (^.) R1 (loadMultiple (FD, R0, [R1;R3;R2], storeMultiple (FD, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 (65536+12) |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),true),true));R 2, (^.) R2 (loadMultiple (FD, R0, [R1;R2;R3], storeMultiple (FD, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 (65536+12) |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),true),true));R 0, (^.) R0 (loadMultiple (FD, R0, [R1;R2;R3], storeMultiple (FD, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 (65536+12) |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),true),true));R 3, (^.) R3 (loadMultiple (FD , R0, [R1;R2;R3], storeMultiple (FD, R0, [R6;R5;R7], (MachineState.make() |> (^=) R0 (65536+12) |> (^=) R5 3|> (^=) R6 4 |> (^=) R7 6),true),true))]
                VisualUnitTest "LDRB test" "LDR R0, =test \n LDRB R1, [R0] \n test DCD 123" "0000" [R 1, (^.) R1 (loadInstructionB (R1, {register= R0; offset = NoOffset}, (MachineState.make() |> (^=) R0 65536 |> (sw) 65536 123)))]
                VisualUnitTest "LDRB test (TempOffset)" "LDR R0, =test \n LDRB R1, [R0,#1] \n test DCD 123,1234" "0000" [R 1, (^.) R1 (loadInstructionB (R1, {register= R0; offset = TempOffset 1}, (MachineState.make() |> (^=) R0 65536 |> (sw) 65536 123|> (sw) 65540 1234)))]
                VisualUnitTest "LDRB test (PreOffset)" "LDR R0, =test \n LDRB R1, [R0,#1]! \n test DCD 123,1234" "0000" [R 1, (^.) R1 (loadInstructionB (R1, {register= R0; offset = PreIndex 1}, (MachineState.make() |> (^=) R0 65536 |> (sw) 65536 123|> (sw) 65540 1234)))]
                VisualUnitTest "LDRB test (PostOffset)" "LDR R0, =test \n LDRB R1, [R0],#1 \n test DCD 123,1234" "0000" [R 1, (^.) R1 (loadInstructionB (R1, {register= R0; offset = PostIndex 1}, (MachineState.make() |> (^=) R0 65536 |> (sw) 65536 123|> (sw) 65540 1234)))]
                VisualUnitTest "STRB test by Proxy" "LDR R0, =test \n MOV R1, #3 \n STRB R1, [R0] \n LDR R2, [R0] \n test FILL 16" "0000" [R 2, (^.) R2 (loadInstructionB (R2, {register= R0; offset = NoOffset}, (storeInstructionB (ID R1, {register = R0; offset = NoOffset},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536)))))]            
                VisualUnitTest "STRB test by Proxy (TempOffset)" "LDR R0, =test \n MOV R1, #3 \n STRB R1, [R0,#1] \n LDRB R2, [R0] \n test FILL 16" "0000" [R 2, (^.) R2 (loadInstructionB (R2, {register= R0; offset = NoOffset}, (storeInstructionB (ID R1, {register = R0; offset = TempOffset 1},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536)))))]              
                VisualUnitTest "STRB test by Proxy (PreOffset)" "LDR R0, =test \n MOV R1, #3 \n STRB R1, [R0,#1]! \n LDRB R2, [R0] \n test FILL 16" "0000" [R 2, (^.) R2 (loadInstructionB (R2, {register= R0; offset = NoOffset}, (storeInstructionB (ID R1, {register = R0; offset = PreIndex 1},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536))))); R 0, (^.) R0 (loadInstructionB (R2, {register= R0; offset = NoOffset}, (storeInstructionB (ID R1, {register = R0; offset = PreIndex 1},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536)))))]             
                VisualUnitTest "STRB test by Proxy (PostOffset)" "LDR R0, =test \n MOV R1, #3 \n STRB R1, [R0], #1 \n LDRB R2, [R0] \n test FILL 16" "0000" [R 2, (^.) R2 (loadInstructionB (R2, {register= R0; offset = NoOffset}, (storeInstructionB (ID R1, {register = R0; offset = PostIndex 1},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536))))); R 0, (^.) R0 (loadInstructionB (R2, {register= R0; offset = NoOffset}, (storeInstructionB (ID R1, {register = R0; offset = PostIndex 1},(MachineState.make() |> (^=) R1 3 |> (^=) R0 65536)))))]
                
                //30 instructions to test specific edge cases that I could come up with       
                VisualUnitTest "MOV test" "MOV R0, #123" (stringConcat (parser "MOV R0, #123" (MachineState.initWithFlags "0000"))) [R 0, ((^.) R0 (parser "MOV R0, #123" (MachineState.initWithFlags "0000")))]
                VisualUnitTest "MVNS test" "MVNS R0, #0" (stringConcat (parser "MVNS R0, #0" (MachineState.initWithFlags "0000"))) [R 0, ((^.) R0 (parser "MVNS R0, #0" (MachineState.initWithFlags "0000")))]
                VisualUnitTest "MVN test" "MVN R0, #-1" (stringConcat (parser "MVN R0, #-1" (MachineState.initWithFlags "0000"))) [R 0, ((^.) R0 (parser "MVN R0, #-1" (MachineState.initWithFlags "0000")))] 
                VisualUnitTest "ADD test" "ADD R1, R0, #-1" (stringConcat (parser "ADD R1, R0, #-1" (MachineState.initWithFlags "0000"))) [R 1, ((^.) R1 (parser "ADD R1, R0, #-1" (MachineState.initWithFlags "0000")))] 
                VisualUnitTest "ADDS test" "ADDS R1, R0, #-1" (stringConcat (parser "ADDS R1, R0, #-1" (MachineState.initWithFlags "0000"))) [R 1, ((^.) R1 (parser "ADDS R1, R0, #-1" (MachineState.initWithFlags "0000")))] 
                VisualUnitTest "MOVS test for LSRS test" "MOVS R0, #0" (stringConcat (parser "MOVS R0, #0" (MachineState.initWithFlags "0000"))) [R 0, ((^.) R0 (parser "MOVS R0, #0" (MachineState.initWithFlags "0000")))]                
                VisualUnitTest "LSRS test" "MOV R1, #-1 \n LSRS R1, R1, #1" (stringConcat (parser "LSRS R1, R1, #1" (MachineState.initWithFlags "0000"|> (^=) R1 -1))) [R 1, ((^.) R1 (parser  "LSRS R1, R1, #1" (MachineState.initWithFlags "0000"|> (^=) R1 -1)))] 
                VisualUnitTest "LSLS test" "MOV R1, #-1 \n LSLS R1, R1, #1" (stringConcat (parser "LSLS R1, R1, #1" (MachineState.initWithFlags "0000"|> (^=) R1 -1))) [R 1, ((^.) R1 (parser  "LSLS R1, R1, #1" (MachineState.initWithFlags "0000"|> (^=) R1 -1)))] 
                VisualUnitTest "ASRS test" "MOV R1, #-1 \n ASRS R1, R1, #1" (stringConcat (parser "ASRS R1, R1, #1" (MachineState.initWithFlags "0000"|> (^=) R1 -1))) [R 1, ((^.) R1 (parser  "ASRS R1, R1, #1" (MachineState.initWithFlags "0000"|> (^=) R1 -1)))] 
                VisualUnitTest "RORS test" "MOV R1, #-1 \n RORS R1, R1, #1" (stringConcat (parser "RORS R1, R1, #1" (MachineState.initWithFlags "0000"|> (^=) R1 -1))) [R 1, ((^.) R1 (parser  "RORS R1, R1, #1" (MachineState.initWithFlags "0000"|> (^=) R1 -1)))] 
                VisualUnitTest "RRXS test" "MOV R1, #-1 \n RRXS R1, R1" (stringConcat (parser "RRXS R1, R1" (MachineState.initWithFlags "0000"|> (^=) R1 -1))) [R 1, ((^.) R1 (parser  "RRXS R1, R1" (MachineState.initWithFlags "0000"|> (^=) R1 -1)))] 
                VisualUnitTest "ADCS test" "MOV R1, #-1 \n RRXS R1, R1 \n ADCS R1, R0, #-1" (stringConcat (parser "ADCS R1, R0, #-1" (MachineState.initWithFlags "0010"))) [R 1, ((^.) R1 (parser "ADCS R1, R0, #-1" (MachineState.initWithFlags "0010")))] 
                VisualUnitTest "SUBS test" "MOV R1, #-1 \n MOV R2, #213 \n SUBS R1, R2, R1" (stringConcat (parser "SUBS R1, R2, R1" (MachineState.initWithFlags "0000"|> (^=) R1 -1 |> (^=) R2 213))) [R 1, ((^.) R1 (parser "SUBS R1, R2, R1" (MachineState.initWithFlags "0000"|> (^=) R1 -1 |> (^=) R2 213)))] 
                VisualUnitTest "SBCS test" "MOV R1, #-1 \n MOV R2, #213 \n RRXS R3, R1 \n SBCS R1, R2, R1" (stringConcat (parser "SBCS R1, R2, R1" (MachineState.initWithFlags "0010"|> (^=) R1 -1 |> (^=) R2 213))) [R 1, ((^.) R1 (parser "SBCS R1, R2, R1" (MachineState.initWithFlags "0010"|> (^=) R1 -1 |> (^=) R2 213)))] 
                VisualUnitTest "RSBS test" "MOV R1, #-1 \n MOV R2, #213 \n RSBS R1, R2, R1" (stringConcat (parser "RSBS R1, R2, R1" (MachineState.initWithFlags "0000"|> (^=) R1 -1 |> (^=) R2 213))) [R 1, ((^.) R1 (parser "RSBS R1, R2, R1" (MachineState.initWithFlags "0000"|> (^=) R1 -1 |> (^=) R2 213)))] 
                VisualUnitTest "RSCS test" "MOV R1, #-1 \n MOV R2, #213 \n RRXS R3, R1 \n RSCS R1, R2, R1" (stringConcat (parser "RSCS R1, R2, R1" (MachineState.initWithFlags "0010"|> (^=) R1 -1 |> (^=) R2 213))) [R 1, ((^.) R1 (parser "RSCS R1, R2, R1" (MachineState.initWithFlags "0010"|> (^=) R1 -1 |> (^=) R2 213)))] 
                VisualUnitTest "CMP test" "CMP R0, #0" (stringConcat (parser "CMP R0, #0" (MachineState.initWithFlags "0000"))) [R 0, ((^.) R0 (parser "CMP R0, #0" (MachineState.initWithFlags "0000")))]
                VisualUnitTest "CMP test 2" "CMP R0, #-1" (stringConcat (parser "CMP R0, #-1" (MachineState.initWithFlags "0000"))) [R 0, ((^.) R0 (parser "CMP R0, #-1" (MachineState.initWithFlags "0000")))]
                VisualUnitTest "CMP test 3" "MOV R0, #-1 \n CMP R0, R0, ASR #3" (stringConcat (parser "CMP R0, R0, ASR #3" (MachineState.initWithFlags "0000"|> (^=) R0 -1))) [R 0, ((^.) R0 (parser "CMP R0, R0, ASR #3" (MachineState.initWithFlags "0000"|> (^=) R0 -1)))]                
                VisualUnitTest "CMP test 4" "MOV R0, #-1 \n CMP R0, R0, RRX" (stringConcat (parser "CMP R0, R0, RRX" (MachineState.initWithFlags "0000"|> (^=) R0 -1))) [R 0, ((^.) R0 (parser "CMP R0, R0, RRX" (MachineState.initWithFlags "0000"|> (^=) R0 -1)))]                      
                VisualUnitTest "CMN test" "CMN R0, #-1" (stringConcat (parser "CMN R0, #-1" (MachineState.initWithFlags "0000"))) [R 0, ((^.) R0 (parser "CMN R0, #-1" (MachineState.initWithFlags "0000")))]
                VisualUnitTest "CMN test 2" "CMN R0, #0" (stringConcat (parser "CMN R0, #0" (MachineState.initWithFlags "0000"))) [R 0, ((^.) R0 (parser "CMN R0, #0" (MachineState.initWithFlags "0000")))]
                VisualUnitTest "CMN test 3" "MOV R0, #-1 \n CMN R0, R0, LSR #3" (stringConcat (parser "CMN R0, R0, LSR #3" (MachineState.initWithFlags "0000"|> (^=) R0 -1))) [R 0, ((^.) R0 (parser "CMN R0, R0, LSR #3" (MachineState.initWithFlags "0000"|> (^=) R0 -1)))]                
                VisualUnitTest "CMN test 4" "MOV R0, #412 \n CMN R0, R0, ROR #3" (stringConcat (parser "CMN R0, R0, ROR #3" (MachineState.initWithFlags "0000"|> (^=) R0 412))) [R 0, ((^.) R0 (parser "CMN R0, R0, ROR #3" (MachineState.initWithFlags "0000"|> (^=) R0 412)))]                
                VisualUnitTest "CMN test 5" "MOV R0, #1 \n CMN R0, R0, ROR #3" (stringConcat (parser "CMN R0, R0, ROR #3" (MachineState.initWithFlags "0000"|> (^=) R0 1))) [R 0, ((^.) R0 (parser "CMN R0, R0, ROR #3" (MachineState.initWithFlags "0000"|> (^=) R0 1)))]                
                VisualUnitTest "CMN test 6" "MOV R0, #-1 \n CMN R0, R0, ROR #3" (stringConcat (parser "CMN R0, R0, ROR #3" (MachineState.initWithFlags "0000"|> (^=) R0 -1))) [R 0, ((^.) R0 (parser "CMN R0, R0, ROR #3" (MachineState.initWithFlags "0000"|> (^=) R0 -1)))]                
                VisualUnitTest "CMN test 7" "MOV R0, #1 \n MOV R1, #8 \n CMN R1, R0, ROR #3" (stringConcat (parser "CMN R1, R0, ROR #3" (MachineState.initWithFlags "0000"|> (^=) R0 1|> (^=) R1 8))) [R 0, ((^.) R0 (parser "CMN R1, R0, ROR #3" (MachineState.initWithFlags "0000"|> (^=) R0 1|> (^=) R1 8)))]                
                VisualUnitTest "CMN test 8" "MOV R0, #1 \n LSRS R4, R0, #1 \n MOV R1, #8 \n CMN R1, R0, RRX" (stringConcat (parser "CMN R1, R0, RRX" (MachineState.initWithFlags "0110"|> (^=) R0 1|> (^=) R1 8))) [R 0, ((^.) R0 (parser "CMN R1, R0, RRX" (MachineState.initWithFlags "0110"|> (^=) R0 1|> (^=) R1 8)))]                
                //VisualUnitTest "RandTest" ("MOV R0, #1 \n LSRS R4, R0, #1 \n MOV R1, #8 \n "+inst) (stringConcat (parser inst (MachineState.initWithFlags "0010"|> (^=) R0 1|> (^=) R1 8))) [R 0, ((^.) R0 (parser inst (MachineState.initWithFlags "0010"|> (^=) R0 1|> (^=) R1 8)))]                
                //VisualUnitTest "SUBSMI test" "SUBS R0, R0, #0" (stringConcat (sub_ (R0, R0, Operand(Literal 0,NoShift), (MachineState.initWithFlags "0100"), true, None))) [R 0, ((^.) R0 (MachineState.initWithFlags "0100"))];
                //VisualUnitTest "ADDS test" "ADDS R0, R0, #-1" (stringConcat (add_ (R0, R0, Operand(Literal -1,NoShift), (MachineState.initWithFlags "0000"), true, None))) [R 0, ((^.) R0 (add_ (R0, R0, Operand(Literal -1,NoShift), (MachineState.initWithFlags "0000"), true, None)))]
                //VisualUnitTest "ADCS test" "ADCS R0, R0, #-1" (stringConcat (adc_ (R0, R0, Operand(Literal -1,NoShift), (MachineState.initWithFlags "0000"), true, None))) [R 0, ((^.) R0 (adc_ (R0, R0, Operand(Literal -1,NoShift), (MachineState.initWithFlags "0000"), true, None)))]

                //VisualUnitTest "This ADDS test should fail" "ADDS R0, R0, #4" "0000" [R 0, 4; R 1, 1] // R1 should be 0 but is specified here as 1
            ]@([1..50] |> List.map (randFunc "0000"))@([1..100] |> List.map (randFunc "0010")))//150 instruction random instructions to be tested
        let rc = runTests seqConfig tests
        System.Console.ReadKey() |> ignore                
        rc // return an integer exit code - 0 if all tests pass