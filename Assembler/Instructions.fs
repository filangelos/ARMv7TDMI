module InstructionsModule =

    open RegisterModule.Register
    open Common

    //Op2 can be either a register or a literal
    //type OP2 = Reg of Register | Op of Data

    let addWithCarry (regD: string) (regN: Register) (op2: Register) (carry: bool) =
        let regNValue = registerValueOf regN

        // let op2ValueOf (op2: OP2) =
        //     match op2 with
        //     | Reg(_) -> registerValueOf op2
        //     | Op(data) -> data

        let op2Value = registerValueOf op2

        let (carryVal: Data) = match carry with
                       |true -> uint32 1
                       |false -> uint32 0

        let result = regNValue + op2Value + carryVal


        makeRgisterWith (regD) (result)


module test =
    open InstructionsModule
    open RegisterModule.Register
    
    let r0 = makeRgisterWith "R0" (uint32 1)
    let r1 = makeRgisterWith "R1" (uint32 2)

    let carry = false

    let r2 = addWithCarry "R2" r0 r1 carry
    printfn "%A" r2

    let carry1 = false

    let newr2 = addWithCarry "R2" r0 r1 carry1
    printfn "%A" newr2
