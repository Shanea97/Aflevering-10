type Gasoline (Effektivitet) =
    let mutable Gas = 0.0
    member this.AddGas(liters) = Gas <- Gas + liters 
    member this.GasLeft = Gas 
    member this.Drive(km) = 
        if Gas - km / Effektivitet > 0.0 then
            printfn "Let's go"
            Gas <- Gas - km / Effektivitet
        else 
            printfn "You havent fuled your car proberly"

let Audi = Gasoline(2.2453)
Audi.AddGas (15.5)
let GolfTureq = Gasoline (0.5)
GolfTureq.AddGas (100.00)
GolfTureq.Drive (45.23)
printfn "How much gast is left: %A\n %A\nAfter your drive, how much gas do you have left: %A" Audi.GasLeft (Audi.Drive(30.3)) Audi.GasLeft
printfn "Golf drive: %f" GolfTureq.GasLeft 
