type Moth(x,y) = 
    let LightPosition = (x*7.54,y*7.54)
    let mutable pos = (x,y)
    member this.GetPosition = pos
    member this.Distance = ((fst LightPosition - fst pos),(snd LightPosition - snd pos))
    member this.MoveToLight = pos <- (fst pos+((fst LightPosition - fst pos)/2.0),snd pos+((snd LightPosition - snd pos)/2.0))


let NewMoth = Moth(10.5,15.4)
printfn "\nGetposition Start: %A" NewMoth.GetPosition
printfn "Distance to light start: %A" NewMoth.Distance
NewMoth.MoveToLight
printfn "\nGetPosition Mid1: %A" NewMoth.GetPosition
printfn "Distance to light Mid1: %A\n " NewMoth.Distance
NewMoth.MoveToLight
printfn "GetPosition Mid2: %A" NewMoth.GetPosition
printfn "Distance to light Mid2: %A\n" NewMoth.Distance
NewMoth.MoveToLight
printfn "GetPosition end: %A" NewMoth.GetPosition
printfn "Distance to light end: %A\n" NewMoth.Distance
// let a = (45,115)
// let b = (25,35)
// let c = ((fst a - fst b), (snd a - snd b))
// printfn "%A" c 
