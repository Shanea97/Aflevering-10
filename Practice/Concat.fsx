let mutable MooseList = [1;2;3;4]
MooseList <- 10 :: MooseList 
printfn "%A" MooseList 

let mutable NewMoose = [10;11;12;13] 
NewMoose <- MooseList @ NewMoose
printfn "%A" NewMoose