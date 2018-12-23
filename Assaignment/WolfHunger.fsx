module Animals

type symbol = char
type position = int * int
type neighbour = position * symbol

let mSymbol : symbol = 'm'
let wSymbol : symbol = 'w'
let eSymbol : symbol = ' '
let rnd = System.Random ()

/// An animal is a base class. It has a position and a reproduction counter.
type animal (symb : symbol, repLen : int) =
  let mutable _reproduction = rnd.Next(1,repLen) // repLen is Reproduction length
  let mutable _pos : position option = None
  let _symbol : symbol = symb

  member this.symbol = _symbol
  member this.position
    with get () = _pos
    and set aPos = _pos <- aPos
  member this.reproduction = _reproduction
  member this.updateReproduction () =
    _reproduction <- _reproduction - 1
  member this.resetReproduction () =
    _reproduction <- repLen
  override this.ToString () =
    string this.symbol


/// A moose is an animal
type moose (repLen : int) =
    inherit animal (mSymbol, repLen)

    member this.tick () : moose option = /// if repLen =  0 then make new moose calf
        base.updateReproduction()
        if base.reproduction = 0 then
            base.resetReproduction()
            Some (moose(repLen))
        else
            None

/// A wolf is an animal with a hunger counter
type wolf (repLen : int, hungLen : int) = // hungLen is its hunger counter
  inherit animal (wSymbol, repLen)
  let mutable _hunger = hungLen

  member this.hunger = _hunger
  member this.updateHunger () =
    _hunger <- _hunger - 1
    if _hunger <= 0 then
      this.position <- None // Starve to death
  member this.resetHunger () =
    _hunger <- hungLen
  member this.tick () : wolf option =
    base.updateReproduction()
    this.updateHunger()
    if base.reproduction = 0 then
      base.resetReproduction()
      Some (wolf(repLen, hungLen))
    else
        None


let AplhaMoose = moose(5)
let mutable MooseList = [AplhaMoose]
for i = 0 to 25 do 
  for j = 0 to MooseList.Length-1 do 
    MooseList.[j].tick()
    // printfn "Moose %i's RepLen: %A" j MooseList.[j].reproduction
    if MooseList.[j].reproduction = 1 then 
      MooseList <- moose(5) :: MooseList
  printfn "MooseList: %A" MooseList

 
 
 
 
 
  // AplhaMoose.tick()
  // printfn "AlphaMoose RepLen: %A" AplhaMoose.reproduction
  // if AplhaMoose.reproduction = 1 then 
  //   MooseList <- moose(5) :: MooseList
  //   printfn "MooseList: %A" MooseList
  // else 
  //   for i = 1 to MooseList.Length-1 do 
  //     MooseList.[i].tick ()
  //     printfn "Moose %i's RepLen %i" i MooseList.[i].reproduction
  //     if AplhaMoose.reproduction = 1 then 
  //     MooseList <- moose(5) :: MooseList
  //     printfn "MooseList: %A" MooseList
      




let AplhaWolf = wolf(5,7)



let BetaWolf = wolf(5,2)

let CharlieWolf = wolf(5,8)

let DeltaWolf = wolf(5,2)

let EchoWolf = wolf(5,6)
// let mutable WolfList = [|AplhaWolf;BetaWolf;CharlieWolf;DeltaWolf|]
// for j = 0 to WolfList.Length-1 do 
//     WolfList <- WolfList |> Array.filter (fun x -> x.hunger > 0)
//     printfn "WolfList: %A" WolfList
//     for i = 0 to WolfList.Length-1 do 
//         WolfList.[i].tick()
//         printfn "Wolf %A's Hunger: %A" i WolfList.[i].hunger
//         printfn "Wolf %A's RepLen: %A" i WolfList.[i].reproduction

