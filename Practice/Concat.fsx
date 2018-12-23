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
  
  
  
  
let Distance (W:wolf) (M:moose)  =
    let WolfPos = W.position 
    let MoosePos = M.position 
    printfn "Wolf: %A\nMoose: %A" WolfPos MoosePos
  //  let Distance = int(float(sqrt(fst (Some MoosePos)- (fst (Some WolfPos))**2 + (( (snd (Some MoosePos)- (snd (Some WolfPos))**2))))))
   // Distance

let Wolf = wolf(5,5)
let Moose = moose(7)
let WP = Wolf.position <- Some  (1,5)
let MP = Moose.position <- Some (5,1)
let WP1 = Some (fst WP)
printfn "WP1: %A" WP1
(Distance Wolf Moose)