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

printfn "White-Box Test for decrease of repLen for moose"
let a = moose (5)
printfn "With the following input for moose: \nrepLen: %i" 5
printfn "Does the repLen for the moose decrease? %b\n" (a.reproduction<5) //expected output true 

printfn "White-Box Test for reset repLen for moose"
let a1 = moose (3)
a1.resetReproduction()
printfn "With the following input for moose: \nrepLen: %i" 3
printfn "Does repLen for the moose reset to the initial repLen with resetReproduction? %b\n" (a1.reproduction=3) //expected output true 

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

printfn "White-Box Test for decrease of repLen for wolf"
let b = wolf (5,6)
printfn "With the following input for wolf: \nrepLen: %i \n hungLen: %i" 5 6 
printfn "Does repLen for the wolf decrease? %b\n" (b.reproduction<5) //expected output true 

printfn "White-Box Test for reset repLen for wolf"
let b1 = wolf (7,4)
b1.resetReproduction()
printfn "With the following input for wolf: \nrepLen: %i \n hungLen: %i" 7 4 
printfn "Does repLen for the wolf reset to the initial repLen with resetReproduction? %b\n" (b1.reproduction=7) //expected output true 


/// A board is a chess-like board implicitly representedy by its width and coordinates of the animals.
type board =
  {width : int;
   mutable moose : moose list;
   mutable wolves : wolf list;}

/// An environment is a chess-like board with all animals and implenting all rules.
type environment (boardWidth : int, NMooses : int, mooseRepLen : int, NWolves : int, wolvesRepLen : int, wolvesHungLen : int, verbose : bool) =
  let _board : board = {
    width = boardWidth;
    moose = List.init NMooses (fun i -> moose(mooseRepLen));
    wolves = List.init NWolves (fun i -> wolf(wolvesRepLen, wolvesHungLen));
  }

  /// Project the list representation of the board into a 2d array.
  let draw (b : board) : char [,] =
    let arr = Array2D.create<char> boardWidth boardWidth eSymbol
    for m in b.moose do
      Option.iter (fun p -> arr.[fst p, snd p] <- mSymbol) m.position
    for w in b.wolves do
      Option.iter (fun p -> arr.[fst p, snd p] <- wSymbol) w.position
    arr

  /// return the coordinates of any empty field on the board.
  let anyEmptyField (b : board) : position =
    let arr = draw b
    let mutable i = rnd.Next b.width
    let mutable j = rnd.Next b.width
    while arr.[i,j] <> eSymbol do
      i <- rnd.Next b.width
      j <- rnd.Next b.width
    (i,j)

  // populate the board with animals placed at random.
  do for m in _board.moose do
       m.position <- Some (anyEmptyField _board)
  do for w in _board.wolves do
       w.position <- Some (anyEmptyField _board)

  member this.size = boardWidth*boardWidth
  member this.count = _board.moose.Length + _board.wolves.Length
  member this.board = _board
  member this.tick () =

    _board.wolves <- _board.wolves |> List.filter (fun x -> x.hunger >= 0) // Fjerner ulve fra listen som har sult 0

    for i = 0 to _board.moose.Length - 1 do

      match _board.moose.[i].tick () with
        | Some (moose) ->
          if _board.moose.[i].position.IsSome then
            moose.position <- Some (anyEmptyField _board)
            _board.moose <- moose  :: _board.moose
        | None ->
          _board.moose.[i].position <- Some (anyEmptyField _board)

    for i = 0 to _board.wolves.Length - 1 do
      match _board.wolves.[i].tick () with
        | Some (wolf) ->
          if _board.wolves.[i].position = None then
            wolf.position <- Some (anyEmptyField _board)
            _board.wolves <- wolf :: _board.wolves
        | None ->
          // if (_board.wolves.[i]._hunger = 0) then
          _board.wolves.[i].position <- Some (anyEmptyField _board)

    for i = 0 to _board.moose.Length - 1 do
      for j = 0 to _board.wolves.Length - 1 do
        // tjek for ulv til venstre
        if (fst _board.wolves.[j].position.Value + 1, snd _board.wolves.[j].position.Value) = _board.moose.[j].position.Value
        then
        //elg spises
          _board.wolves.[j].position <- Some (_board.moose.[i].position.Value)
          _board.wolves.[j].resetHunger ()
          _board.moose.[i].position <- None
        //ulv til højre
        elif (fst _board.wolves.[j].position.Value - 1, snd _board.wolves.[j].position.Value) = _board.moose.[j].position.Value then
          _board.wolves.[j].position <- Some (_board.moose.[i].position.Value)
          _board.wolves.[j].resetHunger ()
          _board.moose.[i].position <- None
        //ulv nedenfor
        elif (fst _board.wolves.[j].position.Value, snd _board.wolves.[j].position.Value + 1) = _board.moose.[j].position.Value then
          _board.wolves.[j].position <- Some (_board.moose.[i].position.Value)
          _board.wolves.[j].resetHunger ()
          _board.moose.[i].position <- None
        //ulv ovenfor
        else if (fst _board.wolves.[j].position.Value, snd _board.wolves.[j].position.Value - 1) = _board.moose.[j].position.Value then
          _board.wolves.[j].position <- Some (_board.moose.[i].position.Value)
          _board.wolves.[j].resetHunger ()
          _board.moose.[i].position <- None
      
        _board.moose <- _board.moose |> List.filter (fun x -> x.position.IsSome) 
      

  override this.ToString () =
    let arr = draw _board
    let mutable ret = "  "
    for j = 0 to _board.width-1 do
      ret <- ret + string (j % 10) + " "
    ret <- ret + "\n"
    for i = 0 to _board.width-1 do
      ret <- ret + string (i % 10) + " "
      for j = 0 to _board.width-1 do
        ret <- ret + string arr.[i,j] + " "
      ret <- ret + "\n"
    ret


printfn "White-Box Test for wolves eating mooses"
let x = environment(10, 30, 11, 10, 11, 11, true)
printfn "With the following input: \nboardWidth: %i \nmoose: %i \nmooseRepLen: %i \nwolves: %i \nwolvesRepLen: %i \nwolvesHungLen: %i" 10 30 11 10 11 11 
for i = 1 to 10 do
  x.tick()
  printfn "%i. tick: Does any of the mooses get eaten by wolves? %b" i (x.board.moose.Length < 30) //expected outcome true

printfn "White-Box Test for wolves reproduction"
let x1 = environment(10, 0, 0, 10, 4, 11, true)
printfn "With the following input: \nboardWidth: %i \nmoose: %i \nmooseRepLen: %i \nwolves: %i \nwolvesRepLen: %i \nwolvesHungLen: %i" 10 0 0 10 4 11 
for i = 1 to 15 do
  x1.tick()
  printfn "%i. tick: Does the wolves reproduce? Is there still 10 wolves at maximum? %b" i (x1.board.wolves.Length > 10) //expected outcome true

printfn "White-Box Test for mooses reproduction"
let x2 = environment(10, 30, 5, 0, 0, 0, true)
printfn "With the following input: \nboardWidth: %i \nmoose: %i \nmooseRepLen: %i \nwolves: %i \nwolvesRepLen: %i \nwolvesHungLen: %i" 10 0 30 0 0 0 
for i = 1 to 15 do
  x2.tick()
  printfn "%i. tick: Does the mooses reproduce? %b" i (x2.board.moose.Length > 30) //expected outcome true

printfn "White-Box Test for wolves death from hunger"
let x3 = environment(10, 0, 0, 10, 10, 2, true)
printfn "With the following input: \nboardWidth: %i \nmoose: %i \nmooseRepLen: %i \nwolves: %i \nwolvesRepLen: %i \nwolvesHungLen: %i" 10 0 0 10 10 2
for i = 1 to 5 do
  x3.tick()
  printfn "%i. tick: Does any of the wolves die from hunger? %b" i (x3.board.wolves.Length < 10) //expected outcome true


(*
let NewEnvironment = environment(10, 4, 2, 5, 4, 8, true)

for i = 0 to 5 do
  NewEnvironment.tick()
  printfn "%s" (NewEnvironment.ToString())
  for j = 0 to NewEnvironment.board.moose.Length-1 do   
    printfn "Moose %i's RepLen: %A\nMoose %i's position: %A" j NewEnvironment.board.moose.[j].reproduction  j NewEnvironment.board.moose.[j].position
  for x = 0 to NewEnvironment.board.wolves.Length-1 do 
    printfn "Wolf %i's RepLen: %A\nWolf %i's position: %A" x NewEnvironment.board.wolves.[x].reproduction x NewEnvironment.board.wolves.[x].position
*)

//