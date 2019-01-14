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
    this.updateHunger()
    base.updateReproduction()
    if base.reproduction = 0 then
      base.resetReproduction()
      Some (wolf(repLen, hungLen))
    else
      None

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

  let anyEmptyOrMooseField (b : board) : position =
    let arr = draw b
    let mutable i = rnd.Next b.width
    let mutable j = rnd.Next b.width
    while arr.[i,j] <> wSymbol do
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
  member this.moosecount = _board.moose.Length
  member this.wolfcount = _board.wolves.Length
  member this.tick () =

    _board.wolves <- _board.wolves |> List.filter (fun x -> x.position.IsSome)
    _board.wolves <- _board.wolves |> List.filter (fun x -> x.hunger > 0)
    _board.moose <- _board.moose |> List.filter (fun x -> x.position.IsSome)
    (*for i = 0 to this.moosecount - 1 do
        _board.moose.[i].tick()
        if _board.moose.[i].reproduction = 1 then

            let newmoose = moose(mooseRepLen)
            newmoose.position <- Some (anyEmptyField _board)
            _board.moose <- _board.moose @ [newmoose]
        _board.moose.[i].position <- Some (anyEmptyField _board)

    for j = 0 to this.wolfcount - 1 do
        _board.wolves.[j].tick ()
        if _board.wolves.[j].reproduction = 1 then
            let newWolf = wolf(wolvesRepLen,wolvesHungLen)
            newWolf.position <- Some (anyEmptyField _board)
            _board.wolves <- _board.wolves @ [newWolf]
        _board.wolves.[j].position <- Some (anyEmptyField _board)*)
    let elge = _board.moose.Length - 1
    for i = 0 to elge do
        let tick = _board.moose.[i].tick()
        let pos = if _board.moose.[i].position.IsSome then
         Some (anyEmptyField _board) else None
        if tick.IsSome && pos.IsSome then
          let newmoose = tick.Value
          newmoose.position <- pos
          newmoose.resetReproduction()
          _board.moose <- _board.moose @ [newmoose]
        else
          if _board.moose.[i].position.IsSome then
            let pos = Some (anyEmptyField _board)
            if pos.IsSome then
              _board.moose.[i].position <- pos

    let ulve = _board.wolves.Length - 1
    for j = 0 to ulve do
        let tick = _board.wolves.[j].tick ()
        let pos = if _board.wolves.[j].position.IsSome then Some (anyEmptyField _board) else None
        if tick.IsSome && pos.IsSome then
          let newwolf = tick.Value
          newwolf.position <- pos
          newwolf.resetReproduction()
          newwolf.resetHunger()
          _board.wolves <- _board.wolves @ [newwolf]
        else
          if _board.wolves.[j].position.IsSome then
            let offer = if _board.wolves.[j].position.IsSome then Some (anyEmptyField _board) else None

            let offer1 = if _board.wolves.[j].position.IsSome then Some (anyEmptyOrMooseField _board) else None

            if offer1.IsSome then
              let exists = List.exists (fun (elem:moose) -> elem.position.Value = offer1.Value) _board.moose
              if exists then 
                (List.find (fun (x: moose) -> x.position.Value = offer1.Value) _board.moose).position <- None
              _board.wolves.[j].resetHunger()
            elif pos.IsSome then
              _board.wolves.[j].position <- offer1
            else
              _board.wolves.[j].position <- _board.wolves.[j].position


    //forsøg på at implementere spisning, der ikke virker
    //for i = 0 to _board.moose.Length - 1 do
    //  for j = 0 to _board.wolves.Length - 1 do
    //    // tjek for ulv til venstre
    //    if (fst _board.wolves.[j].position.Value + 1, snd _board.wolves.[j].position.Value) = _board.moose.[j].position.Value
    //    then
        //elg spises
    //      _board.wolves.[j].position <- Some (_board.moose.[i].position.Value)
    //      _board.wolves.[j].resetHunger ()
    //      _board.moose.[i].position <- None
        //ulv til højre
    //    elif (fst _board.wolves.[j].position.Value - 1, snd _board.wolves.[j].position.Value) = _board.moose.[j].position.Value then
    //      _board.wolves.[j].position <- Some (_board.moose.[i].position.Value)
    //      _board.wolves.[j].resetHunger ()
    //      _board.moose.[i].position <- None
        //ulv nedenfor
    //    elif (fst _board.wolves.[j].position.Value, snd _board.wolves.[j].position.Value + 1) = _board.moose.[j].position.Value then
    //      _board.wolves.[j].position <- Some (_board.moose.[i].position.Value)
    //      _board.wolves.[j].resetHunger ()
    //      _board.moose.[i].position <- None
        //ulv ovenfor
    //    else if (fst _board.wolves.[j].position.Value, snd _board.wolves.[j].position.Value - 1) = _board.moose.[j].position.Value then
    //      _board.wolves.[j].position <- Some (_board.moose.[i].position.Value)
    //      _board.wolves.[j].resetHunger ()
    //      _board.moose.[i].position <- None


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

(*
let NewEnvironment = environment(10, 2, 15, 1, 7, 8, true)
//Dette kan køre, og viser hvordan ulve og elge bevæger sig rundt, samt reproduktion:
for i = 0 to 10 do
  NewEnvironment.tick()
  printfn "%s" (NewEnvironment.ToString())
  for j = 0 to NewEnvironment.board.moose.Length-1 do
    printfn "Moose %i's RepLen: %A\nMoose %i's position: %A" j NewEnvironment.board.moose.[j].reproduction  j NewEnvironment.board.moose.[j].position
  for x = 0 to NewEnvironment.board.wolves.Length-1 do
    printfn "Wolf %i's RepLen: %A\nWolf %i's position: %A" x NewEnvironment.board.wolves.[x].reproduction x NewEnvironment.board.wolves.[x].position
*)
