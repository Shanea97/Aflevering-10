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

/// A board is a chess-like board implicitly representedy by its width and coordinates of the animals.
type board =
  {width : int;
   mutable moose : moose list;
   mutable wolves : wolf list;}
/// An environment is a chess-like board with all animals and implementing all rules.
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
  member this.moosecount = _board.moose.Length
  member this.wolfcount = _board.wolves.Length
  member this.board = _board
  member this.tick () =

    _board.wolves <- _board.wolves |> List.filter (fun x -> x.hunger >= 0) // Fjerner ulve fra listen som har sult 0 // Funktionen her fjerner åbentbart alle ulvene når de burde reproducere. 

    //_board.wolves <- _board.wolves |> List.filter (fun x -> x.position.IsSome) // Her ser det umiddebart ud til at ulvene reproducere, men der er bare ingen der dør

    _board.moose <- _board.moose |> List.filter (fun x -> x.position.IsSome) // Fjerner elge fra listen som har position None


    for i = 0 to _board.moose.Length - 1 do
      match _board.moose.[i].tick () with
        | Some (moose) ->
          if not _board.moose.[i].position.IsSome then
            moose.position <- Some (anyEmptyField _board)
            _board.moose <- moose  :: _board.moose
        | None ->
          _board.moose.[i].position <- Some (anyEmptyField _board)

    for i = 0 to _board.wolves.Length - 1 do
      match _board.wolves.[i].tick () with
        | Some (wolf) ->
          if not _board.wolves.[i].position.IsSome then
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
        elif (fst _board.wolves.[j].position.Value, snd _board.wolves.[j].position.Value - 1) = _board.moose.[j].position.Value then
          _board.wolves.[j].position <- Some (_board.moose.[i].position.Value)
          _board.wolves.[j].resetHunger ()
          _board.moose.[i].position <- None

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
