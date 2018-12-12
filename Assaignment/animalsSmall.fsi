/// A simulator for animals in a closed environment. Presently, two animals are implemented: moose and wolves as prey and preditor.
module animals
/// A symbol to print an animal on a board
type symbol = char
/// A position on a board
type position = int * int
/// Base class for all animals. An animal has a position, age and a time to reproduce.
type animal =
  class
    /// <summary>Create a new animal represented with symbol symb and which reproduces every repLen ticks.</summary>
    /// <param name="symb">A symbol (character) used to represent the animal type when the board is printed.</param>
    /// <param name="repLen">The time in ticks until the animal attempts to produce an offspring.</param>
    new : symb:symbol * repLen:int -> animal
    /// The symbol as string for printing.
    override ToString : unit -> string
    /// The symbol representing this animal.
    member symbol : symbol
    /// Get the position of this animal. If position is None, then the animal is not on the board.
    member position : position option
    /// Set the position of this animal.
    member position : position option with set
    /// Get the reproduction counter in ticks. Starts as repLen and is counted down with every tick. The animals age is repLen - reproduction.
    member reproduction : int
    /// Set the reproduction counter to repLen.
    member resetReproduction : unit -> unit
    /// Reduce the reproduction counter by a tick.
    member updateReproduction : unit -> unit
  end
/// A moose is an animal with methods for updating its age and producing offspring.
type moose =
  class
    inherit animal
    /// <summary>Create a moose with symbol 'm'.</summary>
    /// <param name="repLen">The number of ticks until a moose attempts to produce an offspring.</param>
    new : repLen:int -> moose
    /// Perform a tick for this moose, i.e., call updateReproduction.
    member tick : unit -> moose option
  end
/// A wolf is an animal with hunger and methods for updating its age and hunger and for reproducing offspring. If the wolf has not eaten in a specified number of ticks, then it is taken off the board.
type wolf =
  class
    inherit animal
    /// <summary>Create a wolf with symbol 'w'.</summary>
    /// <param name="repLen">The number of ticks until a wolf attempts to produce an offspring.</param>
    /// <param name="hungLen">The number of ticks since it last ate until a wolf dies.</param>
    new : repLen:int * hungLen:int -> wolf
    /// Reduce the reproduction counter by a tick. If repLen is 0 then a new wolf is returned and the counter is reset to repLen.
    member hunger : int
    /// Set the hunger counter to hungLen.
    member resetHunger : unit -> unit
    /// Reduce the hunger counter by a tick. If hunger reaches 0, then the wolf is removed from the board.
    member updateHunger : unit -> unit
    /// Perform a tick for this wolf, i.e., call updateReproduction and updateHunger and possibly returns cub.
    member tick : unit -> wolf option
  end
/// A square board with length width. The board is implicitly represented by its width and the coordinates in the animals.
type board =
  {
    /// The width of the board. The board size is width x width.
    width: int;
    /// The list of moose. If a moose has position None, then it is not on the board.
    mutable moose: moose list;
    /// The list of wolves. If a wolf has position None, then it is not on the board.
    mutable wolves: wolf list;}
/// An environment. Animals that have no position are considered dead.
type environment =
  class
    /// <summary>Create a new environment.</summary>
    /// <param name="boardWidth">The width of the board.</param>
    /// <param name="NMooses">The initial number of moose.</param>
    /// <param name="NWolves">The initial number of wolves.</param>
    /// <param name="mooseRepLen">The number of ticks until a moose attempts to produce an offspring.</param>
    /// <param name="wolvesRepLen">The number of ticks until a wolf attempts to produce an offspring.</param>
    /// <param name="wolvesHungLen">The number of ticks since it last ate until a wolf dies.</param>
    /// <param name="verbose">If the verbose flag is true, then messages are printed on screen at key events.</param>
    new : boardWidth:int * NMooses:int * mooseRepLen:int * NWolves:int *
          wolvesRepLen:int * wolvesHungLen:int * verbose:bool -> environment
    /// A board as a matrix of symbols for moose and wolves.
    override ToString : unit -> string
    /// The board.
    member board : board
    /// The number of animals on the board.
    member count : int
    /// The positions on the board.
    member size : int
    /// Perform a tick by performing all animal's ticks in random order. Animals perform the following actions: Calves and cubs are added if there is room in a neighbouring position. Wolves eat a random Moose in a neighbouring position. If animals do not give birth, eat or are eaten, then they move to an available neighbouring position.
    member tick : unit -> unit
  end
