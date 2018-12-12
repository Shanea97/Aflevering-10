/// This is the rules for the simulation in an enviroment where wolves and Elks live side by side:
/// 

/// An enviroment with a size of (n X n) 
/// All living creatures must have a coordinate within the grid, and only one animal can stand on a specific grid. If an animal is born, an empty field must be added. At the start of the simulation, there vil be "u" wolves and "e" mooses, placed randomly within the grid. 
/// The enviroment must be updated in time-units called Tics, to this the simulation will run x Tics. Within each Tic, animals can do following. Move, Reproduce, eat(only acounts for the wolf eating mooses). Only one animal will act at a time, and the order will be random.
/// An animal is able to move 1 grid to either of it's 8 sides (If it is empty)
/// Each species will have an specific reproducing time(f) given by Tics, that will count down. If the counter reaches 0 for a living animal, and there is an empty space next to it, and empty neighbour grid will be added. The mother's reproducing counter will reset. 
/// Wolves have starvation(s) given in tics, counting down. If "s" reaches 0, the wolf dies, and is removed from the enviroment. 
/// A wolf can eat a moose, if it is in the neighbouring field-grid. The moose will be removed from the enviroment, the wolf will move to the mooses spot, and it's starvation counter will be reset. 
/// For each Tic, the reproducing counter and starvation counter for each animal will be reduced by 1. 