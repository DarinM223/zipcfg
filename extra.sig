signature EXTRA =
sig
  include GRAPH
  type position
  val eqPosition: position * position -> bool
  val comparePosition: position * position -> order
  val positionToInt: position -> int
  val positionFromInt: int -> position

  type functions =
    { numNodes: int
    , positionToLabel: position -> label option
    , labelToPosition: label option -> position
    , successors: position -> position list
    , predecessors: position -> position list
    }

  val numNodes: graph -> int
  val positionToLabel: graph -> position -> label option
  val labelToPosition: graph -> label option -> position
  val successors: graph -> position -> position list
  val predecessors: graph -> position -> position list
  val precalculate: graph -> functions
end
