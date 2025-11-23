signature EXTRA =
sig
  include GRAPH
  type position
  val eqPosition: position * position -> bool
  val comparePosition: position * position -> order
  val positionToInt: position -> int

  type mapping
  val !! : mapping * position -> position list

  type functions =
    { numNodes: int
    , positionToLabel: position -> label option
    , labelToPosition: label option -> position
    , successors: mapping
    , predecessors: mapping
    }

  val numNodes: graph -> int
  val positionToLabel: graph -> position -> label option
  val labelToPosition: graph -> label option -> position
  val successors: graph -> mapping
  val predecessors: graph -> mapping
  val precalculate: graph -> functions
end
