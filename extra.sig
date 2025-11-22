signature EXTRA =
sig
  include GRAPH
  type position
  val eqPosition: position * position -> bool
  val comparePosition: position * position -> order

  type mapping
  val !! : mapping * position -> position list

  type functions =
    { numNodes: int
    , positionToLabel: position -> label option
    , labelToPosition: label -> position
    , successors: mapping
    , predecessors: mapping
    }

  val numNodes: graph -> int
  val positionToLabel: graph -> position -> label option
  val labelToPosition: graph -> label -> position
  val successors: graph -> mapping
  val predecessors: graph -> mapping
  val precalculate: graph -> functions
end
