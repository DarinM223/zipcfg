signature DOMINATOR =
sig
  include EXTRA
  datatype tree = Leaf of label option | Node of label option * tree list
  val showTree: tree -> string
  val eqTree: tree * tree -> bool

  val idom: functions -> graph -> (position -> position)
  val dominatorTree: functions -> (position -> position) -> graph -> tree
  val dominatorFrontier: functions
                         -> (position -> position)
                         -> graph
                         -> (position -> position list)
end
