signature DOMINATOR =
sig
  include EXTRA
  datatype tree = Leaf of label option | Node of label option * tree list

  val idom: functions -> graph -> (position -> position)
  val dominatorTree: (position -> position) -> graph -> tree
end
