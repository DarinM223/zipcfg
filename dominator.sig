signature DOMINATOR =
sig
  include EXTRA
  datatype tree = Leaf of label option | Node of label option * tree list

  val dominatorTree: graph -> tree
end
