functor DominatorFn(G: EXTRA): DOMINATOR =
struct
  open G
  datatype tree = Leaf of G.label option | Node of G.label option * tree list

  fun dominatorTree _ = raise Fail "not implemented yet"
end
