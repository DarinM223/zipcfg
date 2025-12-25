functor DominatorFn(G: EXTRA where type uid = int): DOMINATOR =
struct
  open G
  datatype tree = Leaf of G.label option | Node of G.label option * tree list
  local
    fun showOption f (SOME s) = "SOME " ^ f s
      | showOption _ NONE = "NONE"
    val rec tree = fn tree_0 =>
      fn Leaf t0 => "Leaf " ^ "(" ^ showOption G.showLabel t0 ^ ")"
       | Node (t1, t2) =>
        "Node " ^ "("
        ^
        String.concatWith ", "
          [ showOption G.showLabel t1
          , "[" ^ String.concatWith ", " (List.map tree_0 t2) ^ "]"
          ] ^ ")"
    val tree = fn () => let val rec tree_0 = fn ? => tree tree_0 ? in tree_0 end
  in val showTree = tree ()
  end
  local
    fun eqList eq (x :: xs, y :: ys) =
          eq (x, y) andalso eqList eq (xs, ys)
      | eqList _ ([], []) = true
      | eqList _ _ = false
    fun eqOption eq (SOME x, SOME y) = eq (x, y)
      | eqOption _ (NONE, NONE) = true
      | eqOption _ _ = false
    val rec tree = fn tree_0 =>
      fn (Leaf t0, Leaf t1) => eqOption G.eqLabel (t0, t1)
       | (Node (t2, t3), Node (t4, t5)) =>
        eqOption G.eqLabel (t2, t4) andalso eqList tree_0 (t3, t5)
       | _ => false
    val tree = fn () => let val rec tree_0 = fn ? => tree tree_0 ? in tree_0 end
  in val eqTree = tree ()
  end

  datatype node_type = Undefined | Defined of G.position
  fun idom ({labelToPosition, numNodes, predecessors, ...}: G.functions) graph =
    let
      val idom = Array.tabulate (numNodes, fn _ => Undefined)
      val setDoms = fn (pos, v) =>
        Array.update (idom, G.positionToInt pos, Defined v)
      val doms = fn pos =>
        case Array.sub (idom, G.positionToInt pos) of
          Defined pos => pos
        | Undefined => raise LibBase.NotFound

      fun intersect (b1, b2) =
        let
          val finger1 = ref b1
          val finger2 = ref b2
        in
          (* positions are in reverse postorder not postorder like the paper *)
          while not (G.eqPosition (!finger1, !finger2)) do
            ( while G.comparePosition (!finger1, !finger2) = GREATER do
                (finger1 := doms (!finger1))
            ; while G.comparePosition (!finger2, !finger1) = GREATER do
                (finger2 := doms (!finger2))
            );
          !finger1
        end

      val changed = ref true
      fun goBlock block =
        let
          val blockPos = labelToPosition (G.blockLabel block)
        in
          case predecessors blockPos of
            p :: ps =>
              let
                fun foldPredecessor (pred, acc) =
                  (doms pred; intersect (pred, acc))
                  handle LibBase.NotFound => acc
                val newIdom = List.foldl foldPredecessor p ps
                val shouldUpdate = not (G.eqPosition (doms blockPos, newIdom))
                                   handle LibBase.NotFound => true
              in
                if shouldUpdate then
                  (setDoms (blockPos, newIdom); changed := true)
                else
                  ()
              end
          | [] => ()
        end
      val rpo = G.reversePostorderDfs graph
    in
      setDoms (labelToPosition NONE, labelToPosition NONE);
      while !changed do (changed := false; List.app goBlock rpo);
      doms
    end

  fun dominatorTree
    ({numNodes, labelToPosition, positionToLabel, ...}: G.functions) idom graph =
    let
      val childrenMapping =
        Array.tabulate (numNodes, fn _ => IntRedBlackSet.empty)
      fun addChildren block =
        let
          val parent = G.positionToInt (idom block)
        in
          Array.update (childrenMapping, parent, IntRedBlackSet.add
            (Array.sub (childrenMapping, parent), G.positionToInt block))
        end
      fun buildTree node =
        let
          val children =
            (List.filter (fn pos => not (G.eqPosition (pos, node)))
             o List.map G.positionFromInt o IntRedBlackSet.toList)
              (Array.sub (childrenMapping, G.positionToInt node))
        in
          case children of
            [] => Leaf (positionToLabel node)
          | _ => Node (positionToLabel node, List.map buildTree children)
        end
    in
      IntRedBlackMap.app (addChildren o labelToPosition o G.blockLabel) graph;
      buildTree (labelToPosition NONE)
    end

  fun dominatorFrontier
    ({numNodes, labelToPosition, predecessors, ...}: G.functions) idom graph =
    let
      val frontier = Array.tabulate (numNodes, fn _ => IntRedBlackSet.empty)
      fun addToFrontier pos n =
        let
          val pos = G.positionToInt pos
          val n = G.positionToInt n
        in
          Array.update (frontier, pos, IntRedBlackSet.add
            (Array.sub (frontier, pos), n))
        end
      fun go b =
        let
          val preds = predecessors b
          fun goPred p =
            let
              val runner = ref p
            in
              while not (G.eqPosition (!runner, idom b)) do
                (addToFrontier (!runner) b; runner := idom (!runner))
            end
        in
          if List.length preds >= 2 then List.app goPred preds else ()
        end
    in
      IntRedBlackMap.app (go o labelToPosition o G.blockLabel) graph;
      fn p =>
        List.map G.positionFromInt (IntRedBlackSet.toList
          (Array.sub (frontier, G.positionToInt p)))
    end
end
