functor Extra(G: GRAPH where type uid = int) :> EXTRA =
struct
  open G

  type position = int
  val eqPosition = op=
  val comparePosition = Int.compare
  val positionToInt = fn a => a

  type mapping = IntRedBlackSet.set array
  val !! = IntRedBlackSet.toList o Array.sub

  type functions =
    { numNodes: int
    , positionToLabel: position -> label option
    , labelToPosition: label option -> position
    , successors: mapping
    , predecessors: mapping
    }

  val numNodes = List.length o G.reversePostorderDfs
  fun positionToLabel' rpo : position -> label option =
    let val arr = Array.fromList (List.map G.blockLabel rpo)
    in fn p => Array.sub (arr, p)
    end
  val positionToLabel = positionToLabel' o G.reversePostorderDfs

  fun labelToPosition' numNodes rpo : label option -> position =
    let
      val table: int IntHashTable.hash_table =
        IntHashTable.mkTable (numNodes, LibBase.NotFound)
      fun go i ((first, _) :: rest) =
            let
              val uid =
                case first of
                  G.Entry => G.entryUid
                | G.Label ((uid, _), _) => uid
            in
              IntHashTable.insert table (uid, i);
              go (i + 1) rest
            end
        | go _ [] = ()
    in
      go 0 rpo;
      fn NONE => IntHashTable.lookup table G.entryUid
       | SOME (uid, _) => IntHashTable.lookup table uid
    end
  fun labelToPosition graph =
    let val rpo = G.reversePostorderDfs graph
    in labelToPosition' (List.length rpo) rpo
    end

  fun successors' rpo labelToPosition =
    let
      val blockSuccs =
        IntRedBlackSet.fromList o List.map (labelToPosition o SOME)
        o G.succsOfLast o G.last o G.unzip
    in
      Array.fromList (List.map blockSuccs rpo)
    end
  fun successors graph : mapping =
    let val rpo = G.reversePostorderDfs graph
    in successors' rpo (labelToPosition' (List.length rpo) rpo)
    end

  fun predecessors' max successors =
    Array.tabulate (max, fn num =>
      let
        val result = ref IntRedBlackSet.empty
        val i = ref 0
      in
        while !i < max do
          ( if IntRedBlackSet.member (Array.sub (successors, !i), num) then
              result := IntRedBlackSet.add (!result, !i)
            else
              ()
          ; i := !i + 1
          );
        !result
      end)
  fun predecessors graph =
    let
      val rpo = G.reversePostorderDfs graph
      val numNodes = List.length rpo
    in
      predecessors' numNodes (successors' rpo (labelToPosition' numNodes rpo))
    end

  fun precalculate graph =
    let
      val rpo = G.reversePostorderDfs graph
      val numNodes = List.length rpo
      val positionToLabel = positionToLabel' rpo
      val labelToPosition = labelToPosition' numNodes rpo
      val successors = successors' rpo labelToPosition
      val predecessors = predecessors' numNodes successors
    in
      { numNodes = numNodes
      , positionToLabel = positionToLabel
      , labelToPosition = labelToPosition
      , successors = successors
      , predecessors = predecessors
      }
    end
end
