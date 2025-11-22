functor GraphFn
  (structure Target: TARGET where type label = int * string
   val showRegs: Target.reg list -> string) :>
  GRAPH
  where type uid = int
  and type Target.instr = Target.instr
  and type Target.reg = Target.reg =
struct
  structure Target = Target
  type uid = int
  fun uidEq a b = a = b
  val entryUid = 0

  type label = uid * string
  type regs = Target.reg list

  datatype locall = Local of bool
  datatype first = Entry | Label of label * locall
  datatype middle = Instruction of Target.instr
  type contedge = {kills: regs, defs: regs, node: label}
  type call =
    { callInstr: Target.instr
    , callContedges: contedge list
    , callUses: regs ref
    , callAltrets: int
    , callUnwindsTo: int
    , callCutsTo: int
    , callReads: string list option
    , callRights: string list option
    }
  datatype last =
    Exit
  | Branch of Target.instr * label
  | CBranch of Target.instr * label * label (* true, false *)
  | Call of call
  | Return of Target.instr * regs

  datatype head =
    First of first
  | Head of head * middle
  and tail =
    Last of last
  | Tail of middle * tail

  type zblock = head * tail
  type block = first * tail

  type graph = block IntRedBlackMap.map
  type zgraph = zblock * block IntRedBlackMap.map
  type nodes = zgraph -> zgraph

  val id = fn (Entry, _) => entryUid | (Label ((uid, _), _), _) => uid
  val blockLabel = fn (Entry, _) => NONE | (Label (l, _), _) => SOME l
  val empty = IntRedBlackMap.singleton (entryUid, (Entry, Last Exit))

  val rec zip: zblock -> block =
    fn (First first, tail) => (first, tail)
     | (Head (head, mid), tail) => zip (head, Tail (mid, tail))

  val unzip: block -> zblock = fn (first, tail) => (First first, tail)

  val rec firstt: head -> first = fn First f => f | Head (h, _) => firstt h
  val first: zblock -> first = fn (h, _) => firstt h
  val rec lastt: tail -> last = fn Last l => l | Tail (_, t) => lastt t
  val last: zblock -> last = fn (_, t) => lastt t

  val gotoStart: zblock -> first * tail = zip
  val rec gotoEnd: zblock -> head * last =
    fn (head, Last last) => (head, last)
     | (head, Tail (mid, tail)) => gotoEnd (Head (head, mid), tail)

  fun focus uid graph =
    let val (graph, block) = IntRedBlackMap.remove (graph, uid)
    in (unzip block, graph)
    end
  val entry = focus entryUid
  val exit = fn graph =>
    let
      fun findExit ((uid, block) :: rest) =
            (case gotoEnd (unzip block) of
               (_, Exit) => uid
             | _ => findExit rest)
        | findExit [] = raise Fail "Exit not found"
      val uid = findExit (IntRedBlackMap.listItemsi graph)
      val (zblock, graph) = focus uid graph
      val (head, last) = gotoEnd zblock
    in
      ((head, Last last), graph)
    end

  structure Blocks =
  struct
    fun insert block graph =
      case block of
        (Entry, _) => IntRedBlackMap.insert (graph, entryUid, block)
      | (Label ((uid, _), _), _) => IntRedBlackMap.insert (graph, uid, block)
    fun union graph1 graph2 =
      IntRedBlackMap.unionWith (fn (a, _) => a) (graph1, graph2)
  end

  val unfocus = fn (zblock, graph) => Blocks.insert (zip zblock) graph

  (* More ways to combine parts *)
  fun htToFirst (head: head) (tail: tail) : first * tail =
    case head of
      First f => (f, tail)
    | Head (h, m) => htToFirst h (Tail (m, tail))
  fun htToLast (head: head) (tail: tail) : head * last =
    case tail of
      Last l => (head, l)
    | Tail (m, l) => htToLast (Head (head, m)) l

  local
    fun prepareForSplicing (graph: graph) (single: tail -> 'a)
      (multi: {entry: tail, exit: head, rest: graph} -> 'a) : 'a =
      let
        (* Ignore the head of the entry block in the spliced graph since we want
           to keep the head of the original block *)
        val ((_, entryTail), graph) = entry graph
      in
        if IntRedBlackMap.isEmpty graph then
          (* single block graph *)
          case lastt entryTail of
            Exit => single entryTail
          | _ => raise Fail "not a single exit block"
        else
          let
            (* multi block graph, go to exit *)
            val (exitBlock, graph) = exit graph
            val (exitHead, exitLast) = gotoEnd exitBlock
          in
            case exitLast of
              Exit => multi {entry = entryTail, exit = exitHead, rest = graph}
            | _ => raise Fail "not a single exit graph"
          end
      end
  in
    (* Result: head ... graph's head *)
    fun spliceHead (head: head) (graph: graph) : graph * head =
      let
        fun spliceOneBlock tail' =
          case htToLast head tail' of
            (head, Exit) => (empty, head)
          | _ => raise Fail "spliced graph without exit"
        fun spliceManyBlocks {entry, exit, rest} =
          (Blocks.insert (htToFirst head entry) rest, exit)
      in
        prepareForSplicing graph spliceOneBlock spliceManyBlocks
      end
    (* Result: graph's tail ... tail *)
    fun spliceTail (graph: graph) (tail: tail) : tail * graph =
      let
        fun spliceOneBlock tail' =
          (* For the one block case the first will always be entry
             so we can use this to convert tail to head *)
          case htToLast (First Entry) tail' of
            (head, Exit) =>
              (case htToFirst head tail of
                 (Entry, tail'') => (tail'', empty)
               | _ => raise Fail "impossible, head is not an entry")
          | _ => raise Fail "spliced graph without exit"
        fun spliceManyBlocks {entry, exit, rest} =
          (entry, Blocks.insert (htToFirst exit tail) rest)
      in
        prepareForSplicing graph spliceOneBlock spliceManyBlocks
      end
  end

  fun spliceHeadOnly (head: head) (graph: graph) : graph =
    let
      val (gentry, graph) = entry graph
    in
      case gentry of
        (First Entry, tail) => Blocks.insert (htToFirst head tail) graph
      | _ => raise Fail "graph to splice doesn't start with entry"
    end

  fun removeEntry graph =
    let
      val (gentry, graph) = entry graph
    in
      case gentry of
        (First Entry, tail) => (tail, graph)
      | _ => raise Fail "removing nonexistent entry"
    end

  fun spliceFocusEntry (((head, tail), blocks): zgraph) (graph: graph) : zgraph =
    let val (tail, blocks') = spliceTail graph tail
    in ((head, tail), Blocks.union blocks' blocks)
    end
  fun spliceFocusExit (((head, tail), blocks): zgraph) (graph: graph) : zgraph =
    let val (blocks', head) = spliceHead head graph
    in ((head, tail), Blocks.union blocks' blocks)
    end

  fun expand (expandMiddle: middle -> graph) (expandLast: last -> graph) graph =
    let
      fun expandTail (((head, tail), blocks): zgraph) : graph =
        case tail of
          Tail (middle, tail) =>
            expandTail
              (spliceFocusExit ((head, tail), blocks) (expandMiddle middle))
        | Last l => Blocks.union (spliceHeadOnly head (expandLast l)) blocks
      fun expandBlock (block, expanded) =
        expandTail (unzip block, expanded)
    in
      IntRedBlackMap.foldl expandBlock empty graph
    end

  fun succsOfLast Exit = []
    | succsOfLast (Branch (_, l)) = [l]
    | succsOfLast (CBranch (_, l1, l2)) = [l1, l2]
    | succsOfLast (Call {callContedges, ...}) = List.map #node callContedges
    | succsOfLast (Return _) = []

  fun reversePostorderDfs (graph: graph) : block list =
    let
      val (entry, blocks) = entry graph
      fun vnode block acc visited k =
        let
          val u = id block
        in
          if IntRedBlackSet.member (visited, u) then
            k (acc, visited)
          else
            vchildren block (getChildren block) acc
              (IntRedBlackSet.add (visited, u)) k
        end
      and getChildren block =
        let
          val uids = List.map #1 (succsOfLast (last (unzip block)))
        in
          List.foldl
            (fn (bid, acc) => IntRedBlackMap.lookup (blocks, bid) :: acc
                              handle LibBase.NotFound => acc) [] uids
        end
      and vchildren block children acc visited k =
        let
          fun next children (acc, visited) =
            case children of
              [] => k (block :: acc, visited)
            | n :: rst => vnode n acc visited (next rst)
        in
          next children (acc, visited)
        end
    in
      vnode (zip entry) [] IntRedBlackSet.empty (fn (acc, _) => acc)
    end

  fun instruction instr ((head, tail), graph) =
    ((head, Tail (Instruction instr, tail)), graph)
  fun label label ((head, tail), graph) =
    ( (head, Last (Branch (Target.goto label, label)))
    , Blocks.insert (Label (label, Local false), tail) graph
    )

  fun unreachable (Last (Branch _)) = ()
    | unreachable (Last Exit) = ()
    | unreachable _ = raise Fail "unreachable code\n"

  fun branch label ((head, tail), graph) =
    ( unreachable tail
    ; ((head, Last (Branch (Target.goto label, label))), graph)
    )
  fun cbranch uses cond {ifso, ifnot} ((head, tail), graph) =
    ( unreachable tail
    ; ( ( head
        , Last (CBranch (Target.cbranch uses cond ifso ifnot, ifso, ifnot))
        )
      , graph
      )
    )
  fun return {uses = regs} ((head, tail), graph) =
    ( unreachable tail
    ; ((head, Last (Return (Target.return {uses = regs}, regs))), graph)
    )

  (************************************************)
  (* Debug string conversions generated by smlgen *)
  (************************************************)
  val showLabel = fn (t0, t1) =>
    "(" ^ String.concatWith ", " [Int.toString t0, "\"" ^ t1 ^ "\""] ^ ")"
  val showLocall = fn Local t0 => "Local " ^ "(" ^ Bool.toString t0 ^ ")"
  val showFirst =
    fn Entry => "Entry"
     | Label (t0, t1) =>
      "Label " ^ "(" ^ String.concatWith ", " [showLabel t0, showLocall t1]
      ^ ")"
  val showMiddle = fn Instruction t0 =>
    "Instruction " ^ "(" ^ Target.showInstr t0 ^ ")"
  val showContedge = fn {kills = t0, defs = t1, node = t2} =>
    "{"
    ^
    String.concatWith ", "
      [ "kills = " ^ showRegs t0
      , "defs = " ^ showRegs t1
      , "node = " ^ showLabel t2
      ] ^ "}"
  local
    fun showOption f (SOME s) = "SOME " ^ f s
      | showOption _ NONE = "NONE"
  in
    val showCall =
      fn { callInstr = t0
         , callContedges = t1
         , callUses = ref t2
         , callAltrets = t3
         , callUnwindsTo = t4
         , callCutsTo = t5
         , callReads = t6
         , callRights = t7
         } =>
        "{"
        ^
        String.concatWith ", "
          [ "callInstr = " ^ Target.showInstr t0
          , "callContedges = " ^ "["
            ^ String.concatWith ", " (List.map showContedge t1) ^ "]"
          , "callUses = " ^ "ref " ^ showRegs t2
          , "callAltrets = " ^ Int.toString t3
          , "callUnwindsTo = " ^ Int.toString t4
          , "callCutsTo = " ^ Int.toString t5
          , "callReads = "
            ^
            showOption
              (fn t0 =>
                 "["
                 ^
                 String.concatWith ", "
                   (List.map (fn t0 => "\"" ^ t0 ^ "\"") t0) ^ "]") t6
          , "callRights = "
            ^
            showOption
              (fn t0 =>
                 "["
                 ^
                 String.concatWith ", "
                   (List.map (fn t0 => "\"" ^ t0 ^ "\"") t0) ^ "]") t7
          ] ^ "}"
  end
  val showLast =
    fn Exit => "Exit"
     | Branch (t0, t1) =>
      "Branch " ^ "("
      ^ String.concatWith ", " [Target.showInstr t0, showLabel t1] ^ ")"
     | CBranch (t2, t3, t4) =>
      "CBranch " ^ "("
      ^ String.concatWith ", " [Target.showInstr t2, showLabel t3, showLabel t4]
      ^ ")"
     | Call t5 => "Call " ^ "(" ^ showCall t5 ^ ")"
     | Return (t6, t7) =>
      "Return " ^ "("
      ^ String.concatWith ", " [Target.showInstr t6, showRegs t7] ^ ")"
  local
    val rec tail = fn tail_0 =>
      fn Last t0 => "Last " ^ "(" ^ showLast t0 ^ ")"
       | Tail (t1, t2) =>
        "Tail " ^ "(" ^ String.concatWith ", " [showMiddle t1, tail_0 t2] ^ ")"
    val tail = fn () => let val rec tail_0 = fn ? => tail tail_0 ? in tail_0 end
  in val showTail = tail ()
  end
  local
    val rec head = fn head_1 =>
      fn First t0 => "First " ^ "(" ^ showFirst t0 ^ ")"
       | Head (t1, t2) =>
        "Head " ^ "(" ^ String.concatWith ", " [head_1 t1, showMiddle t2] ^ ")"
    val head = fn () => let val rec head_1 = fn ? => head head_1 ? in head_1 end
  in val showHead = head ()
  end
  val showZblock = fn (t0, t1) =>
    "(" ^ String.concatWith ", " [showHead t0, showTail t1] ^ ")"
  val showBlock = fn (t0, t1) =>
    "(" ^ String.concatWith ", " [showFirst t0, showTail t1] ^ ")"
  fun showMap showValue g =
    "["
    ^
    String.concatWith ", "
      (List.map (fn (k, v) => "(" ^ Int.toString k ^ ", " ^ showValue v ^ ")")
         (IntRedBlackMap.listItemsi g)) ^ "]"
  val showGraph = showMap showBlock
  val showZgraph = fn (t0, t1) =>
    "(" ^ String.concatWith ", " [showZblock t0, showMap showBlock t1] ^ ")"
end
