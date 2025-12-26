infixr 3 **>
fun op**> (f, x) = f x

structure Target: TARGET =
struct
  type reg = string
  type label = int * string
  (* A target instruction is the simplified equivalent to Rtl.rtl for qc--. *)
  type instr = string
  val showLabel = fn (_, s) => s
  val showInstr = fn i => i

  datatype cond = LT | LE | GT | GE | EQ | NE
  fun goto (_, label) = "j " ^ label
  fun cbranch _ cond (_, l1) l2 =
    let
      val instr =
        case cond of
          LT => "jl "
        | LE => "jle "
        | GT => "jg "
        | GE => "jge "
        | EQ => "jz "
        | NE => "jnz "
    in
      instr ^ l1 ^ "\n" ^ goto l2
    end
  val return = fn _ => "ret"
end

(* Example *)
local
  structure TestGraph =
    GraphFn
      (structure Target = Target
       val showRegs = fn t0 => "[" ^ String.concatWith ", " t0 ^ "]")
  open TestGraph

  val example: nodes = fn zgraph =>
    instruction "a" **> instruction "b" **> return {uses = []} **> zgraph
  val example: graph = unfocus (example (entry empty))

  val testPostorder: graph =
    List.foldl (fn ((k, v), acc) => IntRedBlackMap.insert (acc, k, v))
      IntRedBlackMap.empty
      [ (entryUid, (Entry, Last (CBranch ("", (1, ""), (2, "")))))
      , ( 1
        , (Label ((1, ""), Local false), Last (CBranch ("", (3, ""), (4, ""))))
        )
      , (2, (Label ((2, ""), Local false), Last Exit))
      , (3, (Label ((3, ""), Local false), Last Exit))
      , (4, (Label ((4, ""), Local false), Last Exit))
      ]
  val rpo = List.map id (reversePostorderDfs testPostorder)
  val showUids = fn t0 =>
    "[" ^ String.concatWith ", " (List.map Int.toString t0) ^ "]"
in
  (* Prints [(0, (Entry, Tail (Instruction (a), Tail (Instruction (b), Last (Return (ret, []))))))] *)
  val () = print (showGraph example ^ "\n")
  (* Prints [0, 1, 3, 4, 2] *)
  val () = print ("Reverse Postorder: " ^ showUids rpo ^ "\n")
end

structure Target: TARGET =
struct
  type reg = string
  type label = int * string
  (* A target instruction is the simplified equivalent to Rtl.rtl for qc--. *)
  type instr = {uses: AtomRedBlackSet.set, defs: AtomRedBlackSet.set} * string
  val defUseDefs = {uses = AtomRedBlackSet.empty, defs = AtomRedBlackSet.empty}
  val showLabel = fn (i, s) => s ^ Int.toString i
  val showInstr = fn (_, i) => i

  datatype cond = LT | LE | GT | GE | EQ | NE
  fun goto (_, label) = (defUseDefs, "j " ^ label)
  fun cbranch {uses} cond (_, l1) l2 =
    let
      val instr =
        case cond of
          LT => "jl "
        | LE => "jle "
        | GT => "jg "
        | GE => "jge "
        | EQ => "jz "
        | NE => "jnz "
    in
      ( { uses = AtomRedBlackSet.fromList (List.map Atom.atom uses)
        , defs = AtomRedBlackSet.empty
        }
      , instr ^ l1 ^ "\n" ^ #2 (goto l2)
      )
    end
  val return = fn _ => (defUseDefs, "ret")
end

local
  structure TestGraph =
    GraphFn
      (structure Target = Target
       val showRegs = fn t0 => "[" ^ String.concatWith ", " t0 ^ "]")
  structure Dataflow = DataflowFn (structure G = TestGraph)
  open TestGraph Dataflow
  fun usesDefs (uses: string list) (defs: string list) =
    { uses = (AtomRedBlackSet.fromList o List.map Atom.atom) uses
    , defs = (AtomRedBlackSet.fromList o List.map Atom.atom) defs
    }

  type live_in = AtomRedBlackSet.set
  fun showLive live =
    AtomRedBlackSet.foldl (fn (atom, acc) => acc ^ Atom.toString atom ^ " ")
      "{ " live ^ "}"
  val genKillTable: live_in IntHashTable.hash_table =
    IntHashTable.mkTable (100, LibBase.NotFound)
  val livenessFact: live_in fact =
    { init_info = AtomRedBlackSet.empty
    , add_info = fn a => fn b => AtomRedBlackSet.union (a, b)
    , changed = fn {old, new} =>
        AtomRedBlackSet.numItems new > AtomRedBlackSet.numItems old
    , get = IntHashTable.lookup genKillTable
    , set = fn uid => fn v => IntHashTable.insert genKillTable (uid, v)
    }
  fun handleInstruction ({uses, defs}, _) a =
    AtomRedBlackSet.union (uses, AtomRedBlackSet.difference (a, defs))
  val calcLiveOut: TestGraph.last -> live_in =
    fn Exit => AtomRedBlackSet.empty
     | Branch (_, (uid, _)) => #get livenessFact uid
     | CBranch (instr, (uid1, _), (uid2, _)) =>
      handleInstruction instr (AtomRedBlackSet.union
        (#get livenessFact uid1, #get livenessFact uid2))
     | Call {callInstr, callContedges, ...} =>
      handleInstruction callInstr
        (List.foldl
           (fn ({node = (uid, _), ...}, acc) =>
              AtomRedBlackSet.union (acc, #get livenessFact uid))
           AtomRedBlackSet.empty callContedges)
     | Return (instr, _) => handleInstruction instr AtomRedBlackSet.empty
  val livenessAnalysis: live_in Backwards.analysis_functions =
    { first_in = fn a => fn _ => a
    , middle_in = fn a => fn Instruction instr => handleInstruction instr a
    , last_in = calcLiveOut
    }
  val livenessAnalysis: live_in Backwards.analysis =
    (livenessFact, livenessAnalysis)
  val testLiveness: zgraph =
    label (1, "") **> instruction (usesDefs ["%4"] ["%2"], "sub %2, 1, %4")
    **> instruction (usesDefs ["%5"] ["%3"], "mov %3, %5")
    **> instruction (usesDefs ["%6"] ["%1"], "mov %1, %6") **> label (2, "")
    **> instruction (usesDefs ["%2"] ["%2"], "add %2, %2, 1")
    **> instruction (usesDefs ["%3"] ["%3"], "sub %3, %3, 1")
    **> cbranch {uses = ["%3"]} Target.EQ {ifso = (4, ""), ifnot = (3, "")}
    **> label (3, "") **> instruction (usesDefs ["%7"] ["%1"], "mov %1, %7")
    **> label (4, "") **> instruction (usesDefs ["%8"] ["%2"], "mov %2, %8")
    **> cbranch {uses = ["%2"]} Target.LT {ifso = (2, ""), ifnot = (5, "")}
    **> label (5, "") **> entry empty
  val testLiveness = unfocus testLiveness
in
  (* Should print:
     Liveness iterations: 3
     Block 0 livein: { %4 %5 %6 %7 %8 }
     Block 0 liveout: { %4 %5 %6 %7 %8 }
     Block 1 livein: { %4 %5 %6 %7 %8 }
     Block 1 liveout: { %2 %3 %7 %8 }
     Block 2 livein: { %2 %3 %7 %8 }
     Block 2 liveout: { %3 %7 %8 }
     Block 3 livein: { %3 %7 %8 }
     Block 3 liveout: { %3 %7 %8 }
     Block 4 livein: { %3 %7 %8 }
     Block 4 liveout: { %2 %3 %7 %8 }
     Block 5 livein: { }
     Block 5 liveout: { }
  *)
  val iterations = Backwards.runAnalysis livenessAnalysis testLiveness
  val () = print ("Liveness iterations: " ^ Int.toString iterations ^ "\n")
  fun printBlock (i, block) =
    let
      val liveIn = #get livenessFact i
      val liveOut = calcLiveOut (#2 (gotoEnd (unzip block)))
    in
      print ("Block " ^ Int.toString i ^ " livein: " ^ showLive liveIn ^ "\n");
      print ("Block " ^ Int.toString i ^ " liveout: " ^ showLive liveOut ^ "\n")
    end
  val () = IntRedBlackMap.appi printBlock testLiveness
end

local
  structure TestGraph =
    GraphFn
      (structure Target = Target
       val showRegs = fn t0 => "[" ^ String.concatWith ", " t0 ^ "]")
  structure TestGraph = ExtraFn(TestGraph)
  structure Dominator = DominatorFn(TestGraph)
  open TestGraph
  (*
       NONE
      /    \
     V      V
     5      4
     |     / \
     V    V   V
     1<-->2<->3
  *)
  val graph: zgraph =
    cbranch {uses = []} Target.EQ {ifso = (5, ""), ifnot = (4, "")}
    **> label (5, "") **> branch (1, "") **> label (4, "")
    **> cbranch {uses = []} Target.EQ {ifso = (2, ""), ifnot = (3, "")}
    **> label (3, "") **> branch (2, "") **> label (2, "")
    **> cbranch {uses = []} Target.EQ {ifso = (1, ""), ifnot = (3, "")}
    **> label (1, "") **> branch (2, "") **> entry empty
  val graph = unfocus graph
  val fns = precalculate graph
  val idom = Dominator.idom fns graph

  type opt_label = Target.label option
  local
    fun showOption f (SOME s) = "SOME " ^ f s
      | showOption _ NONE = "NONE"
  in val showOpt_label = showOption Target.showLabel
  end

  fun printIdom label =
    print
      (showOpt_label (#positionToLabel fns (idom (#labelToPosition fns label)))
       ^ "\n")

  val tree = Dominator.dominatorTree fns idom graph
  val expected =
    let
      open Dominator
    in
      Node
        ( NONE
        , [ Leaf (SOME (5, ""))
          , Leaf (SOME (4, ""))
          , Leaf (SOME (3, ""))
          , Leaf (SOME (2, ""))
          , Leaf (SOME (1, ""))
          ]
        )
    end
in
  (* Should print 6 NONE *)
  val () = printIdom NONE
  val () = printIdom (SOME (1, ""))
  val () = printIdom (SOME (2, ""))
  val () = printIdom (SOME (3, ""))
  val () = printIdom (SOME (4, ""))
  val () = printIdom (SOME (5, ""))
  val () = print (Dominator.showTree tree ^ "\n")
  (* Should print true *)
  val () = print (Bool.toString (Dominator.eqTree (tree, expected)) ^ "\n")
end

local
  structure TestGraph =
    GraphFn
      (structure Target = Target
       val showRegs = fn t0 => "[" ^ String.concatWith ", " t0 ^ "]")
  structure TestGraph = ExtraFn(TestGraph)
  structure Dominator = DominatorFn(TestGraph)
  open TestGraph
  (* Figure 19.4 from Modern Compiler Implementation in ML *)
  val graph: zgraph =
    branch (2, "") **> label (2, "")
    **> cbranch {uses = []} Target.EQ {ifso = (3, ""), ifnot = (4, "")}
    **> label (3, "")
    **> cbranch {uses = []} Target.EQ {ifso = (5, ""), ifnot = (6, "")}
    **> label (4, "") **> return {uses = []} **> label (5, "")
    **> branch (7, "") **> label (6, "") **> branch (7, "") **> label (7, "")
    **> branch (2, "") **> entry empty
  val graph = unfocus graph
  val fns = precalculate graph
  val idom = Dominator.idom fns graph

  type opt_label = Target.label option
  type opt_label_list = opt_label list
  local
    fun showOption f (SOME s) = "SOME " ^ f s
      | showOption _ NONE = "NONE"
  in val showOpt_label = showOption Target.showLabel
  end
  val showOpt_label_list = fn t0 =>
    "[" ^ String.concatWith ", " (List.map showOpt_label t0) ^ "]"

  fun printIdom label =
    print
      (showOpt_label (#positionToLabel fns (idom (#labelToPosition fns label)))
       ^ "\n")

  val tree = Dominator.dominatorTree fns idom graph
  val expected =
    let
      open Dominator
    in
      Node
        ( NONE
        , [Node
             ( SOME (2, "")
             , [ Node
                   ( SOME (3, "")
                   , [ Leaf (SOME (5, ""))
                     , Leaf (SOME (6, ""))
                     , Leaf (SOME (7, ""))
                     ]
                   )
               , Leaf (SOME (4, ""))
               ]
             )]
        )
    end
  val df = Dominator.dominatorFrontier fns idom graph

  val showDf =
    showOpt_label_list o List.map (#positionToLabel fns) o df
    o #labelToPosition fns
in
  val () = print (Dominator.showTree tree ^ "\n")
  (* Should print true *)
  val () = print (Bool.toString (Dominator.eqTree (tree, expected)) ^ "\n")
  (* Should print:
     []
     [SOME 2]
     [SOME 2]
     []
     [SOME 7]
     [SOME 7]
     [SOME 2] *)
  val () = print (showDf NONE ^ "\n")
  val () = print (showDf (SOME (2, "")) ^ "\n")
  val () = print (showDf (SOME (3, "")) ^ "\n")
  val () = print (showDf (SOME (4, "")) ^ "\n")
  val () = print (showDf (SOME (5, "")) ^ "\n")
  val () = print (showDf (SOME (6, "")) ^ "\n")
  val () = print (showDf (SOME (7, "")) ^ "\n")
end
