infixr 3 **>
fun op**> (f, x) = f x

structure Target: TARGET =
struct
  type label = int * string
  (* A target instruction is the simplified equivalent to Rtl.rtl for qc--. *)
  type instr = string
  val showLabel = fn (_, s) => s
  val showInstr = fn i => i

  datatype cond = LT | LE | GT | GE | EQ | NE
  fun goto (_, label) = "j " ^ label
  fun cbranch cond (_, l1) l2 =
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
  val return = "ret"
end

(* Example *)
local
  structure TestGraph =
    GraphFn
      (structure Target = Target
       type regs = int list
       val showRegs = fn t0 =>
         "[" ^ String.concatWith ", " (List.map Int.toString t0) ^ "]")
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
  type label = int * string
  (* A target instruction is the simplified equivalent to Rtl.rtl for qc--. *)
  type instr = {uses: AtomRedBlackSet.set, defs: AtomRedBlackSet.set} * string
  val defUseDefs = {uses = AtomRedBlackSet.empty, defs = AtomRedBlackSet.empty}
  val showLabel = fn (_, s) => s
  val showInstr = fn (_, i) => i

  datatype cond = LT | LE | GT | GE | EQ | NE
  fun goto (_, label) = (defUseDefs, "j " ^ label)
  fun cbranch cond (_, l1) l2 =
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
      (defUseDefs, instr ^ l1 ^ "\n" ^ #2 (goto l2))
    end
  val return = (defUseDefs, "ret")
end

local
  structure TestGraph =
    GraphFn
      (structure Target = Target
       type regs = int list
       val showRegs = fn t0 =>
         "[" ^ String.concatWith ", " (List.map Int.toString t0) ^ "]")
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
  val livenessAnalysis: live_in analysis_functions =
    { first_in = fn a => fn _ => a
    , middle_in = fn a => fn Instruction instr => handleInstruction instr a
    , last_in = calcLiveOut
    }
  val livenessAnalysis: live_in analysis = (livenessFact, livenessAnalysis)
  val testLiveness: graph =
    List.foldl (fn ((k, v), acc) => IntRedBlackMap.insert (acc, k, v))
      IntRedBlackMap.empty
      [ (entryUid, (Entry, Last (Branch (Target.goto (1, ""), (1, "")))))
      , (* %2 <- 1 - %4
           %3 <- %5
           %1 <- %6
         *)
        ( 1
        , ( Label ((1, ""), Local false)
          , Tail
              ( Instruction (usesDefs ["%4"] ["%2"], "sub %2, 1, %4")
              , Tail
                  ( Instruction (usesDefs ["%5"] ["%3"], "mov %3, %5")
                  , Tail
                      ( Instruction (usesDefs ["%6"] ["%1"], "mov %1, %6")
                      , Last (Branch (Target.goto (2, ""), (2, "")))
                      )
                  )
              )
          )
        )
      , (* %2 <- %2 + 1
           %3 <- %3 - 1
           4 <- %3 == 0
         *)
        ( 2
        , ( Label ((2, ""), Local false)
          , Tail
              ( Instruction (usesDefs ["%2"] ["%2"], "add %2, %2, 1")
              , Tail
                  ( Instruction (usesDefs ["%3"] ["%3"], "sub %3, %3, 1")
                  , Last (CBranch
                      ((usesDefs ["%3"] [], "%3 == 0"), (4, ""), (3, "")))
                  )
              )
          )
        )
      , (* %1 <- %7 *)
        ( 3
        , ( Label ((3, ""), Local false)
          , Tail
              ( Instruction (usesDefs ["%7"] ["%1"], "mov %1, %7")
              , Last (Branch (Target.goto (4, ""), (4, "")))
              )
          )
        )
      , (* %2 <- %8
           2 <- %2 < 5
         *)
        ( 4
        , ( Label ((4, ""), Local false)
          , Tail
              ( Instruction (usesDefs ["%8"] ["%2"], "mov %2, %8")
              , Last (CBranch
                  ((usesDefs ["%2"] [], "%2 < 5"), (2, ""), (5, "")))
              )
          )
        )
      , (5, (Label ((5, ""), Local false), Last Exit))
      ]
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
  val iterations = runAnalysis livenessAnalysis testLiveness
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
