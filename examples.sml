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

infixr 3 **>
fun op**> (f, x) = f x

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