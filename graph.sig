signature TARGET =
sig
  type label
  (* A target instruction is the simplified equivalent to Rtl.rtl for qc--. *)
  type instr
  val showLabel: label -> string
  val showInstr: instr -> string

  datatype cond = LT | LE | GT | GE | EQ | NE
  val goto: label -> instr (* j <label> *)
  val cbranch: cond -> label -> label -> instr
  val return: instr (* ret *)
end

signature GRAPH =
sig
  type uid
  type label = uid * string
  structure Target: TARGET where type label = label
  type regs

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
  type zgraph = zblock * graph

  val zip: zblock -> block
  val unzip: block -> zblock
  val gotoStart: zblock -> first * tail
  val gotoEnd: zblock -> head * last

  (* focus on edge out of entry *)
  val entry: graph -> zgraph
  (* focus on edge into default exit *)
  val exit: graph -> zgraph
  (* focus on edge out of node with uid *)
  val focus: uid -> graph -> zgraph
  (* lose focus *)
  val unfocus: zgraph -> graph

  (* entry and exit *)
  val empty: graph

  (* splicing operations *)

  (* splice a single-entry, single-exit graph onto a head *)
  val spliceHead: head -> graph -> graph * head
  (* splice a single-entry, single-exit graph onto a tail *)
  val spliceTail: graph -> tail -> tail * graph
  (* splice a single-entry, no-exit graph onto a head *)
  val spliceHeadOnly: head -> graph -> graph
  (* find entry node and remove it, leaving a tail
     leading into the rest of the graph *)
  val removeEntry: graph -> tail * graph

  (* splice a graph into the current focus, new focus is at
     the entry edge of the spliced graph *)
  val spliceFocusEntry: zgraph -> graph -> zgraph
  (* splice a graph into the current focus, new focus is at
     the exit edge of the spliced graph *)
  val spliceFocusExit: zgraph -> graph -> zgraph

  (* rewrite graph, every nontrivial node is replaced with a new subgraph *)
  val expand: (middle -> graph) -> (last -> graph) -> graph -> graph

  type nodes = zgraph -> zgraph

  val label: label -> nodes
  val instruction: Target.instr -> nodes
  val branch: label -> nodes
  (* val cbranch: machine -> Rtl.exp -> {ifso: label, ifnot: label} -> nodes *)
  val return: {uses: regs} -> nodes

  val showBlock: block -> string
  val showZblock: zblock -> string
  val showGraph: graph -> string
  val showZgraph: zgraph -> string
end
