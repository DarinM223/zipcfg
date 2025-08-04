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

  type graph
  type zgraph

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

  type nodes = zgraph -> zgraph
  type regs

  val label: label -> nodes
  val instruction: Target.instr -> nodes
  val branch: label -> nodes
  (* val cbranch: machine -> Rtl.exp -> {ifso: label, ifnot: label} -> nodes *)
  val return: {uses: regs} -> nodes

  val showGraph: graph -> string
end
