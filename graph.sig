signature GRAPH =
sig
  type uid
  type label = uid * string

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
  type machine
  structure Rtl:
  sig
    type rtl
    type exp
  end
  type regs

  (* val label: machine -> label -> nodes *)
  val instruction: Rtl.rtl -> nodes
  (* val branch: machine -> label -> nodes
  val cbranch: machine -> Rtl.exp -> {ifso: label, ifnot: label} -> nodes *)
  val return: Rtl.rtl -> {uses: regs} -> nodes
end
