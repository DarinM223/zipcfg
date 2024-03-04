functor GraphFn
  (structure Rtl:
   sig
     type rtl
     type exp
   end
   structure Spans:
   sig
     type t
   end
   type regs) :> GRAPH =
struct
  type uid = int
  type label = uid * string

  datatype first =
    Entry
  | Label of label * locall * Spans.t option ref
  and locall =
    Local of bool

  datatype middle = Instruction of Rtl.rtl

  type contedge = {kills: regs, defs: regs, node: label, assertion: Rtl.rtl}

  type call =
    { callInstr: Rtl.rtl
    , callContedges: contedge list
    , callSpans: Spans.t option
    , callUses: regs ref
    , callAltrets: int
    , callUnwindsTo: int
    , callCutsTo: int
    , callReads: string list option
    , callRights: string list option
    }

  datatype last =
    Exit
  | Branch of Rtl.rtl * label
  | CBranch of Rtl.rtl * label * label (* true, false *)
  | Call of call
  | Return of Rtl.rtl * regs

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

  val entryUid = 0
  val empty = IntRedBlackMap.singleton (0, (Entry, Last Exit))

  val rec zip: zblock -> block =
    fn (First first, tail) => (first, tail)
     | (Head (head, mid), tail) => zip (head, Tail (mid, tail))

  val unzip: block -> zblock = fn (first, tail) => (First first, tail)

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
  val unfocus = fn (zblock, graph) =>
    let
      val block = zip zblock
    in
      case block of
        (Entry, _) => IntRedBlackMap.insert (graph, entryUid, block)
      | (Label ((uid, _), _, _), _) => IntRedBlackMap.insert (graph, uid, block)
    end

  infixr 3 **>
  fun op**> (f, x) = f x

  structure Rtl = Rtl
  type regs = regs
  type machine = int
  type nodes = zgraph -> zgraph

  fun instruction rtl ((head, tail), graph) =
    ((head, Tail (Instruction rtl, tail)), graph)
  fun return rtl {uses = regs} ((head, _), graph) =
    ((head, Last (Return (rtl, regs))), graph)

  fun example {rtl1, rtl2, rtl3, regs} (zgraph: zgraph) =
    instruction rtl1 **> instruction rtl2 **> return rtl3 {uses = regs}
    **> zgraph
end
