signature DATAFLOW =
sig
  structure G: GRAPH

  type 'a fact =
    { init_info: 'a (* lattice bottom element *)
    , add_info: 'a -> 'a -> 'a (* lattice join (least upper bound) *)
    , changed: {old: 'a, new: 'a} -> bool (* is new one bigger? *)
    , get: G.uid -> 'a (* mutably get state by block id *)
    , set: G.uid -> 'a -> unit (* mutably set state by block id *)
    }
  datatype 'a answer = Dataflow of 'a | Rewrite of G.graph

  val run: 'a fact -> bool ref -> 'a -> (G.block -> unit) -> G.block list -> int

  structure Backwards:
  sig
    type 'a analysis_functions =
      { first_in: 'a -> G.first -> 'a
      , middle_in: 'a -> G.middle -> 'a
      , last_in: G.last -> 'a
      }
    type 'a analysis = 'a fact * 'a analysis_functions

    type 'a pass_functions =
      { first_in: 'a -> G.first -> 'a answer
      , middle_in: 'a -> G.middle -> 'a answer
      , last_in: G.last -> 'a answer
      }
    type 'a pass = 'a fact * 'a pass_functions

    val runAnalysis: 'a analysis -> G.graph -> int
    val solveGraph: 'a pass -> G.graph -> 'a -> 'a
    val solveAndRewrite: 'a pass -> G.graph -> 'a -> 'a * G.graph
  end
  structure Forwards:
  sig
    type 'a analysis_functions =
      { middle_out: 'a -> G.middle -> 'a
      , last_outs: 'a
        -> G.last
        -> (* Function to set the fact for every successor *)
          (G.uid -> 'a -> unit)
        -> unit
      }
    type 'a analysis = 'a fact * 'a analysis_functions

    type 'a pass_functions =
      { middle_out: 'a -> G.middle -> 'a answer
      , last_outs: 'a -> G.last -> ((G.uid -> 'a -> unit) -> unit) answer
      }
    type 'a pass = 'a fact * 'a pass_functions

    val runAnalysis: 'a analysis -> {entryFact: 'a} -> G.graph -> int
    val solveGraph: 'a pass -> G.graph -> 'a -> 'a
    val solveAndRewrite: 'a pass -> G.graph -> 'a -> 'a * G.graph
  end
end
