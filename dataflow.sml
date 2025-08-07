functor Dataflow (structure G: GRAPH) =
struct
  type 'a fact =
    { init_info: 'a (* lattice bottom element *)
    , add_info: 'a -> 'a -> 'a (* lattice join (least upper bound) *)
    , changed: {old: 'a, new: 'a} -> bool (* is new one bigger? *)
    , get: G.uid -> 'a (* mutably get state by block id *)
    , set: G.uid -> 'a -> unit (* mutably set state by block id *)
    }

  type 'a analysis_functions =
    { first_in: 'a -> G.first -> 'a
    , middle_in: 'a -> G.middle -> 'a
    , last_in: G.last -> 'a
    }
  type 'a analysis = 'a fact * 'a analysis_functions

  datatype 'a answer = Dataflow of 'a | Rewrite of G.graph
  type 'a pass_functions =
    { first_in: 'a -> G.first -> 'a answer
    , middle_in: 'a -> G.middle -> 'a answer
    , last_in: G.last -> 'a answer
    }
  type 'a pass = 'a fact * 'a pass_functions

  fun update (fact: 'a fact) (changed: bool ref) (uid: G.uid) (a: 'a) : unit =
    let
      val old_a = #get fact uid
      val new_a = #add_info fact a old_a
    in
      if #changed fact {old = old_a, new = new_a} then
        ( #set fact uid new_a
        ; (* no need to run on new entry *)
          if not (G.uidEq uid G.entryUid) then changed := true else ()
        )
      else
        ()
    end

  (* initializes all stored facts, analyzes all blocks,
     and iterates until fixed point is reached *)
  fun run (fact: 'a fact) (changed: bool ref) (entry_fact: 'a)
    (f: G.block -> unit) (blocks: G.block list) : int =
    let
      fun iterate n =
        let
          val () = changed := false
          val () = List.app f blocks
        in
          if !changed then
            if n < 1000 then iterate (n + 1) else raise Fail "didn't converge"
          else
            n
        end
    in
      List.app (fn block => #set fact (G.id block) (#init_info fact)) blocks;
      #set fact G.entryUid entry_fact;
      iterate 1
    end

  fun runAnalysis ((fact, analysis): 'a analysis) graph =
    let
      val changed = ref false
      fun setBlockFact block =
        let
          val (head, last) = G.gotoEnd (G.unzip block)
          fun headIn (G.Head (h, m)) out =
                headIn h (#middle_in analysis out m)
            | headIn (G.First f) out =
                #first_in analysis out f
          val blockIn = headIn head (#last_in analysis last)
        in
          update fact changed (G.id block) blockIn
        end
      val blocks = List.rev (G.postorderDfs graph)
    in
      run fact changed (#init_info fact) setBlockFact blocks
    end
end
