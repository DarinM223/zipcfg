functor DataflowFn (structure G: GRAPH) =
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

  (* running a backward analysis *)
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
      val blocks = List.rev (G.reversePostorderDfs graph)
    in
      run fact changed (#init_info fact) setBlockFact blocks
    end

  fun passWithExit ((fact, passFns): 'a pass) (exitFact: 'a) : 'a pass =
    let
      fun last_in G.Exit = Dataflow exitFact
        | last_in l = #last_in passFns l
      val passFns =
        { first_in = #first_in passFns
        , middle_in = #middle_in passFns
        , last_in = last_in
        }
    in
      (fact, passFns)
    end

  fun withoutChangingEntry (fact: 'a fact) (f: unit -> 'b) : 'b * 'a =
    let
      val restore = let val oldFact = #get fact G.entryUid
                    in fn () => #set fact G.entryUid oldFact
                    end
                    handle LibBase.NotFound => fn () => ()
    in
      (f (), #get fact G.entryUid before restore ())
    end

  fun solveGraph (pass as (fact, _): 'a pass) graph (exitFact: 'a) : 'a =
    #2 (withoutChangingEntry fact (fn () =>
      generalBackward (passWithExit pass exitFact) graph))
  and generalBackward (pass as (fact, passFns): 'a pass) graph =
    let
      val changed = ref false
      fun setBlockFact b =
        let
          fun headIn (G.Head (h, m)) out =
                headIn h
                  (case #middle_in passFns out m of
                     Dataflow a => a
                   | Rewrite g => solveGraph pass g out)
            | headIn (G.First f) out =
                case #first_in passFns out f of
                  Dataflow a => a
                | Rewrite g => solveGraph pass g out
          val (head, last) = G.gotoEnd (G.unzip b)
          val blockIn = headIn head
            (case #last_in passFns last of
               Dataflow a => a
             | Rewrite g => solveGraph pass g (#init_info fact))
        in
          update fact changed (G.id b) blockIn
        end
      val blocks = List.rev (G.reversePostorderDfs graph)
    in
      run fact changed (#init_info fact) setBlockFact blocks
    end

  fun solveAndRewrite pass graph exitFact =
    ( solveGraph pass graph exitFact
    , backwardRewrite (passWithExit pass exitFact) graph
    )
  and backwardRewrite (pass as (fact, passFns)) graph =
    let
      fun rewriteBlocks rewritten [] = rewritten
        | rewriteBlocks rewritten (b :: bs) =
            let
              fun rewriteNextBlock () =
                let
                  val (head, last) = G.gotoEnd (G.unzip b)
                in
                  case #last_in passFns last of
                    Dataflow a => propagate head a (G.Last last) rewritten
                  | Rewrite g =>
                      let
                        val (a, g) = solveAndRewrite pass g (#init_info fact)
                        val (t, g) = G.removeEntry g
                        val rewritten = G.Blocks.union g rewritten
                      in
                        propagate head a t rewritten
                      end
                end
              and propagate (G.Head (h, m)) a t rewritten =
                    (case #middle_in passFns a m of
                       Dataflow a => propagate h a (G.Tail (m, t)) rewritten
                     | Rewrite g =>
                         let
                           val (a, g) = solveAndRewrite pass g a
                           val (t, g) = G.spliceTail g t
                           val rewritten = G.Blocks.union g rewritten
                         in
                           propagate h a t rewritten
                         end)
                | propagate (G.First f) a t rewritten =
                    (case #first_in passFns a f of
                       Dataflow _ =>
                         rewriteBlocks (G.Blocks.insert (f, t) rewritten) bs
                     | Rewrite _ =>
                         raise Fail "rewriting a label in backwards dataflow")
            in
              rewriteNextBlock ()
            end
    in
      rewriteBlocks IntRedBlackMap.empty
        (List.rev (G.reversePostorderDfs graph))
    end
end
