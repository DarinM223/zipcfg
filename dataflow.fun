functor DataflowFn (structure G: GRAPH): DATAFLOW =
struct
  structure G = G

  type 'a fact =
    { init_info: 'a (* lattice bottom element *)
    , add_info: 'a -> 'a -> 'a (* lattice join (least upper bound) *)
    , changed: {old: 'a, new: 'a} -> bool (* is new one bigger? *)
    , get: G.uid -> 'a (* mutably get state by block id *)
    , set: G.uid -> 'a -> unit (* mutably set state by block id *)
    }
  datatype 'a answer = Dataflow of 'a | Rewrite of G.graph

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
  fun run (fact: 'a fact) (changed: bool ref) (entryFact: 'a)
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
      #set fact G.entryUid entryFact;
      iterate 1
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

  structure Backwards =
  struct
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

    fun runAnalysis ((fact, analysis): 'a analysis) graph : int =
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

    fun solveGraph (pass as (fact, _): 'a pass) graph (exitFact: 'a) : 'a =
      #2 (withoutChangingEntry fact (fn () =>
        generalBackward (passWithExit pass exitFact) graph))
    and generalBackward (pass as (fact, passFns): 'a pass) graph : int =
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

    fun solveAndRewrite pass graph exitFact changed =
      ( solveGraph pass graph exitFact
      , backwardRewrite (passWithExit pass exitFact) graph changed
      )
    and backwardRewrite (pass as (fact, passFns)) graph changed : G.graph * bool =
      let
        fun rewriteBlocks rewritten [] changed = (rewritten, changed)
          | rewriteBlocks rewritten (b :: bs) changed =
              let
                fun rewriteNextBlock () =
                  let
                    val (head, last) = G.gotoEnd (G.unzip b)
                  in
                    case #last_in passFns last of
                      Dataflow a =>
                        propagate head a (G.Last last) rewritten changed
                    | Rewrite g =>
                        let
                          val (a, (g, _)) =
                            solveAndRewrite pass g (#init_info fact) changed
                          val (t, g) = G.removeEntry g
                          val rewritten = G.Blocks.union g rewritten
                        in
                          propagate head a t rewritten true
                        end
                  end
                and propagate (G.Head (h, m)) a t rewritten changed =
                      (case #middle_in passFns a m of
                         Dataflow a =>
                           propagate h a (G.Tail (m, t)) rewritten changed
                       | Rewrite g =>
                           let
                             val (a, (g, _)) = solveAndRewrite pass g a changed
                             val (t, g) = G.spliceTail g t
                             val rewritten = G.Blocks.union g rewritten
                           in
                             propagate h a t rewritten true
                           end)
                  | propagate (G.First f) a t rewritten changed =
                      (case #first_in passFns a f of
                         Dataflow _ =>
                           rewriteBlocks (G.Blocks.insert (f, t) rewritten) bs
                             changed
                       | Rewrite _ =>
                           raise Fail "rewriting a label in backwards dataflow")
              in
                rewriteNextBlock ()
              end
      in
        rewriteBlocks G.empty (List.rev (G.reversePostorderDfs graph)) changed
      end
    val solveAndRewrite = fn pass =>
      fn graph => fn exitFact => solveAndRewrite pass graph exitFact false
  end

  structure Forwards =
  struct
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

    fun runAnalysis ((fact, analysis): 'a analysis) {entryFact} graph =
      let
        val changed = ref false
        fun setSuccessorFacts block =
          let
            val update = update fact changed
            fun forward in' (G.Tail (m, t)) =
                  forward (#middle_out analysis in' m) t
              | forward in' (G.Last l) =
                  #last_outs analysis in' l update
          in
            forward (#get fact (G.id block)) (#2 block)
          end
        val blocks = G.reversePostorderDfs graph
      in
        run fact changed entryFact setSuccessorFacts blocks
      end

    fun passWithExit ((fact, passFns): 'a pass) (exitFactRef: 'a ref) : 'a pass =
      let
        fun last_outs in' G.Exit =
              Dataflow (fn _ => exitFactRef := in')
          | last_outs in' l =
              #last_outs passFns in' l
        val passFns = {middle_out = #middle_out passFns, last_outs = last_outs}
      in
        (fact, passFns)
      end

    fun solveGraph (pass as (fact, _): 'a pass) graph (entryFact: 'a) : 'a =
      let
        val exitFactRef = ref (#init_info fact)
        val _ = generalForward (passWithExit pass exitFactRef) entryFact graph
      in
        !exitFactRef
      end
    and generalForward (pass as (fact, passFns): 'a pass) entryFact graph : int =
      let
        val changed = ref false
        val update = update fact changed
        fun setSuccessorFacts (first, tail) =
          let
            fun setTailFacts in' (G.Tail (m, t)) =
                  (case #middle_out passFns in' m of
                     Dataflow a => setTailFacts a t
                   | Rewrite g => setTailFacts (solveGraph pass g in') t)
              | setTailFacts in' (G.Last l) =
                  (case #last_outs passFns in' l of
                     Dataflow setter => setter update
                   | Rewrite g => ignore (solveGraph pass g in'))
            val in' =
              case first of
                G.Entry => entryFact
              | G.Label ((uid, _), _) => #get fact uid
          in
            setTailFacts in' tail
          end
        val blocks = G.reversePostorderDfs graph
      in
        run fact changed entryFact setSuccessorFacts blocks
      end

    fun checkPropertyMatch (fact: 'a fact) uid a =
      let
        val a' = #get fact uid
        val a = #add_info fact a a'
      in
        if
          #changed fact {old = a, new = a'}
          orelse #changed fact {old = a', new = a}
        then
          raise Fail
            ("property at label " ^ G.showUid uid
             ^ " changed after reaching fixed point")
        else
          ()
      end

    fun solveAndRewrite (pass as (fact, _)) graph entryFact changed =
      let
        val _ = solveGraph pass graph entryFact
        val exitRef = ref (#init_info fact)
        val result =
          forwardRewrite (passWithExit pass exitRef) graph entryFact changed
      in
        (!exitRef, result)
      end
    and forwardRewrite (pass as (fact, passFns)) graph entryFact changed =
      let
        fun rewriteBlocks rewritten [] changed = (rewritten, changed)
          | rewriteBlocks rewritten (b :: bs) changed =
              let
                fun rewriteNextBlock () =
                  let
                    val (first, tail) = b
                    val a =
                      case first of
                        G.Entry => entryFact
                      | G.Label ((uid, _), _) => #get fact uid
                  in
                    propagate (G.First first) a tail rewritten changed
                  end
                and propagate h a (G.Tail (m, t)) rewritten changed =
                      (case #middle_out passFns a m of
                         Dataflow a =>
                           propagate (G.Head (h, m)) a t rewritten changed
                       | Rewrite g =>
                           let
                             val (a, (g, _)) = solveAndRewrite pass g a changed
                             val (g, h) = G.spliceHead h g
                             val (_, g) = G.removeEntry g
                             val rewritten = G.Blocks.union g rewritten
                           in
                             propagate h a t rewritten true
                           end)
                  | propagate h a (G.Last l) rewritten changed =
                      (case #last_outs passFns a l of
                         Dataflow set =>
                           ( set (checkPropertyMatch fact)
                           ; rewriteBlocks
                               (G.Blocks.insert (G.zip (h, G.Last l)) rewritten)
                               bs changed
                           )
                       | Rewrite g =>
                           rewriteBlocks
                             (G.Blocks.union (G.spliceHeadOnly h g) rewritten)
                             bs true)
              in
                rewriteNextBlock ()
              end
      in
        rewriteBlocks G.empty (G.reversePostorderDfs graph) changed
      end
    val solveAndRewrite = fn pass =>
      fn graph => fn entryFact => solveAndRewrite pass graph entryFact false
  end
end
