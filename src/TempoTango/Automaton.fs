namespace M8.TempoTango

open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Graph
open System.Linq

module internal Automaton =

  type edge = Epsilon of expression list
              | Sigma of expression list * expression list
  type state = Set<expression>
  type transition = {
    edge : edge;
    s: state;
    t: state;
  }
  module OrderedTransition = begin
    type t = transition
    let compare a b = a.edge.Equals b.edge && a.s.Equals b.s && a.t.Equals b.t
    let (=) a b = compare a b
  end

  type automaton = {
    starts: state list;
    transitions: Set<transition>;
    alphabet: Set<string>;
  }

  let linkToString edge isSimple =
    let format_conds conds =
      String.concat " & " (List.map LinearTemporalLogic.ToString conds)

    if( isSimple )
    then
      match edge with
        | Epsilon(_)       -> "ε"
        | Sigma(conds, _)  -> format_conds conds
    else
      match edge with
        | Epsilon(postpones)       -> "ε" + (String.concat "" (List.map (fun f -> ", !" + (LinearTemporalLogic.ToString(f))) postpones))
        | Sigma(conds, postpones)  -> "Σ" + (format_conds conds) + (String.concat "" (List.map (fun f -> ", " + (LinearTemporalLogic.ToString(f))) postpones))

  let rec FullGBA transitions ( state : Set<expression> ) =
    let isKnown trans transitions =
      Set.exists (OrderedTransition.(=) trans) transitions

    let AddTransition trans transitions =
      if isKnown trans transitions then
        transitions
      else
        FullGBA (Set.add trans transitions) trans.t

    let epsilonFromOption = function
      | None    -> Epsilon([])
      | Some(p) -> Epsilon([p])

    match EpislonTransition state with
      | None ->
        let (conds, next) = SigmaTransform ( state )
        let trans = { edge = Sigma(conds, []); s = state; t = next }
        AddTransition trans transitions
      | Some(conv_list) ->
        List.fold (fun transitions (next, cond) ->
          let trans = { edge = epsilonFromOption cond; s = state; t = next }
          AddTransition trans transitions
        ) transitions conv_list

  let Union l r =
    let startsWithAnyOf ( states : state list ) transitions =
      let s = Set.ofList states
      Set.filter ( fun trans -> s.Contains trans.s ) transitions

    let automaton = { starts      = [l.starts.Item 0; r.starts.Item 0];
                      transitions = set [];
                      alphabet    = set [] }

    let transitions_l = l.transitions |> startsWithAnyOf l.starts
    let transitions_r = r.transitions |> startsWithAnyOf r.starts

    let crossproduct l1 l2 =
      seq { for el1 in l1 do
              for el2 in l2 do
                yield el1, el2 };

    transitions_l, transitions_r

  let GetAlphabet transitions =
    Set.fold( fun a trans ->
                match trans.edge with
                  | Sigma( exps, _ ) -> List.fold( FindProps ) a exps
                  | _ -> a ) [] transitions

  let public constructFrom start_state =
    let gba = { starts = [start_state]; alphabet = set []; transitions = FullGBA Set.empty start_state }
    let alphabet = GetAlphabet gba.transitions |> Set.ofList
    { gba with alphabet = alphabet }

  /// Returns the Graph form of the automaton
  let ToGraph automaton isSimple =
    let set_to_s = SetToString
    let g = (Graph.NewGraph "Automaton")
    let IsStart s = List.exists ((=) s) automaton.starts
    let addNodeFn s = (if IsStart s then Graph.AddStart else Graph.AddNode )

    let h = new System.Collections.Generic.Dictionary<string, string>()
    let Key s = if h.ContainsKey( s )
                then h.[s]
                else
                  h.[s] <- h.Count.ToString( "X4" )
                  h.[s]

    Set.fold (fun g { edge = edge; s = s; t = t } ->
      let s_string = set_to_s s
      let t_string = set_to_s t
      let s_string_key = Key( s_string )
      let t_string_key = Key( t_string )

      let g = (addNodeFn s) g s_string_key
      let g = (addNodeFn t) g t_string_key
      Graph.edge g s_string_key t_string_key (linkToString edge isSimple)
    ) g automaton.transitions

  let uniquePostpones transitions =
    Seq.distinct (List.fold (fun postponed { edge = edge } ->
      match edge with
        | Epsilon(p) -> p @ postponed
        | _ -> postponed
    ) [] transitions)

  let setupSigmaPostpones postpones  =
    List.map (fun trans ->
      match trans.edge with
        | Sigma(c, p) -> { trans with edge = Sigma(c, p @ postpones) }
        | _ -> trans)

  let public skipEpsilons automaton =
    let transitions = automaton.transitions |> Set.toList
    let postpones   = uniquePostpones transitions |> Seq.toList
    let transitions = setupSigmaPostpones postpones transitions
    let rec skip transitions =
      let (epsilons, sigmas) = List.partition (fun t ->
        match t.edge with
          | Epsilon(_) -> true | Sigma(_, _) -> false ) transitions
      if List.isEmpty epsilons then
        sigmas
      else
        let rest = List.tail epsilons @ sigmas
        let target = List.head epsilons
        if target.s = target.t then
          skip rest
        else
          let replace rewrite_rule =
            let (nexts, rest) = List.partition (fun t -> t.s = target.t) rest
            let transitions = List.fold (fun transitions n ->
              if List.exists (fun t -> t.t = n.s) transitions then (* still referred *)
                (rewrite_rule n) :: n :: transitions
              else
                (rewrite_rule n) :: transitions ) rest nexts
            skip transitions
          match target.edge with
            | Epsilon([]) ->
              replace (fun next -> { next with s = target.s })
            | Epsilon(ps) ->
              let new_link = function
                | Epsilon(ps') -> Epsilon(ps @ ps')
                | Sigma(c, constraints) -> Sigma(c, List.filter (fun p -> not (List.exists ((=) p) ps)) constraints)
              in
              replace (fun next -> { next with s = target.s; edge = new_link next.edge })
            | _ -> failwith "unexpected non-epsilon value"
    { automaton with transitions = Set.ofList (skip transitions) }

  let isMergeable l r =
    if l.s = r.s && l.t = r.t then
      match (l.edge, r.edge) with
        | (Epsilon(l_ps), Epsilon(r_ps))
        | (Sigma(_, l_ps), Sigma(_, r_ps)) -> l_ps = r_ps
        | _ -> false
    else
      false

  let mergeTransitions l r =
    let merged_link =
      match (l.edge, r.edge) with
        | (Epsilon(_), Epsilon(_)) -> l.edge
        | (Sigma(l_cond, ps), Sigma(r_cond, _)) ->
        begin
          match CalculateOr (AndConcat l_cond) (AndConcat r_cond) with
            | True -> Sigma([], ps)
            | prop -> Sigma([prop], ps)
        end
        | _ -> failwith (Printf.sprintf "Unable to merge %s with %s" (linkToString l.edge true) (linkToString r.edge true))
    in
    { l with edge = merged_link }

  let mergeToParallels transitions trans =
    match List.filter (isMergeable trans) transitions with
      | [] -> trans :: transitions
      | merge_to :: _ ->
        mergeTransitions trans merge_to :: List.filter ( fun item -> item <> merge_to) transitions 

  let public joinSigmas automaton =
    let transitions = Set.fold ( fun transitions trans ->
      mergeToParallels transitions trans ) [] automaton.transitions
    { automaton with transitions = Set.ofList transitions }

  let public ConstructAutomatonFrom ltl_set =
    ltl_set |> constructFrom |> skipEpsilons |> joinSigmas

  let rec IsFinal automaton ( states : state list ) =
    let transitions = automaton.transitions |> Set.filter( fun trans -> states.Contains trans.s )

    let rec hasEmpty ( e : expression list ) =
      e.All( fun i -> match i with
                          | Empty         -> true
                          | Not Empty     -> false
                          | Prop p
                          | Not( Prop p ) -> false
                          | Or( l, r )    -> hasEmpty [l] || hasEmpty [r]
                          | And( l, r )   -> hasEmpty [l] && hasEmpty [r]
                          | _             -> failwith "expected Disjunction, Conjunction, Empty, Prop or Not Prop" )

    let emptyTransitions = Set.filter( fun trans -> match trans.edge with
                                                      | Sigma( l, _ ) when l.Any() -> hasEmpty l
                                                      | Sigma( l, _ ) when l = []  -> true
                                                      | _                          -> false )

    let epsilonTransitions = transitions |> emptyTransitions
                                         |> Set.map( fun trans -> trans.t )
                                         |> Set.toList

    transitions.Any( fun trans -> states.Any( fun s -> s = trans.t ) ) || epsilonTransitions.Any() && IsFinal automaton epsilonTransitions

  let rec TangoInternal input automaton ( nextStates : state list ) =

    if input = [] then
      IsFinal automaton nextStates
    else
      let curInput = input.Head
      let rec accept ( e : expression list ) =
        e.All( fun i -> match i with
                          | Empty         -> curInput = [] || curInput.Any( fun item -> item = "" )
                          | Not Empty     -> curInput.Any()
                          | Prop p        -> curInput.Contains p
                          | Not( Prop p ) -> not ( curInput.Contains p )
                          | Or( l, r )    -> accept [l] || accept [r]
                          | And( l, r )   -> accept [l] && accept [r]
                          | _             -> failwith "expected Disjunction, Conjunction, Empty, Prop or Not Prop" )

      let findTransitions ( nextStates : state list ) = automaton.transitions |> Set.filter( fun trans -> nextStates.Contains trans.s )
                                                                              |> Set.filter( fun trans -> match trans.edge with
                                                                                                            | Sigma( l, r ) -> accept l
                                                                                                            | _             -> true )

      let transitions = findTransitions nextStates

      let nextStates = transitions |> Set.map( fun item -> item.t ) |> Set.toList
      match ( Set.toList transitions ) with
        | []         -> false
        | head :: _  -> TangoInternal input.Tail automaton nextStates
      // TODO: Accepting states

  let Tango input automaton =
    TangoInternal input automaton automaton.starts
