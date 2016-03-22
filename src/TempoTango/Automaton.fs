namespace TempoTango
open LinearTimeLogic
open Graph

module Automaton =

  type edge = Epsilon of LinearTimeLogic.expression list
              | Sigma of LinearTimeLogic.expression list * LinearTimeLogic.expression list
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
  }

  let linkToString edge =
    let format_conds conds =
      String.concat " ∧ " (List.map LinearTimeLogic.ToString conds)
    match edge with
    | Epsilon(postpones)       -> "ε" + (String.concat "" (List.map (fun f -> ", !" + (LinearTimeLogic.ToString(f))) postpones))
    | Sigma(conds, postpones)  -> "Σ" + (format_conds conds) + (String.concat "" (List.map (fun f -> ", " + (LinearTimeLogic.ToString(f))) postpones))

  let TransitionToString { edge = edge; s = s; t = t } =
    Printf.sprintf "%s -> %s (%s)" (LinearTimeLogic.SetToString s) (LinearTimeLogic.SetToString t) (linkToString edge)

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

    match LinearTimeLogic.EpislonTransition state with
      | None ->
        let (conds, next) = LinearTimeLogic.SigmaTransform ( state )
        let trans = { edge = Sigma(conds, []); s = state; t = next }
        AddTransition trans transitions
      | Some(conv_list) ->
        List.fold (fun transitions (next, cond) ->
          let trans = { edge = epsilonFromOption cond; s = state; t = next }
          AddTransition trans transitions
        ) transitions conv_list

  let constructFrom start_state =
    { starts = [start_state]; transitions = FullGBA Set.empty start_state }

  /// Returns the Graph form of the automaton
  let ToGraph automaton =
    let set_to_s = LinearTimeLogic.SetToString
    let g = (Graph.NewGraph "Automaton")
    let IsStart s = List.exists ((=) s) automaton.starts
    let addNodeFn s = (if IsStart s then Graph.AddStart else Graph.AddNode )
    Set.fold (fun g { edge = edge; s = s; t = t } ->
      let s_string = set_to_s s
      let t_string = set_to_s t
      let g = (addNodeFn s) g s_string
      let g = (addNodeFn t) g t_string
      Graph.edge g s_string t_string (linkToString edge)
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

  let skipEpsilons automaton =
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
          match LinearTimeLogic.CalculateOr (LinearTimeLogic.AndConcat l_cond) (LinearTimeLogic.AndConcat r_cond) with
            | LinearTimeLogic.True -> Sigma([], ps)
            | prop -> Sigma([prop], ps)
        end
        | _ -> failwith (Printf.sprintf "Unable to merge %s with %s" (linkToString l.edge) (linkToString r.edge))
    in
    { l with edge = merged_link }

  let mergeToParallels transitions trans =
    match List.filter (isMergeable trans) transitions with
      | [] -> trans :: transitions
      | merge_to :: _ ->
        mergeTransitions trans merge_to :: List.filter ( fun item -> item <> merge_to) transitions 

  let joinSigmas automaton =
    let transitions = Set.fold ( fun transitions trans ->
      mergeToParallels transitions trans ) [] automaton.transitions
    { automaton with transitions = Set.ofList transitions }

  let ConstructAutomatonFrom ltl_set =
    ltl_set |> constructFrom |> skipEpsilons |> joinSigmas

