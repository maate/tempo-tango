namespace TempoTango
open LinearTimeLogic
open Graph

module Automaton =

  type link = Epsilon of LinearTimeLogic.expression list
              | Sigma of LinearTimeLogic.expression list * LinearTimeLogic.expression list
  type state = Set<expression>
  type transition = {
    link : link;
    s: state;
    t: state;
  }
  module OrderedTransition = begin
    type t = transition
    let compare a b = a.link.Equals b.link && a.s.Equals b.s && a.t.Equals b.t
    let (=) a b = compare a b
  end

  type automaton = {
    starts: state list;
    finals: state list;
    transitions: Set<transition>;
  }

  let linkToString link =
    let format_conds conds =
      String.concat " ∧ " (List.map LinearTimeLogic.ToString conds)
    in
    match link with
    | Epsilon(postpones)       -> "ε" + (String.concat "" (List.map (fun f -> ", !" + (LinearTimeLogic.ToString(f))) postpones))
    | Sigma(conds, postpones)  -> "Σ" + (format_conds conds) + (String.concat "" (List.map (fun f -> ", " + (LinearTimeLogic.ToString(f))) postpones))

  let TransitionToString { link = link; s = s; t = t } =
    Printf.sprintf "%s -> %s (%s)" (LinearTimeLogic.SetToString s) (LinearTimeLogic.SetToString t) (linkToString link)

  (* known_states: add sigma_transformed states *)
  let rec ReductionGraph transitions ( state : Set<expression> ) =
    let isKnown trans transitions =
      Set.exists (OrderedTransition.(=) trans) transitions
    in
    let AddTransition trans transitions =
      if isKnown trans transitions then
        transitions
      else
        ReductionGraph (Set.add trans transitions) trans.t
    in
    let epsilon_from_option = function
      | None    -> Epsilon([])
      | Some(p) -> Epsilon([p])
    match LinearTimeLogic.EpislonTransition state with
      | None ->
        let (conds, next) = LinearTimeLogic.SigmaTransform ( Set.toList state )
        let trans = { link = Sigma(conds, []); s = state; t = next }
        AddTransition trans transitions
      | Some(conv_list) ->
        List.fold (fun transitions (next, cond) ->
          let trans = { link = epsilon_from_option cond; s = state; t = next }
          AddTransition trans transitions
        ) transitions conv_list

  let construct_from start_state =
    { starts = [start_state]; finals = []; transitions = ReductionGraph Set.empty start_state }

  let to_graph automaton =
    let set_to_s = LinearTimeLogic.SetToString
    let g = (Graph.new_graph "Automaton")
    let is_start s = List.exists ((=) s) automaton.starts in
    let add_node_function s = (if is_start s then Graph.add_start else Graph.add_node) in
    List.fold (fun g { link = link; s = s; t = t } ->
      let s_string = set_to_s s in
      let t_string = set_to_s t in
      let g = (add_node_function s) g s_string in
      let g = (add_node_function t) g t_string in
      Graph.link g s_string t_string (linkToString link)
    ) g (Set.toList automaton.transitions)

  let unique_postpones transitions =
    Seq.distinct (List.fold (fun postponed { link = link } ->
      match link with
        | Epsilon(p) -> p @ postponed
        | _ -> postponed
    ) [] transitions)

  let setup_sigma_postpones postpones  =
    List.map (fun trans ->
      match trans.link with
        | Sigma(c, p) -> { trans with link = Sigma(c, p @ postpones) }
        | _ -> trans)

  let skip_epsilons automaton =
    let transitions = Set.toList automaton.transitions
    let postpones   = unique_postpones transitions
    let transitions = setup_sigma_postpones ( Seq.toList postpones ) transitions
    let rec skip transitions =
      let (epsilons, sigmas) = List.partition (fun t ->
        match t.link with
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
          match target.link with
            | Epsilon([]) ->
              replace (fun next -> { next with s = target.s })
            | Epsilon(ps) ->
              let new_link = function
                | Epsilon(ps')  -> Epsilon(ps @ ps')
                | Sigma(c, constraints) -> Sigma(c, List.filter (fun p -> not (List.exists ((=) p) ps)) constraints)
              in
              replace (fun next -> { next with s = target.s; link = new_link next.link })
            | _ -> failwith "unexpected non-epsilon value"
    { automaton with transitions = Set.ofList (skip transitions) }

  let is_mergeable l r =
    if l.s = r.s && l.t = r.t then
      match (l.link, r.link) with
        | (Epsilon(l_ps), Epsilon(r_ps))
        | (Sigma(_, l_ps), Sigma(_, r_ps)) -> l_ps = r_ps
        | _ -> false
    else
      false

  let merge_transitions l r =
    let merged_link =
      match (l.link, r.link) with
        | (Epsilon(_), Epsilon(_)) -> l.link
        | (Sigma(l_cond, ps), Sigma(r_cond, _)) ->
        begin
          match LinearTimeLogic.CalculateOr (LinearTimeLogic.AndConcat l_cond) (LinearTimeLogic.AndConcat r_cond) with
            | LinearTimeLogic.True -> Sigma([], ps)
            | prop -> Sigma([prop], ps)
        end
        | _ -> failwith (Printf.sprintf "Unable to merge %s with %s" (linkToString l.link) (linkToString r.link))
    in
    { l with link = merged_link }

  let merge_to_parallels transitions trans =
    match List.filter (is_mergeable trans) transitions with
      | [] -> trans :: transitions
      | merge_to :: _ ->
        merge_transitions trans merge_to :: List.filter ( fun item -> item = merge_to) transitions 

  let join_sigmas automaton =
    let transitions = List.fold ( fun transitions trans ->
      merge_to_parallels transitions trans ) [] (Set.toList automaton.transitions)
    { automaton with transitions = Set.ofList transitions }

  let construct_gba_from ltl_set =
    join_sigmas
      (skip_epsilons
         (construct_from ltl_set))

