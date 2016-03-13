namespace TempoTango

module LinearTimeLogic =

  // Represents a linear time logic (LTL) expression
  type expression =
    | True
    | False
    | Prop     of string
    | Not      of expression
    | And      of expression * expression
    | Or       of expression * expression
    | Next     of expression
    | Finally  of expression
    | Globally of expression
    | Until    of expression * expression
    | Release  of expression * expression

  /// Counts the size of an expression
  let rec sizeOf exp =
    match exp with
      | True
      | False
      | Prop(_) -> 1
      | Not(exp)
      | Next(exp)
      | Finally(exp)
      | Globally(exp)  -> 1 + sizeOf exp
      | And(l, r)
      | Or(l, r)
      | Until(l, r)
      | Release(l, r)  -> 1 + max ( sizeOf l ) ( sizeOf r )

  /// Removes the largest item from a set.
  /// Returns a tuple of the removed item and the new set.
  let private popLargest set =
    let _, largest = Set.fold (fun (size, value) formula ->
      let this_size = sizeOf formula in
      if size < this_size then
        (this_size, formula)
      else
        (size, value)) (0, False) set
    ( largest, Set.remove largest set )

  let rec SetToString set =
    let string_formulae = Set.map ( fun item -> item.ToString() )
    "{ " + (String.concat ", " ( string_formulae set ) ) + "}"


  let rec ToString exp = 
    let print_paren exp =
      match exp with
        | True | False | Prop(_) | Not(_) | Next(_) | Finally(_) | Globally(_)
            -> ToString exp
        | _ -> "(" + ( ToString exp ) + ")"
      in
      match exp with
        | True           -> "⊤"
        | False          -> "⊥"
        | Prop(p)        -> p
        | Not(exp)       -> "¬" + (print_paren exp)
        | And(l, r)      -> (print_paren l) + " ∧ " + (print_paren r)
        | Or(l, r)       -> (print_paren l) + " ∨ " + (print_paren r)
        | Next(exp)      -> "X " + (print_paren exp)
        | Finally(exp)   -> "F " + (print_paren exp)
        | Globally(exp)  -> "G " + (print_paren exp)
        | Until(l, r)    -> (print_paren l) + " U " + (print_paren r)
        | Release(l, r)  -> (print_paren l) + " R " + (print_paren r)

  /// Rewrites a formula to it's negative normal form (NNF).
  /// The NNF is a positive Boolean combination of Temporal Formula.
  /// A Temporal Formula is either a literal or a formula where
  /// the outermost connective is X, U, or R.
  let rec NegativeNormalForm formula =
    match formula with
      | True | False | Prop(_) -> formula
      | And(l, r)     -> And(NegativeNormalForm l, NegativeNormalForm r)
      | Or(l, r)      -> Or(NegativeNormalForm l, NegativeNormalForm r)
      | Next(p)       -> Next(NegativeNormalForm p)
      | Finally(p)    -> Finally(NegativeNormalForm p)
      | Globally(p)   -> Globally(NegativeNormalForm p)
      | Until(l, r)   -> Until(NegativeNormalForm l, NegativeNormalForm r)
      | Release(l, r) -> Release(NegativeNormalForm l, NegativeNormalForm r)
      | Not(formula) ->
        match formula with
          | True          -> False
          | False         -> True
          | Prop(_)       -> Not(formula)
          | Not(p)        -> NegativeNormalForm p
          | And(l, r)     -> Or(NegativeNormalForm (Not l), NegativeNormalForm (Not r))
          | Or(l, r)      -> And(NegativeNormalForm (Not l), NegativeNormalForm (Not r))
          | Next(p)       -> Next(NegativeNormalForm (Not p))
          | Finally(p)    -> Finally(NegativeNormalForm (Not p))
          | Globally(p)   -> Globally(NegativeNormalForm (Not p))
          | Until(l, r)   -> Release(NegativeNormalForm (Not l), NegativeNormalForm (Not r))
          | Release(l, r) -> Until(NegativeNormalForm (Not l), NegativeNormalForm (Not r))

  let RewriteNNF formula =
    match formula with
      | And( Next( p ), Next ( q ) ) -> Next( And ( p, q ) )
      | _ -> formula

  let IsConsistent = function
    | False                         -> false
    | And( p, Not( q ) ) when p = q -> false
    | _ -> true

  let IsReduced = function
    | True         -> true
    | False        -> true
    | Prop(_)      -> true
    | Not(Prop(_)) -> true
    | Next(_)      -> true
    | _            -> false

  /// Reduces the largest item in the set
  let EpislonTransition set =
    let transform formula =
      match formula with
        | And(l, r)     -> [(Set [l; r], None)]
        | Or (l, r)     -> [(Set [l], None);
                            (Set [r], None)]
        | Release(l, r) -> [(Set [l; r], None);
                            (Set [r; Next(formula)], None)]
        | Globally(p)   -> [(Set [p; Next(formula)], None)]
        | Until(l, r)   -> [(Set [r], None);
                            (Set [l; Next(formula)], Some(formula))]
        | Finally(p)    -> [(Set [p], None);
                            (Set [(Next(formula))], Some(formula))]
        | _ -> failwith "reduced form given"
    let (reduced, complex) = Set.partition IsReduced set
    if Set.isEmpty complex then
      None
    else
      let (formula, complex) = popLargest complex
      let rest = Set.union reduced complex
      let transformed = transform formula
      Some( List.map ( fun (set, cond) -> ( Set.union set rest, cond ) ) transformed )

  /// Calculate sigma transform condition and result set
  /// input set should be reduced and consistent
  let SigmaTransform set =
    List.fold (fun (conds, next) -> function
      | True    -> (conds, next)
      | False -> failwith "inconsistent False"
      | Prop(p) ->
        (Prop(p) :: conds, next)
      | Not(Prop(p)) ->
        (Not(Prop(p)) :: conds, next)
      | Next(x) ->
        (conds, Set.add x next)
      | other -> failwith ("not reduced " + ToString other)) ([], Set.empty) set

  /// prop is simple NNF. prop1 < prop2
  let CalculateOr prop1 prop2 =
    let rec merge prop1 prop2 =
      if prop1 = prop2 then
        Some( prop1 )
      else if sizeOf prop1 > sizeOf prop2 then
        merge prop2 prop1
      else
        match prop1 with
          | True  -> Some( True )
          | False -> Some( prop2 )
          | _ ->
            match prop2 with
              | True -> Some(True)
              | False -> Some(prop1)
              | Prop(_) -> None
              | Not(_)  -> None
              | And(l, r) ->
              begin
                match merge prop1 l with
                  | Some(result) -> Some(result)
                  | None -> merge prop1 r
              end
              | Or(l, r) -> begin
                match merge prop1 l with
                  | Some(result) -> Some(Or(result, r))
                  | None ->
                    match merge prop1 r with
                      | Some(result) -> Some(Or(l, result))
                      | None -> None
              end
              | _ -> failwith "Not in propositional language"
    match merge prop1 prop2 with
      | Some(result) -> result
      | None -> Or(prop1, prop2)

  let AndConcat props =
    match props with
      | [] -> True
      | _ -> List.reduce (fun a b -> And(a, b)) props
