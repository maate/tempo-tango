﻿namespace M8.TempoTango

module internal LinearTemporalLogic =
  // Represents a linear time logic (LTL) expression
  type expression =
    | True
    | False
    | Empty
    | Prop       of string
    | Not        of expression
    | And        of expression * expression
    | Or         of expression * expression
    | Next       of expression
    | Finally    of expression
    | Globally   of expression
    | Until      of expression * expression
    | Release    of expression * expression
    | WeakUntil  of expression * expression
    | Implicate  of expression * expression
    /// l is optional, but always followed by r.
    | Optional   of expression * expression

  let rec CleanExpression exp =
    match exp with
      | True | False | Empty
      | Prop(_)                -> exp
      | Not p                  -> Not ( CleanExpression p )
      | And ( l, r )           -> And ( CleanExpression l, CleanExpression r )
      | Or ( l, r )            -> Or ( CleanExpression l, CleanExpression r )
      | Next p                 -> Next ( CleanExpression p )
      | Finally p              -> Finally ( CleanExpression p )
      | Globally p             -> Globally ( CleanExpression p )
      | Until ( l, r )         -> Until ( CleanExpression l, CleanExpression r )
      | Release ( l, r )       -> Release ( CleanExpression l, CleanExpression r )
      | WeakUntil ( l, r )     -> Or( Until ( CleanExpression l, CleanExpression r ), Globally ( CleanExpression l ) )
      | Implicate ( l, r )     -> Not( And( CleanExpression l, Not( CleanExpression r ) ) )
      | Optional ( l, r )      -> Or( And( CleanExpression l, Next ( CleanExpression r ) ), CleanExpression r )

  /// Returns the leaf nodes in the expression
  let rec FindProps alphabet expression =
      match expression with
        | True | False   -> alphabet
        | Empty          -> "E"::alphabet
        | Prop p         -> p::alphabet
        | Not p
        | Next p
        | Finally p
        | Globally p     -> ( FindProps [] p ) @ alphabet
        | Until( l, r )
        | Release( l, r )
        | WeakUntil( l, r )
        | Implicate( l, r )
        | Optional( l, r )
        | Or( l, r )
        | And( l, r )    -> ( FindProps [] l ) @ ( FindProps [] r ) @ alphabet

  /// Counts the size of an expression
  let rec sizeOf exp =
    match exp with
      | Empty            -> 0
      | True
      | False
      | Prop(_)          -> 1
      | Not(exp)
      | Next(exp)
      | Finally(exp)
      | Globally(exp)    -> 1 + sizeOf exp
      | And(l, r)
      | Or(l, r)
      | Until(l, r)
      | Release(l, r)    -> 1 + max ( sizeOf l ) ( sizeOf r )
      | WeakUntil(l, r)  -> failwith "Cannot use WeakUntil here. Call CleanExpression first!"
      | Implicate(l, r)  -> failwith "Cannot use Implicate here. Call CleanExpression first!"
      | Optional(l, r)   -> failwith "Cannot use Optional here. Call CleanExpression first!"

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

  let rec ToString exp = 
    let print_paren exp =
      match exp with
        | True | False | Prop(_) | Not(_) | Next(_) | Finally(_) | Globally(_) | Empty
            -> ToString exp
        | _ -> "(" + ( ToString exp ) + ")"
      in
      match exp with
        | True            -> "T"
        | False           -> "F"
        | Empty           -> "E"
        | Prop(p)         -> p
        | Not(exp)        -> "¬" + (print_paren exp)
        | And(l, r)       -> (print_paren l) + " & " + (print_paren r)
        | Or(l, r)        -> (print_paren l) + " | " + (print_paren r)
        | Next(exp)       -> "X " + (print_paren exp)
        | Finally(exp)    -> "F " + (print_paren exp)
        | Globally(exp)   -> "G " + (print_paren exp)
        | Until(l, r)     -> (print_paren l) + " U " + (print_paren r)
        | WeakUntil(l, r) -> failwith "Cannot use WeakUntil here. Call CleanExpression first!"
        | Release(l, r)   -> (print_paren l) + " R " + (print_paren r)
        | Implicate(l, r) -> failwith "Cannot use Implicate here. Call CleanExpression first!"
        | Optional(l, r)  -> failwith "Cannot use Optional here. Call CleanExpression first!"

  let rec SetToString set =
    let string_formulae = Set.map ( fun item -> ToString item )
    "{ " + (String.concat ", " ( string_formulae set ) ) + "}"

  /// Rewrites a formula to it's negative normal form (NNF).
  /// The NNF is a positive Boolean combination of Temporal Formula.
  /// A Temporal Formula is either a literal or a formula where
  /// the outermost connective is X, U, or R.
  let rec public NegativeNormalForm formula =
    match formula with
      | True | False
      | Empty | Prop(_) -> formula
      | And(l, r)       -> And(NegativeNormalForm l, NegativeNormalForm r)
      | Or(l, r)        -> Or(NegativeNormalForm l, NegativeNormalForm r)
      | Next(p)         -> Next(NegativeNormalForm p)
      | Finally(p)      -> Finally(NegativeNormalForm p)
      | Globally(p)     -> Globally(NegativeNormalForm p)
      | Until(l, r)     -> Until(NegativeNormalForm l, NegativeNormalForm r)
      | Release(l, r)   -> Release(NegativeNormalForm l, NegativeNormalForm r)
      | WeakUntil(l,r)  -> failwith "Cannot use WeakUntil here. Call CleanExpression first!"
      | Implicate(l, r) -> failwith "Cannot use Implicate here. Call CleanExpression first!"
      | Optional(l, r) -> failwith "Cannot use Optional here. Call CleanExpression first!"
      | Not(formula) ->
        match formula with
          | True            -> False
          | False           -> True
          | Empty           -> Not(formula)
          | Prop(_)         -> Not(formula)
          | Not(p)          -> NegativeNormalForm p
          | And(l, r)       -> Or(NegativeNormalForm (Not l), NegativeNormalForm (Not r))
          | Or(l, r)        -> And(NegativeNormalForm (Not l), NegativeNormalForm (Not r))
          | Next(p)         -> Next(NegativeNormalForm (Not p))
          | Finally(p)      -> Finally(NegativeNormalForm (Not p))
          | Globally(p)     -> Globally(NegativeNormalForm (Not p))
          | Until(l, r)     -> Release(NegativeNormalForm (Not l), NegativeNormalForm (Not r))
          | Release(l, r)   -> Until(NegativeNormalForm (Not l), NegativeNormalForm (Not r))
          | WeakUntil(l,r)  -> failwith "Cannot use WeakUntil here. Call CleanExpression first!"
          | Implicate(l, r) -> failwith "Cannot use Implicate here. Call CleanExpression first!"
          | Optional(l, r) -> failwith "Cannot use Optional here. Call CleanExpression first!"

  let RewriteNNF formula =
    match formula with
      | And( Next( p ), Next ( q ) ) -> Next( And ( p, q ) )
      | _ -> formula

  let IsConsistent = function
    | False                         -> false
    | And( p, Not( q ) ) when p = q -> false
    | _ -> true

  let IsReduced = function
    | True
    | False
    | Empty
    | Prop(_)
    | Not Empty
    | Not( Prop(_) )
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
                            (Set [Next(formula)], Some(formula))]
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
    Set.fold (fun (conds, next) -> function
      | Not Empty    -> ((Not Empty)::conds, next)
      | Empty        -> (Empty::conds, next)
      | True         -> (conds, next)
      | False        -> failwith "inconsistent False"
      | Prop(p)      -> (Prop(p) :: conds, next)
      | Not(Prop(p)) -> (Not(Prop(p)) :: conds, next)
      | Next(x)      -> (conds, Set.add x next)
      | other        -> failwith ("not reduced " + ToString other)) ([], Set.empty) set

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
