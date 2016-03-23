﻿namespace TempoTango

open TempoTango.Automaton
open TempoTango.LinearTimeLogic
open TempoTango.Parser

/// <summary>
///   Initializes an instance of the Tempo class using the <param name="formula"></param>
///   as a linear temporal logic expression such as "a & Xb".
///   Use lowercased alphanumerics for propositions and the keywords:
///   !: Not
///   X: Next
///   F: Finally
///   G: Globally
///   U: Until
///   W: Weak Until
///   R: Releases
///   ?: Binary Optional (either l holds, and then r holds next, or r holds)
/// </summary>
type public Tempo( formula ) =
  let parse s = Parser.Parse s |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom
  let a = parse formula

  /// <summary>
  ///   A list of input to verify against the Linear Temporal Logic expression provided in the
  ///   constructor
  /// </summary>
  member public this.Tango( input ) =
    a |> Automaton.Tango ( List.ofSeq input )