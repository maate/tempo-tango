namespace M8.TempoTango

open M8.TempoTango.Automaton
open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Parser

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
    let mapped = input |> List.map( fun item -> [item] )
    a |> Automaton.Tango ( List.ofSeq mapped )

  /// <summary>
  ///   A list of input to verify against the Linear Temporal Logic expression provided in the
  ///   constructor
  /// </summary>
  member public this.Tango( [<System.ParamArray>] input : string array ) =
    let mapped = List.ofArray input |> List.map( fun item -> [item] )
    a |> Automaton.Tango ( List.ofSeq mapped )

  /// <summary>
  ///   True if <param name="symbol"></param> is a part of the alphabet of the formula.
  ///   False otherwise.
  /// </summary>
  member public this.TangosWith( symbol ) =
    a.alphabet.Contains( symbol )
