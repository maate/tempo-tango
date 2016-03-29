namespace M8.TempoTango

open M8.TempoTango.Automaton
open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Parser

open System.Collections.Generic
open System.Runtime.InteropServices
open System.Linq

/// <summary>
///   Represents the match
/// </summary>
type public MatchInfo( success : bool, matches : string list list ) =
  let cliMatches = ( matches |> List.map( fun item1 -> ( item1 |> List.map( fun str -> str ) ).ToList() ) ).ToList()

  /// <summary>
  ///   True if match is successful. Otherwise false.
  /// </summary>
  member this.Success = success

  /// <summary>
  ///   Matches as a list of lists. Each list represents the matches for one input symbol.
  /// </summary>
  member this.Matches = cliMatches

  override super.ToString() =
    let sb = new System.Text.StringBuilder()
    ignore ( sb.AppendLine "[" )
    List.iter ( fun group ->
                  let agroup = List.toArray group
                  ignore ( sb.Append "  {" )
                  ignore ( sb.Append( System.String.Join( ", ", agroup :> System.String[] ) ) )
                  ignore (sb.AppendLine "}" ) ) matches
    ignore ( sb.AppendLine "]" )
    sb.ToString()
