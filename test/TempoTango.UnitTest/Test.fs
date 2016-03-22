namespace TempoTango.UnitTest

open NUnit.Framework
open TempoTango
open TempoTango.LinearTimeLogic
open TempoTango.Parser

[<TestFixture>]
module Tests =
  [<Test>]
  let ``Test``() =
//    System.Diagnostics.Debugger.Launch()
    let nounPhrase = "art ? ( adj W noun )" // art W ( adj W noun) ... would mean that article can occur multiple times
    let a = Parser.Parse nounPhrase |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom
//    printfn "%s" ( a.transitions.ToString() )
    let g = Automaton.ToGraph a in
    let writer = new System.IO.StringWriter()
    Graph.PrintGraph writer g;
    System.IO.File.WriteAllText( "dotgraph.txt", writer.ToString() )
//    printfn "%s" (writer.ToString())
    writer.Close()
