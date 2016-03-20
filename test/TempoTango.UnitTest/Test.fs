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
    let a = Parser.Parse "G(!p|Fq)" |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom
    printfn "%s" ( a.transitions.ToString() )
//    let g = Automaton.ToGraph a in
//    let writer = new System.IO.StringWriter()
//    Graph.print_graph writer g;
//    System.IO.File.WriteAllText( "dotgraph.txt", writer.ToString() )
//    printfn "%s" (writer.ToString())
//    writer.Close()
