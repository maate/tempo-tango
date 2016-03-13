namespace TempoTango.UnitTest

open NUnit.Framework
open TempoTango
open TempoTango.LinearTimeLogic
open TempoTango.Parser

[<TestFixture>]
module Tests =
  [<Test>]
  let ``Test``() =
    let a = Parser.Parse "p|q" |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

    let g = Automaton.ToGraph a in
    let writer = new System.IO.StringWriter()
    Graph.print_graph writer g;
//    printfn "%s" (writer.ToString())
    writer.Close()
