namespace TempoTango.UnitTest

open NUnit.Framework
open TempoTango
open TempoTango.LinearTimeLogic

[<TestFixture>]
module Tests =
  [<Test>]
  let ``Test``() =
    let formula = NegativeNormalForm ( Or( Prop "p", Prop "q" ) )
//    printfn "%s" ( ToString formula )
    let formula_set = Set.singleton formula
    let a = Automaton.construct_gba_from formula_set in
    let g = Automaton.to_graph a in
    let writer = new System.IO.StringWriter()
    Graph.print_graph writer g;
//    printfn "%s" (writer.ToString())
    writer.Close()
