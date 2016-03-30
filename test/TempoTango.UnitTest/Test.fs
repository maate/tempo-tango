namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Parser

[<TestFixture>]
module Tests =
  [<Test>]
  let ``Test``() =
//    System.Diagnostics.Debugger.Launch()
    let nounPhrase = "( prep ? ( art ? ( adj U ( noun ? fverb ) ) ) )" // art W ( adj W noun) ... would mean that article can occur multiple times

    let s = "art ? ( adj W noun )"

    let a = Parser.Parse "Gnomen & ( article ? ( adjective W ( noun ) ) ) & ( Gsingular | Gplural ) & ( Gindefinite | Gdefinite )" |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

    let g = Automaton.ToGraph a true

    let writer = new System.IO.StringWriter()
    Graph.PrintGraph writer g;

    Graph.ShowGraph ( writer.ToString() )
    writer.Close()