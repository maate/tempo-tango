namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.LinearTimeLogic
open M8.TempoTango.Parser

[<TestFixture>]
module Tests =
  [<Test>]
  let ``Test``() =
//    System.Diagnostics.Debugger.Launch()
//    let np = "art ? ( adj W noun )"
//    let ap = "prep ? " + np
//    let ap2 = "adv | " + ap
//    let v = "fverb"
//    let np_aux = "sub"

//    let nexus = np * ""
    let nounPhrase = "( prep ? ( art ? ( adj U ( noun ? fverb ) ) ) )" // art W ( adj W noun) ... would mean that article can occur multiple times

    let s = "art & adj W noun"
//    "prep? art?";
    let a = Parser.Parse "art -> ( X ( adj U noun ) )" |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom
//    printfn "%s" ( a.transitions.ToString() )
    let g = Automaton.ToGraph a

    let writer = new System.IO.StringWriter()
    Graph.PrintGraph writer g;

//    Graph.ShowGraph ( writer.ToString() )
    writer.Close()