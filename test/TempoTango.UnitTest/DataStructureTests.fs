namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.LinearTemporalLogic

[<TestFixture>]
module DataStructureTests =
  let private parse s = Parser.Parse s |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

  [<Test>]
  let ``Start transition is start transition``() =
    let a = parse "a"
    Assert.IsTrue( Automaton.GetStartTransition a |> Automaton.IsStartTransition a )