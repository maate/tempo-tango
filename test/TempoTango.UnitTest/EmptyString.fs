namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.Automaton
open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Parser

module EmptyString =
  let private parse s = Parser.Parse s |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

  [<Test>]
  let ``Empty string cannot parse word``() =
    let a = parse "E"
    let input = [ [ "a" ] ]
    Assert.AreEqual( a |> Tango input, None )

  [<Test>]
  let ``Empty string can parse empty input``() =
    let a = parse "E"
    let input = [ [ ] ]
    Assert.IsTrue( ( a |> Tango input ).IsSome )

  [<Test>]
  let ``Empty string can parse array of empty input``() =
    let a = parse "E"
    let input = [ []; [] ]
    Assert.IsTrue( ( a |> Tango input ).IsSome )

  [<Test>]
  let ``Not empty can parse a token``() =
    let a = parse "!E"
    let input = [ [ "a" ] ]
    Assert.IsTrue( ( a |> Tango input ).IsSome )

  [<Test>]
  let ``Not empty cannot parse an empty input``() =
    let a = parse "!E"
    let input = [ [ ] ]
    Assert.AreEqual( a |> Tango input, None )
