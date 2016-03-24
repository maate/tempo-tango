namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.Automaton
open M8.TempoTango.LinearTimeLogic
open M8.TempoTango.Parser

module SimpleTangoTests =
  let private parse s = Parser.Parse s |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

  [<Test>]
  let ``Disjunction``() =
    let a = parse "a | b"

    let input = [ "a" ]
    Assert.IsTrue( a |> Tango input )

    let input = [ "b" ]
    Assert.IsTrue( a |> Tango input )

    let input = [ "c" ]
    Assert.IsFalse( a |> Tango input )


  [<Test>]
  let ``Conjunction using Next``() =
    let a = parse "a & Xb"
    let input = [ "a"; "b" ]
    Assert.IsTrue( a |> Tango input )

    let input = [ "a" ]
    Assert.IsTrue( a |> Tango input )

    let input = [ "a"; "c" ]
    Assert.IsFalse( a |> Tango input )

  [<Test>]
  let ``noun phrase``() =
    let tempo = new Tempo( "art ? ( adj W noun )" )

    let input = [ "art" ]
    Assert.IsTrue( tempo.Tango input )

    let input = [ "adj" ]
    Assert.IsTrue( tempo.Tango input )

    let input = [ "noun" ]
    Assert.IsTrue( tempo.Tango input )

    let input = [ "art"; "adj" ]
    Assert.IsTrue( tempo.Tango input )

    let input = [ "art"; "noun" ]
    Assert.IsTrue( tempo.Tango input )

    let input = [ "adj"; "noun" ]
    Assert.IsTrue( tempo.Tango input )

    let input = [ "art"; "adj"; "noun" ]
    Assert.IsTrue( tempo.Tango input )

    let input = [ "art"; "adj"; "adj"; "noun" ]
    Assert.IsTrue( tempo.Tango input )

    let input = [ "art"; "adj"; "adj"; "noun"; "noun" ]
    Assert.IsFalse( tempo.Tango input )

    let input = [ "art"; "art"; "adj"; "adj"; "noun" ]
    Assert.IsFalse( tempo.Tango input )
