namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.Automaton
open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Parser

module SimpleTangoTests =
  let private parse s = Parser.Parse s |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

  [<Test>]
  let ``Disjunction``() =
    let a = parse "a | b"

    let input = [ [ "a" ] ]
    Assert.IsTrue( a |> Tango input )

    let input = [ [ "b" ] ]
    Assert.IsTrue( a |> Tango input )

    let input = [ [ "c" ] ]
    Assert.IsFalse( a |> Tango input )


  [<Test>]
  let ``Conjunction using Next``() =
    let a = parse "a & Xb"
    let input = [ [ "a" ]; [ "b" ] ]
    Assert.IsTrue( a |> Tango input )

    let input = [ [ "a" ] ]
    Assert.IsFalse( a |> Tango input )

    let input = [ [ "a" ]; [ "c" ] ]
    Assert.IsFalse( a |> Tango input )

  [<Test>]
  let ``Conjunction``() =
    let a = parse "a & b"

    let input = [ [ "a"; "b" ] ]
    Assert.IsTrue( a |> Tango input )

    let input = [ [ "a" ]; [ "b" ] ]
    Assert.IsFalse( a |> Tango input )

    let input = [ [ "a" ] ]
    Assert.IsFalse( a |> Tango input )

    let input = [ [ "a" ]; [ "c" ] ]
    Assert.IsFalse( a |> Tango input )

  [<Test>]
  let ``Future Something``() =
    let tempo = new Tempo( "Xb" )

    let input = [ "a"; "b" ]
    Assert.IsTrue( tempo.Tango input )

  [<Test>]
  let ``conjunctive noun phrase``() =
    let tempo = new Tempo( "( art ? ( adj W ( noun ? GE ) ) ) & ( Gsing | Gplur )" )

    let input = [ [ "art"; "sing" ] ]                                       //e.g. the English singular article "a"
    Assert.IsTrue( tempo.Tango input )

    let input = [ [ "art"; "sing" ]; [ "noun"; "sing" ] ]                   //e.g. the phrase "a man"
    Assert.IsTrue( tempo.Tango input )

    let input = [ [ "art"; "sing"; "indef" ]; [ "noun"; "sing"; "indef" ] ] //e.g. the phrase "a man"
    Assert.IsTrue( tempo.Tango input )

    let input = [ [ "art"; "sing"; "indef" ]; [ "noun"; "sing"; "def" ] ]   //e.g. the phrase "a the-man"
    Assert.IsTrue( tempo.Tango input )

    let input = [ [ "art"; "sing" ]; [ "noun"; "plur" ] ]                   //e.g. the incorrect phrase "a men"
    Assert.IsFalse( tempo.Tango input )

    let tempo = new Tempo( "( art ? ( adj W ( noun & XGE ) ) ) & ( Gsing | Gplur ) & ( Gindef | Gdef )" )

    let input = [ [ "art"; "sing"; "indef" ]; [ "noun"; "sing"; "indef" ] ] //e.g. the phrase "a man"
    Assert.IsTrue( tempo.Tango input )

    let input = [ [ "art"; "sing"; "indef" ]; [ "noun"; "sing"; "indef" ]; [ "noun"; "sing"; "indef" ] ] //e.g. the phrase "a man man"
    Assert.IsFalse( tempo.Tango input )

    let input = [ [ "art"; "sing"; "indef" ]; [ "noun"; "sing"; "def" ] ]   //e.g. the phrase "a the-man"
    Assert.IsFalse( tempo.Tango input )

  [<Test>]
  let ``noun phrase``() =
    let tempo = new Tempo( "art ? ( adj W ( noun ? GE ) )" ) // note that "art ? ( adj W noun )" will accept e.g. noun noun

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
