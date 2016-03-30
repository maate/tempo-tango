namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.Automaton
open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Parser

module SimpleTangoTests =
  let private parse s = Parser.Parse s |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

  [<Test>]
  let ``Output can be reduced from input disjunction``() =
    let a = parse( "( article & XGE | ( article & X( adjective W ( noun & XGE ) ) ) ) & ( Gsingular | Gplural ) & ( Gindefinite | Gdefinite )" )

    let input = [ [ "article"; "definite"; "singular"; "verb" ] ]
    let result = a |> Tango input
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "article"; "definite"; "singular" ] ] ) )

  [<Test>]
  [<CompiledName("Run")>]
  let ``Two outputs are returned correctly``() =
    let a = parse( "a & Xb" )

    let input = [ [ "a" ]; [ "b" ] ]
    let result = a |> Tango input
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value.Length, Is.EqualTo( 2 ) )
    Assert.That( result.Value.Item 0, Is.EqualTo( [ "a" ] ) )
    Assert.That( result.Value.Item 1, Is.EqualTo( [ "b" ] ) )

  [<Test>]
  let ``Disjunction``() =
    let a = parse "a | b"

    let input = [ [ "a" ] ]
    Assert.IsTrue( ( a |> Tango input ).IsSome )

    let input = [ [ "b" ] ]
    Assert.IsTrue( ( a |> Tango input ).IsSome )

    let input = [ [ "c" ] ]
    Assert.IsTrue( ( a |> Tango input ).IsNone )


  [<Test>]
  let ``Conjunction using Next``() =
    let a = parse "a & Xb"
    let input = [ [ "a" ]; [ "b" ] ]
    Assert.IsTrue( ( a |> Tango input ).IsSome )

    let input = [ [ "a" ] ]
    Assert.IsTrue( ( a |> Tango input ).IsNone )

    let input = [ [ "a" ]; [ "c" ] ]
    Assert.IsTrue( ( a |> Tango input ).IsNone )

  [<Test>]
  let ``Conjunction``() =
    let a = parse "a & b"

    let input = [ [ "a"; "b" ] ]
    Assert.IsTrue( ( a |> Tango input ).IsSome )

    let input = [ [ "a" ]; [ "b" ] ]
    Assert.IsTrue( ( a |> Tango input ).IsNone )

    let input = [ [ "a" ] ]
    Assert.IsTrue( ( a |> Tango input ).IsNone )

    let input = [ [ "a" ]; [ "c" ] ]
    Assert.IsTrue( ( a |> Tango input ).IsNone )

  [<Test>]
  let ``Future Something``() =
    let tempo = new Tempo( "Xb" )

    let input = [ "a"; "b" ]
    Assert.IsTrue( ( tempo.Tango input ).Success )

  [<Test>]
  let ``conjunctive noun phrase``() =
    let tempo = new Tempo( "( art ? ( adj W ( noun ? GE ) ) ) & ( Gsing | Gplur )" )

    let input = [ [ "art"; "sing" ] ]                                       //e.g. the English singular article "a"
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ [ "art"; "sing" ]; [ "noun"; "sing" ] ]                   //e.g. the phrase "a man"
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ [ "art"; "sing"; "indef" ]; [ "noun"; "sing"; "indef" ] ] //e.g. the phrase "a man"
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ [ "art"; "sing"; "indef" ]; [ "noun"; "sing"; "def" ] ]   //e.g. the phrase "a the-man"
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ [ "art"; "sing" ]; [ "noun"; "plur" ] ]                   //e.g. the incorrect phrase "a men"
    Assert.IsFalse( ( tempo.Tango input ).Success )

    let tempo = new Tempo( "( art & XGE | ( art ? ( adj W ( noun & XGE ) ) ) ) & ( Gsing | Gplur ) & ( Gindef | Gdef )" )

    let input = [ [ "art"; "sing"; "indef" ]; [ "noun"; "sing"; "indef" ] ] //e.g. the phrase "a man"
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ [ "art"; "sing"; "indef" ] ]                              //e.g. the phrase "a"
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ [ "art"; "sing"; "indef" ]; [ "noun"; "sing"; "indef" ]; [ "noun"; "sing"; "indef" ] ] //e.g. the phrase "a man man"
    Assert.IsFalse( ( tempo.Tango input ).Success )

    let input = [ [ "art"; "sing"; "indef" ]; [ "noun"; "sing"; "def" ] ]   //e.g. the phrase "a the-man"
    Assert.IsFalse( ( tempo.Tango input ).Success )

  [<Test>]
  let ``noun phrase``() =
    let tempo = new Tempo( "art ? ( adj W ( noun ? GE ) )" ) // note that "art ? ( adj W noun )" will accept e.g. noun noun

    let input = [ "art" ]

    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ "adj" ]
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ "noun" ]
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ "art"; "adj" ]
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ "art"; "noun" ]
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ "adj"; "noun" ]
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ "art"; "adj"; "noun" ]
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ "art"; "adj"; "adj"; "noun" ]
    Assert.IsTrue( ( tempo.Tango input ).Success )

    let input = [ "art"; "adj"; "adj"; "noun"; "noun" ]
    Assert.IsFalse( ( tempo.Tango input ).Success )

    let input = [ "art"; "art"; "adj"; "adj"; "noun" ]
    Assert.IsFalse( ( tempo.Tango input ).Success )
