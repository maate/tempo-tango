namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.Automaton
open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Parser

[<TestFixture>]
module OperatorPrecedenceTests =
  let private parse s = Parser.Parse s |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom
  let private rawParse s = Parser.Parse s |> CleanExpression

  [<Test>]
  let ``And's before Or's``() =
    let s = "a&b|c"
    let a = parse s

    let input = [ [ "a"; "b" ] ]
    let result = a |> Tango input

    Assert.AreEqual( Parser.Parse s |> ToString, "(a & b) | c" )
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a"; "b" ] ] ) )

  [<Test>]
  let ``Until's before Or's``() =
    let s = "aUb|c"
    let a = parse s

    let input = [ [ "b" ] ]
    let result = a |> Tango input

    Assert.AreEqual( Parser.Parse s |> ToString, "(a U b) | c" )
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "b" ] ] ) )

  [<Test>]
  let ``And's before Untils's``() =
    let s = "aUb&c"
    let a = parse s

    let input = [ [ "b"; "c" ] ]
    let result = a |> Tango input

    Assert.AreEqual( Parser.Parse s |> ToString, "a U (b & c)" )
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "b"; "c" ] ] ) )

  [<Test>]
  let ``Releases's before Or's``() =
    let s = "aRb|c"
    let a = parse s

    let input = [ [ "a"; "b" ] ]
    let result = a |> Tango input

    Assert.AreEqual( Parser.Parse s |> ToString, "(a R b) | c" )
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a"; "b" ] ] ) )

  [<Test>]
  let ``WeakUntils's before Or's``() =
    let s = "aWb|c"
    let a = parse s

    let input = [ [ "a" ]; [ "b" ] ]
    let result = a |> Tango input

    Assert.AreEqual( Parser.Parse s |> CleanExpression |> ToString, "((a U b) | G a) | c" )
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a" ]; [ "b" ] ] ) )

  [<Test>]
  let ``Or's before Implications's``() =
    let s = "a->b|c"
    let a = parse s

    let input = [ [ "a"; "b" ] ]
    let result = a |> Tango input

    Assert.AreEqual( Parser.Parse s |> CleanExpression |> ToString, "¬(a & ¬(b | c))" )
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a"; "b" ] ] ) )

  [<Test>]
  let ``Optionals's before Or's``() =
    let s = "a?b|c"
    let a = parse s

    let input = [ [ "c" ] ]
    let result = a |> Tango input

    Assert.AreEqual( Parser.Parse s |> CleanExpression |> ToString, "((a & X b) | b) | c" )
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "c" ] ] ) )

  [<Test>]
  let ``Not's before And's``() =
    let s = "a&!b"
    let a = parse s

    let input = [ [ "a" ] ]
    let result = a |> Tango input

    Assert.AreEqual( Parser.Parse s |> CleanExpression |> ToString, "a & ¬b" )
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a" ] ] ) )

  [<Test>]
  let ``Next's before And's``() =
    let s = "a&Xb"
    let a = parse s

    let input = [ [ "a" ]; [ "b" ] ]
    let result = a |> Tango input

    Assert.AreEqual( Parser.Parse s |> CleanExpression |> ToString, "a & X b" )
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a" ]; [ "b" ] ] ) )

  [<Test>]
  let ``Future's before And's``() =
    let s = "a&Fb"
    let a = parse s

    let input = [ [ "a" ]; [ "b" ] ]
    let result = a |> Tango input

    Assert.AreEqual( Parser.Parse s |> CleanExpression |> ToString, "a & F b" )
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a" ]; [ "b" ] ] ) )

  [<Test>]
  let ``Global's before And's``() =
    let s = "a&Gb"
    let a = parse s

    let input = [ [ "a"; "b" ] ]
    let result = a |> Tango input

    Assert.AreEqual( Parser.Parse s |> CleanExpression |> ToString, "a & G b" )
    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a"; "b" ] ] ) )
