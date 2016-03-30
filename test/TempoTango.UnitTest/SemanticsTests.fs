namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.Automaton
open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Parser

[<TestFixture>]
module SemanticsTests =
  let private parse s = Parser.Parse s |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

  [<Test>]
  let ``And's are true when l and r are both true``() =
    let s = "a&b"
    let a = parse s

    let input = [ [ "a"; "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a"; "b" ] ] ) )

  [<Test>]
  let ``And's are false when l is false``() =
    let s = "a&b"
    let a = parse s

    let input = [ [ "a" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``And's are false when r is false``() =
    let s = "a&b"
    let a = parse s

    let input = [ [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``And's are false when l is true and next r is true``() =
    let s = "a&b"
    let a = parse s

    let input = [ [ "a" ]; [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``And's are true when l1 is true and l2 is true and r is true``() =
    let s = "a&b&c"
    let a = parse s

    let input = [ [ "a"; "b"; "c" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a"; "b"; "c" ] ] ) )

  [<Test>]
  let ``And's are false when l1 is true and l2 is true and r is false``() =
    let s = "a&b&c"
    let a = parse s

    let input = [ [ "a"; "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``Or's are true when l is true``() =
    let s = "a|b"
    let a = parse s

    let input = [ [ "a" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a" ] ] ) )

  [<Test>]
  let ``Or's are true when r is true``() =
    let s = "a|b"
    let a = parse s

    let input = [ [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "b" ] ] ) )

  [<Test>]
  let ``Or's are true when l is true and r is true``() =
    let s = "a|b"
    let a = parse s

    let input = [ [ "a"; "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a"; "b" ] ] ) )

  [<Test>]
  let ``Or's are false when l is false and r is false``() =
    let s = "a|b"
    let a = parse s

    let input = [ [ "c" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``Untils's are true when r is true``() =
    let s = "aUb"
    let a = parse s

    let input = [ [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "b" ] ] ) )

  [<Test>]
  let ``Untils's are true when l is true and next r is true``() =
    let s = "aUb"
    let a = parse s

    let input = [ [ "a" ]; [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a" ]; [ "b" ] ] ) )

  [<Test>]
  let ``Untils's are true when l is true and next l is true and next r is true``() =
    let s = "aUb"
    let a = parse s

    let input = [ [ "a" ]; [ "a" ]; [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a" ]; [ "a" ]; [ "b" ] ] ) )

  [<Test>]
  let ``Untils's are false when l is true``() =
    let s = "aUb"
    let a = parse s

    let input = [ [ "a" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``Release's are true when l is false and r is true``() =
    let s = "aRb"
    let a = parse s

    let input = [ [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "b" ] ] ) )

  [<Test>]
  let ``Release's are true when l is true and r is true``() =
    let s = "aRb"
    let a = parse s

    let input = [ [ "a"; "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a"; "b" ] ] ) )

  [<Test>]
  let ``Release's are true when r is true and next both l and r is true and next neither are true``() =
    let s = "aRb"
    let a = parse s

    let input = [ [ "b" ]; [ "a"; "b" ]; [ "c" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "b" ]; [ "a"; "b" ]; [ "c" ] ] ) )

  [<Test>]
  let ``Release's are false when r is true and next l is true and r is false``() =
    let s = "aRb"
    let a = parse s

    let input = [ [ "b" ]; [ "a" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``Release's are false when l is true and r is false``() =
    let s = "aRb"
    let a = parse s

    let input = [ [ "a" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``WeakUntils's are true when r is true``() =
    let s = "aWb"
    let a = parse s

    let input = [ [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "b" ] ] ) )

  [<Test>]
  let ``WeakUntils's are true when l is true and next r is true``() =
    let s = "aWb"
    let a = parse s

    let input = [ [ "a" ]; [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a" ]; [ "b" ] ] ) )

  [<Test>]
  let ``WeakUntils's are true when l is true and next l is true and next r is true``() =
    let s = "aWb"
    let a = parse s

    let input = [ [ "a" ]; [ "a" ]; [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a" ]; [ "a" ]; [ "b" ] ] ) )

  [<Test>]
  let ``WeakUntils's are true when l is true``() =
    let s = "aWb"
    let a = parse s

    let input = [ [ "a" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a" ] ] ) )

  [<Test>]
  let ``Optionals's are true when l is true and next r is true``() =
    let s = "a?b"
    let a = parse s

    let input = [ [ "a" ]; [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a" ]; [ "b" ] ] ) )

  [<Test>]
  let ``Optionals's are true when l is false and r is true``() =
    let s = "a?b"
    let a = parse s

    let input = [ [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "b" ] ] ) )

  [<Test>]
  let ``Optionals's are false when l is true``() =
    let s = "a?b"
    let a = parse s

    let input = [ [ "a" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``Implications's are true when l is true and r is true``() =
    let s = "a->b"
    let a = parse s

    let input = [ [ "a"; "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "a"; "b" ] ] ) )

  [<Test>]
  let ``Implications's are true when r is true``() =
    let s = "a->b"
    let a = parse s

    let input = [ [ "b" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "b" ] ] ) )

  [<Test>]
  let ``Implications's are false when l is true and r is false``() =
    let s = "a->b"
    let a = parse s

    let input = [ [ "a" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``Final's are true when p is true``() =
    let s = "Fp"
    let a = parse s

    let input = [ [ "p" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "p" ] ] ) )

  [<Test>]
  let ``Final's are true when p is false and then p is true``() =
    let s = "Fp"
    let a = parse s

    let input = [ [ "q" ]; [ "p" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "q" ]; [ "p" ] ] ) )

  [<Test>]
  let ``Final's are true when p is false and then p is false and then p is true``() =
    let s = "Fp"
    let a = parse s

    let input = [ [ "q" ]; [ "q" ]; [ "p" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "q" ]; [ "q" ]; [ "p" ] ] ) )

  [<Test>]
  let ``Final's are false when p is false``() =
    let s = "Fp"
    let a = parse s

    let input = [ [ "q" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``Globals's are false when p is false``() =
    let s = "Gp"
    let a = parse s

    let input = [ [ "q" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``Globals's are true when p is true``() =
    let s = "Gp"
    let a = parse s

    let input = [ [ "p" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "p" ] ] ) )

  [<Test>]
  let ``Globals's are true when p is true and then p is true``() =
    let s = "Gp"
    let a = parse s

    let input = [ [ "p" ]; [ "p" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "p" ]; [ "p" ] ] ) )

  [<Test>]
  let ``Globals's are false when p is true and then p is false``() =
    let s = "Gp"
    let a = parse s

    let input = [ [ "p" ]; [ "q" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``Nexts's are true when q is true and then p is true``() =
    let s = "Xp"
    let a = parse s

    let input = [ [ "q" ]; [ "p" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "q" ]; [ "p" ] ] ) )

  [<Test>]
  let ``Nexts's are true when p is true and then p is true``() =
    let s = "Xp"
    let a = parse s

    let input = [ [ "p" ]; [ "p" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "p" ]; [ "p" ] ] ) )

  [<Test>]
  let ``Nexts's are false when p is true and then p is false``() =
    let s = "Xp"
    let a = parse s

    let input = [ [ "p" ]; [ "q" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``Not's are false when p is true``() =
    let s = "!p"
    let a = parse s

    let input = [ [ "p" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsNone )

  [<Test>]
  let ``Not's are true when p is false``() =
    let s = "!p"
    let a = parse s

    let input = [ [ "q" ] ]
    let result = a |> Tango input

    Assert.IsTrue( result.IsSome )
    Assert.That( result.Value, Is.EqualTo( [ [ "q" ] ] ) )
