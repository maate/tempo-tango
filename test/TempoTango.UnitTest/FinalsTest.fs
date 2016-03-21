namespace TempoTango.UnitTest

open NUnit.Framework
open TempoTango
open TempoTango.Automaton
open TempoTango.LinearTimeLogic
open TempoTango.Parser

module FinalsTest =
  let parse s = Parser.Parse s |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

  [<Test>]
  let ``p U (q U r) has ∅ -> Σ -> ∅ final state``() =
    let a = parse "p U (q U r)"
    Assert.That( a.finals.Length, Is.EqualTo( 1 ) )
    Assert.That( (a.finals.Item 0).IsEmpty )

  [<Test>]
  let ``G(¬p ∨ Fq) has Fq,ϕ -> Σ -> Fq,ϕ as final state``() =
    let a = parse "G(¬p ∨ Fq)"
    Assert.That( a.finals.Length, Is.EqualTo( 1 ) )
    Assert.AreEqual( a.finals.Item 0, [ Finally( Prop "q" ); Parser.Parse("G(¬p ∨ Fq)") ] )
