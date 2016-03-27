namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.Automaton
open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Parser

/// Tests that the ReducedGBA for LTL expression (without any reductions)
///               G(!p|Fq)
/// corresponds to /doc/reduced-gba.png
module SkipEpsilonTests =
  let private ϕ = Parser.Parse "G(!p|Fq)"

  let private fullGba = ϕ |> NegativeNormalForm |> Set.singleton |> Automaton.constructFrom
  let private reducedGba = fullGba |> Automaton.skipEpsilons

  let private transitions = reducedGba.transitions |> Set.map ( fun trans -> trans.edge, trans.s, trans.t )

  let private Σ = Sigma([],[])

  [<Test>]
  let ``Start state is equal to input: G(!p|Fq)``() =
    Assert.AreEqual( reducedGba.starts.Item 0, Set.singleton ( Parser.Parse "G(!p|Fq)" ) );

  [<Test>]
  let ``ReducedGBA has transition ϕ -> Σ -> Fq,ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Σ,
                                          ϕ                                  |> Set.singleton,
                                          [ Finally ( Prop "q" ); ϕ ]        |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``ReducedGBA has transition ϕ -> Σq Fq -> ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [Prop "q"], [ Finally ( Prop "q" ) ] ),
                                          ϕ                                  |> Set.singleton,
                                          ϕ                                  |> Set.singleton ] )
                |> Assert.IsTrue

  [<Test>]
  let ``ReducedGBA has transition ϕ -> Σ¬p Fq -> ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [Not ( Prop "p" ) ], [ Finally ( Prop "q" ) ] ),
                                          ϕ                                  |> Set.singleton,
                                          ϕ                                  |> Set.singleton ] )
                |> Assert.IsTrue

  [<Test>]
  let ``ReducedGBA has transition Fq,ϕ -> Σq Fq -> ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [Prop "q" ], [ Finally ( Prop "q" ) ] ),
                                          [ Finally( Prop "q" ); ϕ ]         |> Set.ofList,
                                          ϕ                                  |> Set.singleton ] )
                |> Assert.IsTrue

  [<Test>]
  let ``ReducedGBA has transition Fq,ϕ -> Σ¬p∧q Fq -> ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [ Not( Prop "p" ); Prop "q" ], [ Finally ( Prop "q" ) ] ),
                                          [ Finally( Prop "q" ); ϕ ]         |> Set.ofList,
                                          ϕ                                  |> Set.singleton ] )
                |> Assert.IsTrue

  [<Test>]
  let ``ReducedGBA has transition Fq,ϕ -> Σ -> Fq,ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Σ,
                                          [ Finally( Prop "q" ); ϕ ]         |> Set.ofList,
                                          [ Finally( Prop "q" ); ϕ ]         |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``ReducedGBA has transition Fq,ϕ -> Σ¬p -> Fq,ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [ Not( Prop "p" ) ], [] ),
                                          [ Finally( Prop "q" ); ϕ ]         |> Set.ofList,
                                          [ Finally( Prop "q" ); ϕ ]         |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``ReducedGBA has 7 transitions``() =
    Assert.AreEqual( transitions.Count, 7 )
