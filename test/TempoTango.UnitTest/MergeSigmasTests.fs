namespace TempoTango.UnitTest

open NUnit.Framework
open TempoTango
open TempoTango.Automaton
open TempoTango.LinearTimeLogic
open TempoTango.Parser

/// Tests that the MinifiedGBA for LTL expression (without any reductions)
///               G(!p|Fq)
/// corresponds to /doc/minified-gba.png
module MergedSigmasTests =
  let private ϕ = Parser.Parse "G(!p|Fq)"

  let private fullGba = ϕ |> NegativeNormalForm |> Set.singleton |> Automaton.constructFrom
  let private reducedGba = fullGba |> Automaton.skipEpsilons
  let private minifiedGba = reducedGba |> Automaton.joinSigmas

  let private transitions = minifiedGba.transitions |> Set.map ( fun trans -> trans.edge, trans.s, trans.t )

  let private Σ = Sigma([],[])

  [<Test>]
  let ``Start state is equal to input: G(!p|Fq)``() =
    Assert.AreEqual( minifiedGba.starts.Item 0, Set.singleton ( ϕ ) );


  [<Test>]
  let ``MinifiedGBA has transition ϕ -> Σ -> Fq,ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Σ,
                                          ϕ                                  |> Set.singleton,
                                          [ Finally ( Prop "q" ); ϕ ]        |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``MinifiedGBA has transition Fq,ϕ -> Σ -> Fq,ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Σ,
                                          [ Finally ( Prop "q" ); ϕ ]        |> Set.ofList,
                                          [ Finally ( Prop "q" ); ϕ ]        |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``MinifiedGBA has transition ϕ -> Σ¬p∨q -> ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [ Or( Not( Prop "p" ), Prop "q" ) ], [Finally( Prop "q" )] ),
                                          ϕ                                  |> Set.singleton,
                                          ϕ                                  |> Set.singleton ] )
                |> Assert.IsTrue

  [<Test>]
  let ``MinifiedGBA has transition Fq,ϕ -> Σq -> ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [ Prop "q" ], [Finally( Prop "q" )] ),
                                          [ Finally ( Prop "q" ); ϕ ]        |> Set.ofList,
                                          ϕ                                  |> Set.singleton ] )
                |> Assert.IsTrue

  [<Test>]
  let ``MinifiedGBA has 4 transitions``() =
    Assert.AreEqual( transitions.Count, 4 )
