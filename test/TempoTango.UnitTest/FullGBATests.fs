namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.Automaton
open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Parser

/// Tests that the FullGBA for LTL expression (without any reductions)
///               G(!p|Fq)
/// corresponds to /doc/gba.png
module FullGBATests = 
  let private ϕ = Parser.Parse "G(!p|Fq)"

  let private gba = ϕ |> NegativeNormalForm |> Set.singleton |> Automaton.constructFrom

  let private transitions = gba.transitions |> Set.map ( fun trans -> trans.edge, trans.s, trans.t )

  let private ε = Epsilon([])

  [<Test>]
  let ``Start state is equal to input: G(!p|Fq)``() =
    Assert.AreEqual( gba.starts.Item 0, Set.singleton ( Parser.Parse "G(!p|Fq)" ) );

  [<Test>]
  let ``FullGBA has ε transition G(¬p ∨ Fq) -> ε -> ¬p ∨ Fq,Xϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          ε,
                                          ϕ                                  |> Set.singleton,
                                          [ Parser.Parse "¬p ∨ Fq"; Next ϕ ] |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition ¬p ∨ Fq,Xϕ -> ε -> ¬p,Xϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          ε,
                                          Set.ofList [ Parser.Parse "¬p ∨ Fq"; Next ϕ ], Set.ofList [ Not ( Prop "p" ); Next ϕ ] ] ) |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition ¬p,Xϕ -> Σ(¬p) -> ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [ Not( Prop "p" ) ], [] ),
                                          [ Not ( Prop "p" ); Next ϕ ]       |> Set.ofList,
                                          ϕ                                  |> Set.singleton ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition ¬p ∨ Fq,Xϕ -> ε -> Fq,Xϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          ε,
                                          [ Parser.Parse "¬p ∨ Fq"; Next ϕ ] |> Set.ofList,
                                          [ Finally ( Prop "q" ); Next ϕ ]    |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition Fq,Xϕ -> ε -> q,Xϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          ε,
                                          [ Finally ( Prop "q" ); Next ϕ ] |> Set.ofList,
                                          [ Prop "q"; Next ϕ ]             |> Set.ofList ] ) |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition q,Xϕ -> Σ(q) -> ϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [ Prop "q" ], [] ),
                                          [ Prop "q"; Next ϕ ]       |> Set.ofList,
                                          ϕ                          |> Set.singleton ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition Fq,Xϕ -> ε(Fq) -> X F q,Xϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Epsilon( [ Finally( Prop "q" ) ] ),
                                          [ Finally ( Prop "q" ); Next ϕ ]            |> Set.ofList,
                                          [ Next ( Finally ( Prop "q" ) ); Next ϕ ]   |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition X F q,Xϕ -> Σ -> Fq,ϕ ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [],[] ),
                                          [ Next ( Finally ( Prop "q" ) ); Next ϕ ]   |> Set.ofList,
                                          [ Finally ( Prop "q" ); ϕ ]                 |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition Fq,ϕ -> ε -> Fq,¬p ∨ Fq,Xϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          ε,
                                          [ Finally ( Prop "q" ); ϕ ]                                  |> Set.ofList,
                                          [ Finally ( Prop "q" ); Parser.Parse( "¬p ∨ Fq" ); Next ϕ ]  |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition Fq,¬p ∨ Fq,Xϕ -> ε -> Fq,Xϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          ε,
                                          [ Finally ( Prop "q" ); Parser.Parse( "¬p ∨ Fq" ); Next ϕ ]  |> Set.ofList,
                                          [ Finally ( Prop "q" ); Next ϕ ]                              |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition Fq,¬p ∨ Fq,Xϕ -> ε -> F q,¬p,Xϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          ε,
                                          [ Finally ( Prop "q" ); Parser.Parse( "¬p ∨ Fq" ); Next ϕ ]  |> Set.ofList,
                                          [ Finally ( Prop "q" ); Not( Prop "p" ); Next ϕ ]             |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition F q,¬p,Xϕ -> ε -> q,¬p,Xϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          ε,
                                          [ Finally ( Prop "q" ); Not( Prop "p" ); Next ϕ ]  |> Set.ofList,
                                          [ Prop "q"; Not( Prop "p" ); Next ϕ ]              |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has ε transition q,¬p,Xϕ -> !p∧q -> G(¬p ∨ Fq)``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [ Not( Prop "p" ); Prop "q"], [] ),
                                          [ Not ( Prop "p" ); Next ϕ; Prop "q" ]     |> Set.ofList,
                                          ϕ                                          |> Set.singleton ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition F q,¬p,Xϕ -> ε(Fq) -> X Fq,¬p,Xϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Epsilon( [ Finally( Prop "q" ) ] ),
                                          [ Finally ( Prop "q" ); Not( Prop "p" ); Next ϕ ]            |> Set.ofList,
                                          [ Next ( Finally ( Prop "q" ) ); Not( Prop "p" ); Next ϕ ]   |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has transition X Fq,¬p,Xϕ -> ε(Fq) -> X Fq,¬p,Xϕ``() =
    transitions |> Set.isProperSubset ( Set.ofList [
                                          Sigma( [ Not( Prop "p" )],[] ),
                                          [ Next( Finally ( Prop "q" ) ); Not( Prop "p" ); Next ϕ ]  |> Set.ofList,
                                          [ Finally ( Prop "q" ); ϕ ]                                |> Set.ofList ] )
                |> Assert.IsTrue

  [<Test>]
  let ``FullGBA has 15 transitions``() =
    Assert.AreEqual( transitions.Count, 15 )
