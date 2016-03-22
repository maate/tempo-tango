namespace TempoTango.UnitTest

open NUnit.Framework
open TempoTango
open TempoTango.Automaton
open TempoTango.LinearTimeLogic
open TempoTango.Parser

module UnionTests =
  let parse s = Parser.Parse s |> CleanExpression |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

//  [<Test>]
//  let ``Foo``() =
//    let a = parse "a W b"
//    
//    let l = parse "a & Xb"
//    let r = parse "c & Xa"
//    let u = Automaton.Union l r
////    System.Diagnostics.Debugger.Launch()
//    Assert.IsTrue( false )
