namespace TempoTango.UnitTest

open NUnit.Framework
open TempoTango
open TempoTango.Automaton
open TempoTango.LinearTimeLogic
open TempoTango.Parser

module AlphabetTest =
  let parse s = Parser.Parse s |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

  [<Test>]
  let ``Automaton p&q has Σ: ['p', 'q']``() =
    let a = parse( "p & q" )
    Assert.AreEqual( a.alphabet.Count, 2 )
    Assert.That( a.alphabet.Contains( "p" ) );
    Assert.That( a.alphabet.Contains( "q" ) );

  [<Test>]
  let ``Automaton p|Fp has Σ: ['p']``() =
    let a = parse( "p & q" )
    Assert.AreEqual( a.alphabet.Count, 2 )
    Assert.That( a.alphabet.Contains( "p" ) );

  [<Test>]
  let ``Automaton p&!q has Σ: ['p', 'q']``() =
    let a = parse( "p & !q" )
    Assert.AreEqual( a.alphabet.Count, 2 )
    Assert.That( a.alphabet.Contains( "p" ) );
    Assert.That( a.alphabet.Contains( "q" ) );

  [<Test>]
  let ``Automaton Fp has Σ: ['p']``() =
    let a = parse( "Fp" )
    Assert.AreEqual( a.alphabet.Count, 1 )
    Assert.That( a.alphabet.Contains( "p" ) );
