namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.Automaton
open M8.TempoTango.LinearTimeLogic
open M8.TempoTango.Parser

module AlphabetTest =
  let private parse s = Parser.Parse s |> NegativeNormalForm |> Set.singleton |> Automaton.ConstructAutomatonFrom

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
