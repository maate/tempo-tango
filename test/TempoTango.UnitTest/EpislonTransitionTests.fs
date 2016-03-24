namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.LinearTimeLogic

[<TestFixture>]
module EpsilonTransitionTests =
  [<Test>]
  let ``Reducing True returns None``() =
    let reduced = EpislonTransition( Set.ofList [ True ] )
    Assert.That( reduced.IsNone )

  [<Test>]
  let ``Reducing p&q returns [([p;q], None)]``() =
    // Arrange
    let input = [ And( Prop "p", Prop "q" ) ];
    let expected = [ ( Set [ Prop "p"; Prop "q" ], None ) ]

    // Act
    let reduced = EpislonTransition( Set input ).Value

    // Assert
    Assert.IsTrue( ( reduced = expected ) )

  [<Test>]
  let ``Reducing p|q returns [([p], None); ([q], None)]``() =
    // Arrange
    let input = [ Or( Prop "p", Prop "q" ) ];
    let expected = [ ( Set [ Prop "p" ], None );
                     ( Set [ Prop "q" ], None ) ];

    // Act
    let reduced = EpislonTransition( Set input ).Value

    // Assert
    Assert.IsTrue( ( reduced = expected ) );

  [<Test>]
  let ``Reducing pRq returns [([p;q], None); ([q; XpRq], None)]``() =
    // Arrange
    let input = [ Release( Prop "p", Prop "q" ) ];
    let expected = [ ( Set [ Prop "p"; Prop "q" ], None );
                     ( Set [ Prop "q"; Next ( Release( Prop "p", Prop "q" ) ) ], None ) ]
    // Act
    let reduced = EpislonTransition( Set input ).Value

    // Assert
    Assert.IsTrue( ( reduced = expected ) )

  [<Test>]
  let ``Reducing Gp returns [([p;XGp], None)]``() =
    // Arrange
    let input = [ Globally( Prop "p" ) ];
    let expected = [ ( Set [ Prop "p"; Next ( Globally( Prop "p" ) ) ], None ) ]

    // Act
    let reduced = EpislonTransition( Set input ).Value

    // Assert
    Assert.IsTrue( ( reduced = expected ) )

  [<Test>]
  let ``Reducing pUq returns  [([q], None); ([p; XpUq], Some pUq)]``() =
    // Arrange
    let input = [ Until( Prop "p", Prop "q" ) ];
    let expected = [ ( Set [ Prop "q"], None );
                     ( Set [ Prop "p"; Next ( Until( Prop "p", Prop "q" ) ) ], Some ( Until( Prop "p", Prop "q" ) ) ) ]

    // Act
    let reduced = EpislonTransition( Set input ).Value

    // Assert
    Assert.IsTrue( ( reduced = expected ) )

