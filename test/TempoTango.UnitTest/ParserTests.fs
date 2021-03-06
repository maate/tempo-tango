﻿namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.LinearTemporalLogic
open M8.TempoTango.Parser

[<TestFixture>]
module ParserTests =
  [<Test>]
  let ``Can parse true and false``() =
    Assert.AreEqual( Parser.Parse "false", False )
    Assert.AreEqual( Parser.Parse "⊥", False )
    Assert.AreEqual( Parser.Parse "true", True )
    Assert.AreEqual( Parser.Parse "T", True )
    Assert.AreEqual( Parser.Parse "⊤", True )

  [<Test>]
  let ``Can parse empty string``() =
    Assert.AreEqual( Parser.Parse "E", Empty )
    Assert.AreEqual( Parser.Parse "ε", Empty )

  [<Test>]
  let ``Can parse p``() =
    Assert.AreEqual( Parser.Parse "p ", Prop "p" )

  [<Test>]
  let ``Can parse p1``() =
    Assert.AreEqual( Parser.Parse "p1", Prop "p1" )

  [<Test>]
  let ``Can parse !true``() =
    Assert.AreEqual( Parser.Parse "!true", Not( True ) )

  [<Test>]
  let ``Can parse !p``() =
    Assert.AreEqual( Parser.Parse "!p", Not( Prop "p" ) )

  [<Test>]
  let ``Can parse !Xp``() =
    Assert.AreEqual( Parser.Parse "!Xp", Not( Next ( Prop "p" ) ) )

  [<Test>]
  let ``Can parse p&q``() =
    Assert.AreEqual( Parser.Parse "p&q", And(Prop "p", Prop "q" ) )

  [<Test>]
  let ``Can parse Xp|!q``() =
    Assert.AreEqual( Parser.Parse "Xp|!q", Or( Next( Prop "p" ), Not( Prop "q" ) ) )

  [<Test>]
  let ``Can parse p1|p2&q1|q2``() =
    Assert.AreEqual( Parser.Parse "p1|p2&q1|q2", Or( Or( Prop "p1", And( Prop "p2", Prop "q1" ) ), Prop "q2" ) )

  [<Test>]
  let ``Can parse p1|(p2&q1)|q2``() =
    Assert.AreEqual( Parser.Parse "p1|(p2&q1)|q2", Or( Or( Prop "p1", And( Prop "p2", Prop "q1" ) ), Prop "q2" ) )

  [<Test>]
  let ``Can parse p1∨(p2∧q1)|q2``() =
    Assert.AreEqual( Parser.Parse "p1∨(p2∧q1)|q2", Or( Or( Prop "p1", And( Prop "p2", Prop "q1" ) ), Prop "q2" ) )

  [<Test>]
  let ``Can parse p1|(p2&q1)|q2 with white spaces``() =
    Assert.AreEqual( Parser.Parse @"p1
                                    |(
                                      p2 & q1
                                    )
                                    |q2
                                    ", Or( Or( Prop "p1", And( Prop "p2", Prop "q1" ) ), Prop "q2" ) )
