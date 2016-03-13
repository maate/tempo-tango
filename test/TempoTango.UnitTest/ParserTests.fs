namespace TempoTango.UnitTest

open NUnit.Framework
open TempoTango
open TempoTango.LinearTimeLogic
open TempoTango.Parser

[<TestFixture>]
module ParserTests =
  [<Test>]
  let ``Can parse true and false``() =
    Assert.AreEqual( Parser.Parse "false", False )
    Assert.AreEqual( Parser.Parse "true", True )

  [<Test>]
  let ``Can parse p``() =
    Assert.AreEqual( Parser.Parse "p ", Prop "p" )

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
    Parser.PrintParse "Xp|!q"
    Assert.AreEqual( Parser.Parse "Xp|!q", Or( Next( Prop "p" ), Not( Prop "q" ) ) )
