namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.LinearTemporalLogic

[<TestFixture>]
module SizeTests =
  [<Test>]
  let ``Primitives count as one``() =
    Assert.AreEqual( sizeOf True, 1 )
    Assert.AreEqual( sizeOf False, 1 )
    Assert.AreEqual( sizeOf ( Prop "p" ), 1 )

  [<Test>]
  let ``Not Primitive counts as two``() =
    Assert.AreEqual( sizeOf ( Not True ), 2 )

  [<Test>]
  let ``Not is recursive``() =
    Assert.AreEqual( sizeOf ( Not ( Not True ) ), 3 )

  [<Test>]
  let ``And counts 1 + the largest antecedents size``() =
    Assert.AreEqual( sizeOf ( And ( Not True, False ) ), 3 )
