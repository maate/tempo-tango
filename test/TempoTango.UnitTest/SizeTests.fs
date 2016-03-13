﻿namespace TempoTango.UnitTest

open NUnit.Framework
open TempoTango
open TempoTango.LinearTimeLogic

[<TestFixture>]
module SizeTests =
  [<Test>]
  let ``Primitives count as one``() =
    Assert.AreEqual( LinearTimeLogic.sizeOf True, 1 )
    Assert.AreEqual( LinearTimeLogic.sizeOf False, 1 )
    Assert.AreEqual( LinearTimeLogic.sizeOf ( Prop "p" ), 1 )

  [<Test>]
  let ``Not Primitive counts as two``() =
    Assert.AreEqual( LinearTimeLogic.sizeOf ( Not True ), 2 )

  [<Test>]
  let ``Not is recursive``() =
    Assert.AreEqual( LinearTimeLogic.sizeOf ( Not ( Not True ) ), 3 )

  [<Test>]
  let ``And counts 1 + the largest antecedents size``() =
    Assert.AreEqual( LinearTimeLogic.sizeOf ( And ( Not True, False ) ), 3 )
