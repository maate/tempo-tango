namespace M8.TempoTango.UnitTest

open NUnit.Framework
open M8.TempoTango
open M8.TempoTango.LinearTemporalLogic

[<TestFixture>]
module NegativeNormalFormTests =
  [<Test>]
  let ``Negative Normal Form Of True is True``() =
    let nnf = NegativeNormalForm True
    Assert.AreEqual( nnf, True )

  [<Test>]
  let ``Negative Normal Form Of False is False``() =
    let nnf = NegativeNormalForm False
    Assert.AreEqual( nnf, False )

  [<Test>]
  let ``Negative Normal Form Of Prop is Prop``() =
    let nnf = NegativeNormalForm ( Prop "p" )
    Assert.AreEqual( nnf, Prop( "p" ) )

  [<Test>]
  let ``Negative Normal Form Of And is And``() =
    let nnf = NegativeNormalForm ( And ( True, True ) )
    Assert.AreEqual( nnf, And( True, True ) )

  [<Test>]
  let ``Negative Normal Form Of Or is Or``() =
    let nnf = NegativeNormalForm ( Or ( True, True ) )
    Assert.AreEqual( nnf, Or( True, True ) )

  [<Test>]
  let ``Negative Normal Form Of Next is Next``() =
    let nnf = NegativeNormalForm ( Next True )
    Assert.AreEqual( nnf, Next True )

  [<Test>]
  let ``Negative Normal Form Of Finally is Finally``() =
    let nnf = NegativeNormalForm ( Finally True )
    Assert.AreEqual( nnf, Finally True )

  [<Test>]
  let ``Negative Normal Form Of Globally is Globally``() =
    let nnf = NegativeNormalForm ( Globally True )
    Assert.AreEqual( nnf, Globally True )

  [<Test>]
  let ``Negative Normal Form Of Until is Until``() =
    let nnf = NegativeNormalForm ( Until ( True, False ) )
    Assert.AreEqual( nnf, Until ( True, False ) )

  [<Test>]
  let ``Negative Normal Form Of Release is Release``() =
    let nnf = NegativeNormalForm ( Release ( True, False ) )
    Assert.AreEqual( nnf, Release ( True, False ) )

  [<Test>]
  let ``Negative Normal Form Of Not True is False``() =
    let nnf = NegativeNormalForm ( Not True )
    Assert.AreEqual( nnf, False )

  [<Test>]
  let ``Negative Normal Form Of Not False is True``() =
    let nnf = NegativeNormalForm ( Not False )
    Assert.AreEqual( nnf, True )

  [<Test>]
  let ``Negative Normal Form Of Not Prop is Not Prop``() =
    let nnf = NegativeNormalForm ( Not ( Prop "p" ) )
    Assert.AreEqual( nnf, Not ( Prop "p" ) )

  [<Test>]
  let ``Negative Normal Form Of Not Not Prop is Prop``() =
    let nnf = NegativeNormalForm ( Not ( Not ( Prop "p" ) ) )
    Assert.AreEqual( nnf, Prop "p" )

  [<Test>]
  let ``Negative Normal Form Of Not And is Or Not``() =
    let nnf = NegativeNormalForm ( Not ( And ( True, False ) ) )
    Assert.AreEqual( nnf, Or( False, True ) ) // note reversed False, True because Not True is False and Not False is True

  [<Test>]
  let ``Negative Normal Form Of Not Or is And Not``() =
    let nnf = NegativeNormalForm ( Not ( Or ( True, False ) ) )
    Assert.AreEqual( nnf, And( False, True ) ) // same note as above

  [<Test>]
  let ``Negative Normal Form Of Not Next True is Next False``() =
    let nnf = NegativeNormalForm ( Not ( Next ( True ) ) )
    Assert.AreEqual( nnf, Next( False ) )

  [<Test>]
  let ``Negative Normal Form Of Not Finally True is Finally False``() =
    let nnf = NegativeNormalForm ( Not ( Finally ( True ) ) )
    Assert.AreEqual( nnf, Finally( False ) )

  [<Test>]
  let ``Negative Normal Form Of Not Globally True is Globally False``() =
    let nnf = NegativeNormalForm ( Not ( Globally ( True ) ) )
    Assert.AreEqual( nnf, Globally( False ) )

  [<Test>]
  let ``Negative Normal Form Of Not Until True False is Release False True``() =
    let nnf = NegativeNormalForm ( Not ( Until ( True, False ) ) )
    Assert.AreEqual( nnf, Release( False, True ) )

  [<Test>]
  let ``Negative Normal Form Of Not Release True False is Until False True``() =
    let nnf = NegativeNormalForm ( Not ( Release ( True, False ) ) )
    Assert.AreEqual( nnf, Until( False, True ) )