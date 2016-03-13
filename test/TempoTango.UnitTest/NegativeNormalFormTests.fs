namespace TempoTango.UnitTest

open NUnit.Framework
open TempoTango
open TempoTango.LinearTimeLogic

[<TestFixture>]
module NegativeNormalFormTests =
  [<Test>]
  let ``Negative Normal Form Of True is True``() =
    let nnf = LinearTimeLogic.NegativeNormalForm True
    Assert.AreEqual( nnf, True )

  [<Test>]
  let ``Negative Normal Form Of False is False``() =
    let nnf = LinearTimeLogic.NegativeNormalForm False
    Assert.AreEqual( nnf, False )

  [<Test>]
  let ``Negative Normal Form Of Prop is Prop``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Prop "p" )
    Assert.AreEqual( nnf, Prop( "p" ) )

  [<Test>]
  let ``Negative Normal Form Of And is And``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( And ( True, True ) )
    Assert.AreEqual( nnf, And( True, True ) )

  [<Test>]
  let ``Negative Normal Form Of Or is Or``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Or ( True, True ) )
    Assert.AreEqual( nnf, Or( True, True ) )

  [<Test>]
  let ``Negative Normal Form Of Next is Next``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Next True )
    Assert.AreEqual( nnf, Next True )

  [<Test>]
  let ``Negative Normal Form Of Finally is Finally``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Finally True )
    Assert.AreEqual( nnf, Finally True )

  [<Test>]
  let ``Negative Normal Form Of Globally is Globally``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Globally True )
    Assert.AreEqual( nnf, Globally True )

  [<Test>]
  let ``Negative Normal Form Of Until is Until``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Until ( True, False ) )
    Assert.AreEqual( nnf, Until ( True, False ) )

  [<Test>]
  let ``Negative Normal Form Of Release is Release``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Release ( True, False ) )
    Assert.AreEqual( nnf, Release ( True, False ) )

  [<Test>]
  let ``Negative Normal Form Of Not True is False``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Not True )
    Assert.AreEqual( nnf, False )

  [<Test>]
  let ``Negative Normal Form Of Not False is True``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Not False )
    Assert.AreEqual( nnf, True )

  [<Test>]
  let ``Negative Normal Form Of Not Prop is Not Prop``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Not ( Prop "p" ) )
    Assert.AreEqual( nnf, Not ( Prop "p" ) )

  [<Test>]
  let ``Negative Normal Form Of Not Not Prop is Prop``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Not ( Not ( Prop "p" ) ) )
    Assert.AreEqual( nnf, Prop "p" )

  [<Test>]
  let ``Negative Normal Form Of Not And is Or Not``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Not ( And ( True, False ) ) )
    Assert.AreEqual( nnf, Or( False, True ) ) // note reversed False, True because Not True is False and Not False is True

  [<Test>]
  let ``Negative Normal Form Of Not Or is And Not``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Not ( Or ( True, False ) ) )
    Assert.AreEqual( nnf, And( False, True ) ) // same note as above

  [<Test>]
  let ``Negative Normal Form Of Not Next True is Next False``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Not ( Next ( True ) ) )
    Assert.AreEqual( nnf, Next( False ) )

  [<Test>]
  let ``Negative Normal Form Of Not Finally True is Finally False``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Not ( Finally ( True ) ) )
    Assert.AreEqual( nnf, Finally( False ) )

  [<Test>]
  let ``Negative Normal Form Of Not Globally True is Globally False``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Not ( Globally ( True ) ) )
    Assert.AreEqual( nnf, Globally( False ) )

  [<Test>]
  let ``Negative Normal Form Of Not Until True False is Release False True``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Not ( Until ( True, False ) ) )
    Assert.AreEqual( nnf, Release( False, True ) )

  [<Test>]
  let ``Negative Normal Form Of Not Release True False is Until False True``() =
    let nnf = LinearTimeLogic.NegativeNormalForm ( Not ( Release ( True, False ) ) )
    Assert.AreEqual( nnf, Until( False, True ) )