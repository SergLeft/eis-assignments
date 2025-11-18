package geometry

import geometry.loops.*
import collection.immutable.ArraySeq

import geometry.Rectangle
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.funsuite.AnyFunSuite

class RectangleLoopSuite extends AnyFunSuite {

  // ScalaTest uses this to compare Doubles when we write "==="
  implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(1e-5)

  private val sampleRectangles = ArraySeq(
    Rectangle(0, 0, 5, 4),
    Rectangle(10, 5, 2, 3),
    Rectangle(-9.5, 2, 4, 0.5),
    Rectangle(-23.8, -19.6, 17, 12),
    Rectangle(23, 25, 10, 13),
  )


  test("scale all by 2 should yield rectangles with double the width and height") {
    val scaled = scaleAll(2, sampleRectangles)
    val expected = ArraySeq(
      Rectangle(0, 0, 10, 8),
      Rectangle(10, 5, 4, 6),
      Rectangle(-9.5, 2, 8, 1),
      Rectangle(-23.8, -19.6, 34, 24),
      Rectangle(23, 25, 20, 26),
    )
    assert(scaled == expected)
  }

  test("scaleAll with an empty set should yield an empty set again") {
    assert(scaleAll(42, ArraySeq()).isEmpty)
  }

  test("scaleAll should not accept a negative scale factor") {
    assertThrows[IllegalArgumentException] {
      scaleAll(-1, sampleRectangles)
    }
  }

  test("centerAroundOrigin should place the center of the rectangles at the origin") {
    val centered = centerAroundOrigin(sampleRectangles)
    val expected = ArraySeq(
      Rectangle(-2.5, -2, 5, 4),
      Rectangle(-1, -1.5, 2, 3),
      Rectangle(-2, -0.25, 4, 0.5),
      Rectangle(-8.5, -6, 17, 12),
      Rectangle(-5, -6.5, 10, 13),
    )
    assert(centered == expected)
  }

  test("centerAroundOrigin with an empty set should yield an empty set again") {
    assert(centerAroundOrigin(ArraySeq()).isEmpty)
  }

  test("filterAreaBetween with 12, 130 and rectangles should yield a set with the first and last rectangle") {
    val filtered = filterAreaBetween(12, 130, sampleRectangles)
    val expected = ArraySeq(
      sampleRectangles.head,
      sampleRectangles.last
    )
    assert(filtered == expected)
  }

  test("filterAreaBetween with 2, 20 and rectangles should yield the first 3 rectangles") {
    val filtered = filterAreaBetween(2, 20, sampleRectangles)
    val expected = sampleRectangles.slice(0, 3)
    assert(filtered == expected)
  }

  test("filterAreaBetween with 0, 1 and rectangles should yield an empty set") {
    assert(filterAreaBetween(0, 1, sampleRectangles).isEmpty)
  }

  test("filterAreaBetween with 0, 1000 and an empty set should yield an empty set again") {
    assert(filterAreaBetween(0, 1000, ArraySeq()).isEmpty)
  }

  test("removeEmpty with only non-empty rectangles should yield the same set again") {
    assert(removeEmpty(sampleRectangles).zip(sampleRectangles).forall(t => t._1 === t._2))
  }

  test("removeEmpty with a set containing a single empty rectangle should yield the set without this rectangle") {
    val rectangles = ArraySeq(
      Rectangle(21, 3, 5, 2),
      Rectangle(-98, 176, 0, 72),
      Rectangle(4, 7, 21, 10),
    )
    val emptyRemoved = removeEmpty(rectangles)
    val expected = ArraySeq(
      rectangles.head,
      rectangles.last
    )
    // this test will also fail if you create new rectangles instead of reusing the existing ones
    assert(emptyRemoved.zip(expected).forall(t => t._1 === t._2))
  }

  test("removeEmpty with a set containing multiple empty rectangles should yield a set without all these rectangles") {
    val rectangles = ArraySeq(
      Rectangle(0, 0, 21, 0),
      Rectangle(12, 32, 127, 98),
      Rectangle(98, 69, 21, 0),
      Rectangle(76, 2, 4, 7),
    )
    val emptyRemoved = removeEmpty(rectangles)
    val expected = ArraySeq(
      rectangles(1),
      rectangles.last
    )
    assert(emptyRemoved.zip(expected).forall(t => t._1 === t._2))
  }

  test("hasSquares with only non-square rectangles should yield false") {
    assert(!hasSquare(sampleRectangles))
  }

  test("hasSquares with a single square rectangle should yield true") {
    val rectangles = ArraySeq(
      Rectangle(0, 0, 21, 32),
      Rectangle(0, 0, 12, 12),
    )
    assert(hasSquare(rectangles))
  }

  test("hasSquares with multiple square rectangles should yield true") {
    val rectangles = ArraySeq(
      Rectangle(0, 0, 21, 32),
      Rectangle(0, 0, 12, 12),
      Rectangle(0, 0, 87, 32),
      Rectangle(12, 43, 3, 3),
    )
    assert(hasSquare(rectangles))
  }

  test("allContainedIn should return false if the rectangle to check against is empty") {
    assert(!allContainedIn(EmptyRectangle, sampleRectangles))
  }

  test("allContainedIn should return true if the list of candidates is empty even if checking against an empty rectangle") {
    assert(allContainedIn(EmptyRectangle, ArraySeq[Rectangle]()))
  }

  test("allContainedIn should return true for the check of candiates against a rectangle spanning from -50 to +50 on both axis") {
    assert(allContainedIn(Rectangle(-50, -50, 100, 100), sampleRectangles))
  }

  test("findOverlappingRectangle with a square rectangle with langth 1 and offset (1, 1) should return the first sample rectangle") {
    val input = Rectangle(1, 1, 1, 1)
    val result = findOverlappingRectangle(input, sampleRectangles).getOrElse(fail("no rectangle found"))
    assert(result === sampleRectangles.head)
  }

  test("findOverlappingRectangle with an empty set of rectangles should not return a rectangle") {
    assert(findOverlappingRectangle(Rectangle(-100, -100, 200, 200), ArraySeq()).isEmpty)
  }

  test("findOverlappingRectangle with a rectangle that is outside of all candidates should not return a rectangle") {
    assert(findOverlappingRectangle(Rectangle(-100, -100, 1, 1), sampleRectangles).isEmpty)
  }

  test("scaleToUnitArea with an empty set should return an empty set again") {
    assert(scaleToUnitArea(ArraySeq.empty).isEmpty)
  }

  test("scaleToUnitArea should return rectangles with an area of 1") {
    val input = ArraySeq(Rectangle(0, 0, 3, 3))
    val output = scaleToUnitArea(input)
    assert(output.head.area == 1)
  }

  test("scaleToUnitArea should filter out empty rectangles") {
    val input = ArraySeq(EmptyRectangle)
    assert(scaleToUnitArea(input).isEmpty)
  }

  test("totalCircumference with the sample rectangles should return 143") {
    assert(totalCircumference(sampleRectangles) == 141.0)
  }

  test("totalCircumference with an empty set should return 0") {
    assert(totalCircumference(ArraySeq()) == 0)
  }

  test("overlap with an empty set should not return a rectangle") {
    assert(overlap(ArraySeq.empty).isEmpty)
  }

  test("overlap with a single rectangle should return that rectangle") {
    val rectangle = Rectangle(1, 1, 1, 1)
    val input = ArraySeq(rectangle)
    val output = overlap(input)
    assert(output == Some(rectangle))
  }

  test("overlap should return a rectangle that is the overlap of all rectangles") {
    val input = ArraySeq(
      Rectangle(0, 0, 10, 10),
      Rectangle(2, 2, 3, 9),
      Rectangle(1, 2, 4, 1)
    )
    val expected = Some(Rectangle(2, 2, 3, 1))
    val output = overlap(input)
    assert(output == expected)
  }

  test("subdivide should return a tiling for each rectangle") {
    val split = subdivide(sampleRectangles)
    val expected = ArraySeq(
      Rectangle(0.0, 0.0, 2.5, 2.0),
      Rectangle(2.5, 0.0, 2.5, 2.0),
      Rectangle(0.0, 2.0, 2.5, 2.0),
      Rectangle(2.5, 2.0, 2.5, 2.0),
      Rectangle(10.0, 5.0, 1.0, 1.5),
      Rectangle(11.0, 5.0, 1.0, 1.5),
      Rectangle(10.0, 6.5, 1.0, 1.5),
      Rectangle(11.0, 6.5, 1.0, 1.5),
      Rectangle(-9.5, 2.0, 2.0, 0.25),
      Rectangle(-7.5, 2.0, 2.0, 0.25),
      Rectangle(-9.5, 2.25, 2.0, 0.25),
      Rectangle(-7.5, 2.25, 2.0, 0.25),
      Rectangle(-23.8, -19.6, 8.5, 6.0),
      Rectangle(-15.3, -19.6, 8.5, 6.0),
      Rectangle(-23.8, -13.6, 8.5, 6.0),
      Rectangle(-15.3, -13.6, 8.5, 6.0),
      Rectangle(23.0, 25.0, 5.0, 6.5),
      Rectangle(28.0, 25.0, 5.0, 6.5),
      Rectangle(23.0, 31.5, 5.0, 6.5),
      Rectangle(28.0, 31.5, 5.0, 6.5)
    )
    val equal = split.zip(expected).forall((l, r) =>
      l.xmin === r.xmin &&
      l.ymin === r.ymin &&
      l.width === r.width &&
      l.height === r.height)
    assert(equal)
  }

  test("subdivide should return the tiles in a fixed order") {
    val rectangles = ArraySeq(Rectangle(-1, -1, 2, 2))
    val expected = ArraySeq(
      Rectangle(-1.0, -1.0, 1.0, 1.0),
      Rectangle(0.0, -1.0, 1.0, 1.0),
      Rectangle(-1.0, 0.0, 1.0, 1.0),
      Rectangle(0.0, 0.0, 1.0, 1.0)
    )
    val splitted = subdivide(rectangles)
    assert(splitted == expected)
  }

  test("subdivide should filter out empty rectangles") {
    val rectangles = ArraySeq(EmptyRectangle)
    val split = subdivide(rectangles)
    assert(split.isEmpty)
  }

  test("subdivide with an empty input should return an empty output") {
    assert(subdivide(ArraySeq()).isEmpty)
  }

  test("allIntersections with an empty rectangle and the sample rectangles should return an empty set again") {
    val emptyRectangle = ArraySeq(EmptyRectangle)
    assert(allIntersections(emptyRectangle, sampleRectangles).isEmpty)
  }

  test("allIntersections with the sample rectangles and an empty one set should return an empty set again") {
    val emptyRectangle = ArraySeq(EmptyRectangle)
    assert(allIntersections(sampleRectangles, emptyRectangle).isEmpty)
  }

  test("allIntersections with a rectangle that contains all sample rectangles and the sample rectangles should return the sample rectangles again") {
    val intersections = allIntersections(ArraySeq(Rectangle(-50, -50, 100, 100)), sampleRectangles)
    assert(intersections == sampleRectangles)
  }

  test("allIntersections should correctly identify overlaps with Rectangle(1, 1, 10, 5)") {
    val intersections = allIntersections(ArraySeq(Rectangle(1, 1, 10, 5)), sampleRectangles)
    val expected = ArraySeq(
      Rectangle(1.0, 1.0, 4.0, 3.0),
      Rectangle(10.0, 5.0, 1.0, 1.0)
    )
    assert(intersections == expected)
  }

  test("allIntersections should correctly work with multiple targets and candidates") {
    val targets = ArraySeq(
      Rectangle(1, 1, 10, 5),
      Rectangle(-50, -50, 100, 100)
    )
    val expected = ArraySeq(
      Rectangle(1.0, 1.0, 4.0, 3.0),
      Rectangle(10.0, 5.0, 1.0, 1.0)
    ) ++ sampleRectangles
    val intersections = allIntersections(targets, sampleRectangles)
    assert(intersections == expected)
  }

  test("smallestAreaRectangle with the sample rectangles should return the first rectangle with the smallest area") {
    assert(smallestAreaRectangle(sampleRectangles).getOrElse(fail("expected a result")) === sampleRectangles(2))
  }

  test("smallestAreaRectangle with an empty set should not return a rectangle") {
    assert(smallestAreaRectangle(ArraySeq()).isEmpty)
  }

  test("smallestScaleFactorToContain should return the smallest scala factor sufficient to make the first rectangle contain the second") {
    val scaleFactors = ArraySeq(0.0, 1.0, 2.5, 3.0, 4.0, 6.5)
    val inner = Rectangle(0, 0, 7, 7)
    val outer = Rectangle(0, 0, 2, 2)
    assert(smallestScaleFactorToContain(outer, inner, scaleFactors) == Some(4.0))
  }

  test("smallestScaleFactorToContain should return None when the input is empty") {
    val inner = Rectangle(0, 0, 1, 1)
    val outer = Rectangle(0, 0, 1, 1)
    val result = smallestScaleFactorToContain(outer, inner, ArraySeq())
    val expected = None
    assert(result == expected)
  }

  test("smallestScaleFactorToContain should return None when no scale factor satisfies the condition") {
    val inner = Rectangle(0, 0, 10, 10)
    val outer = Rectangle(0, 0, 1, 1)
    val result = smallestScaleFactorToContain(outer, inner, ArraySeq(0.5, 1, 2))
    val expected = None
    assert(result == expected)
  }

  test("smallestScaleFactorToContain should return the smallest scale factor that satisfies the condition") {
    val inner = Rectangle(0, 0, 10, 10)
    val outer = Rectangle(0, 0, 1, 1)
    val result = smallestScaleFactorToContain(outer, inner, ArraySeq(100, 20, 1, 200, 10))
    val expected = Some(10)
    assert(result == expected)
  }

  test("boundingBox should return an empty rectangle when the input is empty") {
    val result = boundingBox(ArraySeq())
    assert(result.area == 0)
  }

  test("boundingBox should return the correct bounding box for disjoint rectangles") {
    val result = boundingBox(ArraySeq(
      Rectangle(0, 0, 1, 1),
      Rectangle(10, 5, 1, 1),
    ))
    val expected = Rectangle(0, 0, 11, 6)
    assert(result === expected)
  }

  test("boundingBox should return the correct bounding box for overlapping rectangles") {
    val result = boundingBox(ArraySeq(
      Rectangle(0, 0, 10, 5),
      Rectangle(1, 1, 10, 5),
    ))
    val expected = Rectangle(0, 0, 11, 6)
    assert(result === expected)
  }

  test("boundingBox should return the correct bounding box for more than two rectangles") {
    val result = boundingBox(sampleRectangles)
    val expected = Rectangle(-23.8, -19.6, 56.8, 57.6)
    assert(result === expected)
  }

  test("findSmallestOverlappingRectangle should return None if the input is empty") {
    val target = Rectangle(10, 20, 1, 2)
    val result = findSmallestOverlappingRectangle(target, ArraySeq())
    assert(result == None)
  }

  test("findSmallestOverlappingRectangle should return None if no rectangle overlaps the target") {
    val target = Rectangle(10, 20, 1, 2)
    val result = findSmallestOverlappingRectangle(target, ArraySeq(
      Rectangle(0, 0, 1, 1),
      Rectangle(12, 23, 1, 1),
      Rectangle(10, 0, 1, 1),
    ))
    assert(result == None)
  }

  test("findSmallestOverlappingRectangle should return the smallest overlapping rectangle") {
    val target = Rectangle(10, 20, 1, 2)
    val candidates = ArraySeq(
      Rectangle(0, 0, 1, 1),
      Rectangle(10, 20, 2, 2),
      Rectangle(5, 5, 20, 20),
      Rectangle(0, 0, 0, 0)
    )
    val result = findSmallestOverlappingRectangle(target, candidates)
      .getOrElse(fail("expected a rectangle"))
    val expected = candidates(1)
    assert(result === expected)
  }

  test("largestAreaWithCircumferenceAtMost should return None if the input is empty") {
    val result = largestAreaWithCircumferenceAtMost(5, ArraySeq())
    assert(result == None)
  }

  test("largestAreaWithCircumferenceAtMost should return None if all rectangles exceed the circumference limit") {
    val result = largestAreaWithCircumferenceAtMost(5, sampleRectangles)
    assert(result == None)
  }

  test("largestAreaWithCircumferenceAtMost accepts an empty rectangle") {
    val result = largestAreaWithCircumferenceAtMost(5, ArraySeq(EmptyRectangle))
      .getOrElse(fail("expected a result, got None"))
    assert(result == 0)
  }

  test("largestAreaWithCircumferenceAtMost yields the correct result for limit 46") {
    val result = largestAreaWithCircumferenceAtMost(46, sampleRectangles)
      .getOrElse(fail("expected a result, got None"))
    assert(result == 130)
  }

  test("largestAreaWithCircumferenceAtMost yields the correct result for limit 10") {
    val result = largestAreaWithCircumferenceAtMost(10, sampleRectangles)
      .getOrElse(fail("expected a result, got None"))
    assert(result == 6)
  }
}
