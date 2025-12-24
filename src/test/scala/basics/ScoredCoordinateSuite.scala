package basics

import org.scalatest.funsuite.AnyFunSuite


class ScoredCoordinateSuite extends AnyFunSuite {

  def coordinatesEqual(op1: Coordinate, op2: Coordinate): Boolean = op1.x == op2.x && op1.y == op2.y

  test("(34,87) should return the correct x and y values") {
    val coordinate = Coordinate(34.0, 87.0)
    assert(coordinate.x == 34.0)
    assert(coordinate.y == 87.0)
  }

  test("(-95,23) should return the correct x and y values") {
    val coordinate = Coordinate(-95.0, 23.0)
    assert(coordinate.x == -95.0)
    assert(coordinate.y == 23.0)
  }

  test("The origin should be at (0,0)") {
    val origin = basics.origin
    assert(origin.x == 0.0)
    assert(origin.y == 0.0)
  }

  test("The string representation of the origin should be correct") {
    assert(origin.asString == "(0.0, 0.0)")
  }

  test("The string representation of (4.8,315.5) should be correct") {
    val coordinate = Coordinate(4.8, 315.5)
    assert(coordinate.asString == "(4.8, 315.5)")
  }

  test("The string representation of (-32.2,-243.6) should be correct") {
    val coordinate = Coordinate(-32.2, -243.6)
    assert(coordinate.asString == "(-32.2, -243.6)")
  }

  test("The distance from (64,-89) to itself should be 0") {
    val coordinate = Coordinate(64.0, -89.0)
    assert(coordinate.distanceTo(coordinate) == 0.0)
  }

  test("The distance from (-7936.8,979.6) to itself should be 0") {
    val coordinate = Coordinate(-7936.8, 979.6)
    assert(coordinate.distanceTo(coordinate) == 0.0)
  }

  test("The distance between (2,5) and (5,9) should be 5") {
    val coordinate1 = Coordinate(2.0, 5.0)
    val coordinate2 = Coordinate(5.0, 9.0)
    assert(coordinate1.distanceTo(coordinate2) == 5.0)
  }

  test("The distance between (-4,10) and (-9,-2) should be 13") {
    val coordinate1 = Coordinate(-4.0, 10.0)
    val coordinate2 = Coordinate(-9.0, -2.0)
    assert(coordinate1.distanceTo(coordinate2) == 13.0)
  }

  test("The distance between (3,6) and (4,12) should be symmetric") {
    val coordinate1 = Coordinate(3.0, 6.0)
    val coordinate2 = Coordinate(4.0, 12.0)
    assert(coordinate1.distanceTo(coordinate2) == coordinate2.distanceTo(coordinate1))
  }  

  test("The distance between (43,-6) and (-65,97) should be symmetric") {
    val coordinate1 = Coordinate(43.0, -6.0)
    val coordinate2 = Coordinate(-65.0, 97.0)
    assert(coordinate1.distanceTo(coordinate2) == coordinate2.distanceTo(coordinate1))
  }

  test("Of (34,48) and (-32,127) the former should be closest to (21,54)") {
    val reference = Coordinate(21.0, 54.0)
    val nearer = Coordinate(34.0, 48.0)
    val further = Coordinate(-32.0, 127.0)
    val result = findNearestPoint(reference, nearer, further)
    assert(coordinatesEqual(result, nearer))
  }

  test("Of (46.8,1.6) and (0.25,-67.4) the latter should be closest to (-89.2,34)") {
    val reference = Coordinate(-89.2, 34.0)
    val nearer = Coordinate(0.25, -67.4)
    val further = Coordinate(46.8, 1.6)
    val result = findNearestPoint(reference, nearer, further)
    assert(coordinatesEqual(result, nearer))
  }

  test("(21,54) should be the closest point to itself") {
    val reference = Coordinate(21.0, 54.0)
    val other = Coordinate(20.0, 54.0)
    val result = findNearestPoint(reference, other, reference)
    assert(coordinatesEqual(result, reference))
  }

  test("Of (10,16) and (-10,16) the closest point to (0,16) should be either one") {
    val reference = Coordinate(0, 16.0)
    val coordinate1 = Coordinate(10.0, 16.0)
    val coordinate2 = Coordinate(-10.0, 16.0)
    val result = findNearestPoint(reference, coordinate1, coordinate2)
    assert(coordinatesEqual(result, coordinate1) || coordinatesEqual(result, coordinate2))
  }

  test("Of (-12,9) and (22,-4) the closest point to (5,2.5) should be either one") {
    val reference = Coordinate(0, 16.0)
    val coordinate1 = Coordinate(10.0, 16.0)
    val coordinate2 = Coordinate(-10.0, 16.0)
    val result = findNearestPoint(reference, coordinate1, coordinate2)
    assert(coordinatesEqual(result, coordinate1) || coordinatesEqual(result, coordinate2))
  }

  test("The x coordinate of Coordinate should be immutable") {
    val coordinate = Coordinate(-54.678, 315.8)
    assertDoesNotCompile("coordinate.x = 69.1")
  }

  test("The y coordinate of Coordinate should be immutable") {
    val coordinate = Coordinate(-54.678, 315.8)
    assertDoesNotCompile("coordinate.y = -231.3")
  }
}

