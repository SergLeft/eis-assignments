package basics


// put your scala code here
class Coordinate(val x: Double, val y: Double) {

  /** Calculates the distance to another Coordinate
   *
   * @param other Coordinate to calculate the Distance to
   * @return distance between two coordinates
   */
  def distanceTo(other: Coordinate): Double = {
    val xDiff = x - other.x
    val yDiff = y - other.y
    Math.hypot(xDiff, yDiff)
  }

  /** Returns the Coordinate as String
   *
   * @return String of the Coordinate
   */
  def asString: String = {
    s"($x, $y)"
  }
}
  
/** Generates a Coordinate at the origin (0.0, 0.0)
 *
 * @return Coordinate with x = y = 0.0
 */
def origin: Coordinate = {
  new Coordinate(0.0, 0.0)
}

/** Returns the Coordinate that is closest to the Reference Coordinate
 *
 * @param reference Coordinate to calculate the distance to
 * @param option1   Coordinate to calculate the distance to reference and compare with option2
 * @param option2   Coordinate to calculate the distance to reference and compare with option1
 * @return Coordinate closest to reference
 */
def findNearestPoint(reference: Coordinate, option1: Coordinate, option2: Coordinate): Coordinate = {
  if (reference.distanceTo(option1) < reference.distanceTo(option2)) {
    option1
  } else {
    option2
  }
}



