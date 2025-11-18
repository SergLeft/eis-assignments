package geometry

/** Represents an axis-aligned rectangle in two-dimensional space.
 *
 *  @constructor creates a [[Rectangle]] from an origin point and a non-negative extent along both axes.
 *  @param xmin the lower bound along the x (horizontal) axis
 *  @param ymin the lower bound along the y (vertical) axis
 *  @param width the extent along the x axis
 *  @param height the extent along the y axis
 *  @throws IllegalArgumentException if width or height are negative
 */
class Rectangle(
  val xmin: Double,
  val ymin: Double,
  val width: Double,
  val height: Double
) {
  if (width < 0) throw IllegalArgumentException("Rectangle has negative width")
  if (height < 0) throw IllegalArgumentException("Rectangle has negative height")

  /** Returns the upper bound of the rectangle in the horizontal direction. */
  def xmax: Double = xmin + width

  /** Returns the upper bound of the rectangle in the vertical direction. */
  def ymax: Double = ymin + height

  /** Returns the area of the rectangle. */
  def area: Double = width * height

  /** Returns the circumference of the rectangle. */
  def circumference: Double = 2 * (width + height)

  /** Returns true if this rectangle is also a square, false otherwise. */
  def isSquare: Boolean = width == height

  /** Returns true if this [[Rectangle]] intersects the other [[Rectangle]].
   *
   *  @param that the [[Rectangle]] to be checked for intersection
   */
  def intersects(that: Rectangle): Boolean = {
    val xSeparation = xmax < that.xmin || that.xmax < xmin
    val ySeparation = ymax < that.ymin || that.ymax < ymin
    !xSeparation && !ySeparation
  }

  /** Returns true if this [[Rectangle]] fully contains the other [[Rectangle]].
   *
   *  @param that the [[Rectangle]] to be checked if it is contained in this [[Rectangle]]
   */
  def contains(that: Rectangle): Boolean = {
    xmin <= that.xmin && ymin <= that.ymin && xmax >= that.xmax && ymax >= that.ymax
  }

  /** Compute the intersection of two rectangles.
   *
   *  @param that the [[Rectangle]] to compute the intersection with
   *  @return the largest [[Rectangle]] contained in both this and that. If no such rectangle exists, returns an empty [[Rectangle]]
   */
  def overlap(that: Rectangle): Rectangle = {
    if (intersects(that)) {
      makeRectangleFromBounds(
        xmin max that.xmin,
        xmax min that.xmax,
        ymin max that.ymin,
        ymax min that.ymax
      )
    } else {
      EmptyRectangle
    }
  }

  /** Returns the bounding box of this and that, i.e. the smallest [[Rectangle]] containing both this and that.
   *
   *  @param that the [[Rectangle]] to compute the bounding box with
   */
  def boundingBox(that: Rectangle): Rectangle = {
    makeRectangleFromBounds(
      xmin min that.xmin,
      xmax max that.xmax,
      ymin min that.ymin,
      ymax max that.ymax
    )
  }

  /** Returns a new [[Rectangle]] with its offset moved by the specified deltas.
   *
   *  @param xDelta the amount to move the rectangle along the x (horizontal) axis
   *  @param yDelta the amount to move the rectangle along the y (vertical) axis
   */
  def offset(xDelta: Double, yDelta: Double): Rectangle = {
    Rectangle(
      xmin + xDelta,
      ymin + yDelta,
      width,
      height
    )
  }

  /** Returns a new [[Rectangle]] scaled by the specified non-negative factor.
   *
   *  @param scaleFactor the factor by which to scale the rectangle's width and height
   *  @throws IllegalArgumentException if the scale factor is negative
   */
  def scale(scaleFactor: Double): Rectangle = {
    if (scaleFactor < 0) throw IllegalArgumentException("negative scale factor")
    Rectangle(
      xmin,
      ymin,
      width * scaleFactor,
      height * scaleFactor
    )
  }

  override def equals(other: Any): Boolean = other match {
    case rectangle: Rectangle =>
      xmin == rectangle.xmin &&
      ymin == rectangle.ymin &&
      width == rectangle.width &&
      height == rectangle.height
    case _ => false  
  }

  override def hashCode: Int = (xmin, ymin, width, height).hashCode

  override def toString: String = 
    s"Rectangle(xmin=$xmin, ymin=$ymin, width=$width, height=$height)"
}


/** Creates a [[Rectangle]] from lower and upper bounds along each axis.
 *
 *  @param xmin the lower bound along the x (horizontal) axis
 *  @param xmax the upper bound along the x (horizontal) axis
 *  @param ymin the lower bound along the y (vertical) axis
 *  @param ymax the upper bound along the y (vertical) axis
 *  @return the constructed [[Rectangle]]
 */
def makeRectangleFromBounds(xmin: Double, xmax: Double, ymin: Double, ymax: Double): Rectangle = {
  Rectangle(xmin, ymin, xmax - xmin, ymax - ymin)
}


/** Constant representing an empty rectangle. */
val EmptyRectangle = Rectangle(0, 0, 0, 0)


