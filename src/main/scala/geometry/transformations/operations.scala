package geometry.transformations

import scala.collection.immutable.ArraySeq

import geometry.*

/** Scale all [[Rectangle]]s by the same amount.
 *
 *  @param scaleFactor the factor to scale each [[Rectangle]] by
 *  @param rectangles the input [[Rectangle]]s
 *  @return the scaled [[Rectangle]]s in the order of input
 */
def scaleAll(scaleFactor: Double, rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = {
  rectangles.map((r: Rectangle) => r.scale(scaleFactor))
}



/** Center all [[Rectangle]]s around the origin of the coordinate system.
 *
 *  @param rectangles the input [[Rectangle]]s
 *  @return the centered [[Rectangle]]s
 */
def centerAroundOrigin(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = {
  rectangles.map((r: Rectangle) => r.offset(-r.xmin - r.width / 2, -r.ymin -r.height /2 ))
}


/** Returns all [[Rectangle]]s whose area is between the lower bound and the upper bound.
 *
 *  @param lowerBound the lower area bound
 *  @param upperBound the upper area bound
 *  @param rectangles the input [[Rectangle]]s
 */
def filterAreaBetween(lowerBound: Double, upperBound: Double, rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = {
  rectangles.filter((r: Rectangle) => r.area >= lowerBound && r.area <= upperBound)
}


/** Returns all non-empty [[Rectangle]]s, i.e. all [[Rectangle]]s with a positive area.
 *
 *  @param rectangles the input [[Rectangle]]s
 */
def removeEmpty(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = {
  rectangles.filter((r:Rectangle) => r.area > 0)
}


/** Returns true if the input [[Rectangle]]s contain at least one square, false otherwise.
 *
 *  @param rectangles the input [[Rectangle]]s
 */
def hasSquare(rectangles: ArraySeq[Rectangle]): Boolean = {
  rectangles.exists((r:Rectangle) => r.isSquare)
}


/** Returns true if all [[Rectangle]]s are contained in a given [[Rectangle]], false otherwise.
 *
 *  @param target the [[Rectangle]] potentially containing all other [[Rectangle]]s
 *  @param candidates the [[Rectangle]]s to be checked if they are contained in `target`
 */
def allContainedIn(target: Rectangle, candidates: ArraySeq[Rectangle]): Boolean = {
  candidates.forall((r: Rectangle) => target.contains(r))
}


/** Finds any rectangle from a list that intersects with a given [[Rectangle]].
 *
 *  @param target the [[Rectangle]] to find an intersecting [[Rectangle]] for
 *  @param candidates the [[Rectangle]]s to be checked if they intersect with `target`
 *  @return a [[Rectangle]] from `candidates` intersecting with `target` if one exists, `None` otherwise
 */
def findOverlappingRectangle(target: Rectangle, candidates: ArraySeq[Rectangle]): Option[Rectangle] = {
  candidates.find((r: Rectangle) => r.intersects(target))
}


/** Scales a [[Rectangle]] sequence to area 1, preserving their aspect ratio, discarding empty [[Rectangle]]s.
 *
 *  @param rectangles the [[Rectangle]]s to be scaled
 *  @return the scaled [[Rectangle]]s, excluding empty ones
 */
def scaleToUnitArea(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = {
  removeEmpty(rectangles).map((r: Rectangle) => r.scale(1 / math.sqrt(r.area)))
}


/** Returns the total circumference of a list of [[Rectangle]]s.
 *
 *  @param rectangles the input [[Rectangle]]s
 */
def totalCircumference(rectangles: ArraySeq[Rectangle]): Double = {
  rectangles.map((r: Rectangle) => r.circumference).sum
}


/** Finds the overlap of a sequence of [[Rectangle]]s, i.e. the area of the largest [[Rectangle]] that is contained in all [[Rectangle]]s.
 *
 *  @param rectangles the input [[Rectangle]]s
 *  @return the area of the intersection. If `rectangles` is empty, returns `None`
 */
def overlap(rectangles: ArraySeq[Rectangle]): Option[Rectangle] = {
  rectangles.reduceOption(_.overlap(_))
}


/** Splits all non-empty [[Rectangle]]s into four tiles of half width and half height.
 *
 *  @param rectangles the input [[Rectangle]]s
 *  @return a tiling for each non-empty [[Rectangle]]
 */
def subdivide(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = {
  rectangles.flatMap { (r: Rectangle) =>
    if (r.area > 0) {
      val w2 = r.width / 2
      val h2 = r.height / 2
      ArraySeq(
        Rectangle(r.xmin, r.ymin, w2, h2),
        Rectangle(r.xmin + w2, r.ymin, w2, h2),
        Rectangle(r.xmin, r.ymin + h2, w2, h2),
        Rectangle(r.xmin + w2, r.ymin + h2, w2, h2)
      )
    } else {
      ArraySeq.empty
    }
  }
}

/** Returns the non-empty intersections of all pairs of rectangles from two sequences of [[Rectangle]]s.
 *
 *  @param targets the first sequence of [[Rectangle]]s
 *  @param candidates the second sequence of [[Rectangle]]s
 */
def allIntersections(targets: ArraySeq[Rectangle], candidates: ArraySeq[Rectangle]): ArraySeq[Rectangle] = {
  targets.flatMap((t: Rectangle) => candidates.map((c: Rectangle) => t.overlap(c)).filter(_.area > 0))
}


/** Finds the [[Rectangle]] with the smallest area from a sequence of [[Rectangle]]s.
 *
 *  @param rectangles the input [[Rectangle]]s
 *  @return the smallest [[Rectangle]] if the input sequence is non-empty, `None` otherwise
 */
def smallestAreaRectangle(rectangles: ArraySeq[Rectangle]): Option[Rectangle] = {
  rectangles.minByOption(_.area)
}


/** Finds the smallest scale factor from a sequence such that one rectangle contains another rectangle after scaling.
 *
 *  @param outer the rectangle to be scaled
 *  @param inner the rectangle to be contained in `outer`
 *  @param scaleFactors the potential scale factors
 *  @return the smallest scale factor in `scaleFactors` so that `outer`, when scaled, contains `inner`. If no such scale factor exists, returns `None`.
 */
def smallestScaleFactorToContain(outer: Rectangle, inner: Rectangle, scaleFactors: ArraySeq[Double]): Option[Double] = {
  scaleFactors.filter((scaleFactor: Double) => outer.scale(scaleFactor)
    .contains(inner))
    .minOption
}


/** Finds the bounding box of a sequence of [[Rectangle]]s, i.e. the smallest [[Rectangle]] that contains all [[Rectangle]]s.
 *
 *  @param rectangles the input [[Rectangle]]s
 *  @return the bounding box of all [[Rectangle]]s. If `rectangles` is empty, returns an empty [[Rectangle]].
 */
def boundingBox(rectangles: ArraySeq[Rectangle]): Rectangle = {
  rectangles.reduceOption(_.boundingBox(_)).getOrElse(EmptyRectangle)
}


/** Finds the [[Rectangle]] with the smallest area that overlaps with a given [[Rectangle]].
 *
 *  @param target the [[Rectangle]] to be overlapped
 *  @param candidates the input [[Rectangle]]s
 *  @return the smallest [[Rectangle]] from `candidates` that overlaps with `target`. If no such [[Rectangle]] exists, returns `None`.
 */
def findSmallestOverlappingRectangle(target: Rectangle, candidates: ArraySeq[Rectangle]): Option[Rectangle] = {
  candidates.filter(_.intersects(target)).minByOption(_.area)
}


/** Finds the area of the largest [[Rectangle]] whose circumference does not exceed a given limit.
 *
 *  @param maxCircumference the circumference limit
 *  @param rectangles the input [[Rectangle]]s
 *  @return the area of the largest [[Rectangle]] within the circumference limit. If `rectangles` is empty, returns `None`
 */
def largestAreaWithCircumferenceAtMost(maxCircumference: Double, rectangles: ArraySeq[Rectangle]): Option[Double] = {
  rectangles
    .filter(_.circumference <= maxCircumference)
    .map(_.area)
    .maxOption
}
