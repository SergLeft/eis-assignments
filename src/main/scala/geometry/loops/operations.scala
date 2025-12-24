package geometry.loops

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

import geometry.*


/** Scale all [[Rectangle]]s by the same amount.
 *
 *  @param scaleFactor the factor to scale each [[Rectangle]] by
 *  @param rectangles the input [[Rectangle]]s
 *  @return the scaled [[Rectangle]]s in the order of input
 */
def scaleAll(scaleFactor: Double, rectangles: ArraySeq[Rectangle]): ArrayBuffer[Rectangle] = {
  val result = ArrayBuffer[Rectangle]()
  var i = 0
  while (i < rectangles.size) {
    result.append(rectangles(i).scale(scaleFactor))
    i += 1
  }
  result
}


/** Center all [[Rectangle]]s around the origin of the coordinate system.
 *
 *  @param rectangles the input [[Rectangle]]s
 *  @return the centered [[Rectangle]]s
 */
def centerAroundOrigin(rectangles: ArraySeq[Rectangle]): ArrayBuffer[Rectangle] = {
  val result = ArrayBuffer[Rectangle]()
  var i = 0
  while (i < rectangles.size) {
    val r = rectangles(i)
    result.append(r.offset(- r.xmin - r.width / 2, - r.ymin - r.height / 2))
    i += 1
  }
  result
}


/** Returns all [[Rectangle]]s whose area is between the lower bound and the upper bound.
 *
 *  @param lowerBound the lower area bound
 *  @param upperBound the upper area bound
 *  @param rectangles the input [[Rectangle]]s
 */
def filterAreaBetween(lowerBound: Double, upperBound: Double, rectangles: ArraySeq[Rectangle]): ArrayBuffer[Rectangle] = {
  val result = ArrayBuffer[Rectangle]()
  var i = 0
  while (i < rectangles.size) {
    val r = rectangles(i)
    if (r.area >= lowerBound && r.area <= upperBound) {
      result.append(r)
    }
    i += 1
  }
  result
}


/** Returns all non-empty [[Rectangle]]s, i.e. all [[Rectangle]]s with a positive area.
 *
 *  @param rectangles the input [[Rectangle]]s
 */
def removeEmpty(rectangles: ArraySeq[Rectangle]): ArrayBuffer[Rectangle] = {
  val result = ArrayBuffer[Rectangle]()
  var i = 0
  while (i < rectangles.size) {
    val r = rectangles(i)
    if (r.area > 0) {
      result.append(r)
    }
    i += 1
  }
  result
}


/** Returns true if the input [[Rectangle]]s contain at least one square, false otherwise.
 *
 *  @param rectangles the input [[Rectangle]]s
 */
def hasSquare(rectangles: ArraySeq[Rectangle]): Boolean = {
  var hasSquare = false
  var i = 0
  while (i < rectangles.size) {
    if (rectangles(i).isSquare) {
      hasSquare = true
    }
    i += 1
  }
  hasSquare
}


/** Returns true if all [[Rectangle]]s are contained in a given [[Rectangle]], false otherwise.
 *
 *  @param target the [[Rectangle]] potentially containing all other [[Rectangle]]s
 *  @param candidates the [[Rectangle]]s to be checked if they are contained in `target`
 */
def allContainedIn(target: Rectangle, candidates: ArraySeq[Rectangle]): Boolean = {
  var allContained = true
  var i = 0
  while (i < candidates.size) {
    if (!target.contains(candidates(i))) {
      allContained = false
    }
    i += 1
  }
  allContained
}


/** Finds any rectangle from a list that intersects with a given [[Rectangle]].
 *
 *  @param target the [[Rectangle]] to find an intersecting [[Rectangle]] for
 *  @param candidates the [[Rectangle]]s to be checked if they intersect with `target`
 *  @return a [[Rectangle]] from `candidates` intersecting with `target` if one exists, `None` otherwise
 */
def findOverlappingRectangle(target: Rectangle, candidates: ArraySeq[Rectangle]): Option[Rectangle] = {
  var result: Option[Rectangle] = None
  var i = 0
  while (i < candidates.size) {
    val r = candidates(i)
    if (r.intersects(target)) {
      result = Some(r)
    }
    i += 1
  }
  result
}


/** Scales a [[Rectangle]] sequence to area 1, preserving their aspect ratio, discarding empty [[Rectangle]]s.
 *
 *  @param rectangles the [[Rectangle]]s to be scaled
 *  @return the scaled [[Rectangle]]s, excluding empty ones
 */
def scaleToUnitArea(rectangles: ArraySeq[Rectangle]): ArrayBuffer[Rectangle] = {
  val result = ArrayBuffer[Rectangle]()
  var i = 0
  while (i < rectangles.size) {
    val r = rectangles(i)
    if (r.area > 0) {
      result.append(r.scale(1 / math.sqrt(r.area)))
    }
    i += 1
  }
  result
}


/** Returns the total circumference of a list of [[Rectangle]]s.
 *
 *  @param rectangles the input [[Rectangle]]s
 */
def totalCircumference(rectangles: ArraySeq[Rectangle]): Double = {
  var result: Double = 0
  var i = 0
  while (i < rectangles.size) {
    result += rectangles(i).circumference
    i += 1
  }
  result
}


/** Finds the overlap of a sequence of [[Rectangle]]s, i.e. the area of the largest [[Rectangle]] that is contained in all [[Rectangle]]s.
 *
 *  @param rectangles the input [[Rectangle]]s
 *  @return the area of the intersection. If `rectangles` is empty, returns `None`
 */
def overlap(rectangles: ArraySeq[Rectangle]): Option[Rectangle] = {
  if (rectangles.isEmpty) {
    None
  } else {
    var result = rectangles(0)
    var i = 1
    while (i < rectangles.size) {
      result = result.overlap(rectangles(i))
      i += 1
    }
    Some(result)
  }
}


/** Splits all non-empty [[Rectangle]]s into four tiles of half width and half height.
 *
 *  @param rectangles the input [[Rectangle]]s
 *  @return a tiling for each non-empty [[Rectangle]]
 */
def subdivide(rectangles: ArraySeq[Rectangle]): ArrayBuffer[Rectangle] = {
  val result = ArrayBuffer[Rectangle]()
  var i = 0
  while (i < rectangles.size) {
    val r = rectangles(i)
    val w2 = r.width / 2
    val h2 = r.height / 2
    if (r.area > 0) {
      result.append(Rectangle(r.xmin, r.ymin, w2, h2))
      result.append(Rectangle(r.xmin + w2, r.ymin, w2, h2))
      result.append(Rectangle(r.xmin, r.ymin + h2, w2, h2))
      result.append(Rectangle(r.xmin + w2, r.ymin + h2, w2, h2))
    }
    i += 1
  }
  result
}


/** Returns the non-empty intersections of all pairs of rectangles from two sequences of [[Rectangle]]s.
 *
 *  @param targets the first sequence of [[Rectangle]]s
 *  @param candidates the second sequence of [[Rectangle]]s
 */
def allIntersections(targets: ArraySeq[Rectangle], candidates: ArraySeq[Rectangle]): ArrayBuffer[Rectangle] = {
  val result = ArrayBuffer[Rectangle]()
  var i = 0
  while (i < targets.size) {
    var j = 0
    while (j < candidates.size) {
      val overlap = targets(i).overlap(candidates(j))
      if (overlap.area > 0) {
        result.append(overlap)
      }
      j += 1
    }
    i += 1
  }
  result
}


/** Finds the [[Rectangle]] with the smallest area from a sequence of [[Rectangle]]s.
 *
 *  @param rectangles the input [[Rectangle]]s
 *  @return the smallest [[Rectangle]] if the input sequence is non-empty, `None` otherwise
 */
def smallestAreaRectangle(rectangles: ArraySeq[Rectangle]): Option[Rectangle] = {
  if (rectangles.isEmpty) {
    None
  } else {
    var result = rectangles(0)
    var i = 1
    while (i < rectangles.size) {
      if (result.area > rectangles(i).area) {
        result = rectangles(i)
      }
      i += 1
    }
    Some(result)
  }
}


/** Finds the smallest scale factor from a sequence such that one rectangle contains another rectangle after scaling.
 *
 *  @param outer the rectangle to be scaled
 *  @param inner the rectangle to be contained in `outer`
 *  @param scaleFactors the potential scale factors
 *  @return the smallest scale factor in `scaleFactors` so that `outer`, when scaled, contains `inner`. If no such scale factor exists, returns `None`.
 */
def smallestScaleFactorToContain(outer: Rectangle, inner: Rectangle, scaleFactors: ArraySeq[Double]): Option[Double] = {
  var result: Double = -1
  var i = 0
  while (i < scaleFactors.size) {
    val scaleFactor = scaleFactors(i)
    if (outer.scale(scaleFactor).contains(inner)) {
      result = scaleFactor
    }
    i += 1
  }
  i = 0
  while (i < scaleFactors.size) {
    val scaleFactor = scaleFactors(i)
    if (outer.scale(scaleFactor).contains(inner) && scaleFactor < result) {
      result = scaleFactor
    }
    i += 1
  }
  if (result != -1) {
    Some(result)
  } else {
    None
  }
}


/** Finds the bounding box of a sequence of [[Rectangle]]s, i.e. the smallest [[Rectangle]] that contains all [[Rectangle]]s.
 *
 *  @param rectangles the input [[Rectangle]]s
 *  @return the bounding box of all [[Rectangle]]s. If `rectangles` is empty, returns an empty [[Rectangle]].
 */
def boundingBox(rectangles: ArraySeq[Rectangle]): Rectangle = {
  if (rectangles.isEmpty) {
    EmptyRectangle
  } else {
    var result = rectangles(0)
    var i = 1
    while (i < rectangles.size) {
      result = result.boundingBox(rectangles(i))
      i += 1
    }
    result
  }
}


/** Finds the [[Rectangle]] with the smallest area that overlaps with a given [[Rectangle]].
 *
 *  @param target the [[Rectangle]] to be overlapped
 *  @param candidates the input [[Rectangle]]s
 *  @return the smallest [[Rectangle]] from `candidates` that overlaps with `target`. If no such [[Rectangle]] exists, returns `None`.
 */
def findSmallestOverlappingRectangle(target: Rectangle, candidates: ArraySeq[Rectangle]): Option[Rectangle] = {
  val temp = ArrayBuffer[Rectangle]()
  var i = 0
  while (i < candidates.size) {
    val r = candidates(i)
    if (r.intersects(target)) {
      temp.append(r)
    }
    i += 1
  }
  if (temp.isEmpty) {
    None
  } else {
    i = 1
    var result = temp(0)
    while (i < temp.size) {
      if (temp(i).area < result.area) {
        result = temp(i)
      }
      i += 1
    }
    Some(result)
  }
}


/** Finds the area of the largest [[Rectangle]] whose circumference does not exceed a given limit.
 *
 *  @param maxCircumference the circumference limit
 *  @param rectangles the input [[Rectangle]]s
 *  @return the area of the largest [[Rectangle]] within the circumference limit. If `rectangles` is empty, returns `None`
 */
def largestAreaWithCircumferenceAtMost(maxCircumference: Double, rectangles: ArraySeq[Rectangle]): Option[Double] = {
  val temp = ArrayBuffer[Double]()
  var i = 0
  while (i < rectangles.size) {
    val r = rectangles(i)
    if (r.circumference <= maxCircumference) {
      temp.append(r.area)
    }
    i += 1
  }
  if (temp.isEmpty) {
    None
  } else {
    i = 1
    var result = temp(0)
    while (i < temp.size) {
      if (temp(i) > result) {
        result = temp(i)
      }
      i += 1
    }
    Some(result)
  }
}
