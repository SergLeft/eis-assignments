package geometry.transformations

import scala.collection.immutable.ArraySeq

import geometry.*


def scaleAll(scaleFactor: Double, rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = ???


def centerAroundOrigin(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = ???


def filterAreaBetween(lowerBound: Double, upperBound: Double, rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = ???


def removeEmpty(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = ???


def hasSquare(rectangles: ArraySeq[Rectangle]): Boolean = ???


def allContainedIn(target: Rectangle, candidates: ArraySeq[Rectangle]): Boolean = ???


def findOverlappingRectangle(target: Rectangle, candidates: ArraySeq[Rectangle]): Option[Rectangle] = ???


def scaleToUnitArea(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = ???


def totalCircumference(rectangles: ArraySeq[Rectangle]): Double = ???


def overlap(rectangles: ArraySeq[Rectangle]): Option[Rectangle] = ???


def subdivide(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] = ???


def allIntersections(targets: ArraySeq[Rectangle], candidates: ArraySeq[Rectangle]): ArraySeq[Rectangle] = ???


def smallestAreaRectangle(rectangles: ArraySeq[Rectangle]): Option[Rectangle] = ???


def smallestScaleFactorToContain(outer: Rectangle, inner: Rectangle, scaleFactors: ArraySeq[Double]): Option[Double] = ???


def boundingBox(rectangles: ArraySeq[Rectangle]): Rectangle = ???


def findSmallestOverlappingRectangle(target: Rectangle, candidates: ArraySeq[Rectangle]): Option[Rectangle] = ???


def largestAreaWithCircumferenceAtMost(maxCircumference: Double, rectangles: ArraySeq[Rectangle]): Option[Double] = ???
