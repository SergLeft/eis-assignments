package rational

import scala.collection.mutable.ArrayBuffer

import rational.mutable.Rational

@main def bufferTest(): Unit = {

  // create an empty buffer
  val buffer = ArrayBuffer[Rational]()

  // append 2/3 to the buffer twice
  val twoThirds = Rational(2, 3)
  buffer.append(twoThirds)
  buffer.append(twoThirds)

  // print the contents of the buffer
  println(s"Entry 0: ${buffer(0).numerator}/${buffer(0).denominator}")
  println(s"Entry 1: ${buffer(1).numerator}/${buffer(1).denominator}")

  // add 5 to the first Rational only
  buffer(0).add(5)

  // print the contents of the buffer (again)
  println(s"Entry 0: ${buffer(0).numerator}/${buffer(0).denominator}")
  println(s"Entry 1: ${buffer(1).numerator}/${buffer(1).denominator}")

  // Erklärung: Variable ist eigentlich ein Pointer. Wenn wir darauf addieren, addieren wir auf dem Pointer
  // dieser wird als Verweis für beide Stellen genutzt und dadurch kommt das Ergebnis zustande
}
