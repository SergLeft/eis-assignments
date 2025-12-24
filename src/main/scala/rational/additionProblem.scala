package rational

import rational.immutable.Rational

@main def additionTest(): Unit = {

  val frac1 = Rational(5, 936449)
  val frac2 = Rational(2, 879812)

  val result = frac1 + frac2

  println(s"Result: ${result.numerator}/${result.denominator}")

  // Erklärung: In dieser Addition kommt es einfach zu einem Overflow, da die Zahlen
  // zu groß werden bei der Multiplikation, die dafür nötig ist. Dadurch kommt die 
  // falsche Zahl zustande
}
