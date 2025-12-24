package rational

import rational.immutable.Rational


def rationalToDecimal(r: Rational) = {
  r.numerator / r.denominator
}


@main def decimalTest(): Unit = {

  val frac1 = Rational(0, 9)
  val frac2 = Rational(1, 4)
  val frac3 = Rational(7, 8)

  val dec1 = rationalToDecimal(frac1)
  val dec2 = rationalToDecimal(frac2)
  val dec3 = rationalToDecimal(frac3)

  println(s"First fraction: $dec1")
  println(s"Second fraction: $dec2")
  println(s"Third fraction: $dec3")

  // Erklärung: Integer können wir in Scala nicht dividieren,
  // da Ints nicht automatisch zu Double oder Float umgewandelt werden
  // Theoretisch könnte das Problem dadurch gelöst werden, indem man
  // numerator und denominator als doubles darstellt in der obigen Methode
  // mit val n und val d: Double jeweils und dann n/d
}
