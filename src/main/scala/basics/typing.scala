package basics


def milesToKilometers(miles: Double) = miles * 1.609344

def centimetersToFeetAndInches(cm: Int) = {
  val inches = cm * 3937 / 10000
  (inches / 12, inches % 12)
}

def makeUhf(a: Long, b: Long, m: Long) = (x: Long) => (a * x + b) % m


def main(): Unit = {

  // the type of v0 is: String
  val v0 = "The cake is a lie!"

  // the type of v1 is: Int
  val v1 = v0.length

  // the type of v2 is: Double
  val v2 = 42 / 13.37

  // the type of v3 is: (Double) => Double
  val v3 = milesToKilometers

  // the type of v4 is: Double
  val v4 = v3(500 + 500)

  // the type of v5 is: (Int, Int)
  val v5 = centimetersToFeetAndInches(185)

  // the type of v6 is: (String, (Int, Int)
  val v6 = ("height:", v5)

  // the type of v7 is: ((Int) => (Int, Int), Int, Int)
  val v7 = (centimetersToFeetAndInches, 0, 210)

  // the type of v8 is: Long => Long
  val v8 = makeUhf(167, 1, 104729)

  // the type of v9 is: (Long) => Long
  val v9 = (m: Long) => makeUhf(167, 1, m)(1337)
}
