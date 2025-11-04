package rational.mutable

/** Represents a rational number
 * 
 * @param n the passed numerator (will be reduced)
 * @param d the passed denominator (will be reduced)
 */
class Rational(n: Int, d: Int) {

  /* Primary constructor */
  if (d <= 0)
    throw new IllegalArgumentException("The provided denominator is not positive.")
    
  private var g = gcd(n.abs, d.abs)
  var numerator: Int = n / g
  var denominator: Int = d / g
  //println("Created " + numerator + "/" + denominator)

  /** Alternative constructor for creating a relational from an integer */
  def this(n: Int) = this(n, 1)

  /** Adds a Rational to this Rational and reduces this Rational again
   * 
   * @param that the Rational to add to this Rational
   */ 
  def add(that: Rational): Unit = {
    numerator = numerator * that.denominator + that.numerator * denominator
    denominator = denominator * that.denominator

    g = gcd(numerator.abs, denominator.abs)
    numerator = numerator / g
    denominator = denominator / g
  }

  /** Adds an Int to this Rational 
   * 
   * @param that the Int to add to this Rational
   */ 
  def add(that: Int): Unit = {
    numerator = numerator + that * denominator

    g = gcd(numerator.abs, denominator.abs)
    numerator = numerator / g
    denominator = denominator / g
  }

  /** Returns true iff that Rational is less than this Rational 
   * 
   * @param that the Rational for which it is tested whether it is less than this Rational
   */
  def lessThan(that: Rational): Boolean = {
    numerator * that.denominator < that.numerator * denominator
  }

  /** Returns the larger Rational of this and that
   * 
   * @param that the Rational to compare with this Rational
   */ 
  def max(that: Rational): Rational = {
    if (lessThan(that)) that else this
  }

  /** Prints a textual representation of this Rational */
  def print(): Unit = {
    println(numerator.toString + "/" + denominator.toString)
  }

  /** Returns the gcd of two passed Ints */ 
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  /** Returns a textual representation of this Rational */
  override def toString: String = {
    if (denominator == 1) {
      numerator.toString
    } else {
      s"$numerator/$denominator"
    }
  }
}
