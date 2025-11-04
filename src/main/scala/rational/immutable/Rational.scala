package rational.immutable

import scala.annotation.tailrec

/** Represents a rational number.
 *
 *  @param n the passed numerator (might be reduced)
 *  @param d the passed denominator (might be reduced)
 *  @throws IllegalArgumentException if the provided denominator is zero
 */
class Rational(n: Int, d: Int) {

  if (d == 0)
    throw new IllegalArgumentException("The provided denominator is zero")

  private val g = gcd(n.abs, d.abs) * math.signum(d)
  val numerator: Int = n / g
  val denominator: Int = d / g

  /** Alternative constructor for creating a Rational from an Integer */
  def this(n: Int) = this(n, 1)

  /** Adds a Rational to this Rational
   *
   *  @param that the Rational to add to this Rational
   *  @return a new Rational representing the result of the addition
   */
  def +(that: Rational): Rational = {
    new Rational(
      numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator
    )
  }

  /** Adds an Int to this Rational
   *
   *  @param that the Int to add to this Rational
   *  @return a new Rational representing the result of the addition
   */
  def +(that: Int): Rational = {
    new Rational(numerator + that * denominator, denominator)
  }

  def unary_- : Rational = ???

  def reciprocal : Rational = ???

  def -(that: Rational): Rational = ???

  def -(that: Int): Rational = ???

  def *(that: Rational): Rational = ???

  def /(that: Rational): Rational = ???

  /** Returns true iff that Rational is less than this Rational
   *
   *  @param that the Rational for which it is tested whether it is less than this Rational
   */
  def lessThan(that: Rational): Boolean = {
    numerator * that.denominator < that.numerator * denominator
  }

  /** Returns the larger Rational of this and that
   *
   *  @param that the Rational to compare with this Rational
   */
  def max(that: Rational): Rational = {
    if (lessThan(that)) that else this
  }

  def min(that: Rational): Rational = ???

  /** Prints a textual representation of this Rational */
  def print(): Unit = {
    println(s"$numerator/$denominator")
  }

  override def toString: String = ???

  /** Returns the gcd of two passed Ints */
  @tailrec
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }
}
