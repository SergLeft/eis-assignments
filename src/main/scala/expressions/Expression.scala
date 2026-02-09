package expressions


sealed abstract class Expression {

  /** Returns a string representation of this [[Expression]] */
  override def toString: String = this match {
    case Variable(name) => name
    case Constant(value) => value.toString
    case UnaryOperation(op, ex) => s"($op $ex)"
    case BinaryOperation(op, left, right) => s"($left $op $right)"
  }
}

/** Represents a variable, i.e. a value to be set later.
 *
 * @param name the name of the variable
 */
case class Variable(name: String) extends Expression

/** Represents a constant, i.e. a fixed value.
 *
 * @param value the value of the constant
 */
case class Constant(value: Double) extends Expression

/** Represents any operation that takes a single argument.
 *
 * @param op a symbol representing the operator
 * @param ex the argument
 */
case class UnaryOperation(op: String, ex: Expression) extends Expression

/** Represents any operation that takes two arguments.
 *
 * @param op    a symbol representing the operator
 * @param left  the left argument
 * @param right the right argument
 */
case class BinaryOperation(op: String, left: Expression, right: Expression) extends Expression
