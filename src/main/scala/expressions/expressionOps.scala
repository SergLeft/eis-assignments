package expressions


/** Evaluates an [[Expression]] with the specified variable bindings.
 *
 *  @param variables the variable bindings
 *  @return the final result
 */
def evaluate(ex: Expression, variables: Map[String, Double]): Double = ex match {
  case Variable(name)                    => variables.getOrElse(name, throw new NoSuchElementException(s"variable $name not found"))
  case Constant(value)                   => value
  case BinaryOperation("+", left, right) => evaluate(left, variables) + evaluate(right, variables)
  case BinaryOperation("*", left, right) => evaluate(left, variables) * evaluate(right, variables)
  case UnaryOperation("-", ex)           => - evaluate(ex, variables)
  case UnaryOperation("abs", ex)         => evaluate(ex, variables).abs
  case UnaryOperation("invert", ex)      =>
    val denominator = evaluate(ex, variables)
    if (denominator == 0) throw new ArithmeticException("the denominator evaluated to 0")
    1 / denominator
  case _ => throw new IllegalStateException("illegal operator")
}

/** Returns the names of all variables referenced by an [[Expression]]. */
def referencedVariables(ex: Expression): Set[String] = ex match {
  case Variable(name) => Set(name)
  case Constant(value) => Set()
  case UnaryOperation(op, ex) => referencedVariables(ex)
  case BinaryOperation(op, left, right) => referencedVariables(left) ++ referencedVariables(right)
}

/** Transforms an [[Expression]] into an equivalent but potentially simpler expression.
 *
 *  @return the simplified expression
 */
protected def simplified(ex: Expression): Expression = ex match {
  case BinaryOperation("+", Constant(0), term)            => term
  case BinaryOperation("+", term, Constant(0))            => term
  case _ => ex
}

/** Recursively simplifies an [[Expression]]  */
def fullySimplified(ex: Expression): Expression = ex match {
  case UnaryOperation(op, ex)           => simplified(UnaryOperation(op, fullySimplified(ex)))
  case BinaryOperation(op, left, right) => simplified(BinaryOperation(op, fullySimplified(left), fullySimplified(right)))
  case _ => ex
}
