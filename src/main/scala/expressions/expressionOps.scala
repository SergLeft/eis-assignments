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
  
  // 1) 0 is left absorbing for multiplication
  case BinaryOperation("*", Constant(0), _)               => Constant(0)
  // 2) 0 is right absorbing for multiplication
  case BinaryOperation("*", _, Constant(0))               => Constant(0)
  // 5) (-1)*x = -x must come before 3) since -1 is more specific than any constant
  case BinaryOperation("*", Constant(-1), term)            => UnaryOperation("-", term)
  // 6) x*(-1) = -x same reasoning
  case BinaryOperation("*", term, Constant(-1))            => UnaryOperation("-", term)
  // 3) 1 is left neutral for multiplication
  case BinaryOperation("*", Constant(1), term)             => term
  // 4) 1 is right neutral for multiplication
  case BinaryOperation("*", term, Constant(1))             => term
  // 7) double negation
  case UnaryOperation("-", UnaryOperation("-", term))      => term
  // 8) idempotence of abs: abs(abs(x)) = abs(x)
  case UnaryOperation("abs", UnaryOperation("abs", term)) => UnaryOperation("abs", term)
  // constant folding: if all operands are constants, evaluate differently
  case UnaryOperation(op, Constant(v)) =>
    try {
      Constant(evaluate(ex, Map()))
    } catch {
      case _: ArithmeticException => throw ArithmeticException("division by zero during constant folding")
    }
  case BinaryOperation(op, Constant(l), Constant(r)) =>
    try {
      Constant(evaluate(ex, Map()))
    } catch {
      case _: ArithmeticException => throw ArithmeticException("division by zero during constant folding")
    }
  
  // no simplification applicable 
  case _ => ex
}

/** Recursively simplifies an [[Expression]]  */
def fullySimplified(ex: Expression): Expression = ex match {
  case UnaryOperation(op, ex)           => simplified(UnaryOperation(op, fullySimplified(ex)))
  case BinaryOperation(op, left, right) => simplified(BinaryOperation(op, fullySimplified(left), fullySimplified(right)))
  case _ => ex
}
