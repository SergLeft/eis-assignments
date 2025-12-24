package optional.matching


/** Applies a transformation to the value stored within an [[Option]], if it exists.
 *
 *  @param transform the transformation to apply
 *  @param option the [[Option]] to apply the transformation to
 *  @return an [[Option]] containing the transformed value
 */
def map[A, B](transform: A => B, option: Option[A]): Option[B] = option match {
  case Some(option) => Some(transform(option))
  case None => None
}


/** Returns true if this [[Option]] contains a value which satisfies the condition, false otherwise. */
def satisfies[A](condition: A => Boolean, option: Option[A]): Boolean = option match {
  case Some(option) => condition(option)
  case None => false
}


/** Returns this [[Option]]'s contained value if it exists, evaluate the alternative otherwise. */
def getOrElse[A](alternative: () => A, option: Option[A]): A = option match {
  case Some(option) => option
  case None => alternative()
}


/** If both [[Option]]s in the specified tuple contain a value, returns a tuple of them, None otherwise. */
def both[A, B](tuple: (Option[A], Option[B])): Option[(A, B)] = tuple match {
  case (Some(a),Some(b)) => Some((a,b))
  case _ => None
}


/** Returns a function that runs two transformations in sequence.
 *
 *  The created function will return None if the intermediate result is None.
 */
def concat[A, B, C](firstTransform: A => Option[B], secondTransform: B => Option[C]): A => Option[C] = {
  def Funct1(something: A): Option[C] = {
    firstTransform(something) match {
      case Some(intermediate) => secondTransform(intermediate)
      case None => None
    }
  }
  Funct1
}
