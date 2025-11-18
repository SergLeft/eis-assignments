package optional.matching


/** Applies a transformation to the value stored within an [[Option]], if it exists.
 *
 *  @param transform the transformation to apply
 *  @param option the [[Option]] to apply the transformation to
 *  @return an [[Option]] containing the transformed value
 */
def map[A, B](transform: A => B, option: Option[A]): Option[B] = ???


/** Returns true if this [[Option]] contains a value which satisfies the condition, false otherwise. */
def satisfies[A](condition: A => Boolean, option: Option[A]): Boolean = ???


/** Returns this [[Option]]'s contained value if it exists, evaluate the alternative otherwise. */
def getOrElse[A](alternative: () => A, option: Option[A]): A = ???


/** If both [[Option]]s in the specified tuple contain a value, returns a tuple of them, None otherwise. */
def both[A, B](tuple: (Option[A], Option[B])): Option[(A, B)] = ???


/** Returns a function that runs two transformations in sequence.
 *
 *  The created function will return None if the intermediate result is None.
 */
def concat[A, B, C](firstTransform: A => Option[B], secondTransform: B => Option[C]): A => Option[C] = ???
