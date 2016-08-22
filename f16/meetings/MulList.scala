/**
 * Twelve ways to multiply all the elements of a list.
 * 
 * A poor attempt at a parody of the "Twelve Days of Christmas".
 */
object MulList {
  
  /* On the first day of 3155, I learned how to implement ...
   * 
   * A function to multiply the elements of a list by 
   * direct recursion.
   * 
   * It recurses to the end of the list and then multiples
   * on return, so the elements get multiplied from right
   * to left.
   */
  def mulListDirect(l: List[Int]): Int = l match {
    case Nil => 1
    case h :: t => h * mulListDirect(t)
  }
  
  /* On the second day of 3155, I learned how to implement ...
   * 
   * A version with a fold that behaves in the same way as
   * the direct recursive version.
   */
  def mulListDirectFold(l: List[Int]): Int =
    l.foldRight(1) { (i, acc) => i * acc }
  
  /* On the third day of 3155, I learned how to implement ...
   * 
   * A direct recursive version that stops the list walk
   * "early" when a 0 is encountered.
   * 
   * However, the elements of the input list l before the
   * 0 (from left to right) still get multiplied on the
   * return.  It seems kind of wasteful because we now each
   * of those elements get multiplied with 0.
   */
  def mulListDirectShortCircuit(l: List[Int]): Int = l match {
    case Nil => 1
    case 0 :: _ => 0
    case h :: t => h * mulListDirectShortCircuit(t)
  }
  
  /* On the forth day of 3155, I tried to ...
   * 
   * match mulListDirectShortCircuit using a fold.
   * 
   * A fold walks over all elements of the list.  While all
   * elements are traversed, we do stop multiplying when
   * we have seen a 0 but from the right.  This is not quite the
   * same as mulListDirectShortCircuit, which stops searching
   * when it encounters a 0 from the left.
   * 
   * Exercise: What would the explicit recursive version of this
   * function look like?  Hint: There would be only two cases
   * and look more like mulListDirect.
   */
  def mulListDirectShortCircuitFold(l: List[Int]): Int =
    l.foldRight(1) { (i, acc) => if (acc == 0 || i == 0) 0 else i * acc }
  
  /* On the fifth day of 3155, I learned how to implement an iteration.
   * 
   * Instead of multiplying on return, we use an integer accumulator
   * to multiply as we recurse down.  This function is tail recursive
   * whereas the prior ones were not.
   * 
   * Note that this is iteration corresponding to using a "while loop."
   */
  def mulListResultAcc(l: List[Int]): Int = {
    def mul(acc: Int, l: List[Int]): Int = l match {
      case Nil => acc
      case h :: t => mul(acc * h, t)
    }
    mul(1, l)
  }
  
  /* On the sixth day of 3155, I learned how to implement an iteration
   * with a fold.
   * 
   * We write the iteration version mulListResultAcc using foldLeft.
   * The foldLeft combinator abstracts exactly the iteration pattern
   * over lists.
   */
  def mulListResultAccFold(l: List[Int]): Int =
    l.foldLeft(1) { (acc, i) => acc * i }
  
  /* On the seventh day of 3155, I combined iteration and "stopping
   * early."
   * 
   * We can "stop early" in the same way as mulListDirectShortCircuit,
   * though now we have already multiplied the elements before the
   * first 0 (from left to right).  We have a tail recursive/iterative
   * version but the same elements get multiplied.
   * 
   * We would like to find some way to also avoid multiplying anything
   * until we are sure there are no 0s (while traversing the input
   * list l just once).  For example, we imagine l as a stream
   * coming from reading off the network.
   */
  def mulListResultAccShortCircuit(l: List[Int]): Int = {
    def mul(acc: Int, l: List[Int]): Int = l match {
      case Nil => acc
      case 0 :: _ => 0
      case h :: t => mul(acc * h, t)
    }
    mul(1, l)
  }
  
  /* On the eighth day of 3155, I tried to "stop early" with a
   * foldLeft.
   * 
   * It's not quite stopping early. With foldLeft, the callback
   * is invoked on all elements, but multiplications stop happening
   * after the first 0 (from left to right) is found.
   */
  def mulListResultAccShortCircuitFold(l: List[Int]): Int =
    l.foldLeft(1) { (acc, i) => if (acc == 0 || i == 0) 0 else acc * i }
  
  /* On the ninth day of 3155, I saw my first continuation.
   * 
   * If we can only traverse l once and we don't want to do any
   * multiplications until we have checked that there are no 0s
   * in the whole list, then we have to "save" the elements
   * without multiplying as we walk through the list.
   * 
   * One approach is to change the Int accumulator into, say,
   * a List[Int], essentially "caching locally" the input
   * stream until we have seen the whole stream.  When we
   * reach the end of the stream (i.e., Nil), then we 
   * do the multiplication (using any of the functions
   * above).  This is essentially traversing l twice.
   * 
   * We adopt a slight twist of the above.  Instead of
   * "caching locally" in a List[Int], we cache in a function
   * Int => Int.  Whoa, we are using functions as a data structure
   * (repeat: "functions are values").
   * 
   * Let's for the moment ignore the "stop early" on 0 and focus
   * on accumulating a function that suspends "what should be done."
   * 
   * No multiplications happen until we get to the end of the input
   * list l where we have decided to actually "do it" (i.e., do the
   * the multiplications).  We have essentially "cached" the elements
   * of l inside sc but also packaged the "multiplying actions" with
   * the elements.
   * 
   * Stepping back, we see that mulListDelayed is exactly the
   * continuation-passing style version of mulListDirect.  In
   * mulListDirect, the "cached" elements of l are on the
   * stack of activation records.
   * 
   * Remember that "continuation" is just a jargony word for
   * a callback for "myself."
   */
  def mulListDelayed(l: List[Int]): Int = {
    def mul(sc: Int => Int, l: List[Int]): Int = l match {
      case Nil => sc(1)
      case h :: t => mul(acc => sc(h * acc), t)
    }
    mul(r => r, l)
  }
  
  /* On the tenth day of 3155, I understood that functions 
   * are values.
   * 
   * To emphasize that we can accumulate a function of "what
   * to do later," we rewrite using foldLeft.
   */
  def mulListDelayedFold(l: List[Int]): Int = {
    val domul = l.foldLeft[Int => Int](r => r) { (sc, i) =>
      acc => sc(i * acc)
    }
    domul(1)
  }
  
  /* On the eleventh day of 3155, I learned how to use explicit
   * continuations.
   * 
   * Finally, we have our desired function that multiplies the
   * elements of a list but (1) "stops early" at the first 0
   * and (2) only does multiplications if all elements are
   * non-zero.
   * 
   * We see that once we have explicit continuations, we can do
   * things with it like drop it on the floor.
   */
  def mulListDelayedShortCircuit(l: List[Int]): Int = {
    def mul(sc: Int => Int, l: List[Int]): Int = l match {
      case Nil => sc(1)
      case 0 :: _ => 0
      case h :: t => mul(acc => sc(h * acc), t)
    }
    mul(r => r, l)
  }
  
  /* On the twelfth day of 3155, I really understood that
   * functions are values.
   * 
   * Like previous versions with fold, we go over all elements
   * regardless of whether or not there's a 0.  This is a
   * somewhat weird function.  Whenever it encounters a 0,
   * it drops the current function accumulator sc, dropping
   * the "to do later" for the previous elements.  The
   * elements after the last 0 do get multiplied together.
   */
  def mulListDelayedShortCircuitFold(l: List[Int]): Int = {
    val domul = l.foldLeft[Int => Int](r => r) { (sc, i) =>
      if (i == 0) { _ => 0 } else { acc => sc(i * acc) }
    }
    domul(1)
  }

}