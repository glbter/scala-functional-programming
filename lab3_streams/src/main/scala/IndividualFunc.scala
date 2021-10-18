import scala.annotation.tailrec

object IndividualFunc {
  def powerSum(m: Int, n: Int): PartialFunction[Int, Double] = {
    val function: PartialFunction[Int, Double] = {
      case x: Int if x != 2 => IndividualFunc.function(x, m, n)
    }

    function
  }

 def powerSumList(m: Int, n: Int): Seq[Int] => List[Double] = {
   def f = powerSum(m, n)

   def toList(r: Seq[Int]): List[Double] = r.collect(f).toList

   toList
 }



  def function(x: Double, m: => Int, n: => Int): Double = {
    require(x != 2, "function is not defined at x=2")
    @tailrec
    def power(acc: Double, currPow: Int, pow: Int): Double =
      if (pow == currPow) acc
      else power(acc*x, currPow+1, pow)

    def pp = (power _).curried(1)(0)
    def p = (pow: Int) => if (pow >= 0) pp(pow) else 1 / pp(-pow)

    if (x < 2) 2
    else p(n) + p(m)
  }

  def filter[T](l: List[T], f: T => Boolean): List[T] =
    if (l.isEmpty) Nil
    else if (f(l.head)) l.head :: filter(l.tail, f)
    else filter(l.tail, f)

  def map[TI, TO](l: List[TI], f: TI => TO): List[TO] =
    if (l.isEmpty) Nil
    else f(l.head) :: map(l.tail, f)

  def fold[T](l: List[T], f: (T, T) => T): T = {
    require(l.nonEmpty, "cannot fold an empty list")

    reduce(l.head, l.tail, f)
  }

  @tailrec
  def reduce[T](acc: T, l: List[T], f: (T, T) => T): T =
    if (l.isEmpty) acc
    else reduce(f(acc, l.head), l.tail, f)


  @tailrec
  def foreach[T](l: List[T], f: T => Unit): Unit = {
    if (l.isEmpty) return
    f(l.head)
    foreach(l.tail, f)
  }



//  def flatMap[TI, TO](l: List[TI], f: TI => List[TO]): List[TO] =
//    if (l.isEmpty) Nil
//    else f(l.head) :: flatMap[TI, TO](l.tail, f)
}


