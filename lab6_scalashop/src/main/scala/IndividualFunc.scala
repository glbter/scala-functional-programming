import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.concurrent.Future
import scala.util.Try


object IndividualFunc  {
  def main(args: Array[String]): Unit = {
    val interval = -250 to 250
    interval.par
      .map(it => Future.fromTry(Try{optionPf(it, 1, 1)})).map(_.value)

    val numTasks = 2
    val perTask = interval.length / numTasks
    val dots = (interval).by(perTask)

    println(dots.zip(dots.tail)
      .par
      .map(it => Future.fromTry(
        Try {
          (it._1 until it._2).flatMap(optionPf(_, 1, 1))
        }
      )).flatMap(_.value.get.get))
  }

  val pf: PartialFunction[(Int, Int, Int), Double] = {
    case x: (Int, Int, Int) if x._1 != 2 => function(x._1, x._2, x._3)
  }

  val optionPf: ((Int, Int, Int)) => Option[Double] = pf.lift

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
}
