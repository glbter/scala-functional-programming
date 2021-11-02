import org.scalacheck._

import scala.annotation.tailrec
import Prop.{forAll, propBoolean}

object IndividualFunc extends Properties("Int") {
  val pf: PartialFunction[(Int, Int, Int), Double] = {
    case x: (Int, Int, Int) if x._1 != 2 => function(x._1, x._2, x._3)
  }

  val optionPf: ((Int, Int, Int)) => Option[Double] = pf.lift

  property("if x less than 2 res is 2") = forAll { (x: Int, m: Int, n: Int) =>
    (x <= 2) ==> {
      if (x < 2) optionPf((x, m, n)).isDefined && optionPf((x, m, n)).get == 2
      else optionPf((x, m, n)).isEmpty
    }
  }

  property("x eq 2 res") = forAll { (x: Int, m: Int, n: Int) =>
    (x > 2 ) ==> {
      if (-1000 < m && m < 1000 && -1000 < n && n < 1000)
        (optionPf((x, m, n)).isDefined && optionPf((x, m, n)).get > 0)
      else true
    }
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
}

