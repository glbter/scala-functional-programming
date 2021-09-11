package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */

    def pascal(c: Int, r: Int): Int =
      if (c == 0 || r == 1) 1
      else pascal(c-1, r-1) + pascal(c, r-1)

  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val opening = "(".charAt(0)
      val closing = ")".charAt(0)
      @tailrec
      def innerBalance(chars: List[Char], opened: Int): Int =
        if (chars.isEmpty) opened
        else if (opened < 0) opened
        else if (chars.head == opening) innerBalance(chars.tail, opened + 1)
        else if (chars.head == closing) innerBalance(chars.tail, opened - 1)
        else innerBalance(chars.tail, opened)

      innerBalance(chars,0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def innerCount(money: Int, coins: List[Int]): Int = {
        if (money < 0) 0
        else if (money == 0) 1
        else if (coins.isEmpty) 0
        else innerCount(money - coins.head, coins) +
            innerCount(money,  coins.tail)
      }

      if (money == 0) return 0
      val reversedCoins = coins.sortWith((a,b) => a < b)
      innerCount(money: Int, reversedCoins: List[Int])
    }
  }
