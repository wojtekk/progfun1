package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def innerBalance(chars: List[Char], level: Int): Int = {
      if (chars.isEmpty) {
        0
      } else {
        chars.head match {
          case '(' => 1 * level * 10 + innerBalance(chars.tail, level * 10)
          case ')' => -1 * level + innerBalance(chars.tail, level / 10)
          case _ => 0 + innerBalance(chars.tail, level)
        }
      }
    }
    innerBalance(chars, 1) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    (coins.isEmpty, money compare 0) match {
      case (_, 0) => 1
      case (false, 1) => countChange(money - coins.head, coins) + countChange(money, coins.tail) // greater
      case _ => 0
    }
  }
}
