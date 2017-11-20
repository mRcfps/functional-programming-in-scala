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
      if (c == 0 || r == c) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def doesMatch(chars: List[Char], leftParenthesis: Int): Boolean = {
        if (chars.isEmpty && leftParenthesis == 0) true
        else if (chars.head == '(') doesMatch(chars.tail, leftParenthesis + 1)
        else if (chars.head == ')' && leftParenthesis > 0) doesMatch(chars.tail, leftParenthesis - 1)
        else if (chars.head != '(' && chars.head != ')') doesMatch(chars.tail, leftParenthesis)
        else false
      }
      doesMatch(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.length == 0 && money > 0) 0
      else if (money == 0) 1
      else if (coins.max > money) countChange(money, coins.sorted.slice(0, coins.length-1))
      else {
        var result = 0
        var i = 0
        for (i <- 0 to (money / coins.max)) {
          result += countChange(money - i*coins.max, coins.sorted.slice(0, coins.length-1))
        }
        result
      }
    }
  }
