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
      // parenthesis matching has completed
      if (chars.isEmpty && leftParenthesis == 0) true
      // if we find a parenthesis, increase the num of leftParenthesis
      else if (chars.head == '(') doesMatch(chars.tail, leftParenthesis + 1)
      // if a parenthesis is closed, decrease leftParenthesis
      else if (chars.head == ')' && leftParenthesis > 0) doesMatch(chars.tail, leftParenthesis - 1)
      // skip non-parenthesis character
      else if (chars.head != '(' && chars.head != ')') doesMatch(chars.tail, leftParenthesis)
      else false
    }

    doesMatch(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def accumulate(acc: Int, untriedCoins: List[Int]): Int = {
      if (acc == money) 1
      else if (acc > money || untriedCoins.isEmpty) 0
      else accumulate(acc + untriedCoins.head, untriedCoins) + accumulate(acc, untriedCoins.tail)
    }

    if (money == 0) 0
    else accumulate(0, coins)
  }
}
