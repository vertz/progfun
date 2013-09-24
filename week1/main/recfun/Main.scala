package recfun
import common._

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
    if(c == 0 || c == r) 1
    else pascal(c-1 ,r-1) + pascal(c ,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
      
    	def loop(chars: List[Char], bal: Int): Boolean = 
    	  if(bal < 0) false 
    	  else if(chars.isEmpty) bal == 0
    	  else if(chars.head == '(') loop(chars.tail, bal + 1)
    	  else if(chars.head == ')') loop(chars.tail, bal - 1) 
    	  else loop(chars.tail, bal)
    	  
    	loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
	if(money == 0) 1
	else if(coins.isEmpty) 0
	else if(coins.head <= money) (countChange(money, coins.tail) +  
		                        countChange(money - coins.head, coins))
	else countChange(money, coins.tail)
}

