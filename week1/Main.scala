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
   *  takes a column c and a row r, counting from 0 
   *  returns the number at that spot in the triangle (Pascal’s triangle).
   */
  def pascal(c: Int, r: Int): Int = 
    if(c == 0 || c == r) 1
    else pascal(c-1 ,r-1) + pascal(c ,r-1)

  /**
   *  recursive function which verifies the balancing of parentheses in a string
   *  which we represent as a List[Char]
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
   *  recursive function that counts how many different ways you can make change for an amount
   *  given a list of coin denominations
   * 
   *  example: 
   *  there are 3 ways to give change for 4 
   *  if you have coins with denomiation 1 and 2
   * 
   *  1+1+1+1, 1+1+2, 2+2.
   */
  def countChange(money: Int, coins: List[Int]): Int = 
	if(money == 0) 1
	else if(coins.isEmpty) 0
	else if(coins.head <= money) (countChange(money, coins.tail) +  
		                        countChange(money - coins.head, coins))
	else countChange(money, coins.tail)
}

