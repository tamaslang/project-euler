package org.talangsoft.euler

import scala.annotation.tailrec

/**
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
*/
object p002evenFibonacci {

  def fibonacci(upperLimit:Int):List[Int] = {
    // TODO: optimize it with @tailrec
    def fibonacci(second: Int, nextNumber: Int, upperLimit: Int): List[Int] = {
      if (nextNumber > upperLimit) List()
      else {
        (nextNumber) :: fibonacci(nextNumber, second + nextNumber, upperLimit)
      }
    }
    fibonacci(1,1,upperLimit)
  }

  def evenFibonacciSum(upperLimit:Int) = fibonacci(upperLimit) filter (_%2==0) sum

}
