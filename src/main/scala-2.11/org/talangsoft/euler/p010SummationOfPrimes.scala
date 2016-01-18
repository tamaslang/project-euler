package org.talangsoft.euler

import scala.annotation.tailrec

/**
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
 */
object p010summationOfPrimes {

  def isPrime(nr: Long): Boolean = {
    if(nr<=1) return false
    else if(nr <=3) return true
    else if(nr%2 == 0 || nr%3==0) return false
    else (5l to math.sqrt(nr).toLong by 2).foreach(divisor => {
      if(nr % divisor==0) return false
    })
    return true
  }

  def sumOfPrimesUnder(upperLimit:Long): Long = {
    @tailrec
    def sumOfPrimesUnderRec(upperLimit:Long, startFrom: Long = 3, sum: Long = 2): Long = {
      if(startFrom > upperLimit) sum
      else if(isPrime(startFrom)) sumOfPrimesUnderRec(upperLimit,startFrom+2,sum+startFrom)
      else sumOfPrimesUnderRec(upperLimit,startFrom+2,sum)
    }
    sumOfPrimesUnderRec(upperLimit)
  }

}
