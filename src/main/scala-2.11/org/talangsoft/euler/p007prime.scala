package org.talangsoft.euler

import scala.annotation.tailrec

/**
  * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
  *
  * What is the 10 001st prime number?
  */
object p007prime {
  def isPrime(nr: Long): Boolean = {
    if(nr<=1) return false
    else if(nr <=3) return true
    else if(nr%2 == 0 || nr%3==0) return false
    else (5l to math.sqrt(nr).toLong by 2).foreach(divisor => {
        if(nr % divisor==0) return false
    })
    return true
  }

  def firstNPrimes(n:Int): List[Long] = {
    @tailrec
    def findNPrimesRec(n:Int,primeNumbers: List[Long] = List(2), startFrom: Long = 3): List[Long] = {
      if(primeNumbers.size == n) primeNumbers
      else if(isPrime(startFrom)) findNPrimesRec(n,primeNumbers:+startFrom,startFrom+2)
      else findNPrimesRec(n,primeNumbers,startFrom+2)
    }
    findNPrimesRec(n)
  }


}
