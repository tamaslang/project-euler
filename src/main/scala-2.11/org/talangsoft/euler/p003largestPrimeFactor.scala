package org.talangsoft.euler

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * The prime factors of 13195 are 5, 7, 13 and 29.
  *
  * What is the largest prime factor of the number 600851475143 ?
  */
object p003largestPrimeFactor {
  def primeFactorsRec(number: Long): List[Long]= {
    @tailrec
    def primeFactors(remainder: Long, divisor : Long,factors: List[Long]): List[Long] = {
      if(divisor > remainder) factors
      else if(remainder.%(divisor) == 0) primeFactors((remainder/divisor), divisor,factors:+divisor)
      else primeFactors(remainder,divisor+1,factors)
    }
    primeFactors(number,2,List())
  }

  def largestPrimeFactor(number:Long):Long = primeFactorsRec(number).last
}
