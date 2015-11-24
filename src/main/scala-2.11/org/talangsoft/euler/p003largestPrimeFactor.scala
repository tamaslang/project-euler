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
      else if(remainder.%(divisor) == 0) primeFactors((remainder/divisor).toLong, divisor,factors:+divisor)
      else primeFactors(remainder,divisor+1,factors)
    }
    primeFactors(number,2,List())
  }

  def primeFactors(number: Long): List[Long]= {
    var remainder = number
    var factors=mutable.MutableList[Long]()
    // precollect divisor 2 so we can iterate by 2 later (odd numbers)
    while(remainder%2==0){remainder/=2; factors+=2}
    var divisor = 3
    while(remainder!=1){
      // if real divider, collect and divide
      if(remainder%divisor == 0) { factors+=divisor; remainder/=divisor}
      // else increment divisor to next odd nr
      else {divisor +=2}
    }
    factors.toList
  }

  def largestPrimeFactor(number:Long):Long = primeFactors(number).last
  def largestPrimeFactorRec(number:Long):Long = primeFactorsRec(number).last
}
