package org.talangsoft.euler

import scala.annotation.tailrec
import scala.collection.mutable

/**
  *
  * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
  *
  * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 */
object p005smallestMultiple {

  def factorialForMultipleNumbers(numbers: List[Long]): List[Long] = {
    if(numbers.size == 0) List()
    else if(numbers.head == 1) factorialForMultipleNumbers(numbers.drop(1))
    else {
      val divisor:Long = (2l to numbers.head).toStream.takeWhile(divisor => numbers.head%divisor != 0).count(f=>true) + 2
      List(divisor) ::: factorialForMultipleNumbers(numbers map (nr=> {if(nr%divisor ==0) nr/divisor else nr}))
    }
  } sortWith(_ < _)

  def smallestMultiple(numbers: List[Long]) = factorialForMultipleNumbers(numbers) reduceLeft(_ * _)
}
