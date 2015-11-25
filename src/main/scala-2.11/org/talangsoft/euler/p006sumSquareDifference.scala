package org.talangsoft.euler

/**
  *
  * The sum of the squares of the first ten natural numbers is,
  * 12 + 22 + ... + 102 = 385
  *
  * The square of the sum of the first ten natural numbers is,
  * (1 + 2 + ... + 10)2 = 552 = 3025
  *
  * Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
  *
  * Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
  */
object p006sumSquareDifference {
  def sumOfSquares(upperLimit: Int): Long ={
    (1 to upperLimit) map (nr => nr*nr) reduceLeft(_ + _)
  }

  def squareOfSum(upperLimit: Int): Long ={
    val sum = (1 to upperLimit) reduceLeft(_ + _)
    sum*sum
  }

  def difference(upperLimit: Int): Long = p006sumSquareDifference.squareOfSum(upperLimit) - p006sumSquareDifference.sumOfSquares(upperLimit)
}
