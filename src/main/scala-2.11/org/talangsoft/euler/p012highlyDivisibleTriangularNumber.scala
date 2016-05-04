package org.talangsoft.euler


/**
The sequence of triangle numbers is generated by adding the natural numbers.
So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

 1: 1
 3: 1,3
 6: 1,2,3,6
10: 1,2,5,10
15: 1,3,5,15
21: 1,3,7,21
28: 1,2,4,7,14,28
We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred divisors?

  */
object p012highlyDivisibleTriangularNumber {

  def triangularNrIndexWithOverNDivisor(expectedMinDivisorNr: Int): Long = {
    var currentTriangularNr = 0l
    for (x <- 1 to Int.MaxValue) {
      currentTriangularNr = nextTriangularNr(currentTriangularNr, x)
      if (nrOfDivisorsBasedOnPrimeFactor(currentTriangularNr) >= expectedMinDivisorNr) return currentTriangularNr
    }
    currentTriangularNr
  }

  def nrOfDivisorsNotEfficient(nr: Long): Int = (1l to Math.round(nr/2) count(possibleDivisor => nr % possibleDivisor == 0)) + 1 /* 1 and itself */

  def nrOfDivisorsBasedOnPrimeFactor(nr: Long): Int = nr match {
    case 1 => 1
    case _ => {
      val primeFactors = p003largestPrimeFactor.primeFactorsRec(nr)
      // based on:
      // https://online.math.uh.edu/MiddleSchool/Modules/Module_1_Number_Operations/Activities/NumberofDivisorsTeacherNotes.pdf
      countIndividualGroups(primeFactors) map (size => size + 1) reduce (_ * _)
    }
  }

  def countIndividualGroups[T](l: List[T]): List[Int] = {
    if (l.isEmpty) List()
    else {
      val (packed, next) = l.span(_ == l.head)
      (packed.size) :: countIndividualGroups(next)
    }
  }

  def triangularNr(n: Long): Long = 0l to n sum

  def nextTriangularNr(prev: Long, n: Long) = prev + n
}
