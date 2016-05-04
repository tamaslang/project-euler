package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}

class p012highlyDivisibleTriangularNumberSpec extends WordSpec with ShouldMatchers{

  "firstTriangularNrWithNDivisor" should {
    "solve the problem with nr having over 500 divisors" in {
      p012highlyDivisibleTriangularNumber.triangularNrIndexWithOverNDivisor(500) shouldBe 76576500
    }
  }

  "firstTriangularNrWithNDivisor" should {
    "return triangular nr having min 6 divisors" in {
      p012highlyDivisibleTriangularNumber.triangularNrIndexWithOverNDivisor(6) shouldBe 28
    }

    "return triangular nr having min 3 divisors" in {
      p012highlyDivisibleTriangularNumber.triangularNrIndexWithOverNDivisor(3) shouldBe 6
    }

    "return triangular nr having min 2 divisors" in {
      p012highlyDivisibleTriangularNumber.triangularNrIndexWithOverNDivisor(2) shouldBe 3
    }
  }

  "triangularNr" should {
    "return 1 as 1st nr" in {
      p012highlyDivisibleTriangularNumber.triangularNr(1) shouldBe 1
    }

    "return 3 as 2nd nr" in {
      p012highlyDivisibleTriangularNumber.triangularNr(2) shouldBe 3
    }

    "return 28 as 7th nr" in {
      p012highlyDivisibleTriangularNumber.triangularNr(7) shouldBe 28
    }
  }

  "nrOfDivisorsNotEfficient" should {
    "return nr of divisors for nr 1" in {
      p012highlyDivisibleTriangularNumber.nrOfDivisorsNotEfficient(1l) shouldBe 1
    }
    "return nr of divisors for nr 2" in {
      p012highlyDivisibleTriangularNumber.nrOfDivisorsNotEfficient(2l) shouldBe 2
    }

    "return nr of divisors for nr 8" in {
      p012highlyDivisibleTriangularNumber.nrOfDivisorsNotEfficient(8l) shouldBe 4
    }

    "return nr of divisors for nr 16499640" in {
      p012highlyDivisibleTriangularNumber.nrOfDivisorsNotEfficient(16499640l) shouldBe 64
    }

    "return nr of divisors for nr 33583110" in {
      p012highlyDivisibleTriangularNumber.nrOfDivisorsNotEfficient(33583110l) shouldBe 64
    }
  }

  "nrOfDivisorsWithPrimeFactor" should {
    "return nr of divisors for nr 1" in {
      p012highlyDivisibleTriangularNumber.nrOfDivisorsBasedOnPrimeFactor(1l) shouldBe 1
    }
    "return nr of divisors for nr 2" in {
      p012highlyDivisibleTriangularNumber.nrOfDivisorsBasedOnPrimeFactor(2l) shouldBe 2
    }

    "return nr of divisors for nr 8" in {
      p012highlyDivisibleTriangularNumber.nrOfDivisorsBasedOnPrimeFactor(8l) shouldBe 4
    }

    "return nr of divisors for nr 48" in {
      p012highlyDivisibleTriangularNumber.nrOfDivisorsBasedOnPrimeFactor(48l) shouldBe 10
    }

    "return nr of divisors for nr 16499640" in {
      p012highlyDivisibleTriangularNumber.nrOfDivisorsBasedOnPrimeFactor(16499640l) shouldBe 64
    }

    "return nr of divisors for nr 33583110" in {
      p012highlyDivisibleTriangularNumber.nrOfDivisorsBasedOnPrimeFactor(33583110l) shouldBe 64
    }
  }

  "countGroups" should {
    "count individual strings" in {
      p012highlyDivisibleTriangularNumber.countIndividualGroups(List("a","a","a","b","b")) shouldBe List(3,2)
    }

    "count individual numbers" in {
      p012highlyDivisibleTriangularNumber.countIndividualGroups(List(2,2,3,4,5,6)) shouldBe List(2, 1, 1, 1, 1)
    }
  }

}
