package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}


class p003largestPrimeFactorSpec extends WordSpec with ShouldMatchers{
  "primeFactors" should {
    "factorize number 13195" in {
        p003largestPrimeFactor.primeFactorsRec(13195) shouldBe List(5, 7, 13, 29)
    }
  }

  "primeFactors" should {
    "factorize number 100" in {
      p003largestPrimeFactor.primeFactorsRec(100) shouldBe List(2,2,5,5)
    }
  }

  "largestPrimeFactor" should {
    "be 29 for number 13195" in {
      p003largestPrimeFactor.largestPrimeFactor(13195) shouldBe 29
    }
  }

  "primeFactors" should {
    "factorize number 600851475143" in {
      p003largestPrimeFactor.primeFactorsRec(600851475143l) shouldBe List(71, 839, 1471, 6857)
    }
  }

  "largestPrimeFactor" should {
    "solve the task to ? for number 600851475143" in {
      p003largestPrimeFactor.largestPrimeFactor(600851475143l) shouldBe 6857
    }
  }

  "PrimeFactors Recursive" should {
    "factorize big prime number 3367900313" in {
      p003largestPrimeFactor.primeFactorsRec(58947631) shouldBe List(58947631)
    }
  }

  "primeFactors" should {
    "factorize big prime number 3367900313" in {
      p003largestPrimeFactor.largestPrimeFactor(58947631) shouldBe 58947631
    }
  }



}
