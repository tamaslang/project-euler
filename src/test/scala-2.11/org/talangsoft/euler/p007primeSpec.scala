package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}

class p007primeSpec extends WordSpec with ShouldMatchers{
  "isPrime" should {
    "return true for prime nr 2" in {
      p007prime.isPrime(2) shouldBe true
    }

    "return true for prime nr 3" in {
      p007prime.isPrime(3) shouldBe true
    }

    "return false for non-prime nr 1" in {
      p007prime.isPrime(1) shouldBe false
    }

    "return false for non-prime nr 0" in {
      p007prime.isPrime(0) shouldBe false
    }

    "return false for non-prime nr -1" in {
      p007prime.isPrime(-1) shouldBe false
    }

    "return false for non-prime nr 4" in {
      p007prime.isPrime(4) shouldBe false
    }

    "return false for non-prime nr 6" in {
      p007prime.isPrime(6) shouldBe false
    }

    "return true for prime nr 5" in {
      p007prime.isPrime(5) shouldBe true
    }

    "return true for prime nr 7" in {
      p007prime.isPrime(7) shouldBe true
    }

    "return false for non-prime nr 8" in {
      p007prime.isPrime(8) shouldBe false
    }

    "return false for non-prime nr 9" in {
      p007prime.isPrime(9) shouldBe false
    }
  }

  "firstNPrimes" should {
    "return first 6 primes" in {
      p007prime.firstNPrimes(6) shouldBe List(2, 3, 5, 7, 11, 13)
    }
  }

  "6th prime" should {
    "be 13" in {
      p007prime.firstNPrimes(6).last shouldBe 13
    }
  }

  "10001sd prime" should {
    "solve the problem to ?" in {
      p007prime.firstNPrimes(10001).last shouldBe 104743
    }
  }
}
