package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}

class p010summationOfPrimesSpec extends WordSpec with ShouldMatchers {
  "sumOfPrimesUnder" should {
    "return 17 for primes under 10" in {
      p010SummationOfPrimes.sumOfPrimesUnder(10) shouldBe 17
    }

    "solve problem to ? for primes under 2million" in {
      p010SummationOfPrimes.sumOfPrimesUnder(2000000) shouldBe 142913828922L
    }
  }
}
