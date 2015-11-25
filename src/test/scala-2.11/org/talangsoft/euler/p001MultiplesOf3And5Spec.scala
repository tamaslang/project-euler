package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}

class p001MultiplesOf3And5Spec extends WordSpec with ShouldMatchers{
  "multiplesOf3And5" should {
    "return 23 for numbers below 10" in {
      p001MultiplesOf3And5.multiplesOf3And5(1,9) shouldBe 23
    }
  }

  "multiplesOf3And5" should {
    "solve the task to ? for numbers below 1000" in {
      p001MultiplesOf3And5.multiplesOf3And5(1,999) shouldBe 233168
    }
  }
}
