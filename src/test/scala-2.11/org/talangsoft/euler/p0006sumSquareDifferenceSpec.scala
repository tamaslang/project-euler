package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}

class p0006sumSquareDifferenceSpec extends WordSpec with ShouldMatchers{
  "sumOfSquares" should {
    "return 385 for first 10 numbers" in {
      p006sumSquareDifference.sumOfSquares(10) shouldBe 385
    }
  }

  "squareOfSums" should {
    "return 3025 for first 10 numbers" in {
      p006sumSquareDifference.squareOfSum(10) shouldBe 3025
    }
  }

  "difference" should {
    "return 2640 for first 10 numbers" in {
      p006sumSquareDifference.difference(10) shouldBe 2640
    }
  }

  "difference" should {
    "return ? for first 100 numbers" in {
      p006sumSquareDifference.difference(100) shouldBe 25164150
    }
  }
}
