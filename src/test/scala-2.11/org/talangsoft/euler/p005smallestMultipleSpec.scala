package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}

class p005smallestMultipleSpec extends WordSpec with ShouldMatchers {
  "factorialForMultipleNumbers" should {
    "return lsit for some 25" in {
      p005smallestMultiple.factorialForMultipleNumbers(List(25,25,25)) shouldBe List(5,5)
    }
  }

  "factorialForMultipleNumbers" should {
    "return list for 10 25 30" in {
      p005smallestMultiple.factorialForMultipleNumbers(List(10,25,30)) shouldBe List(2,3,5,5)
    }
  }

  "smallestMultiple" should {
    "return 2520 for list 1 to 10" in {
      p005smallestMultiple.smallestMultiple(1l to 10l toList) shouldBe 2520
    }
  }

  "smallestMultiple" should {
    "return ? for list 1 to 20" in {
      p005smallestMultiple.smallestMultiple(1l to 20l toList) shouldBe 232792560
    }
  }

}
