package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}

class p002evenFibonacciSpec extends WordSpec with ShouldMatchers {
  "fibonacci" should {
    "return fibonacci till n" in {
      p002evenFibonacci.fibonacci(100) shouldBe List(1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
    }
  }

  "evenFibonacciSum" should {
    "solve the task to ? for fibonacci till 4000000" in {
      p002evenFibonacci.evenFibonacciSum(4000000) shouldBe 4613732
    }
  }

}
