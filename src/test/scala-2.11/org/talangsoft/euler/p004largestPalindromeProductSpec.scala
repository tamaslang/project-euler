package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}


class p004largestPalindromeProductSpec extends WordSpec with ShouldMatchers{

  "palindromes" should {
    "return palindromes made as product of 2 numbers for range 1-10" in {
      p004largestPalindromeProduct.palindromes(10,20) shouldBe Vector(121, 252, 272, 323)
    }
  }

  "palindromes" should {
    "should return largest palindrome 9009 for 2 digit numbers" in {
      p004largestPalindromeProduct.palindromes(10,99).last shouldBe 9009
    }
  }

  "palindromes" should {
    "should return largest palindrome ? for 3 digit numbers" in {
      p004largestPalindromeProduct.palindromes(100,999).last shouldBe 906609
    }
  }
}
