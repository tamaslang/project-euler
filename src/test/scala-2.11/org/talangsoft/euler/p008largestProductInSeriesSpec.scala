package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}

/**
  * Created by admin on 26/11/2015.
  */
class p008largestProductInSeriesSpec extends WordSpec with ShouldMatchers{
  val thousandDigitNumber =
  """73167176531330624919225119674426574742355349194934
  |96983520312774506326239578318016984801869478851843
  |85861560789112949495459501737958331952853208805511
  |12540698747158523863050715693290963295227443043557
  |66896648950445244523161731856403098711121722383113
  |62229893423380308135336276614282806444486645238749
  |30358907296290491560440772390713810515859307960866
  |70172427121883998797908792274921901699720888093776
  |65727333001053367881220235421809751254540594752243
  |52584907711670556013604839586446706324415722155397
  |53697817977846174064955149290862569321978468622482
  |83972241375657056057490261407972968652414535100474
  |82166370484403199890008895243450658541227588666881
  |16427171479924442928230863465674813919123162824586
  |17866458359124566529476545682848912883142607690042
  |24219022671055626321111109370544217506941658960408
  |07198403850962455444362981230987879927244284909188
  |84580156166097919133875499200524063689912560717606
  |05886116467109405077541002256983155200055935729725
  |71636269561882670428252483600823257530420752963450""".stripMargin.replaceAll("\n", "")


  "largestProduct" should {
    "return 5832 for 4 adjacent digits in 1000 digit number" in {
      p008largestProductInSeries.adjacentDigitWithGreatestProduct(4,thousandDigitNumber) shouldBe "9989"
    }
  }

  "largestProduct" should {
    "throw exception for an input with not enough digits" in {
      an [IllegalArgumentException] should be thrownBy {
        p008largestProductInSeries.adjacentDigitWithGreatestProduct(4,"123")
      }
    }
  }

  "largestProduct" should {
    "return the number with same adjacent digits as number length" in {
      p008largestProductInSeries.adjacentDigitWithGreatestProduct(4,"1234") shouldBe "1234"
    }
  }

  "largestProduct" should {
    "find the greatest 13 adjacent digits to multiply in the 1000 digit number" in {
      p008largestProductInSeries.adjacentDigitWithGreatestProduct(13,thousandDigitNumber) shouldBe "5576689664895"
    }
  }

  "largestProduct" should {
    "solve the problem to ? for 13 adjacent digits in the 1000 digit number" in {
      p008largestProductInSeries.adjacentDigitWithGreatestProductValue(13,thousandDigitNumber) shouldBe 23514624000l
    }
  }

  "largestProduct" should {
    "be able to handle large numbers" in {
      p008largestProductInSeries.adjacentDigitWithGreatestProductValue(13,"7536978179778") shouldBe 7841473920l
    }
  }

}
