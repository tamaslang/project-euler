package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}

class p009specialPythagoreanTripletSpec extends WordSpec with ShouldMatchers{

  "isPythagoreanTriplet" should {
    "return true for pithagorean triplet 2^2 + 3^2 = 5^2" in {
      p009specialPythagoreanTriplet.isPythagoreanTriplet(3,4,5) shouldBe true
    }
  }

  "findPythagoreanTripletForTripletSummary" should {
    "return tuple 3,4,5 for summary 12" in {
      p009specialPythagoreanTriplet.findPythagoreanTripletForTripletSummary(12) shouldBe Some(3,4,5)
    }
  }


  "findPythagoreanTripletForTripletSummary" should {
    "return tuple (?,?,?) for summary 1000" in {
      p009specialPythagoreanTriplet.findPythagoreanTripletForTripletSummary(1000) shouldBe Some(200,375,425)
    }
  }

  "findPythagoreanTripletForTripletSummary product of abc" should {
    "solve the problem to ?" in {
      p009specialPythagoreanTriplet.findPythagoreanTripletForTripletSummary(1000)
        .fold()(triplet => {
          triplet._1 * triplet._2 * triplet._3 shouldBe 31875000l
        })
    }
  }
}
