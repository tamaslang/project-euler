package org.talangsoft.euler

import org.scalatest.{ShouldMatchers, WordSpec}

class p011largestProductInAGridSpec extends WordSpec with ShouldMatchers{

  "p011largestProductInAGrid" should {
    "load a matrix from resource" in {
      val matrix = p011largestProductInAGrid.loadMatrix("/p011grid.txt")
      matrix.length shouldBe 20
    }

    "parse matrix and return product" in {
      val matrix3x3 = Array(Array(1,2,3),
                            Array(4,5,6),
                            Array(7,8,9))
      val products = p011largestProductInAGrid.findProducts(matrix3x3,3)
      products(0).max shouldBe 45
    }

    "parse matrix from file and solve the task" in {
      val matrix20x20 = p011largestProductInAGrid.loadMatrix("/p011grid.txt")
      val products = p011largestProductInAGrid.findProducts(matrix20x20, 4)
      println(s"element containing max adjacent ${products.maxBy(_.max)}")
      products.maxBy(_.max).max shouldBe 70600674

    }
  }
}
