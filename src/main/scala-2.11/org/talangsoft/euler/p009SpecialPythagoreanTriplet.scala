package org.talangsoft.euler

/**
  A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

  a2 + b2 = c2
  For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
 */
object p009SpecialPythagoreanTriplet {
  def isPythagoreanTriplet(a:Long, b: Long, c: Long) = a < b && b < c && (a*a + b*b == c*c)


  def findPythagoreanTripletForTripletSummary(sumOfNumbers:Long) : Option[(Long,Long,Long)] = {
    for(a<-1l to sumOfNumbers; b<-a+1 to sumOfNumbers; c=sumOfNumbers-a-b) {
      if(isPythagoreanTriplet(a,b,c)) { return Some(a,b,c)}
    }
    None
  }


}
