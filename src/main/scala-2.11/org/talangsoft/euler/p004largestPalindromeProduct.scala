package org.talangsoft.euler
import collection.breakOut
/**
  * A palindromic number reads the same both ways.
  * The largest palindrome made from the product of two 2-digit numbers is  = 91 × 99.
  * Find the largest palindrome made from the product of two 3-digit numbers.
  */
object p004largestPalindromeProduct {

  def isPalindrome(maybePalindrome:Long):Boolean = {
    def isPalindromeRec(maybePalindrome:String): Boolean = {
      if(maybePalindrome.size<2) true
      else maybePalindrome.head == maybePalindrome.last && isPalindromeRec(maybePalindrome.substring(1,maybePalindrome.size-1))
    }
    isPalindromeRec(maybePalindrome.toString)
  }


  def palindromes(begin: Int, end: Int): IndexedSeq[Int] =
    ((begin to end map (firstNumber => {firstNumber to end map (secondNumber => firstNumber * secondNumber) })) flatten) filter (isPalindrome(_)) sortWith (_ < _) distinct
}
