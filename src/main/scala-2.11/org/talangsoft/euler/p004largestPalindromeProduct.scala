package org.talangsoft.euler
import collection.breakOut
/**
  * A palindromic number reads the same both ways.
  * The largest palindrome made from the product of two 2-digit numbers is  = 91 × 99.
  * Find the largest palindrome made from the product of two 3-digit numbers.
  */
object p004largestPalindromeProduct {

  def palindromes(begin: Int, end: Int): IndexedSeq[Int] =
    ((begin to end map (firstNumber => {firstNumber to end map (secondNumber => firstNumber * secondNumber) })) flatten) filter (nr => {nr.toString.reverse == nr.toString}) sortWith (_ < _) distinct
}
