package org.sixteentotwenty

import scala.annotation.tailrec

object SixteenToTwenty {
  // assert(dropEvery(3)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  // this funny syntax called curried arguments. See: https://en.wikipedia.org/wiki/Currying, which has a quite famous
  // founder with the name: Haskell Curry! A further research subject: partial application

  // to understand zipWithIndex, look at this code snippet in scala console
  // scala> List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
  // res0: List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
  // scala> res0.zipWithIndex
  // res1: List[(Symbol, Int)] = List(('a,0), ('b,1), ('c,2), ('d,3), ('e,4), ('f,5), ('g,6), ('h,7), ('i,8), ('j,9), ('k,10))

  // first I thought about using takeWhile or dropWhile but they are only concerned with the first elements of the collection
  def dropEvery[A](n: Int)(xs: List[A]): List[A] = xs.zipWithIndex.filter{ case (e, i) => (i+1) % 3 != 0 }.map{ _._1 }
  // check the original solution, too
  // def dropEvery[A](n: Int)(xs: List[A]): List[A] = xs.grouped(n).flatMap { _.take(n - 1) }.toList
  // scala> res0.grouped(3).toList
  // res2: List[List[Symbol]] = List(List('a, 'b, 'c), List('d, 'e, 'f), List('g, 'h, 'i), List('j, 'k))

  // split using standart library
  def split[A](n: Int, xs: List[A]): (List[A], List[A]) = xs.splitAt(n)

  // split using take and drop functions
  def split_takeDrop[A](n: Int, xs: List[A]): (List[A], List[A]) = (xs.take(n), xs.drop(n))

  // this solution may seem a little strange, the first case is for an index that greater than the size of the list
  // we need to drop elements one by one until we reach the index, of course we need to reverse the left collection
  def split_recursive[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    @tailrec
    def iterate(left: List[A], right: List[A], i: Int): (List[A], List[A]) = (right, i) match {
      case (Nil, _) => (left.reverse, right)
      case (_, 0) => (left.reverse, right)
      case (head :: tail, c) => iterate(head :: left, tail, c - 1)
    }

    iterate(List(), xs, n)
  }

  // slice using standart library
  def slice[A](s: Int, e: Int, xs: List[A]): List[A] = xs.slice(s, e)

  // slice using take and drop functions
  def slice_takeDrop[A](s: Int, e: Int, xs: List[A]): List[A] = xs.drop(s).take(xs.length - e)

  // recursive solution: until we reach the starting index, we drop elements without adding them to the accumulator
  // after reacing the starting index, we add to the accumulater until we reach the end of the List or the end index
  // of course we need to reverse the accumulator
  def slice_recursive[A](s: Int, e: Int, xs: List[A]): List[A] = {
    @tailrec
    def iterate(c: Int, ys: List[A], acc: List[A]): List[A] = (ys, c) match {
      case (Nil, _) => acc
      case (_, i) if i >= e => acc
      case (head :: tail, i) => {
        if(i >= s) iterate(i + 1, tail, head :: acc)
        else iterate(i + 1, tail, acc)
      }
    }

    iterate(0, xs, List()).reverse
  }

  // assert(rotate(3, alphabetList) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  // assert(rotate(-2, alphabetList) == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  def rotate[A](n: Int, xs: List[A]): List[A] = {
    def iterate(i: Int, acc: List[A]): List[A] = (acc, i) match {
      case (_, 0) => acc
      case (head :: tail, c) if c > 0 => iterate(c - 1, tail ::: List(head))
      case (init :+ last, c) if c < 0 => iterate(c + 1, last :: init)
    }

    iterate(n, xs)
  }

  // assert(removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd), Some('b)))
  // assert(removeAt(4, List('a, 'b, 'c, 'd)) == (List('a, 'b, 'c, 'd), None))
  // I want to change the behaviour a little bit: instead of getting java.util.NoSuchElementException, I will return
  // a None value which is the functional way of returning null value. If not None, we need to return Some.
  def removeAt[A](i: Int, xs: List[A]): (List[A], Option[A]) = {
    val dropped = xs.drop(i)
    if(dropped.isEmpty) (xs, None) else (xs.take(i) ::: dropped.tail, Some(dropped.head))
  }

  /*
    WHAT I LEARNED?

   * I learned zipWithIndex and know there are the functions like take, takeWhile, drop and dropWhile
   * Saw the grouped function showed in the original article
   * For the slice and removeAt functions, random access collections would be more efficient.
   * :+ can be used for pattern match to get the List.init and List.last
   * I used a functional monad type called Option to able to return a None value.
       This None, Some pattern is called Option Monad which is very easy to use and very helpful in our programs.

  */
}
