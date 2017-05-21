package org.onetofive

import scala.annotation.tailrec

object OneToFive extends App {
  def last_procedural[A](xs: List[A]): A = xs.last

  def last_recursive[A](xs: List[A]): A = xs match {
    case x :: Nil => x
    case head :: tail => last_recursive(tail)
    case _ => throw new NoSuchElementException
  }

  def penultimate_procedural[A](xs: List[A]): A = if(xs.isEmpty) throw new NoSuchElementException else xs.init.last

  def penultimate_recursive[A](xs: List[A]): A = xs match {
    case head :: List(x) => head
    case head :: tail => penultimate_recursive(tail)
    case _ => throw new NoSuchElementException
  }

  def take_last_nth_procedural[A](xs: List[A], n: Int): A = {
    if(xs.isEmpty) throw new NoSuchElementException
    if(n < 1) throw new IllegalArgumentException

    xs.takeRight(n).head
  }

  // first I thought that is smart to take the last element as looking to (length - index)
  // but then I realized that Lists doesn't have a normal length property, it is calculated
  // again and again recursively through traversing the whole List. So just a small optimization:
  def take_last_nth_recursive[A](xs: List[A], n: Int): A = {
    val l = xs.length

    def iter[A](ys: List[A], n: Int): A = ys match {
      case _ if(n < 1) => throw new IllegalArgumentException
      case _ :: tail if(l - n != 0) => take_last_nth_recursive(tail, n)
      case head :: _ if(l - n == 0) => head
      case _ => throw new NoSuchElementException
    }

    iter(xs, n)
  }

  // even better
  def nth_procedural[A](xs: List[A], n: Int): A = xs.takeRight(xs.length - n + 1).head

  def nth_recursive[A](xs: List[A], n: Int): A = xs match {
    case _ if(n < 1) => throw new IllegalArgumentException
    case _ if(xs.length == 0) => throw new NoSuchElementException
    case head :: _ if(n == 1) => head
    case _ :: tail => nth_recursive(tail, n - 1)
  }

  // oh yay! tail recursive
  def count_recursive[A](xs: List[A]): Int = {
    @tailrec
    def iter(ys: List[A], n: Int): Int = ys match {
      case Nil => n
      case _ :: tail => iter(tail, n + 1)
    }

    iter(xs, 0)
  }

  // even better
  def count_with_fold[A](xs: List[A]): Int = xs.foldLeft(0){ (acc, _) => acc + 1 }

  // the obvious
  def reverse[A](xs: List[A]): List[A] = xs.reverse

  // not tail recursive
  def reverse_recursive[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case head :: tail => reverse_recursive(tail) ::: List(head)
  }

  // better
  def reverse_tailrecursive[A](xs: List[A]): List[A] = {
    @tailrec
    def iter(acc: List[A], ys: List[A]): List[A] = ys match {
      case Nil => acc
      case head :: tail => iter(head :: acc, tail)
    }

    iter(Nil, xs)
  }
  // even better
  def reverse_withfold[A](xs: List[A]): List[A] = xs.foldLeft(Nil: List[A]){ (ys, x) => x :: ys }

  /*
   WHAT I LEARNED?

   * You can check thrown exceptions in your tests with assertThrows
   * you could add a @tailrec annotation so that you can be sure that your function is optimized by compiler as a tail recursive function.
   * in Lists nearly every function's complexity except head is linear. So better think twice when you want to write your own recursive functions
   * You can use fold or take functions for your recursive needs!

  */
}