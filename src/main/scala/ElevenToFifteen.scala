package org.eleventofifteen

import scala.annotation.tailrec
import org.sixtoten.SixToTen.{ pack_foldRight => pack }

object ElevenToFifteen extends App {
  // assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  // author decides to use Either monad to represent a proper type system.
  // when your list has more than one consequtive element than your type is a tuple, in the other case is just the element
  // we can read this type List[Either[A, (Int, A)]] as => List of elements that either A or (Int, A)
  def encode_either[A](l: List[A]): List[Either[A, (Int, A)]] = {
    pack(l).map(e => if (e.length == 1) Left(e.head) else Right((e.length, e.head)))
  }

  // assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  // I just want my simple List, than I need to use more general type which in this case is Any
  def encode[A](l: List[A]): List[Any] = encode_either(l).map{ case Right(x) => x; case Left(x) => x }

  // assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  def decode[A](l: List[(Int, A)]): List[A] = l.flatMap{ case (length, s) => List.fill(length)(s) }

  // that was so easy. I want to try the either case
  def decode_either[A](l: List[Either[A, (Int, A)]]): List[A] = l.flatMap{
    case Right((length, s)) => List.fill(length)(s)
    case Left(s) => List(s)
  }

  // now it's time to write an encode function with a built-in scala function: span
  // assert(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  def encodeDirect[A](xs: List[A]): List[(Int, A)] = xs match {
    case Nil => Nil
    case _ => {
      val (first, rest) = xs.span(_ == xs.head)
      (first.length, first.head) :: encodeDirect(rest)
    }
  }

  // this version is better because it is now tail recursive
  // we should take attention to the order of the accumulator List that is added in reverse order in contrast to above
  def encodeDirect_tailRecursive[A](xs: List[A]): List[(Int, A)] = {
    @tailrec
    def iterate(acc: List[(Int, A)], ys: List[A]): List[(Int, A)] = ys match {
      case Nil => acc
      case _ => {
        val (first, rest) = ys.span(_ == ys.head)
        iterate((first.length, first.head) :: acc, rest)
      }
    }

    iterate(List(), xs).reverse
  }


  def duplicate[A](xs: List[A], n: Int = 2): List[A] = xs.flatMap(List.fill(n)(_))

  /*
   WHAT I LEARNED?

   * Either is a abstract type that can be used for exceptions or like in this case for conditional types
   * I can import and alias it like: original => alias
   * I can use default parameter values and override them when necessary

  */
}