package org.sixtoten

import scala.annotation.tailrec

object SixToTen {
  def isPolindrome[A](xs: List[A]): Boolean = xs == xs.reverse

  // in the scala 99 problems blog, the author wants to make it tail recursive even it is already, so I put this annotation here
  @tailrec
  def isPolindrome_recursive[A](xs: List[A]): Boolean = xs match {
    case Nil => true
    case head :: Nil => true
      // it didn't matter on tailrec to use && operator or if statement like below
    case head :: tail => (tail.last == head) && isPolindrome_recursive(tail.init)
  }

  // scala 99 problems blog author creates an extractor for head, last and init
  // then he uses that in pattern matching. too much extra steps for me but just to learn it I'll do it, too
  @tailrec
  def isPolindrome_recursive2[A](xs: List[A]): Boolean = xs match {
    case Nil => true
    case head :: Nil => true
    case hli(head, last, init) => if(last != head) false else isPolindrome_recursive2(init)
  }

  // flatten(List(List(1, 1), 2, List(3, List(5, 8))))

  // i wanted to make it a little bit interesting by adding an extra level of List
  // but it shouldn't matter because we are going to make it recursive

  // flatten(List(List(1, List(1)), 2, List(3, List(5, 8))))

  // i didn't know that lists can be non-homogenious
  // thanks to type inference of scala, the type of the list is in that case List[Any], not List[Int]
  // so you just couldn't use any type patterns in your pattern matches
  def flatten(l: List[Any]): List[Any] = l flatMap {
    case ls: List[_] => flatten(ls)
    case h => List(h)
  }

  // compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e)
  def compress[A](xs: List[A]): List[A] = {
    @tailrec
    def iter(prev: A, rest: List[A], acc: List[A]): List[A] = rest match {
      case Nil => acc
      case head :: tail => if(head == prev) iter(head, tail, acc) else iter(head, tail, head :: acc)
    }

    iter(xs.head, xs.tail, List(xs.head)).reverse
  }

  // compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e)
  def compress_foldLeft[A](xs: List[A]): List[A] = xs.foldLeft(List[A]()){
    case (acc, x) if acc.isEmpty => List(x)
    case (acc, x) => if(acc.head != x) x :: acc else acc
  }.reverse

  // compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e)
  // override def foldRight[B](z: B)(op: (A, B) => B): B =
  //    reverse.foldLeft(z)((right, left) => op(left, right))
  def compress_foldRight[A](xs: List[A]): List[A] = xs.foldRight(List[A]()){
    case (x, acc) if acc.isEmpty => List(x)
    case (x, acc) => if(acc.head != x) x :: acc else acc
  }

  // pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  def pack_foldRight[A](xs: List[A]): List[List[A]] = xs.foldRight(List[List[A]](List[A]())){
    case (x, acc) => {
      val hxs = acc.head

      if(hxs.isEmpty) List(x) :: acc.tail
      else if(hxs.head == x) (x :: hxs) :: acc.tail
      else List(x) :: acc
    }
  }

  //encode_foldRight(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  def encode_foldRight(xs: List[Symbol]): List[(Int, Symbol)] = xs.foldRight(List[(Int, Symbol)]((0, null))) {
    case (x, acc: List[(Int, Symbol)]) => {
      val tpl = acc.head

      if(tpl._2 == null) (1, x) :: acc.tail
      else if(tpl._2 == x) (tpl._1 + 1, x) :: acc.tail
      else (1, x) :: acc
    }
  }
}

object hli {
  def unapply[A](arg: List[A]): Option[(A, A, List[A])] = arg match {
    case Nil => None
    case head :: Nil => Some(head, head, Nil)
    case head :: tail => Some(head, tail.last, tail.init)
  }
}

/*
   WHAT I'VE LEARNED?

   * You can create extractors (see hli) to use in pattern matches
   * It turns out that "foldRight" is same as using "reverse + foldLeft" for a List type. It is cheaper to reverse the
      list once and foldLeft than to access the last element in every iteration. If we would work for a Vector type
      than it would probably make sence to use the original foldRight function because you can directly access with an index
   * I had one seen Symbols in Ruby, too. I haven't understand it and now I have to! To be able to compare strings faster,
      and not character by character, we can use symbols instead: same symbols are represented in the
      same memory (interning). So it is O(1) to compare Symbols. see: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#intern()
*/