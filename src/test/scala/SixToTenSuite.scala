import org.scalatest._
import org.sixtoten.SixToTen._

class SixToTenSuite extends FunSuite {
  val poli_list = List(1, 2, 3, 2, 1)
  val not_poli_list = List(1, 2, 3, 4)

  test("6. Find out whether a list is a palindrome") {
    assert(isPolindrome(poli_list))
    assert(!isPolindrome(not_poli_list))
    assert(isPolindrome(Nil))

    assert(isPolindrome_recursive(poli_list))
    assert(!isPolindrome_recursive(not_poli_list))
    assert(isPolindrome_recursive(Nil))

    assert(isPolindrome_recursive2(poli_list))
    assert(!isPolindrome_recursive2(not_poli_list))
    assert(isPolindrome_recursive2(Nil))
  }

  test("7. Flatten a nested list structure.") {
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
    assert(flatten(List(List(1, List(1)), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  }

  test("8. Eliminate consecutive duplicates of list elements") {
    assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
    assert(compress_foldLeft(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
    assert(compress_foldRight(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("9. Pack consecutive duplicates of list elements into sublists ") {
    assert(pack_foldRight(List()) == List(List()))
    assert(pack_foldRight(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("10. Run-length encoding of a list") {
    assert(encode_foldRight(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
}