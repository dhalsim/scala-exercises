import org.scalatest.FunSuite

import org.sixteentotwenty.SixteenToTwenty._

class SixteentoTwentySuite extends FunSuite {
  val alphabetList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

  test("16. Drop every Nth element from a list") {
    assert(dropEvery(3)(alphabetList) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("17. Split a list into two parts") {
    assert(split(3, alphabetList) == (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    assert(split_takeDrop(3, alphabetList) == (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    assert(split_recursive(3, alphabetList) == (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

    assert(split(15, alphabetList) == (alphabetList, List()))
    assert(split_takeDrop(15, alphabetList) == (alphabetList, List()))
    assert(split_recursive(15, alphabetList) == (alphabetList, List()))
  }

  test("18. Extract a slice from a list") {
    assert(slice(3, 7, alphabetList) == List('d, 'e, 'f, 'g))
    assert(slice_takeDrop(3, 7, alphabetList) == List('d, 'e, 'f, 'g))
    assert(slice_recursive(3, 7, alphabetList) == List('d, 'e, 'f, 'g))
  }

  test("19. Rotate a list N places to the left") {
    assert(rotate(3, alphabetList) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    assert(rotate(-2, alphabetList) == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  test("20. Remove the Kth element from a list") {
    assert(removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd), Some('b)))
    assert(removeAt(4, List('a, 'b, 'c, 'd)) == (List('a, 'b, 'c, 'd), None))
  }
}
