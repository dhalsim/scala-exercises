package org.onetofive

import org.scalatest._
import OneToFive._

class OneToFiveSuite extends FunSuite {
  val list = List(1, 2, 3, 4, 5)

  test("1. get the last element of a list") {
    assert(last_procedural(list) === 5)
    assertThrows[NoSuchElementException] { last_procedural(Nil) }

    assert(last_recursive(list) === 5)
    assertThrows[NoSuchElementException] { last_recursive(Nil) }
  }

  test("2. get the second last element of a list") {
    assert(penultimate_procedural(list) === 4)
    assertThrows[NoSuchElementException] { penultimate_procedural(Nil) }

    assert(penultimate_recursive(list) === 4)
    assertThrows[NoSuchElementException] { penultimate_recursive(Nil) }

    assert(take_last_nth_procedural(list, 3) === 3)
    assertThrows[NoSuchElementException] { take_last_nth_procedural(Nil, 3) }
    assertThrows[IllegalArgumentException] { take_last_nth_procedural(list, 0) }

    assert(take_last_nth_recursive(list, 2) === 4)
    assertThrows[NoSuchElementException] { take_last_nth_recursive(Nil, 2) }
    assertThrows[IllegalArgumentException] { take_last_nth_recursive(list, 0) }
  }

  test("3. Find the Kth element of a list") {
    assert(nth_procedural(list, 2) === 2)
    assertThrows[NoSuchElementException] { nth_procedural(Nil, 2) }

    assert(nth_recursive(list, 2) === 2)
    assertThrows[NoSuchElementException] { nth_recursive(Nil, 2) }
  }

  test("4. Find the number of elements of a list") {
    assert(count_recursive(Nil) === 0)
    assert(count_with_fold(Nil) === 0)

    assert(count_recursive(list) === list.length)
    assert(count_with_fold(list) === list.length)
  }

  test("5. Reverse a list") {
    val reversed = list.reverse

    assert(reverse(list) === reversed)
    assert(reverse_recursive(list) === reversed)
    assert(reverse_tailrecursive(list) === reversed)
    assert(reverse_withfold(list) === reversed)
  }
}