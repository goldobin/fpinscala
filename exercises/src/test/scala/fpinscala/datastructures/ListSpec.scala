package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {

  "A List" should "tail" in {

    val l1 = Cons(1, Nil)
    val l2 = Cons(1, Cons(2, Nil))
    val l3 = Cons(1, Cons(2, Cons(3, Nil)))

    List.tail(l1) === Nil
    List.tail(l2) === Cons(2, Nil)
    List.tail(l3) === Cons(1, Cons(2, Nil))
  }

  it should "drop" in {
    val l1 = Cons(1, Nil)
    val l2 = Cons(1, Cons(2, Nil))
    val l3 = Cons(1, Cons(2, Cons(3, Nil)))

    List.drop(l1, 1) === Nil

    a [IndexOutOfBoundsException] should be thrownBy {
      List.drop(l1, 2)
    }

    List.drop(l2, 1) === Cons(2, Nil)
    List.drop(l2, 2) === Nil

    a [IndexOutOfBoundsException] should be thrownBy {
      List.drop(l2, 3)
    }

    List.drop(l3, 1) === Cons(2, Cons(3, Nil))
    List.drop(l3, 2) === Cons(3, Nil)
    List.drop(l3, 3) === Nil

    a [IndexOutOfBoundsException] should be thrownBy {
      List.drop(l3, 4)
    }
  }

  it should "map" in {
    val l1 = Cons(1, Nil)
    val l2 = Cons(1, Cons(2, Nil))
    val l3 = Cons(1, Cons(2, Cons(3, Nil)))

    List.map[Int, Int](Nil)(_ + 1) === Nil
    List.map(l1) (_ + 1) === Cons(2, Nil)
    List.map(l2) { v => v + 1 } === Cons(2, Cons(3, Nil))
  }

  it should "length" in {

    val l1 = Cons(1, Nil)
    val l2 = Cons(1, Cons(2, Nil))
    val l3 = Cons(1, Cons(2, Cons(3, Nil)))

    List.length(Nil) === 0
    List.length(l1) === 1
    List.length(l2) === 2
    List.length(l3) === 3
  }
}
