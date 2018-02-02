package fpinscala.state

import org.scalatest.{FlatSpec, MustMatchers}

class StateSpec extends FlatSpec with MustMatchers {
  "A State" should "sequence states" in {

    val plus2 = State { s: Int =>
      val newS = s + 2
      ("+ 2", newS)
    }

    val multiplyBy2 = State { s: Int =>
      val newS = s * 2
      ("* 2", newS)
    }

    val seq1 = State.sequence(List(plus2, multiplyBy2))
    val seq2 = State.sequence(List(multiplyBy2, plus2))

    val (opList1, state1) = seq1.run(10)
    val (opList2, state2) = seq2.run(10)

    opList1 must equal(List("+ 2", "* 2"))
    state1 must equal(24)

    opList2 must equal(List("* 2", "+ 2"))
    state2 must equal(22)
  }

  it should "simulate candy machine" in {

    val inputs: List[Input] = {
      for {
        _ <- 0 until 4
        input <- List(Coin, Turn)
      } yield input
    }.toList

    val transformation1 = State.simulateMachine(inputs)
    val transformation2 = State.simulateMachine(Coin :: inputs)
    val transformation3 = State.simulateMachine(Turn :: inputs)

    val initialState = Machine(locked = true, coins = 10, candies = 5)
    val expectedEndState = Machine(locked = true, coins = 14, candies = 1)
    val expectedEndValue = (14, 1)

    val expectedResult = (expectedEndValue, expectedEndState)

    List(transformation1, transformation2, transformation3)
      .map(_.run(initialState))
      .foreach { v =>
        v must equal(expectedResult)
      }
  }
}
