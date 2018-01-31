package fpinscala.state

import org.scalatest.{FlatSpec, Matchers}

class RngSpec extends FlatSpec with Matchers {

  "A Rng" should "produce the same random number when called twice" in {

    val seed = RNG.Simple(1)

    seed.nextInt === seed.nextInt
    RNG.ints(10)(seed) === RNG.ints(10)(seed)
  }

  it should "generate ints" in {
    val seed = RNG.Simple(2)

    val (ints, rng) = RNG.ints(10)(seed)

    ints.length === 10
  }

  it should "map2" in {

    case class Pair (v1: Int, v2: Double)

    val pairRng = RNG.map2(RNG.int, RNG.double)(Pair)


    val seed = RNG.Simple(2)
    pairRng(seed) === pairRng(seed)
  }

  it should "sequence" in {

    val seq = RNG.sequence(List.fill(10)(RNG.int))

    val seed = RNG.Simple(3)
    val (values, rng) = seq(seed)

    values.length === 10
  }

  it should "flatMap" in {
     val g = RNG.flatMap(RNG.nonNegativeInt) { i => RNG.ints(i % 30) }

    val seed = RNG.Simple(4)

    val (ints, gen) = g(seed)

    ints.length should be < 30

    g(seed) === g(seed)
  }
}
