package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG  = SimpleRNG(nextSeed)
      val n = (nextSeed >>>16).toInt
      (n, nextRNG)
    }
  }
}