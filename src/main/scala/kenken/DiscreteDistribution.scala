package kenken

import scala.util.Random._

trait DiscreteDistribution {
  def sample(): Int
}

case class Poisson(mean: Int) extends DiscreteDistribution {
  override def sample() = {
    val lambda = math.exp(-mean)
    var k = 0
    var p = 1.0
    do {
      k += 1
      p *= nextDouble
    } while (p >= lambda)
    k - 1
  }
}

case class Multinomial(xs: Double*) extends DiscreteDistribution {
  val cdf = (1 to xs.size).map(xs.map(_ / xs.sum).slice(0, _).sum)

  override def sample() = cdf.indexWhere(_ > nextDouble)

  override def toString = cdf.map("%.3f".format(_)).zipWithIndex.map(t => t._2 + ":" + t._1).mkString(" ")
}