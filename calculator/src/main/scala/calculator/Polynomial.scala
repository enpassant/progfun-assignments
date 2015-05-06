package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal({
      val bVal = b()
      bVal * bVal - 4 * a() * c()
    })
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(delta() match {
      case d if (d < 0) => Set()
      case 0 => Set(-b() / 2 / a())
      case _ =>
        val sd = Math.sqrt(delta())
        Set((-b() + sd) / 2 / a(), (-b() - sd) / 2 / a())
    })
  }
}
