package calculator

object Polynomial {

  //Δ = b² - 4ac
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal ((b() * b()) - (4 * a() * c()))
  }

  //(-b ± √Δ) / 2a
  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    delta match {
      case noRoots if (a() == 0 || delta() < 0) => Set.empty
      case _ => def root(sqrDeltaUsed: Double) = (-1 * b() + sqrDeltaUsed) / 2 * a()
        Set(root(Math.sqrt(delta()))) ++ Set(root(-Math.sqrt(delta())))
    }
  }

  /*

  //(-b ± √Δ) / 2a
  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    if (a() == 0 || delta() < 0) Set.empty
    else {
      def root(sqrDeltaUsed: Double) = (-1*b() + sqrDeltaUsed) / 2*a()
      Set(root(Math.sqrt(delta())))++Set(root(-Math.sqrt(delta())))
    }
  }


  //(-b ± √Δ) / 2a
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    if (a() == 0) Set.empty
    else if (delta() < 0) Set.empty//no roots
    else {
      def root(sqrDeltaUsed: Double) = (-1*b() + sqrDeltaUsed) / 2*a()
      Set(root(Math.sqrt(delta())))++Set(root(-Math.sqrt(delta())))
    }
  }
  */



}
