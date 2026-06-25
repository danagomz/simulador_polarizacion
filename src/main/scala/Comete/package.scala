package object Comete {

  type DistributionValues = Vector[Double]
  type Frequency = Vector[Double]
  type Distribution = (Frequency, DistributionValues)
  type MedidaPol = Distribution => Double

  @annotation.tailrec
  def min_p(
             f: Double => Double,
             min: Double,
             max: Double,
             prec: Double
           ): Double = {

    if (max - min < prec)
      (min + max) / 2.0
    else {
      val m1 = min + (max - min) / 3.0 // primer tercio
      val m2 = max - (max - min) / 3.0 // segundo tercio

      if (f(m1) < f(m2))
        min_p(f, min, m2, prec) // descartar tercio derecho
      else
        min_p(f, m1, max, prec) // descartar tercio izquierdo
    }
  }

  def rhoAuxValue(
                   alpha: Double,
                   beta: Double,
                   pi: Frequency,
                   y: DistributionValues,
                   p: Double
                 ): Double = {

    pi.zip(y).map { case (piI, yI) =>
      math.pow(piI, alpha) * math.pow(math.abs(yI - p), beta)
    }.sum
  }

  def rhoCMT_Gen(alpha: Double, beta: Double): MedidaPol = {
    (dist: Distribution) => {
      val (pi, y) = dist

      val rhoAux: Double => Double =
        (p: Double) => rhoAuxValue(alpha, beta, pi, y, p)

      val pOpt = min_p(rhoAux, 0.0, 1.0, 0.0001)

      rhoAux(pOpt)
    }
  }

  def normalizar(m: MedidaPol): MedidaPol = {
    (dist: Distribution) => {
      val (_, y) = dist

      val piPeorCaso: Frequency = Vector.tabulate(y.length) { i =>
        if (i == 0 || i == y.length - 1) 0.5 else 0.0
      }

      val peorCaso = m((piPeorCaso, y))
      val resultado = m(dist)

      if (peorCaso == 0.0) 0.0
      else resultado / peorCaso
    }
  }

}