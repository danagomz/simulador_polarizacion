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

  // Wrapper para conservar compatibilidad con rhoCMT_Gen
  def rhoAux(alpha: Double, beta: Double)
            (p: Double, dist: Distribution): Double = {

    val (pi, y) = dist
    rhoAuxValue(alpha, beta, pi, y, p)
  }


  // Seccion Integrante C - Borrador

  def rhoCMT_Gen(alpha: Double, beta: Double): MedidaPol = {
    (dist: Distribution) => {

      val curvaPolarizacion: Double => Double =
        (p: Double) => rhoAux(alpha, beta)(p, dist)

      val pOptimo = min_p(curvaPolarizacion, 0.0, 1.0, 0.0001)

      rhoAux(alpha, beta)(pOptimo, dist)
    }
  }
}