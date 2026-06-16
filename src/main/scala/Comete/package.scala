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
      val m1 = min + (max - min) / 3.0  // primer tercio
      val m2 = max - (max - min) / 3.0  // segundo tercio

      if (f(m1) < f(m2))
        min_p(f, min, m2, prec)         // descartar tercio derecho
      else
        min_p(f, m1, max, prec)         // descartar tercio izquierdo
    }
  }
}
