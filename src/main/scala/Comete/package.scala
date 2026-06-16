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

  //borrador funcion integrante B sin logica para que rhoCMT_Gen se pueda crear
  def rhoAux(p: Double, dist: Distribution): Double = {
    0.0
  }

  def rhoCMT_Gen(alpha: Double, beta: Double): MedidaPol = {
    (dist: Distribution) => {

      val curvaPolarizacion: Double => Double = (p: Double) => rhoAux(p, dist)
      val pOptimo = min_p(curvaPolarizacion, 0.0, 1.0, 0.0001)

      rhoAux(pOptimo, dist)
    }
  }
}
