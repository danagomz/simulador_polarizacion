package object Opinion {
  import scala.collection.parallel.CollectionConverters._
  import common._

  type SpecificBelief = Vector[Double]
  type GenericBeliefConf = Int => SpecificBelief
  type WeightedGraph = (Int, Int) => Double
  type SpecificWeightedGraph = (WeightedGraph, Int)
  type GenericWeightedGraph = Int => SpecificWeightedGraph
  type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph) => SpecificBelief
  type AgentsPolMeasure =
    (SpecificBelief, Comete.DistributionValues) => Double

  def betaFactor(bi: Double, bj: Double): Double =
    1.0 - math.abs(bj - bi)

  def contribucion(
                    i: Int,
                    j: Int,
                    sb: SpecificBelief,
                    influencia: WeightedGraph
                  ): Double = {

    val bi = sb(i)
    val bj = sb(j)
    val Iji = influencia(j, i)
    val beta_ij = betaFactor(bi, bj)

    beta_ij * Iji * (bj - bi)
  }

  def calcularCajones(dist: Comete.DistributionValues): Vector[(Double, Double)] = {
    val k = dist.length

    Vector.tabulate(k) { i =>
      val lo =
        if (i == 0) 0.0
        else (dist(i - 1) + dist(i)) / 2.0

      val hi =
        if (i == k - 1) 1.0
        else (dist(i) + dist(i + 1)) / 2.0

      (lo, hi)
    }
  }

  def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
    val medida = Comete.normalizar(Comete.rhoCMT_Gen(alpha, beta))

    (sb: SpecificBelief, dist: Comete.DistributionValues) => {
      val n = sb.length.toDouble
      val k = dist.length
      val cajones = calcularCajones(dist)

      val frecuencias: Comete.Frequency = Vector.tabulate(k) { i =>
        val (lo, hi) = cajones(i)
        val count =
          if (i == k - 1)
            sb.count(op => op >= lo && op <= hi)
          else
            sb.count(op => op >= lo && op < hi)

        count.toDouble / n
      }

      medida((frecuencias, dist))
    }
  }

  def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {
    val medida = Comete.normalizar(Comete.rhoCMT_Gen(alpha, beta))

    (sb: SpecificBelief, dist: Comete.DistributionValues) => {
      val n = sb.length.toDouble
      val k = dist.length
      val cajones = calcularCajones(dist)

      val frecuencias: Comete.Frequency = Vector.tabulate(k) { i =>
        val (lo, hi) = cajones(i)

        val count =
          if (i == k - 1)
            sb.par.count(op => op >= lo && op <= hi)
          else
            sb.par.count(op => op >= lo && op < hi)

        count.toDouble / n
      }

      medida((frecuencias, dist))
    }
  }

  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
    val (influencia, n) = swg
    Vector.tabulate(n) { i =>
      Vector.tabulate(n) { j =>
        influencia(i, j)
      }
    }
  }
  def vecindades(swg: SpecificWeightedGraph): Vector[Vector[Int]] = {
    val (influencia, n) = swg
    Vector.tabulate(n)(i => (0 until n).filter(j => influencia(j, i) > 0.0).toVector)
  }


  def nuevaCreencia(
                     i: Int,
                     sb: SpecificBelief,
                     influencia: WeightedGraph,
                     vecinos: Vector[Int]
                   ): Double = {
    val bi = sb(i)

    if (vecinos.isEmpty) bi
    else {
      val suma = vecinos.map { j =>
        contribucion(i, j, sb, influencia)
      }.sum

      bi + suma / vecinos.length.toDouble
    }
  }

  def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    val (influencia, n) = swg
    val vecinos = vecindades(swg)

    Vector.tabulate(n)(i => nuevaCreencia(i, sb, influencia, vecinos(i)))
  }

  def simulate(
                fu: FunctionUpdate,
                swg: SpecificWeightedGraph,
                b0: SpecificBelief,
                t: Int
              ): IndexedSeq[SpecificBelief] = {
    Vector.iterate(b0, t + 1)(b => fu(b, swg))
  }

  def confBiasUpdatePar(
                         sb: SpecificBelief,
                         swg: SpecificWeightedGraph
                       ): SpecificBelief = {
    val (influencia, n) = swg

    def sumaVecinosPar(i: Int, vecinos: IndexedSeq[Int]): Double = {
      if (vecinos.isEmpty) 0.0
      else if (vecinos.length <= 32) {
        vecinos.map(j => contribucion(i, j, sb, influencia)).sum
      } else {
        val mid = vecinos.length / 2
        val (izq, der) = vecinos.splitAt(mid)

        val (sumaIzq, sumaDer) = parallel(
          sumaVecinosPar(i, izq.toIndexedSeq),
          sumaVecinosPar(i, der.toIndexedSeq)
        )

        sumaIzq + sumaDer
      }
    }

    def construir(desde: Int, hasta: Int): Vector[Double] = {
      val len = hasta - desde

      if (len <= 32) {
        Vector.tabulate(len) { offset =>
          val i = desde + offset
          val bi = sb(i)
          val vecinos = (0 until n).filter(j => influencia(j, i) > 0.0).toIndexedSeq

          if (vecinos.isEmpty) bi
          else bi + sumaVecinosPar(i, vecinos) / vecinos.length.toDouble
        }
      } else {
        val medio = desde + len / 2

        val (izq, der) = parallel(
          construir(desde, medio),
          construir(medio, hasta)
        )

        izq ++ der
      }
    }
    construir(0, n)
  }
}