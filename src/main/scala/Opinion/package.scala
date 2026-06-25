package object Opinion {

  type SpecificBelief = Vector[Double]
  type GenericBelief = Int => SpecificBelief
  type WeightedGraph = (Int, Int) => Double
  type SpecificWeightedGraph = (WeightedGraph, Int)
  type GenericWeightedGraph = Int => SpecificWeightedGraph
  type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph) => SpecificBelief
  type AgentsPolMeasure =
    (SpecificBelief, Comete.DistributionValues) => Double

  def uniformBelief(nags: Int): SpecificBelief =
    Vector.tabulate(nags)(i => (i + 1).toDouble / nags.toDouble)

  def allExtremeBelief(nags: Int): SpecificBelief = {
    val mid = nags / 2
    Vector.tabulate(nags)(i => if (i < mid) 0.0 else 1.0)
  }

  def midlyBelief(nags: Int): SpecificBelief = {
    val middle = nags / 2

    Vector.tabulate(nags) { i =>
      if (i < middle)
        math.max(0.25 - 0.01 * (middle - i - 1), 0.0)
      else
        math.min(0.75 + 0.01 * (i - middle), 1.0)
    }
  }

  def allTripleBelief(nags: Int): SpecificBelief = {
    val one3 = nags / 3
    val two3 = (nags / 3) * 2

    Vector.tabulate(nags) { i =>
      if (i < one3) 0.0
      else if (i >= two3) 1.0
      else 0.5
    }
  }

  def consensusBelief(b: Double)(nags: Int): SpecificBelief =
    Vector.tabulate(nags)(_ => b)

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

  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
    val (influencia, n) = swg
    Vector.tabulate(n) { i =>
      Vector.tabulate(n) { j =>
        influencia(i, j)
      }
    }
  }

}
