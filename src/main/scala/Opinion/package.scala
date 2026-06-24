package object Opinion {

  type SpecificBelief = Vector[Double]
  type GenericBelief = Int => SpecificBelief

  type AgentsPolMeasure =
    (SpecificBelief, Comete.DistributionValues) => Double

  def uniformBelief(nags: Int): SpecificBelief =
    Vector.tabulate(nags)(i => (i + 1).toDouble / nags.toDouble)

  def allExtremeBelief(nags: Int): SpecificBelief = {
    val mid = nags / 2
    Vector.tabulate(nags)(i => if (i < mid) 0.0 else 1.0)
  }

  def midlyBelief(nags: Int): SpecificBelief = {
    val mid = nags / 2

    Vector.tabulate(nags) { i =>
      if (i < mid)
        math.max(0.25 - 0.01 * (mid - i - 1), 0.0)
      else
        math.min(0.75 + 0.01 * (i - mid), 1.0)
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

}
