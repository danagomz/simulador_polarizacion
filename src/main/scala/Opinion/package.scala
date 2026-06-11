package object Opinion {

  type SpecificBelief = Vector[Double]
  type GenericBelief = Int => SpecificBelief

  type AgentsPolMeasure =
    (SpecificBelief, Comete.DistributionValues) => Double
}
