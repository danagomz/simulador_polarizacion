package object Comete {
  
  type DistributionValues = Vector[Double]
  type Frequency = Vector[Double]
  type Distribution = (Frequency, DistributionValues)
  type MedidaPol = Distribution => Double

}
