package chemi

case class IsotopeData (
  massNr: Int,
  atomicNr: Int = 0,
  abundance: Option[Double] = None,
  exactMass: Option[Double] = None
)