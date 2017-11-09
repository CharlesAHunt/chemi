package chemi

case class IsotopeData (
  massNr: Int,
  atomicNr: Int = 0,
  abundance: Option[Double] = None,
  exactMass: Option[Double] = None
)


object IsotopeData {

  def get (e: Element, massNr: Int): Option[IsotopeData] =
    isotopes(e) get massNr

  def isotopes (e: Element): Map[Int,IsotopeData] = try {
    data(e.atomicNr)
  } catch {case e: IndexOutOfBoundsException ⇒ Map.empty}

  private[this] val data: Array[Map[Int,IsotopeData]] = {
    ???
  }
}