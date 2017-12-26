package chemi

import scala.util.Try

case class IsotopeData (
  massNr: Int,
  atomicNr: Int = 0,
  abundance: Option[Double] = None,
  exactMass: Option[Double] = None
)


object IsotopeData {

  def get(e: Element, massNr: Int): Option[IsotopeData] =
    isotopes(e) get massNr

  def isotopes(e: Element): Map[Int, IsotopeData] = Try {
    data(e.atomicNr)
  } match {
    case e: IndexOutOfBoundsException ⇒ Map.empty
  }

  private val data: Array[Map[Int, IsotopeData]] = {
    ???
  }
}