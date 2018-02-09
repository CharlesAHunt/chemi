package chemi

import com.typesafe.scalalogging.LazyLogging

case class IsotopeData (
  massNr: Int,
  atomicNr: Int = 0,
  abundance: Option[Double] = None,
  exactMass: Option[Double] = None
)

object IsotopeData extends LazyLogging {

  def get(e: Element, massNr: Int): Option[IsotopeData] =
    isotopes(e).find(_.massNr == massNr)

  def isotopes(e: Element): List[IsotopeData] =
    data.getOrElse(e.atomicNr, List.empty[IsotopeData])

  //TODO
  private val data: Map[Int, List[IsotopeData]] = {
    Map.empty[Int, List[IsotopeData]]
  }

}