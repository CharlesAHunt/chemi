package chemi

package chemf

import collection.immutable.IntMap

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
    def single (ns: Seq[Node]): IsotopeData = {

      def scalarToEndo (ns: Seq[Node]): Endo[IsotopeData] =
        ns \ "@dictRef" text match {
          case "bo:atomicNumber" ⇒
            Endo(_ copy (atomicNr = ns.text.toInt))
          case "bo:exactMass" ⇒
            Endo(_ copy (exactMass = ns.text.toDouble.some))
          case "bo:relativeAbundance" ⇒
            Endo(_ copy (abundance = (ns.text.toDouble / 100D).some))
          case _ ⇒ Endo(identity)
        }

      (ns \ "scalar").toList foldMap scalarToEndo apply
        IsotopeData((ns \ "@number" text) toInt)
    }

    def isos = XML load getClass.getResourceAsStream("isotopes.xml")

    val res = Array.fill(Element.values.size)(
      IntMap.empty[IsotopeData]: Map[Int,IsotopeData]
    )

    (isos \ "isotopeList" \ "isotope") map single foreach
      {d ⇒ res(d.atomicNr) = res(d.atomicNr) + (d.massNr → d)}

    res
  }
}