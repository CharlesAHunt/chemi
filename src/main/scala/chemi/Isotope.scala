package chemi

import cats.Show
import cats.kernel.Eq
import chemi.chemf.IsotopeData

sealed trait Isotope {
  def element: Element

  def massNr: Option[Int]

  lazy val iData = massNr flatMap (IsotopeData get (element, _))

  lazy val mass = iData flatMap (_.exactMass) orElse element.mass

  lazy val exactMass = iData flatMap (_.exactMass) orElse element.exactMass

  override def toString: String =
    massNr.fold(element.toString)(_ + element.toString)

  lazy val isotopeDist: Seq[(Isotope,Double)] = iData cata (
    _.abundance cata (a ⇒ Seq(this → a), Seq.empty),
    element.isotopeDist
  )

  lazy val formulaDist: Seq[(Formula,Double)] =
    isotopeDist map {case (i,d) ⇒ (Map(i → 1), d)}
}

object Isotope {

  def apply (e: Element): Isotope = elems(e.atomicNr)

  def apply (e: Element, mn: Int): Isotope = Impl (e, Some(mn))

  def apply (e: Element, mn: Option[Int]): Isotope =
    mn cata (apply(e, _), apply(e))

  implicit val IsotopeEqual: Eq[Isotope] =
    Eq by (i ⇒ (i.element, i.massNr))

  implicit val IsotopeShow: Show[Isotope] = Show.show(_.toString)

  private case class Impl(element: Element, massNr: Option[Int])
    extends Isotope

  //Flyweight isotopes for all elements
  private[this] val elems = Element.values map (Impl(_, None)) toArray
}