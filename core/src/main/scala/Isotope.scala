package chemi.core

import cats.implicits._
import cats.Show
import cats.kernel.Eq
import mouse.all._

/**
  *
  */
sealed trait Isotope {

  def element: Element

  def massNr: Option[Int]

  lazy val iData: Option[IsotopeData] = massNr flatMap (IsotopeData get (element, _))

  lazy val mass = iData flatMap (_.exactMass) orElse element.mass

  lazy val exactMass = iData flatMap (_.exactMass) orElse element.exactMass

  override def toString: String =
    massNr.fold(element.toString)(_ + element.toString)

  lazy val isotopeDist: Seq[(Isotope, Double)] = iData cata (
    _.abundance cata (a ⇒ Seq(this → a), Seq.empty),
    element.isotopeDist
  )

  lazy val formulaDist: Seq[(Formula, Double)] =
    isotopeDist map { case (i,d) ⇒ (Map(i → 1), d)}
}

object Isotope {

  class IsotopEq extends Eq[(Element, Option[Int])] {
    override def eqv(x: (Element, Option[Int]), y: (Element, Option[Int])): Boolean = {
      Eq.eqv(x._1.atomicNr, y._1.atomicNr) && Eq.eqv(x._2, y._2)
    }
  }

  implicit val isotopEq = new IsotopEq

  def apply (e: Element): Isotope = elems(e.atomicNr)

  def apply (e: Element, massNumber: Int): Isotope = Impl (e, Option(massNumber))

  def apply (e: Element, massNumber: Option[Int]): Isotope =
    massNumber cata (apply(e, _), apply(e))

  implicit val IsotopeEqual: Eq[Isotope] =
    Eq by (i ⇒ (i.element, i.massNr))

  implicit val IsotopeShow: Show[Isotope] = Show.show(_.toString)

  private case class Impl(element: Element, massNr: Option[Int])
    extends Isotope

  //Flyweight isotopes for all elements
  private val elems = Element.values.map(a => Impl(a, None)).toArray
}