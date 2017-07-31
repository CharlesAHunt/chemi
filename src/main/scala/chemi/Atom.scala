package chemi

import Element.H
import cats._, cats.instances.all._, cats.syntax.cartesian._

/**
  *
  * @param isotope
  * @param charge
  * @param hydrogens
  * @param stereo
  */
case class Atom (
  isotope: Isotope,
  charge: Int,
  hydrogens: Int,
  stereo: Stereo
) {
  require(hydrogens >= 0, "Hydrogen count must be >= 0")

  def element: Element = isotope.element

  def exactMass: Option[Double] =
    isotope.exactMass |@| H.exactMass map (_ + hydrogens * _)

  def formula: Formula =
    Map(isotope → 1) ++ (if(hydrogens != 0) Map(Isotope(H) → hydrogens) else Map.empty)

  def mass: Option[Double] = 
    isotope.mass |@| H.mass map (_ + hydrogens * _)

  override def toString: String = {
    def formatCharge = charge match {
      case 0           ⇒ ""
      case x if x < 0  ⇒ "(%d)" format x
      case x           ⇒ "(+%d)" format x
    }

    def formatStereo = stereo match {
      case Stereo.Undefined ⇒ ""
      case x                ⇒ "(%s)" format x.symbol
    }

    def formatHs = hydrogens match {
      case 0 ⇒ ""
      case 1 ⇒ "H"
      case x ⇒ "H" + x
    }

    isotope.toString + formatHs + formatCharge + formatStereo
  }
}

object Atom {
  def fromElement(e: Element): Atom = fromIsotope (Isotope(e))

  def fromIsotope (i: Isotope) = Atom (i, 0, 0, Stereo.Undefined)

  implicit val AtomEqual: Eq[Atom] =
    Eq.by(a ⇒ (a.isotope, a.charge, a.hydrogens))

  implicit val AtomShow: Show[Atom] = Show.show(_.toString)
}