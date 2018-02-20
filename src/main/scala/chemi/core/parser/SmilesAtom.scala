package chemi.core.parser

import cats.data.{NonEmptyList, Validated}
import chemi.core.Element.{B, Br, C, Cl, F, I, N, O, P, S}
import mouse.all._
import cats.implicits._
import chemi.core.Bond.{Aromatic, Single}
import chemi.core._

/**
 * A data type representing information about atoms available
 * from SMILES strings
 */
case class SmilesAtom (
  isotope: Isotope,
  charge: Int,
  hydrogens: Option[Int],
  stereo: Stereo,
  atomClass: Int
) {
  def element: Element = isotope.element
}

object SmilesAtom {
  /**
   * Calculate implicit hydrogens for element e with adjacent bonds bs.
   * In most cases it is just summing up the valences of the bonds
   * and subtracting this number from the natural valence of the element.
   * Multiple aromatic bonds mean different things for different
   * elements and are treated on a case by case basis. 
   *
   * Valid valences for organic subset elements are sometimes a bit
   * less strict that defined by the OpenSMILES standard:
   * 1, 3, 5, 7 for Cl, Br, and I (instead of just 1)
   */
  def implicitHydrogens (bonds: List[Bond], e: Element): ValRes[Int] = {
    def msg = "Invalid bond set for element %s: %s" format (e, bonds mkString ",")
    def fail = Validated.Invalid(NonEmptyList.one(msg))

    def default (v: Int): ValRes[Int] =
      valences get e flatMap (_ find (_ <= v)) cata (n => Validated.valid(n - v), fail)

    bonds count (_ == Aromatic) match {
      case 1 ⇒ default (2 + (bonds foldMap (_.valence)))
      case 0 ⇒ default (bonds foldMap (_.valence))
      case _ ⇒ bonds sortBy (_.valence) match {
        case Aromatic::Aromatic::Aromatic::Nil ⇒
          if (e == C || e == N || e == P || e == B) Validated.valid(0) else fail
        case Aromatic::Aromatic::Single::Nil   ⇒
          if (e == C || e == N || e == P || e == B) Validated.valid(0) else fail
        case Aromatic::Aromatic::Bond.Double::Nil   ⇒
          if (e == C || e == S) Validated.valid(0) else fail
        case Aromatic::Aromatic::Nil           ⇒
          if (e == C || e == B) Validated.valid(1)
          else if (e == N || e == P || e == O || e == S) Validated.valid(0)
          else fail
        //other combos with 2 or more aromatic bonds don't make sense
        case _ ⇒ fail
      }
    }
  }

  private val valences: Map[Element,Seq[Int]] = Map(
    B → Seq(3),
    C → Seq(4),
    N → Seq(3,5),
    O → Seq(2),
    P → Seq(3,5),
    S → Seq(2,4,6),
    F → Seq(1),
    Cl → Seq(1, 3, 5, 7),
    Br → Seq(1, 3, 5, 7),
    I → Seq(1, 3, 5, 7)
  )
}