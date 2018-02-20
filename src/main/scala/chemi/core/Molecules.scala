package chemi.core

import cats.implicits._

trait Molecules {

  /**
   * Calculates the exact mass for a given molecule. Returns None,
   * if the exact mass for one or more isotopes was not defined.
   */
  def exactMass (molecule: Molecule): Option[Double] = {
    if(molecule.nodes.forall(_.exactMass.isDefined))
      molecule.nodes.foldMap(_.exactMass)
    else None
  }

  /**
   * Calculates the exact mass for a given formula. Returns None,
   * if the exact mass for one or more isotopes was not defined.
   */
  def exactMassFormula (f: Formula): Option[Double] =
    (f.toList map (p => p._1.exactMass map (_ * p._2))).partition(_.isEmpty) match {
      case (Nil, masses) => Some(masses.flatten.sum)
      case _ => None
    }

  /**
   * Calculates the total formula of a molecule
   */
  def formula (molecule: Molecule): Formula = molecule.nodes.groupBy(_.isotope).map(i => i._1 -> i._2.size)

  /**
   * Calculates the molar weight of a molecule. Returns None
   * if the mass for one or more isotopes was not defined.
   */
  def mass (molecule: Molecule): Option[Double] = {
    if(molecule.nodes.forall(_.mass.isDefined))
      molecule.nodes.foldMap(_.mass)
    else None
  }

}

object Molecules extends Molecules