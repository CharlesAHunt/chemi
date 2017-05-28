package chemi

import Element.H

trait Molecules {

  /**
   * Calculates the exact mass for a given molecule. Returns None,
   * if the exact mass for one or more isotopes was not defined.
   */
  def exactMass (m: Molecule): Option[Double] = m foldMap (_.exactMass)

  /**
   * Calculates the exact mass for a given formula. Returns None,
   * if the exact mass for one or more isotopes was not defined.
   */
  def exactMass (f: Formula): Option[Double] =
    f.toList foldMap (p ⇒ p._1.exactMass map (_ * p._2))

  /**
   * Calculates the total formula of a molecule
   */
  def formula (m: Molecule): Formula = m foldMap (_.formula)

  /**
   * Calculates the molar weight of a molecule. Returns None
   * if the mass for one or more isotopes was not defined.
   */
  def mass (m: Molecule): Option[Double] = m.foldMap (_.mass)

}

object Molecules extends Molecules