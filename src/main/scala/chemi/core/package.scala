package chemi

import cats.Bifunctor
import cats.data.{NonEmptyList, Validated}
import quiver.Graph

package object core {

  type Formula = Map[Isotope, Int]

  type Molecule = Graph[Atom, Int, Int]

  type ValNel[+E,+A] = Validated[NonEmptyList[E], A]

  type ValRes[+A] = ValNel[String, A]

  type DisRes[+A] = Either[String, A]

  /**
    * Adjust all error messages (if any) in v by applying function f.
    */
  def mapErr[E,F,A](v: ValNel[E,A])(f: E ⇒ F): ValNel[F, A] =
    Bifunctor[Validated].leftMap(v)(_ map f)

}