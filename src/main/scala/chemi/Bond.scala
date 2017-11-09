package chemi

import cats.Show
import cats.kernel.Eq

import scalax.collection.edge.WUnDiEdge

sealed abstract class Bond(val symbol: String, val valence: Int)

object Bond {

  type ChemBond = WUnDiEdge[Int]

  case class Single(x: Product) extends ChemBond(x, 1)
  case class Double(x: Product) extends ChemBond(x, 2)
  case class Triple(x: Product) extends ChemBond(x, 3)
  case class Quadruple(x: Product) extends ChemBond(x, 4)
  case class Aromatic(x: Product) extends ChemBond(x, 0)

  implicit val BondEqual = Eq.allEqual[Bond]

  implicit val BondShow = Show.show[Bond](_.symbol)
}