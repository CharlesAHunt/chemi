package chemi

import cats.Show
import cats.kernel.Eq

import scalax.collection.edge.WUnDiEdge

sealed case class Bond(symbol: String, valence: Int)

object Bond {

  type ChemBond = WUnDiEdge[Int]

  case object Single extends Bond("-", 1)
  case object Double extends Bond("=", 2)
  case object Triple extends Bond("#", 3)
  case object Quadruple extends Bond("$", 4)
  case object Aromatic extends Bond(":", 0)

  val values = List[Bond] (Single, Double, Triple, Quadruple, Aromatic)

  implicit val BondEqual = Eq.allEqual[Bond]

  implicit val BondShow = Show.show[Bond](_.symbol)
}