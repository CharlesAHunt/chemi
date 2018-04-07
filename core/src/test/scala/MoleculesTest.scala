package chemi

import cats.kernel.Eq
import chemi.core._
import org.scalacheck._
import quiver.LNode

object MoleculesTest extends Properties("Molecules") {

  private def singleAtomProp[A] (a: Molecule ⇒ A, f: (Element,A) ⇒ Boolean)
  : Boolean = {
    def mol(e: Element) = {
      val atom = Atom fromElement e
      quiver.empty[Int, Atom, Bond].addNode(LNode(atom.charge, atom))
    }

    Element.values forall (e ⇒ f(e, a(mol(e))))
  }
  
  property("formula") = singleAtomProp[Map[Isotope,Int]](
    Molecules.formula, (e,m) ⇒ m == Map(Isotope(e) → 1))
    
}