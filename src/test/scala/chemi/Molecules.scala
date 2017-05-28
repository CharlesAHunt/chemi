package chemi

import org.scalacheck._, Prop._

object MoleculesTest extends Properties("Molecules") {

  private def singleAtomProp[A] (a: Molecule ⇒ A, f: (Element,A) ⇒ Boolean)
  : Boolean = {
    def mol(e: Element) =
      LGraph.empty[Bond,Atom] addVertex (Atom fromElement e)

    Element.values ∀ (e ⇒ f(e, a(mol(e))))
  }

  val iq = Eq[Map[String,Int]]

  property("formula") = singleAtomProp[Map[Isotope,Int]](
    Molecules.formula, (e,m) ⇒ m == Map(Isotope(e) → 1))
    
}

// vim: set ts=2 sw=2 et:
