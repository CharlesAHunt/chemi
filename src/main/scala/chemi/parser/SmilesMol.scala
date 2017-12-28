package chemi.parser

import cats.data.{NonEmptyList, Validated}
import chemi._
import mouse.all._
import chemi.Bond.{Aromatic, Single}

import collection.immutable.{IndexedSeq => IxSq}
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

case class SmilesMol (
  atoms: IxSq[SmilesAtom],
  bonds: List[UnDiEdge[Bond]],
  stack: List[SmilesMol.AtomInfo] = Nil,
  keep: Boolean = false,
  bond: Option[Bond] = None,
  dbStereo: Option[Char] = None,
  rings: SmilesMol.Rings = Map.empty
) {
  import SmilesMol.{Rings, AtomInfo}

  def apply (i: Int): SmilesAtom = atoms(i)

  def addAtom (a: SmilesAtom, aromatic: Boolean) = copy(
    atoms = atoms :+ a,
    keep = false,
    stack = (keep, stack) match {
      case (false, a::as) ⇒ (order, aromatic) :: as
      case (_, as)        ⇒ (order, aromatic) :: as
    }
  )

  def addBond (x: AtomInfo, y: AtomInfo, b: Option[Bond] = None) = {
    def bnd = bond orElse b match {
      case None if x._2 && y._2   ⇒ Aromatic
      case None                   ⇒ Single
      case Some(a)                ⇒ a
    }

    copy (bonds = (UnDiEdge(x._1, y._1), bnd) :: bonds).noBond.success
  }

  def closeBranch: ValRes[SmilesMol] = stack match {
    case a::as ⇒ Validated.Valid(copy(stack = as))
    case _ ⇒ Validated.invalid(NonEmptyList.one("No branch opened"))
  }

  def modRings (f: Rings ⇒ Rings) = copy (rings = f(rings))

  def noBond = copy(bond = None)

  def openBranch = copy(keep = true)

  def order = atoms.size
}

object SmilesMol {
  /**
   * Index of atom plus boolean flag representing aromaticity
   */
  type AtomInfo = (Int, Boolean)

  /**
   * Atom info plus option type of bond to the atom in question
   */
  type RingInfo = (AtomInfo, Option[Bond])

  /**
   * Mapping from ring index to ring info
   */
  type Rings = Map[Int, RingInfo]

  /**
   * Transforms a SmilesMol to a Molecule by calculating
   * the implicit hydrogens for each atom. Aromaticity and
   * stereocenters are ignored by this function.
   */
  def toMolecule (m: SmilesMol): ValRes[Molecule] = {
    val graph = Graph(m.atoms, m.bonds: _*)
    def toAtom (a: SmilesAtom, i: Int): ValRes[Atom] = {
      def hs = SmilesAtom implicitHydrogens (graph edgesTo i, a.element)
      def toAtom (hs: Int) = Atom(a.isotope, a.charge, hs, a.stereo)

      a.hydrogens cata (Validated.Valid(toAtom(_)), hs map toAtom)
    }
    
    (graph mapI toAtom).sequence[ValRes,Atom]
  }

  /**
   * SmilesBuilder implementation. Does not yet provide error messages
   * for all possibly invalid SMILES strings. For instance, unclosed
   * braces, several successive braces, several successive bonds, 
   * and unclosed rings are all accepted silently.
   */
  implicit val SmilesMolBuilder = new SmilesBuilder[SmilesMol] {
    val empty = SmilesMol(IxSq.empty, Nil)
    val clear: STrans = m ⇒ Validated.Valid(SmilesMol(m.atoms, m.bonds, rings = m.rings))
    val closeBranch: STrans = _.closeBranch
    val openBranch: STrans = _.openBranch
    def setBond (b: Bond): STrans = mol => Validated.Valid(mol.copy(bond = Some(b)))
    def setDbStereo (c: Char): STrans = mol => Validated.Valid(mol.copy(dbStereo = Some(c)))

    def addAtom (
      i: Isotope, c: Int, h: Option[Int], a: Boolean, s: Stereo, ac: Int
    ) = m ⇒ 
      m.addAtom(SmilesAtom(i, c, h, s, ac), a) |>
      (n ⇒ m.stack.headOption cata (n addBond (_, (m.order, a)), Validated.Valid(n)))

    def ring (i: Int) = m ⇒ (m.rings get i, m.stack.headOption) match {
      case (Some((x, bo)), Some(y)) ⇒ m modRings (_ - i) addBond (x, y, bo)
      case (None, Some(y)) ⇒ m.modRings (a => a + i -> (y, m.bond)).noBond.success
      case (_, None) ⇒ Validated.Invalid(NonEmptyList.one("Atom stack empty when opening ring."))
    }
  }
}