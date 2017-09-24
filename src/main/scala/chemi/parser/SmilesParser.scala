package chemi.parser

import FAState.dummy
import cats.data.Validated.{Invalid, Valid}
import chemi.Element.{B, Br, C, Cl, F, I, N, O, P, S, Xx}
import chemi._
import mouse.all._

import scala.util.Success

sealed abstract class SmilesParser[A](implicit SB: SmilesBuilder[A]) {
  import SmilesParser._

  type STrans = A ⇒ ValRes[A]
  type FAS = FAState[A]

  def parse (s: String): ValRes[A] = FAState parse (s, char, SB.empty)

  lazy val char: FAS = FAState[A]((a, c) ⇒ c match {
      case EOT ⇒ Valid(dummy[A], a)
      case 'C' ⇒ Valid(chars('l'==, _ ⇒ SB addElem Cl, SB addElem C), a)
      case 'B' ⇒ Valid(chars('r'==, _ ⇒ SB addElem Br, SB addElem B), a)
      case '[' ⇒ Valid(accumBracket(""), a)
      case '%' ⇒ Valid(ring, a)
      case x if (x.isDigit) ⇒ next (a)(SB ring x.asDigit)
      case x   ⇒ unique get c cata (next(a), unknown(c))
    }
  )

  private def next (a: A)(s: STrans) = s(a) ∘ ((char, _))

  private def chars(p: Char ⇒ Boolean, y: Char ⇒ STrans, n: STrans) =
    FAState[A]((a,c) ⇒ c match {
        case EOT        ⇒ n(a) ∘ ((dummy[A], _))
        case x if(p(x)) ⇒ y(x)(a) ∘ ((char, _))
        case x          ⇒ n(a) flatMap (char next (_, x))
      }
    )

  private lazy val ring = FAState[A]((a,c) ⇒ c match {
      case EOT ⇒ failRing
      case x if(!x.isDigit) ⇒ failDigit(c)
      case x ⇒ (chars(_.isDigit, SB ring (x, _), _ ⇒ failDigits), a).success
    }
  )

  private def accumBracket (s: String): FAS = FAState((a,c) ⇒ c match {
      case EOT ⇒ failFormat(s)
      case ']' ⇒ parseBracket(s) flatMap (_(a) ∘ ((char, _)))
      case c   ⇒ Success(accumBracket(s + c), a)
    }
  )

  private val unique: Map[Char, STrans] = Map (
    'b' → SB.addAromElem (B),    'c' → SB.addAromElem (C),
    'O' → SB.addElem (O),        'o' → SB.addAromElem (O),
    'P' → SB.addElem (P),        'p' → SB.addAromElem (P),
    'N' → SB.addElem (N),        'n' → SB.addAromElem (N),
    'S' → SB.addElem (S),        's' → SB.addAromElem (S),
    'I' → SB.addElem (I),        'F' → SB.addElem (F),
    '-' → SB.setBond (Bond.Single),   '=' → SB.setBond (Bond.Double),
    '#' → SB.setBond (Bond.Triple),   '$' → SB.setBond (Quadruple),
    ':' → SB.setBond (Aromatic), '(' → SB.openBranch,
    ')' → SB.closeBranch,        '/' → SB.setDbStereo('/'),
    '\\'→ SB.setDbStereo('\\'),  '.' → SB.clear,
    '*' → SB.addElem (Xx)
  )

  private def parseBracket (s: String): ValRes[STrans] = s match {
    case atomRegex(m, s, stereo, h, c, id) ⇒ Element fromSymbolV s map {e ⇒
      val mass = Option(m) map (_.toInt)
      val arom = s.head.isLower
      val st = (Option(stereo) flatMap Stereo.fromSymbol) | Stereo.Undefined
      val aClass = Option(id) cata (_.tail.toInt, 0)

      val hs =  h match {
        case null       ⇒ 0
        case hRepeat(x) ⇒ s.length
        case hCount(x)  ⇒ x.toInt
      }

      val charge = c match {
        case null     ⇒ 0
        case plus(x)  ⇒ x.length
        case minus(x) ⇒ - x.length
        case pos(x)   ⇒ x.toInt
        case x        ⇒ x.toInt
      }

      SB.addAtom(Isotope(e, mass), charge, Some(hs), arom, st, aClass)
    }
    case _ ⇒ failFormat(s)
  }
}

object SmilesParser {
  def apply[A:SmilesBuilder]: SmilesParser[A] = new SmilesParser[A]{}

  val Default = apply[SmilesMol]
  
  def failFormat(s: String) = Invalid("Unknown format for atom: " + s).toValidatedNel
  val failRing = Invalid("SMILES string ends on %").toValidatedNel
  val failDigits = Invalid("% is not followed by two digits").toValidatedNel
  def failDigit(c: Char) = Invalid("Not a digit after %: " + c).toValidatedNel
  def unknown(c: Char) = Invalid("Unknown character in SMILES String: " + c).toValidatedNel

  //regular expressions
  private val (plus, minus, pos) = ("(\\++)".r, "(\\-+)".r, "\\+(\\d)".r)
  private val (hRepeat, hCount) = ("(H+)".r, "H(\\d)".r)
  private val atomRegex = """(\d+)?([A-Za-z\*][a-z]?)""" + Stereo.pattern +
                          """(H+|H\d)?(\++|\-+|[\+\-]\d)?(:\d)?""" r
}