package chemi

import chemi.core.{Element, IsotopeData}
import org.scalacheck._
import cats.implicits._

object ElementTest extends Properties("Element") {
  
  property("orderNr") = Element.values forall (e ⇒ (Element fromNumber e.atomicNr) == e)

  property("symbol") = Element.values forall (e ⇒ (Element fromSymbol e.symbol) == e.some)

  property("listOrder") =
    Element.values.zipWithIndex.forall (p ⇒ p._1.atomicNr == p._2)

  property("isotopes") =
    Element.values forall (e ⇒ e.isotopes.size == IsotopeData.isotopes(e).size)

  property("isotopes") =
    Element.values forall (e ⇒ e.isotopes forall (_.element == e))

  property("massDist") =
    Element.values forall (e ⇒
      e.isotopeDist forall (
        p ⇒ (p._1.element == e) && p._2 <= 1.0
      )
    )

}