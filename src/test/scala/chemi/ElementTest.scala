package chemi

import org.scalacheck._

/**
 * @author Stefan Höck
 */
object ElementTest extends Properties("Element") {
  
  property("orderNr") = Element.values.∀ (e ⇒ (Element fromNr e.atomicNr) ≟ e) 
  
  property("symbol") = Element.values.∀ (e ⇒ (Element fromSymbol e.symbol) ≟ e.some) 
  
  property("listOrder") =
    Element.values.zipWithIndex.∀ (p ⇒ p._1.atomicNr ≟ p._2) 

  property("isotopes") =
    Element.values ∀ (e ⇒ e.isotopes.size ≟ IsotopeData.isotopes(e).size)

  property("isotopes") =
    Element.values ∀ (e ⇒ e.isotopes ∀ (_.element ≟ e))

  property("massDist") =
    Element.values ∀ (e ⇒ 
      e.isotopeDist ∀ (
        p ⇒ (p._1.element ≟ e) && p._2 <= 1.0
      )
    )

}