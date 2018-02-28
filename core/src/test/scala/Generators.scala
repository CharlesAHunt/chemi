package chemi

import chemi.core.{Element, Isotope, Stereo}
import org.scalacheck._

trait Generators {
  implicit val ElementArbitrary = Arbitrary (Gen oneOf Element.values)

  implicit val BondArbitrary = Arbitrary (Gen oneOf Bond.values)

  implicit val StereoArbitrary = Arbitrary (Gen oneOf Stereo.values)

  implicit val IsotopeArbitrary = ElementArbitrary ∘ (Isotope.apply (_))

  implicit val AtomArbitrary = Arbitrary (
    arbitrary[Isotope] ⊛
    Gen.choose(-4,4) ⊛
    Gen.choose(0, 4) ⊛ 
    arbitrary[Stereo] apply Atom.apply
  )
    
}

object Generators extends Generators