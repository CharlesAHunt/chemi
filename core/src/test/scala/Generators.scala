package chemi

import chemi.core._
import org.scalacheck._
import Arbitrary.arbitrary

trait Generators {
  implicit val ElementArbitrary = Arbitrary (Gen oneOf Element.values)

  implicit val BondArbitrary = Arbitrary (Gen oneOf Bond.values)

  implicit val StereoArbitrary = Arbitrary (Gen oneOf Stereo.values)

  implicit val IsotopeArbitrary = Arbitrary[Isotope](ElementArbitrary.arbitrary.map(Isotope.apply))

  implicit val AtomArbitrary = Arbitrary (
    for {
      a <- arbitrary[Isotope]
      b <- Gen.choose(-4,4)
      c <- Gen.choose(0, 4)
      d <- arbitrary[Stereo]
    } yield Atom.apply(a,b,c,d)
  )
    
}

object Generators extends Generators