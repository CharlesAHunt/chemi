package chemi

import chemi.core.Stereo
import org.scalacheck._

object StereoTest extends Properties("Stereo") {
  property("fromSymbol") = 
    Stereo.values forall (s ⇒ (Stereo fromSymbol s.symbol) == s.some)

  property("values_size") =
    Stereo.values.size == (1 + 1 + 1 + 2 + 2 + 3 + 20 + 30)

  property("values_distinct") =
    Stereo.values.distinct == Stereo.values

  property("regexp") = Stereo.values.tail forall { s ⇒
    s.symbol match {
      case Stereo.regexp(x) ⇒ x match {
        case null ⇒ true
        case x    ⇒ Stereo.fromSymbol(x) == s.some
      }
      case _ ⇒ false
    }
  }
    
}