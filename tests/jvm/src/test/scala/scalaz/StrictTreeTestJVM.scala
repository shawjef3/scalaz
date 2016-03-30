package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import StrictTree._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object StrictTreeTestJVM extends SpecLite {

  "ScalazArbitrary.strictTreeGenSized" ! forAll(Gen.choose(1, 200)){ size =>
    val gen = strictTreeGenSized[Unit](size)
    Stream.continually(gen.sample).flatten.take(10).map(Foldable[StrictTree].length(_)).forall(_ == size)
  }

  "deep Tree flatten should not cause a stack overflow" ! {
    val size = 1000000
    val tree = (1 to size).foldLeft(Leaf(0))((x, y) => Node(y, Vector(x)))
    tree.flatten must_== (size to 0 by -1).toVector
  }

}
