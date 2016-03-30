package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import Tree._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object TreeTestJVM extends SpecLite {

  "ScalazArbitrary.treeGenSized" ! forAll(Gen.choose(1, 200)){ size =>
    val gen = treeGenSized[Unit](size)
    Stream.continually(gen.sample).flatten.take(10).map(Foldable[Tree].length(_)).forall(_ == size)
  }

  "deep Tree flatten should not cause a stack overflow" ! {
    val size = 1000000
    val tree = (1 to size).foldLeft(Leaf(0))((x, y) => Node(y, Stream(x)))
    tree.flatten must_== (size to 0 by -1).toStream
  }

  "deep Tree toStrictTree should not cause a stack overflow" ! {
    val size = 1000000
    val tree = (1 to size).foldLeft(Leaf(0))((x, y) => Node(y, Stream(x)))
    val strictTree = (1 to size).foldLeft(StrictTree.Leaf(0))((x, y) => StrictTree.Node(y, Vector(x)))
    tree.toStrictTree must_== strictTree
  }

}
