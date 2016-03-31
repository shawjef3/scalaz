package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import StrictTree._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import std.AllInstances._

object StrictTreeTestJVM extends SpecLite {

  "ScalazArbitrary.strictTreeGenSized" ! forAll(Gen.choose(1, 200)){ size =>
    val gen = strictTreeGenSized[Unit](size)
    Stream.continually(gen.sample).flatten.take(10).map(Foldable[StrictTree].length(_)).forall(_ == size)
  }

  def genTree(size: Int): StrictTree[Int] =
    (1 to size).foldLeft(Leaf(0))((x, y) => Node(y, Vector(x)))

  "deep StrictTree flatten should not cause a stack overflow" ! {
    val size = 1000000
    val tree = genTree(size)
    tree.flatten must_== (size to 0 by -1).toVector
  }

  "deep StrictTree size should not cause a stack overflow" ! {
    val size = 1000000
    val tree = genTree(size)
    tree.size must_== size + 1
  }

  "deep equal should not cause a stack overflow" ! {
    val size = 1000000
    val tree = genTree(size)
    Equal[StrictTree[Int]].equal(tree, tree) must_== true
  }

  "deep StrictTree toTree should not cause a stack overflow" ! {
    val size = 1000000
    val tree = genTree(size)
    val expectedTree = TreeTestJVM.genTree(size)
    val actualTree = tree.toTree
    Equal[Tree[Int]].equal(actualTree, expectedTree) must_== true
  }

}
