package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import StrictTree._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import std.AllInstances._

object StrictTreeTestJVM extends SpecLite {

  val E = Equal[StrictTree[Int]]

  "ScalazArbitrary.strictTreeGenSized" ! forAll(Gen.choose(1, 200)){ size =>
    val gen = strictTreeGenSized[Unit](size)
    Stream.continually(gen.sample).flatten.take(10).map(Foldable[StrictTree].length(_)).forall(_ == size)
  }

  def genTree(size: Int): StrictTree[Int] =
    (1 to size).foldLeft(Leaf(0))((x, y) => Node(y, Vector(x)))

  val size = 1000000

  val deepTree = genTree(size)

  "deep StrictTree flatten should not cause a stack overflow" ! {
    deepTree.flatten must_== (size to 0 by -1).toVector
  }

  "deep StrictTree size should not cause a stack overflow" ! {
    deepTree.size must_== size + 1
  }

  "deep Equal.equal should not cause a stack overflow" ! {
    E.equal(deepTree, deepTree) must_== true
  }

  "deep equals should not cause a stack overflow" ! {
    deepTree.equals(deepTree) must_== true
  }

  "deep toTree should not cause a stack overflow" ! {
    val expectedTree = TreeTestJVM.deepTree
    val actualTree = deepTree.toTree
    TreeTestJVM.E.equal(actualTree, expectedTree) must_== true
  }

  "deep map should not cause a stack overflow" ! {
    val actualTree = deepTree.map(_ + 1)
    val expectedTree = (2 to size + 1).foldLeft(Leaf(1))((x, y) => Node(y, Vector(x)))
    E.equal(actualTree, expectedTree) must_== true
  }

  "deep flatMap should not cause a stack overflow" ! {
    val actualTree = deepTree.flatMap(Leaf(_))
    E.equal(deepTree, actualTree) must_== true
  }

  "deep align should not cause a stack overflow" ! {
    val aligned = Align[StrictTree].align(deepTree, deepTree)
    true
  }

}
