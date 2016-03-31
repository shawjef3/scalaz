package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import Tree._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import std.AllInstances._

object TreeTestJVM extends SpecLite {

  "ScalazArbitrary.treeGenSized" ! forAll(Gen.choose(1, 200)){ size =>
    val gen = treeGenSized[Unit](size)
    Stream.continually(gen.sample).flatten.take(10).map(Foldable[Tree].length(_)).forall(_ == size)
  }

  def genTree(size: Int): Tree[Int] =
    (1 to size).foldLeft(Leaf(0))((x, y) => Node(y, Stream(x)))

  val size = 1000000

  val deepTree = genTree(size)

  "deep Tree flatten should not cause a stack overflow" ! {
    deepTree.flatten must_== (size to 0 by -1).toStream
  }

  "deep equal should not cause a stack overflow" ! {
    Equal[Tree[Int]].equal(deepTree, deepTree) must_== true
  }

  "deep Tree toStrictTree should not cause a stack overflow" ! {
    val expectedTree = StrictTreeTestJVM.deepTree
    val actualTree = deepTree.toStrictTree
    Equal[StrictTree[Int]].equal(actualTree, expectedTree) must_== true
  }

}
