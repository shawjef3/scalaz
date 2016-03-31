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

  "deep Tree flatten should not cause a stack overflow" ! {
    val size = 1000000
    val tree = genTree(size)
    tree.flatten must_== (size to 0 by -1).toStream
  }

  "deep equal should not cause a stack overflow" ! {
    val size = 1000000
    val tree = genTree(size)
    Equal[Tree[Int]].equal(tree, tree) must_== true
  }

  "deep Tree toStrictTree should not cause a stack overflow" ! {
    val size = 1000000
    val tree = genTree(size)
    val expectedTree = StrictTreeTestJVM.genTree(size)
    val actualTree = tree.toStrictTree
    Equal[StrictTree[Int]].equal(actualTree, expectedTree) must_== true
  }

}
