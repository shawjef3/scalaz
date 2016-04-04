package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import StrictTree._
import org.scalacheck.{Gen, Prop}
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

  def time[A](x: => A): Boolean = {
    import java.time._
    val time0 = Instant.now()
    val result = x
    val time1 = Instant.now()
    println(s"time taken = ${Duration.between(time0, time1)}")
    true
  }

  "deep foldMap should not cause a stack overflow" ! {
    //must_== (1 to size).sum
    time(deepTree.foldMap(identity))
  }

  "deep foldRight should not cause a stack overflow" ! {
    // must_== (1 to size).sum
    time(deepTree.foldRight[Int](0)(_ + _))
  }

  "deep flatten should not cause a stack overflow" ! {
    time{deepTree.flatten}
    //must_=== (size to 0 by -1).toVector
  }

  "deep levels should not cause a stack overflow" ! {
    time{deepTree.levels}
    //must_=== (size to 0 by -1).map(i => Vector(i)).toVector
  }

  "deep scanr should not cause a stack overflow" ! {
    def f(a: Int, b: Vector[StrictTree[Int]]): Int = a + b.size
//    E.equal(deepTree.scanr[Int](f _), deepTree.map { case 0 => 0; case n => n + 1 }) must_== true
    time{deepTree.scanr[Int](f _)}
  }

  "deep size should not cause a stack overflow" ! {
    time(deepTree.size)
    //must_== size + 1
  }

  "deep Equal.equal should not cause a stack overflow" ! {
    time(E.equal(deepTree, deepTree))
    //must_== true
  }

  "deep equals should not cause a stack overflow" ! {
    time(deepTree.equals(deepTree))
    //must_== true
  }

  "deep toTree should not cause a stack overflow" ! {
//    val expectedTree = TreeTestJVM.deepTree
//    val actualTree = deepTree.toTree
//    TreeTestJVM.E.equal(actualTree, expectedTree) must_== true
      time(deepTree.toTree)
  }

  "deep map should not cause a stack overflow" ! {
//    val actualTree = deepTree.map(_ + 1)
//    val expectedTree = (2 to size + 1).foldLeft(Leaf(1))((x, y) => Node(y, Vector(x)))
//    E.equal(actualTree, expectedTree) must_== true
    time(deepTree.map(_ + 1))
  }

  "deep flatMap should not cause a stack overflow" ! {
//    val actualTree = deepTree.flatMap(Leaf(_))
//    E.equal(deepTree, actualTree) must_== true
    time(deepTree.flatMap(Leaf(_)))
  }

  "deep align should not cause a stack overflow" ! {
    time(Align[StrictTree].align(deepTree, deepTree))
  }

}
