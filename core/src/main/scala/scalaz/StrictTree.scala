package scalaz

import java.util.NoSuchElementException
import scala.collection.mutable
import scalaz.Free.Trampoline
import scalaz.Trampoline._
import std.vector.{vectorInstance, vectorMonoid}

/**
  *
  * @param rootLabel The label at the root of this tree.
  * @param subForest The child nodes of this tree.
  * @tparam A
  */
case class StrictTree[A](
  rootLabel: A,
  subForest: Vector[StrictTree[A]]
) {

  import StrictTree._

  def foldMapTrampoline[B: Monoid](f: A => Trampoline[B]): Trampoline[B] = {
    for {
      root <- f(rootLabel)
      subForests <- Foldable[Vector].foldMap[StrictTree[A], Trampoline[B]](subForest)(_.foldMapTrampoline(f))
    } yield Monoid[B].append(root, subForests)
  }

  /** Maps the elements of the StrictTree into a Monoid and folds the resulting StrictTree. */
  def foldMap[B: Monoid](f: A => B): B =
    foldMapTrampoline(tree => done(f(tree))).run

  def foldRight[B](z: B)(f: (A, => B) => B): B =
    Foldable[Vector].foldRight(flatten, z)(f)

  /** A 2D String representation of this StrictTree. */
  def drawTree(implicit sh: Show[A]): String = {
    toTree.drawTree
  }

  def scanrTrampoline[B](g: (A, Vector[StrictTree[B]]) => Trampoline[B]): Trampoline[StrictTree[B]] = {
    for {
      c <- Applicative[Trampoline].traverse(subForest)(_.scanrTrampoline(g))
      root <- g(rootLabel, c)
    } yield StrictTree(root, c)
  }

  /** A histomorphic transform. Each element in the resulting tree
    * is a function of the corresponding element in this tree
    * and the histomorphic transform of its children.
    * */
  def scanr[B](g: (A, Vector[StrictTree[B]]) => B): StrictTree[B] =
    scanrTrampoline[B]{ case (arg0, arg1) => done(g(arg0, arg1)) }.run

  /** Pre-order traversal. */
  def flatten: Vector[A] = {
    val stack = mutable.Stack(this)

    val result = mutable.Buffer.empty[A]

    while (stack.nonEmpty) {
      val popped = stack.pop()
      result += popped.rootLabel
      stack.pushAll(popped.subForest.reverse)
    }

    result.toVector
  }

  def size: Int = {
    val stack = mutable.Stack(this.subForest)

    var result = 1

    while (stack.nonEmpty) {
      val popped = stack.pop()
      result += popped.size
      stack.pushAll(popped.map(_.subForest))
    }

    result
  }

  /** Breadth-first traversal. */
  def levels: Vector[Vector[A]] = {
    val f = (s: Vector[StrictTree[A]]) => {
      Foldable[Vector].foldMap(s)((_: StrictTree[A]).subForest)
    }
    Vector.iterate(Vector(this), size)(f) takeWhile (!_.isEmpty) map (_ map (_.rootLabel))
  }

  def toTree: Tree[A] = {
    Tree.Node[A](rootLabel, subForest.toStream.map(_.toTree))
  }

  /** Binds the given function across all the subtrees of this tree. */
  def cobind[B](f: StrictTree[A] => B): StrictTree[B] = unfoldTree(this)(t => (f(t), () => t.subForest))

  /** A TreeLoc zipper of this tree, focused on the root node. */
  def loc: TreeLoc[A] = TreeLoc.loc(this.toTree, Stream.Empty, Stream.Empty, Stream.Empty)

  /** Turns a tree of pairs into a pair of trees. */
  def unzip[A1, A2](implicit p: A => (A1, A2)): (StrictTree[A1], StrictTree[A2]) = {
    val uz = subForest.map(_.unzip)
    val fst = uz map (_._1)
    val snd = uz map (_._2)
    (Node(rootLabel._1, fst), Node(rootLabel._2, snd))
  }

  def foldNode[Z](f: A => Vector[StrictTree[A]] => Z): Z =
    f(rootLabel)(subForest)

  def mapTrampoline[B](f: A => Trampoline[B]): Trampoline[StrictTree[B]] = {
    for {
      root <- f(rootLabel)
      subForests <- Applicative[Trampoline].traverse(subForest)(tree => tree.mapTrampoline(f))
    } yield StrictTree(root, subForests)
  }

  def map[B](f: A => B): StrictTree[B] = {
    val fTrampoline = (node: A) => Trampoline.delay(f(node))
    mapTrampoline(fTrampoline).run
  }

  private case class DuplicateStack[B](
    rootLabel: B,
    subForest: Vector[StrictTree[A]],
    var mappedSubForest: Vector[DuplicateStack[B]] = Vector.empty
  ) {
    def toStrictTree: StrictTree[B] = {

      StrictTree(rootLabel, mappedSubForest.map(_.toStrictTree))

      val stack = mutable.Stack[(B, mutable.Buffer[StrictTree[B]], mutable.Stack[DuplicateStack[B]])]()

      var here = rootLabel

      for (tree <- mappedSubForest) {
        stack.push(tree.rootLabel)

      }

    }
  }

  private object DuplicateStack {
    def apply[B](f: A => B)(t: StrictTree[A]): DuplicateStack[B] = {
      DuplicateStack(f(t.rootLabel), t.subForest)
    }
  }

  def mapProcedural[B](f: A => B): StrictTree[B] = {
    val stack = mutable.Stack[DuplicateStack[B]]()

    val root = DuplicateStack[B](f(rootLabel), subForest)

    var here = root

    while (stack.nonEmpty || here != null) {
      if (here != null) {
        if (here.subForest.isEmpty) here = null
        else {
          here.mappedSubForest =
            here.subForest.map(DuplicateStack(f))

          stack.pushAll(here.mappedSubForest)

          here = here.mappedSubForest.head
        }

      } else {
        here =
          if (stack.isEmpty) null
          else stack.pop()
      }
    }

    root.toStrictTree
  }

  def duplicate: StrictTree[Unit] = {
    subForest match {
      case children => StrictTree((), children.map(_.duplicate))
    }
  }

  def flatMapTrampoline[B](f: A => Trampoline[StrictTree[B]]): Trampoline[StrictTree[B]] = {
    for {
      r <- f(rootLabel)
      subForests <- Applicative[Trampoline].traverse(subForest)(_.flatMapTrampoline(f))
    } yield StrictTree(r.rootLabel, r.subForest ++ subForests)
  }

  def flatMap[B](f: A => StrictTree[B]): StrictTree[B] = {
    val fTrampoline = (node: A) => done(f(node))
    flatMapTrampoline(fTrampoline).run
  }

  def traverse1[G[_] : Apply, B](f: A => G[B]): G[StrictTree[B]] = {
    val G = Apply[G]

    subForest match {
      case Vector() => G.map(f(rootLabel))(Leaf(_))
      case x +: xs => G.apply2(f(rootLabel), NonEmptyList.nel(x, IList.fromFoldable(xs)).traverse1(_.traverse1(f))) {
        case (h, t) => Node(h, t.list.toVector)
      }
    }
  }

  def hashCodeTrampoline(): Trampoline[Int] = {
    for {
      rootHash <- Trampoline.done(rootLabel.hashCode())
      subForestHashes <- Applicative[Trampoline].traverse(subForest)(_.hashCodeTrampoline())
    } yield (rootHash, subForestHashes).hashCode()
  }

  override def hashCode(): Int = {
    hashCodeTrampoline().run
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case other: StrictTree[A] =>
        StrictTree.badEqInstance[A].equal(this, other)
      case _ =>
        false
    }
  }
}

sealed abstract class StrictTreeInstances {
  implicit val strictTreeInstance: Traverse1[StrictTree] with Monad[StrictTree] with Comonad[StrictTree] with Align[StrictTree] with Zip[StrictTree] = new Traverse1[StrictTree] with Monad[StrictTree] with Comonad[StrictTree] with Align[StrictTree] with Zip[StrictTree] {
    def point[A](a: => A): StrictTree[A] = StrictTree.Leaf(a)
    def cobind[A, B](fa: StrictTree[A])(f: StrictTree[A] => B): StrictTree[B] = fa cobind f
    def copoint[A](p: StrictTree[A]): A = p.rootLabel
    override def map[A, B](fa: StrictTree[A])(f: A => B) = fa map f
    def bind[A, B](fa: StrictTree[A])(f: A => StrictTree[B]): StrictTree[B] = fa flatMap f
    def traverse1Impl[G[_]: Apply, A, B](fa: StrictTree[A])(f: A => G[B]): G[StrictTree[B]] = fa traverse1 f
    override def foldRight[A, B](fa: StrictTree[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
    override def foldMapRight1[A, B](fa: StrictTree[A])(z: A => B)(f: (A, => B) => B) = (fa.flatten.reverse: @unchecked) match {
      case h +: t => t.foldLeft(z(h))((b, a) => f(a, b))
    }
    override def foldLeft[A, B](fa: StrictTree[A], z: B)(f: (B, A) => B): B =
      fa.flatten.foldLeft(z)(f)
    override def foldMapLeft1[A, B](fa: StrictTree[A])(z: A => B)(f: (B, A) => B): B = fa.flatten match {
      case h +: t => t.foldLeft(z(h))(f)
    }
    override def foldMap[A, B](fa: StrictTree[A])(f: A => B)(implicit F: Monoid[B]): B = fa foldMap f

    def alignWithTrampoline[A, B, C](f: (\&/[A, B]) ⇒ Trampoline[C])(a: StrictTree[A], b: StrictTree[B]): Trampoline[StrictTree[C]] = {
      def alignTrampoline(ta: StrictTree[A], tb: StrictTree[B]): Trampoline[StrictTree[C]] =
        for {
          roots <- f(\&/(ta.rootLabel, tb.rootLabel))
          subForests <- Applicative[Trampoline].sequence(
            Align[Vector].alignWith[StrictTree[A], StrictTree[B], Trampoline[StrictTree[C]]] {
              case \&/.This(sta) ⇒ sta mapTrampoline {a ⇒ f(\&/.This(a))}
              case \&/.That(stb) ⇒ stb mapTrampoline {b ⇒ f(\&/.That(b))}
              case \&/(sta, stb) ⇒ alignTrampoline(sta, stb)
            } (ta.subForest, tb.subForest)
          )
        } yield StrictTree(roots, subForests)
      alignTrampoline(a, b)
    }

    override def alignWith[A, B, C](f: (\&/[A, B]) => C): (StrictTree[A], StrictTree[B]) => StrictTree[C] = {
      (a, b) =>
        alignWithTrampoline[A, B, C](aligned => Trampoline.delay(f(aligned)))(a, b).run
    }

    def zip[A, B](a: => StrictTree[A], b: => StrictTree[B]): StrictTree[(A, B)] = {
      StrictTree.Node(
        (a.rootLabel, b.rootLabel),
        Zip[Vector].zipWith(a.subForest, b.subForest)(zip(_, _))
      )
    }
  }

  implicit def treeEqual[A](implicit A0: Equal[A]): Equal[StrictTree[A]] =
    new StrictTreeEqual[A] { def A = A0 }

  implicit def treeOrder[A](implicit A0: Order[A]): Order[StrictTree[A]] =
    new Order[StrictTree[A]] with StrictTreeEqual[A] {
      def A = A0
      import std.vector._
      override def order(x: StrictTree[A], y: StrictTree[A]) =
        A.order(x.rootLabel, y.rootLabel) match {
          case Ordering.EQ =>
            Order[Vector[StrictTree[A]]].order(x.subForest, y.subForest)
          case x => x
        }
    }

  /* TODO
  def applic[A, B](f: StrictTree[A => B]) = a => StrictTree.node((f.rootLabel)(a.rootLabel), implicitly[Applic[newtypes.ZipVector]].applic(f.subForest.map(applic[A, B](_)).?)(a.subForest ?).value)
   */
}


object StrictTree extends StrictTreeInstances {
  /**
   * Node represents a tree node that may have children.
   *
   * You can use Node for tree construction or pattern matching.
   */
  object Node {
    def apply[A](root: A, forest: Vector[StrictTree[A]]): StrictTree[A] = {
      StrictTree[A](root, forest)
    }

    def unapply[A](t: StrictTree[A]): Option[(A, Vector[StrictTree[A]])] = Some((t.rootLabel, t.subForest))
  }

  /**
   *  Leaf represents a a tree node with no children.
   *
   *  You can use Leaf for tree construction or pattern matching.
   */
  object Leaf {
    def apply[A](root: A): StrictTree[A] = {
      Node(root, Vector.empty)
    }

    def unapply[A](t: StrictTree[A]): Option[A] = {
      t match {
        case Node(root, Vector()) =>
          Some(root)
        case _ =>
          None
      }
    }
  }

  def unfoldForest[A, B](s: Vector[A])(f: A => (B, () => Vector[A])): Vector[StrictTree[B]] =
    s.map(unfoldTree(_)(f))

  def unfoldTree[A, B](v: A)(f: A => (B, () => Vector[A])): StrictTree[B] =
    f(v) match {
      case (a, bs) => Node(a, unfoldForest(bs.apply())(f))
    }

  //Only used for .equals.
  private def badEqInstance[A] = new StrictTreeEqual[A] {
    override def A: Equal[A] = new Equal[A] {
      override def equal(a1: A, a2: A): Boolean = a1.equals(a2)
    }
  }
}

private trait StrictTreeEqual[A] extends Equal[StrictTree[A]] {
  def A: Equal[A]
  override final def equal(a1: StrictTree[A], a2: StrictTree[A]) = {
    def corresponds[B](a1: Vector[StrictTree[A]], a2: Vector[StrictTree[A]]): Trampoline[Boolean] = {
      (a1.isEmpty, a2.isEmpty) match {
        case (true, true) => Trampoline.done(true)
        case (_, true) | (true, _) => Trampoline.done(false)
        case _ =>
          for {
            heads <- trampolined(a1.head, a2.head)
            tails <- corresponds(a1.tail, a2.tail)
          } yield heads && tails
      }
    }

    def trampolined(a1: StrictTree[A], a2: StrictTree[A]): Trampoline[Boolean] = {
      for {
        roots <- Trampoline.done(A.equal(a1.rootLabel, a2.rootLabel))
        subForests <- corresponds(a1.subForest, a2.subForest)
      } yield roots && subForests
    }

    trampolined(a1, a2).run
  }
}
