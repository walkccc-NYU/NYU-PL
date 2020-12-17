// 1.
abstract class Tree[+T]
case class Node[T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[T](value: T) extends Tree[T]

// 2.
trait Addable[T] {
  def +(other: T): T
}

// 3.
class A(x: Int) extends Addable[A] {
  val value = x
  def get = x
  def +(other: A) = new A(x + other.get)
  override def toString(): String = "A(" + x + ")"
}

// 4.
class B(x: Int) extends A(x) {
  override def toString(): String = "B(" + x + ")"
}

// 5.
class C(x: Int) extends B(x) {
  override def toString(): String = "C(" + x + ")"
}

// 6.
object Part2 {
  def inOrder[T](tree: Tree[T]): List[T] = {
    tree match {
      case Leaf(value) => List(value)
      case Node(value, left, right) =>
        inOrder(left) ++
          List(value) ++
          inOrder(right)
    }
  }

  def treeSum[T <: Addable[T]](tree: Tree[T]): T = {
    tree match {
      case Leaf(value) => value
      case Node(value, left, right) =>
        treeSum(left) + value + treeSum(right)
    }
  }

  def treeMap[T, V](func: T => V, tree: Tree[T]): Tree[V] = {
    tree match {
      case Leaf(value) => Leaf(func(value))
      case Node(value, left, right) =>
        Node(func(value), treeMap(func, left), treeMap(func, right))
    }
  }

  def BTreeMap(func: B => B, tree: Tree[B]): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(func(value))
      case Node(value, left, right) =>
        Node(func(value), BTreeMap(func, left), BTreeMap(func, right))
    }
  }

  def test(): Unit = {
    def faa(a: A): A = new A(a.value + 10)
    def fab(a: A): B = new B(a.value + 20)
    def fba(b: B): A = new A(b.value + 30)
    def fbb(b: B): B = new B(b.value + 40)
    def fbc(b: B): C = new C(b.value + 50)
    def fcb(c: C): B = new B(c.value + 60)
    def fcc(c: C): C = new C(c.value + 70)
    def fac(a: A): C = new C(a.value + 80)
    def fca(c: C): A = new A(c.value + 90)

    val myBTree: Tree[B] = Node(
      new B(4),
      Node(new B(2), Leaf(new B(1)), Leaf(new B(3))),
      Node(new B(6), Leaf(new B(5)), Leaf(new B(7)))
    )

    val myATree: Tree[A] = myBTree

    println("inOrder = " + inOrder(myATree))
    println("Sum = " + treeSum(myATree))

    // println(BTreeMap(faa, myBTree)) // faa: A => A is not a subtype of B => B (A is not B)
    println(BTreeMap(fab, myBTree))
    // println(BTreeMap(fba, myBTree)) // fba: B => A is not a subtype of B => B (A is not B)
    println(BTreeMap(fbb, myBTree))
    println(BTreeMap(fbc, myBTree))
    // println(BTreeMap(fcb, myBTree)) // fcb: C => B is not a subtype of B => B
    // println(BTreeMap(fcc, myBTree)) // fcc: C => C is not a subtype of B => B
    println(BTreeMap(fac, myBTree))
    // println(BTreeMap(fca, myBTree)) // fca: C => A is not a subtype of B => B (A is not B)

    println(treeMap(faa, myATree))
    println(treeMap(fab, myATree))
    // println(treeMap(fba, myATree)) // fba: B => A doesn't match the type of myATree (A)
    // println(treeMap(fbb, myATree)) // fbb: B => B doesn't match the type of myATree (A)
    // println(treeMap(fbc, myATree)) // fbb: B => C doesn't match the type of myATree (A)
    // println(treeMap(fcb, myATree)) // fcb: C => B doesn't match the type of myATree (A)
    // println(treeMap(fcc, myATree)) // fcc: C => C doesn't match the type of myATree (A)
    println(treeMap(fac, myATree))
    // println(treeMap(fca, myATree)) // fca: C => A doesn't match the type of myATree (A)
  }

  def main(args: Array[String]) = {
    test()
  }
}
