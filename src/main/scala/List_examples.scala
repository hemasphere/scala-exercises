import scala.annotation.tailrec
import scala.collection.immutable.List

object List_examples {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  sealed trait Tree[+A]
  case class Leaf[A](value:A) extends Tree[A]
  case class Branch[A](leaf: Tree[A], right: Tree[A]) extends Tree[A]

  //Exercise 2
  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  //Exercise 3
  @tailrec
  def drop[A](n: Int, l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ if n <= 0 => l
    case Cons(_, t) => drop(n - 1, t)
  }


  //Exercise 4
  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t)(f) //t passed through the function
    case _ => l
  }


  //Exercise 5
  def setHead[A](x: A, l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(x, t)
  }


  //Exercise 6
  def init[A](l: List[A]): List[A] = l match {
   case Nil => Nil
   case Cons(h, Nil) => Nil
   case Cons(h, t) => Cons(h, init(t))
  }

  //Exercise 7
  //No because foldRight traverses through the list but doesn't do any of the operations

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]): Unit =
    foldRight(l, 1.0)(_ * _)

  //Exercise 8

  //Exercise 9
  def length2[A](l:List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  //Exercise 10
  def foldLeft[A, B](l: List[A], z:B)(f: (B, A) => B): B = {
    //non-recursive
    //case Nil => z
    //case Cons(h, t) => foldLeft(t, f(z, h))(f)

    //recursive
    @tailrec
    def go(l: List[A], count:B): B = l match{
      case Nil => count
      //apply f(count, h) to current count and h
      //recursively call go(t, f(count, h)) using t and new count as arguments
      case Cons(h, t) => go(t, f(count, h))
    }
    go(l, z)
  }

  //Exercise 11
  def sum3(l: List[Int]) =
    foldLeft(l, 0.0)((acc, x) => acc + x)

  def product3(l: List[Double]): Unit =
    foldLeft(l, 1.0)((acc, x) => acc * x)

  def length3[A](l:List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)


  //Exercise 12
  def reverse[A](l: List[A]): List[A] =
  //starts off as Nil, for each element x in l, add x to Nil in beginning, which
  //will reverse it
    foldLeft (l, Nil: List[A])((acc, x) => Cons(x, acc))

  //Exercise 13

  //Exercise 14
  def appendLeft[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, appendLeft(t, l2))
    //    foldLeft(l1, l2)((acc, x) => Cons(x, acc))
  }

  def appendRight[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    //foldRight(l1, l2)((x, acc) => Cons(x, acc))
    case Nil => l2
    case Cons(h, t) => Cons(h, appendLeft(t, l2))
  }

  //Exercise 15
  def concatenate[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])((l, acc) => appendRight(l, acc))


  //Exercise 16
  def add1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, add1(t))
  }

  //Example 17
  def convertDoubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(head, tail) => Cons(head.toString, convertDoubleToString(tail))
  }

  //Example 18
  def map[A, B](l:List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  //Example 19
  def filter[A](l: List[A])(predicate: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (predicate(h)) Cons(h, filter(t)(predicate))  // If the element satisfies the predicate, include it.
      else filter(t)(predicate)  // Otherwise, skip it.
  }

  //Example 20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => appendRight(f(h), flatMap(t)(f))
  }

  //Example 21

  //Example 22
  def addListValues[A, B](l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match{
    case (Nil, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addListValues(t1, t2))
    case _ => Nil
  }

  //Example 23
  def generalizeListValues[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), generalizeListValues(t1, t2)(f))
    case _ => Nil
  }

  //Example 25
  def countNodes[A](tree: Tree[A]): Int = tree match{
    case Leaf(_) => 1 //matches leaf and returns 1
    case Branch(left, right) => countNodes(left) + countNodes(right)
  }

  //Example 26
//  def maxElement[A](tree: Tree[A]): A = tree match{
//    case Leaf(value) => value
//    case Branch(left, right) =>
//      val leftMax = maxElement(left)
//      val rightMax = maxElement(right)
//      if()
//      
//  }

  //Example 27




}
