object exercises {

//  val l = List(1, 2, 3, 4, 5)
//  val l2 = List(6, 7, 8, 9, 10)
//
//  def x: List[Int] = l
//  def y: List[Int] = l2
//
//  def r(list: List[Int], i: Int): List[(Int, Int)] = list.map(j => (i, j))
//  def s(list: List[Int])(i: Int): List[(Int, Int)] = list.map(j => (i, j))
//
//  def z = {
//    val firstList = x
//    val secondList = y
//    firstList.flatMap(s(secondList))
////    firstList.flatMap(i =>
////      r(secondList, i)
//////      secondList.map(j => (i, j))
////    )
//  }
//
//  def z2 = {
//    val firstList = x
//    val secondList = y
//    firstList.flatMap(i =>
//      r(secondList, i)
//    //      secondList.map(j => (i, j))
//      )
//  }

//  def foo[A, B, C](a: A, b: B): C = ???
//  def bar[A, B, C](a: A)(b: B): C = ???

  //Exercise 1
  def fib(n: Int): Int = {
    @annotation.tailrec // Tail-recursive helper function
    def go(a: Int, b: Int, i: Int): Int = {
      if (i == 0) a
      else go(b, a + b, i - 1)
    }
    go(0, 1, n)
  }

  //Exercise 2
  def isSorted[A] (array: Array[A], gt: (A, A) => Boolean): Boolean = {
    for (i <- 0 until array.length - 1) {
      if (!gt(array(i), array(i + 1))) {
        return false
      }
    }
    true
  }

  //Exercise 3
  //f(A, B) takes in 2 arguments and returns type C
  //B => C is the type of result returned by function
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    //we already have argument a, now user fills in b and implements that into function
    (b: B) => f(a, b)
  }
  def sub(a: Int, b: Int): Int = b - a
//  def add(a: Int, b: Int): Int = a + b
//  def times(a: Int, b: Int): Int = a * b

  //Exercise 4
  //f takes (A, B) and returns type C
  //A => (B => C) means f takes A and returns new function which takes B and then results in C
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    //a is first argument and returns new function, return function (b: B) takes in b and
    // applies f to a and b
    (a: A) => (b: B) => f(a, b)
  }

  type T = String => (Int => Int)

  //Exercise 5
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    //a applies to f which gives us B => C and then we apply b and it gives us C
    //takes both a and b at the same time
    (a: A, b: B) => f(a)(b)
  }

  //Exercise 6
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (x: A) => f(g(x))
  }

  def main(args: Array[String]): Unit = {

//    println(z)
//    println(z2)
    //Exercise 1
    println(fib(10))

    //Exercise 2
    //val ascend: (a: Int, b: Int) => Boolean = (a, b) => a < b
    val descend: (x: Int, y: Int) => Boolean = (x, y) => x > y

    //println(isSorted(Array(1, 2, 7, 2), ascend))

    //Exercise 3
    val sub2 = partial1(2, sub)
    println(sub2(5))

    //Exercise 4
    val curriedSub = curry(sub)
    val sub3 = curriedSub(3) //sub3 is a function that subtracts 3 to its input
    val result = sub3(7) //4 is result
    println(result)

    //Exercise 5
    val unCurriedSub = uncurry(curriedSub)
    println(unCurriedSub(3, 7))

    //Exercise
    val add2: Int => Int = x => x+2
    val sub1: Int => Int = y => y-1
    val composed = compose(sub1, add2)
    println(composed(5)) //5-1 = 4+2 = 6
  }
}
