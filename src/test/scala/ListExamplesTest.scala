import List_examples.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps

class ListExamplesTest extends AnyFreeSpec with Matchers {
  "tail should return a list without the first element" in {
    val inputList = Cons(1, Cons(2, Cons(3, Nil)))
    val expected: List[Int] = Cons(2, Cons(3, Nil)) // expected output
    List_examples.tail(inputList) shouldBe expected
  }

  "drop should remove the first 'n' elements from the list" in {
    val inputList = Cons(1, Cons(2, Cons(3, Nil)))
    val expected: List[Int] = Cons(3, Nil)
    List_examples.drop(2, inputList) shouldBe expected
  }

  "dropWhile should drop elements while the condition is true" in {
    val inputList = Cons(1, Cons(2, Cons(3, Nil)))
    val expected: List[Int] = Cons(2, Cons(3, Nil))
    List_examples.dropWhile(inputList)((x:Int) => x < 2) shouldBe expected
  }

  "setHead should change the head of the list" in {
    val inputList = Cons(1, Cons(2, Cons(3, Nil)))
    val expected: List[Int] = Cons(7, Cons(2, Cons(3, Nil)))
    List_examples.setHead(7, inputList) shouldBe expected
  }

  "init should return the list without the last element" in {
    val inputList = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val expected: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
    List_examples.init(inputList) shouldBe expected
  }

  "length2 should return the length of the list" in {
    val inputList = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val expected: Int = 4
    List_examples.length2(inputList) shouldBe expected
  }

  "length3 should return the length of the list" in {
    val inputList = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val expected: Int = 4
    List_examples.length3(inputList) shouldBe expected
  }

  "reverse should return the reversed list" in {
    val inputList = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val expected: List[Int] = Cons(4, Cons(3, Cons(2, Cons(1, Nil))))
    List_examples.reverse(inputList) shouldBe expected
  }

  "appendRight should append a list to the right" in {
    val l1 = Cons(1, Cons(2, Nil))
    val l2 = Cons(3, Cons(4, Nil))
    val expected: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    appendRight(l1, l2) shouldBe expected
  }

  "appendLeft should append a list to the left" in {
    val l1 = Cons(1, Cons(2, Nil))
    val l2 = Cons(3, Cons(4, Nil))
    val expected: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    appendLeft(l1, l2) shouldBe expected
  }

  "concatenate should concatenate all of the lists" in {
    val l1 = Cons(1, Cons(2, Nil))
    val l2 = Cons(3, Cons(4, Nil))
    val l3 = Cons(5, Cons(6, Nil))
    val listOfLists = Cons(l1, Cons(l2, Cons(l3, Nil)))
    val expected: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
    // List(1, 2, 3, 4, 5, 6)
    concatenate(listOfLists) shouldBe expected
  }

  "add1 should add 1 to each element of the list" in {
    val inputList = Cons(1, Cons(2, Nil))
    val expected: List[Int] = Cons(2, Cons(3, Nil))
    // List(1, 2, 3, 4, 5, 6)
    add1(inputList) shouldBe expected
  }

  "convertDoubleToString should convert all doubles to strings in list" in {
    val list: List[Double] = Cons(1.1, Cons(2.2, Cons(3.3, Nil)))
    val expected: List[String] = Cons("1.1", Cons("2.2", Cons("3.3", Nil)))
    convertDoubleToString(list) shouldBe expected
  }

  "map should convert all elements in list to whatever function it is" in {
    val list = Cons(1, Cons(2, Nil))
    val expected: List[Int] = Cons(2, Cons(4, Nil))
    map(list)(x => x*2) shouldBe expected
  }

  "filter should take out all odd numbers" in {
    val list = Cons(1, Cons(2, Nil))
    val expected: List[Int] = Cons(2, Nil)
    filter(list)(x => x % 2 == 0) shouldBe expected
  }

  "flatMap should duplicate i" in {
    val list = Cons(1, Cons(2, Nil))
    val expected: List[Int] = Cons(1, Cons(1, Cons(2, Cons(2, Nil))))
    flatMap(list)(i => Cons(i, Cons(i, Nil))) shouldBe expected
  }

  "addListValues should add both lists and create one" in {
    val list1 = Cons(1, Cons(2, Nil))
    val list2 = Cons(1, Cons(2, Nil))
    val expected: List[Int] = Cons(2, Cons(4, Nil))
    addListValues(list1, list2) shouldBe expected
  }

  "generalizeListValues should take function and apply to both lists" in {
    val list1 = Cons(3, Cons(4, Nil))
    val list2 = Cons(1, Cons(2, Nil))
    val expected: List[Int] = Cons(4, Cons(6, Nil))
    generalizeListValues(list1, list2)((x: Int, y: Int) => x + y) shouldBe expected
  }

//  "hasSubsequence should take list and see if there is a subset" in {
//    val list1 = Cons(1, Cons(2, Cons(3, Nil)))
//    val expected: Boolean = true
//    generalizeListValues(list1, Cons(1, Cons(2, Nil))) shouldBe expected
//  }

  "countNodes should count all nodes in tree" in {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val expected: Int = 3
    countNodes(tree) shouldBe expected
  }

  "maxElement should say what is the maximum element in the tree" in {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val expected: Int = 3
    maxElement(tree) shouldBe expected
  }


}
