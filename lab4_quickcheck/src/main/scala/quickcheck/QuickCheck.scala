package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //Якщо ви вставляєте будь-які два елементи в порожню купу, знаходження мінімуму результуючої купи повинно повернути найменший з двох елементів.
  property("min from two") = forAll { (m1: Int, m2: Int) =>
    val h = insert(m2,
      insert(m1, empty))

    val min = if (m1 > m2) m2 else m1
    findMin(h) == min
  }


//Якщо ви вставляєте елемент у порожню купу, а потім видаляєте мінімальний елемент, то отримана купа повинна бути порожньою.
  property("empty after delete min") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(
      deleteMin(h))
  }

  //Для будь-якої купи, ви повинні отримувати відсортовану послідовність елементів при постійному пошуку та видаленні мінімумів. (Підказка: рекурсія та допоміжні функції - ваші друзі.)
  property("find min should be sorted") = forAll{ h: H =>
    @tailrec
    def checkSort(h: H, prev: A): Boolean =
      if (isEmpty(h)) true
      else if (findMin(h) < prev) false
      else checkSort(deleteMin(h), findMin(h))

    (!isEmpty(h)) ==> {
      checkSort(deleteMin(h), findMin(h))
    }
  }

  property("find min should be sorted variant 2") = forAll{ h: H =>
    @tailrec
    def checkSort(h: H, prev: A): Boolean =
      isEmpty(h) ||
        (!(findMin(h) < prev) &&
          checkSort(deleteMin(h), findMin(h)))

    (!isEmpty(h)) ==> {
      checkSort(deleteMin(h), findMin(h))
    }
  }

  //Знаходження мінімуму злиття будь-яких двох куп повинно повернути мінімум першої або другої купи.
  property("merge heaps gives global minimum") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      val min = if (m1 > m2) m2 else m1

      findMin(meld(h1, h2)) == min
    }
  }


//  Додавання та видалення двох елементів до пустої купи повертає пусту купу
  property("add and delete on empty heap") = forAll { (a1: A, a2: A) =>
    val h = insert(a2,
      insert(a1, empty))

    isEmpty(
      deleteMin(
        deleteMin(h)))
  }

  //Якщо додати елемент більше мінімального, мінімальний елемент купи має залишитися без змін
  property("") = forAll { (a: A, h: H) => {
    def check(): Boolean = {
      if (isEmpty(h)) return true

      val min = findMin(h)
      if (min < a) findMin(insert(a, h)) == min
      else true
    }

    check()
    }
  }

  //Якщо поєднати дві черги, потім видалити мінімум першої, вставити в другу, поєднати і перевірити елементи черг, то вони мають бути однакові//Список елементів поєднання черг має не залежати від порядку поєднання
//  property("merged hips") = forAll{ (h1: H, h2: H) =>
//
//    @tailrec
//    def findElem(h: H): Boolean =
//      (!isEmpty(h)
//        && (findMin(h) == a
//        || findElem(deleteMin(h))))
//
//    findElem(
//      insert(a, h))
//  }

  //Якщо вставити елемент в купу, то вона буде його містити, навіть, якщо він не мінімальний
  property("find elem in heap") = forAll{ (a: A, h: H) =>
    @tailrec
    def findElem(h: H): Boolean =
      (!isEmpty(h)
        && (findMin(h) == a
          || findElem(deleteMin(h))))

    findElem(
      insert(a, h))
  }
}
