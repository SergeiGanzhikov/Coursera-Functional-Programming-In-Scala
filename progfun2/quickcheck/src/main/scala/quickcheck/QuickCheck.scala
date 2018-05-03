package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // Add element to empty
  // findMin returns this element
  property("min1") = forAll { value: Int =>
    val heap = insert(value, empty)
    findMin(heap) == value
  }

  // Add min element, findMin
  // gives this element
  property("gen1") = forAll { (heap: H) =>
    val min =
      if (isEmpty(heap)) 0
      else findMin(heap)
    findMin(insert(min, heap)) == min
  }

  // Insert two
  // findMin returns smallest
  property("min after two insertions is smallest of values") = forAll { (x: A, y: A) =>
    val heap = insert(x, insert(y, empty))
    findMin(heap) == Math.min(x, y)
  }

  // Insert to empty, delete minimum
  // makes heap empty
  property("min of heap of one makes heap empty") = forAll { value: A =>
    val heap = insert(value, empty)
    deleteMin(heap) == empty
  }

  // consequent findMin in any heap
  // gives sorted sequence
  property("heap sort") = forAll { (list: List[A]) =>
    def toList(h: H): List[A] =
      if (isEmpty(h)) Nil
      else findMin(h) :: toList(deleteMin(h))

    val heap = list.foldRight(empty)(insert)
    toList(heap) == list.sorted
  }

  // findMin of melding of two heaps
  // gives min of one of the other
  property("min of melding is min of one of the heaps") = forAll { (heapA: H, heapB: H) =>
    findMin(meld(heapA, heapB)) == Math.min(findMin(heapA), findMin(heapB))
  }
}
