package com.rockthejvm.lists

import java.security.InvalidParameterException

import scala.annotation.tailrec
import scala.util.Random

object ListPractice extends App {
  sealed abstract class RList[+T] {
    /**
      * Standard functions
      */
    def head: T
    def tail: RList[T]
    def isEmpty: Boolean
    // prepend
    def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

    /**
      * Easy problems
      */
    // get element at a given index
    def apply(index: Int): T

    // the size of the list
    def length: Int

    // reverse the list
    def reverse: RList[T]

    // concatenate another list to this one
    def ++[S >: T](anotherList: RList[S]): RList[S]

    // remove an element at a given index, return a NEW list
    def removeAt(index: Int): RList[T]

    // the big 3
    def map[S](f: T => S): RList[S]
    def flatMap[S](f: T => RList[S]): RList[S]
    def filter(f: T => Boolean): RList[T]

    /**
      * Medium difficulty problems
      */
    // run-length encoding
    def rle: RList[(T, Int)]

    // duplicate each element a number of times in a row
    def duplicateEach(k: Int): RList[T]

    // rotation by a number of positions to the left
    def rotate(k: Int): RList[T]

    // random sample
    def sample(k: Int): RList[T]

    /**
      * Hard problems
      */
    // sorting the list in the order defined by the Ordering object
    def insertionSort[S >: T](ordering: Ordering[S]): RList[S]
    def mergeSort[S >: T](ordering: Ordering[S]): RList[S]
//    def quickSort[S >: T](ordering: Ordering[S]): RList[S]
  }

  case object RNil extends RList[Nothing] {
    override def head: Nothing = throw new NoSuchElementException
    override def tail: RList[Nothing] = throw new NoSuchElementException
    override def isEmpty: Boolean = true
    override def toString: String = "[]"
    override def apply(index: Int): Nothing = throw new IndexOutOfBoundsException
    override def length: Int = 0
    override def reverse: RList[Nothing] = this
    override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList
    override def removeAt(index: Int): RList[Nothing] = this
    override def map[S](f: Nothing => S): RList[S] = this
    override def flatMap[S](f: Nothing => RList[S]): RList[S] = this
    override def filter(f: Nothing => Boolean): RList[Nothing] = this
    override def rle: RList[(Nothing, Int)] = this
    override def duplicateEach(k: Int): RList[Nothing] = this
    override def rotate(k: Int): RList[Nothing] = this
    override def sample(k: Int): RList[Nothing] = this
    override def insertionSort[S >: Nothing](ordering: Ordering[S]): RList[S] = this
    override def mergeSort[S >: Nothing](ordering: Ordering[S]): RList[S] = this
  }

  case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
    override def isEmpty: Boolean = false

    override def toString: String = {
      @tailrec
      def toStringTailrec(remaining: RList[T], result: String): String = {
        if (remaining.isEmpty) result
        else if (remaining.tail.isEmpty) s"$result${remaining.head}"
        else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
      }

      "[" + toStringTailrec(this, "") + "]"
    }

    override def apply(index: Int): T = {
      @tailrec
      def get(remaining: RList[T], currentIndex: Int): T = {
        if (currentIndex == index) remaining.head
        else get(remaining.tail, currentIndex + 1)
      }

      if (index < 0) throw new NoSuchElementException
      get(this, 0)
    }

    override def length: Int = {
      @tailrec
      def tailRecLength(remaining: RList[T], currentLength: Int): Int = {
        if (remaining == RNil) currentLength
        else tailRecLength(remaining.tail, currentLength + 1)
      }

      tailRecLength(this, 0)
    }

    override def reverse: RList[T] = {
      @tailrec
      def reverseTailRec(reversed: RList[T], tl: RList[T]): RList[T] = {
        tl match {
          case RNil => reversed
          case remaining => reverseTailRec(remaining.head :: reversed, remaining.tail)
        }
      }
      reverseTailRec(RNil, this)
    }

    override def ++[S >: T](anotherList: RList[S]): RList[S] = {
      @tailrec
      def concatTailRec(remaining: RList[S], acc: RList[S]): RList[S] = {
        if (remaining.isEmpty) acc
        else {
          concatTailRec(remaining.tail, remaining.head :: acc)
        }
      }

      // reverse is O(M), concatTailRec is O(M), M is length of this list
      // so the complexity is O(2M)
      concatTailRec(this.reverse, anotherList)
    }

    override def removeAt(index: Int): RList[T] = {
      @tailrec
      def removeTailRec(currentIdx: Int, first: RList[T], remaining: RList[T]): RList[T] = {
        if (currentIdx == index) remaining.reverse ++ first.tail
        else {
          removeTailRec(currentIdx + 1, first.tail, first.head :: remaining)
        }
      }

      removeTailRec(0, this, RNil)
    }

    override def map[S](f: T => S): RList[S] = {
      @tailrec
      def mapTailRec(remaining: RList[T], acc: RList[S]): RList[S] = {
        if(remaining.isEmpty) acc.reverse
        else {
          mapTailRec(remaining.tail, f(remaining.head) :: acc)
        }
      }

      mapTailRec(this, RNil)
    }

    // TODO: this is O(N^2) complexity, needs to be improved. betterMapTailRec is implemented as O(N)

    override def flatMap[S](f: T => RList[S]): RList[S] = {
      // traverse the whole list: O(N)
      // during each traverse, ++ is O(M)
      // So the complexity is O(Z^2), Z is sum length of f(...)
      @tailrec
      def flatMapTailRec(remaining: RList[T], acc: RList[S]): RList[S] = {
        if (remaining.isEmpty) acc
        else {
          flatMapTailRec(remaining.tail, acc ++ f(remaining.head))
        }
      }

      // Implement this with O(N) complexity
      @tailrec
      def betterMapTailRec(remaining: RList[T], acc: RList[RList[S]]): RList[S] = {
        if (remaining.isEmpty) concatenateAll(acc)
        else {
          betterMapTailRec(remaining.tail, f(remaining.head).reverse :: acc)
        }
      }

      // concatenateAll behaves like flatten
      def concatenateAll(elements: RList[RList[S]]): RList[S] = {
        @tailrec
        def concatenateAllTailRec(remaining: RList[RList[S]], current: RList[S], acc: RList[S]): RList[S] = {
          if (remaining.isEmpty && current.isEmpty) acc
          else if (!remaining.isEmpty && current.isEmpty) concatenateAllTailRec(remaining.tail, remaining.head, acc)
          else concatenateAllTailRec(remaining, current.tail, current.head :: acc)
        }
        concatenateAllTailRec(elements, RNil, RNil)
      }

//      flatMapTailRec(this, RNil)
      betterMapTailRec(this, RNil)
    }

    override def filter(f: T => Boolean): RList[T] = {
      @tailrec
      def filterTailRec(remaining: RList[T], acc: RList[T]): RList[T] = {
        if (remaining.isEmpty) acc.reverse
        else if (f(remaining.head)){
          filterTailRec(remaining.tail, remaining.head :: acc)
        } else {
          filterTailRec(remaining.tail, acc)
        }
      }

      filterTailRec(this, RNil)
    }

    /**
      * Medium difficulty problems
      */
    override def rle: RList[(T, Int)] = {
      @tailrec
      def rleTailRec(remaining: RList[T], acc: RList[(T, Int)]): RList[(T, Int)] = {
        if (remaining.isEmpty) acc.reverse
        else if (acc.isEmpty){
          rleTailRec(remaining.tail, (remaining.head, 1) :: RNil)
        } else if (acc.head._1 == remaining.head) {
          rleTailRec(remaining.tail, (acc.head._1, acc.head._2 + 1) :: acc.tail)
        } else {
          rleTailRec(remaining.tail, (remaining.head, 1) :: acc)
        }
      }

      rleTailRec(this, RNil)
    }

    override def duplicateEach(k: Int): RList[T] = {
      @tailrec
      def duplicateEachTailRec(remaining: RList[T], currCount: Int, currItem: T, acc: RList[T]): RList[T] = {
        if (remaining.isEmpty) {
          if (currCount == 0) acc.reverse
          else duplicateEachTailRec(remaining, currCount - 1, currItem, currItem :: acc)
        } else {
          if (currCount == 0) duplicateEachTailRec(remaining.tail, k, remaining.head, acc)
          else duplicateEachTailRec(remaining, currCount - 1, currItem, currItem :: acc)
        }
      }

      if (this.isEmpty && k < 0) RNil
      else if (k == 0) this
      else duplicateEachTailRec(this.tail, k, this.head, RNil)
    }

    override def rotate(k: Int): RList[T] = {
      @tailrec
      def rotateTailRec(remaining: RList[T], currCount: Int, acc: RList[T]): RList[T] = {
        if (remaining.isEmpty) acc
        else if (currCount == 0) remaining ++ (acc.reverse)
        else {
          rotateTailRec(remaining.tail, currCount - 1, remaining.head :: acc)
        }
      }

      if (k < 0) throw new InvalidParameterException
      else if (k == 0) this
      else rotateTailRec(this, k, RNil)
    }

    override def sample(k: Int): RList[T] = {
      @tailrec
      def sampleTailRec(remaining: RList[T], currentCount: Int, acc: RList[T]): RList[T] = {
        if (currentCount == 0) acc.reverse
        else if (remaining.isEmpty) acc.reverse
        else {
          val remainingLen = remaining.length
          // The same item could only be selected once
          val randomIdx = Random.nextInt(remainingLen)
          val randomNode = remaining(randomIdx)
          sampleTailRec(remaining.removeAt(randomIdx), currentCount - 1, randomNode :: acc)
        }
      }

      val length = this.length
      def elegantSample(): RList[T] = {
        val randomed = Range(1, k).map { _ =>
          // The same item could be selected multiple times
          val idx = Random.nextInt(length)
          apply(idx)
        }
        RList.from(randomed)
      }

      if (k < 0) RNil
      else {
//        sampleTailRec(this, k, RNil)
        elegantSample()
      }
    }

    /**
      * Hard problems
      */
    override def insertionSort[S >: T](ordering: Ordering[S]): RList[S] = {

      @tailrec
      def insertionSortTailRec(remaining: RList[S], acc: RList[S]): RList[S] = {
        if (remaining.isEmpty) acc
        else insertionSortTailRec(remaining.tail, insertionSorted(remaining.head, RNil, acc))
      }

      @tailrec
      def insertionSorted(element: S, first: RList[S], second: RList[S]): RList[S] = {
        if (second.isEmpty || ordering.lteq(element, second.head)) first.reverse ++ (element :: second)
        else insertionSorted(element, second.head :: first, second.tail)
      }

      insertionSortTailRec(this, RNil)
    }

    override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = {

//      def partitionTailRec(first: RList[S], second: RList[S])
      ???
    }
  }

  object RList {
    def from[T](iterable: Iterable[T]): RList[T] = {
      @tailrec
      def fromRec(acc: RList[T], remaining: Iterable[T]): RList[T] = {
        if (remaining.isEmpty) acc
        else fromRec(remaining.head :: acc, remaining.tail)
      }
      fromRec(RNil, iterable).reverse
    }
  }

  val list2 = 1 :: 2 :: 3 :: 3 :: 3 :: 2 :: 2 :: 4 :: RNil
  println(list2(1))
  println(list2)
//  println(list2(6))
  println(list2.length)
  println(list2.reverse)
  println(RList.from(Range(1, 10)))
  println(RList.from(1 to 10) ++ RList.from(20 to 30))
  println(RList.from(1 to 10).removeAt(3))
  println("----")
  println(RList.from(1 to 1000).map(_ * 2))
  println("flatMap---")
  println(RList.from(1 to 1000).flatMap(x => x :: (x * 2) :: RNil))
  println("filter----")
  println(RList.from(1 to 1000).filter(_ % 2 == 0))
  println(list2.rle)
  println(RNil.rle)
  println((2::3::2::4::RNil).duplicateEach(0))
  Range(1, 21).foreach { x =>
    println(RList.from(1 to 20).rotate(x))
  }
  println("Random RList")
  Range(1, 20).foreach { x =>
    println(RList.from(1 to 20).sample(6))
  }
  println(s"-----sort")
  val anUnorderedList = 3 :: 1 :: 2 :: 4 :: 5 :: RNil
  val ordering = Ordering.fromLessThan[Int](_ < _)
  println(anUnorderedList.insertionSort(ordering))
}
