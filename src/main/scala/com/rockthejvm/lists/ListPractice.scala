package com.rockthejvm.lists

import java.security.InvalidParameterException

import scala.annotation.tailrec
import scala.runtime.Nothing$

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

//    // random sample
//    def sample(k: Int): RList[T]
//
//    /**
//      * Hard problems
//      */
//    // sorting the list in the order defined by the Ordering object
//    def insertionSort[S >: T](ordering: Ordering[S]): RList[S]
//    def mergeSort[S >: T](ordering: Ordering[S]): RList[S]
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

    // TODO: this is O(N^2) complexity, needs to be improved
    override def flatMap[S](f: T => RList[S]): RList[S] = {
      @tailrec
      def flatMapTailRec(remaining: RList[T], acc: RList[S]): RList[S] = {
        if (remaining.isEmpty) acc
        else {
          flatMapTailRec(remaining.tail, acc ++ f(remaining.head))
        }
      }

      flatMapTailRec(this, RNil)
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
  println(RList.from(1 to 1000).flatMap(x => (x + 3) :: RNil))
  println(RList.from(1 to 1000).filter(_ % 2 == 0))
  println(list2.rle)
  println(RNil.rle)
  println((2::3::2::4::RNil).duplicateEach(0))
  Range(1, 21).foreach { x =>
    println(RList.from(1 to 20).rotate(x))
  }
}
