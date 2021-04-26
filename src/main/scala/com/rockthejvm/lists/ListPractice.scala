package com.rockthejvm.lists

import scala.annotation.tailrec

object ListPractice extends App {
  sealed abstract class RList[+T] {
    /**
      * Standard functions
      */
    def head: T
    def tail: RList[T]
    def isEmpty: Boolean
    def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

    /**
      * Easy problems
      */
    // get element at a given index
    def apply(index: Int): T

    // the size of the list
    def length: Int

//    // reverse the list
//    def reverse: RList[T]
//
//    // concatenate another list to this one
//    def ++[S >: T](anotherList: RList[S]): RList[S]
//
//    // remove an element at a given index, return a NEW list
//    def removeAt(index: Int): RList[T]
//
//    // the big 3
//    def map[S](f: T => S): RList[S]
//    def flatMap[S](f: T => RList[S]): RList[S]
//    def filter(f: T => Boolean): RList[T]
//
//    /**
//      * Medium difficulty problems
//      */
//    // run-length encoding
//    def rle: RList[(T, Int)]
//
//    // duplicate each element a number of times in a row
//    def duplicateEach(k: Int): RList[T]
//
//    // rotation by a number of positions to the left
//    def rotate(k: Int): RList[T]
//
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
  }

  val list2 = 1 :: 2 :: 3 :: 4 :: RNil
  println(list2(1))
  println(list2)
//  println(list2(6))
  println(list2.length)
}
