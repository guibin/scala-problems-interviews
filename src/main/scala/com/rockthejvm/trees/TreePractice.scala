package com.rockthejvm.trees

import scala.annotation.tailrec

object TreePractice extends App {

  sealed abstract class Tree[+T] {
    def value: T
    def left: Tree[T]
    def right: Tree[T]
    def isEmpty: Boolean

    /**
      * Easy problems
      */
    def isLeaf: Boolean
    def collectLeaves: List[Tree[T]]
    def leafCount: Int

    /**
      * Medium difficulty problems
      */
    // the number of nodes in the tree
    def size: Int

    // nodes at a given level
    def collectNodes(level: Int): List[Tree[T]]

    // mirror a tree
    /*
          _____1_____                     _____1_____
         /           \                   /           \
       __2__       __6__       ->      __6__       __2__
      /     \     /     \             /     \     /     \
      3     4     7     8             8     7     4     3
             \                                   /
              5                                 5
     */
//    def mirror: Tree[T]

    // compare the shape of two trees
    /*
          _____1_____                     _____8_____
         /           \                   /           \
       __2__       __6__       ~~      __9__       __2__
      /     \     /     \             /     \     /     \
      3     4     7     8             1     3     2     7
             \                               \
              5                               4
    */
//    def sameShapeAs[S >: T](that: Tree[S]): Boolean

    // tree is symmetrical with respect to the root node
    /*
          _____1_____
         /           \
       __2__       __6__
      /     \     /     \
      3     4     7     8
     */
//    def isSymmetrical: Boolean

    // collect all nodes to a list
//    def toList: List[T]
  }

  case object End extends Tree[Nothing] {
    override def value: Nothing = throw new NoSuchElementException
    override def left: Tree[Nothing] = throw new NoSuchElementException
    override def right: Tree[Nothing] = throw new NoSuchElementException
    override def isEmpty: Boolean = true

    /**
      * Easy problems
      */
    override def isLeaf: Boolean = false
    override def collectLeaves: List[Tree[Nothing]] = List()
    override def leafCount: Int = 0
    override def size: Int = 0
    override def collectNodes(level: Int): List[Tree[Nothing]] = List()
  }

  case class Node[+T](override val value: T, override val left: Tree[T], override val right: Tree[T]) extends Tree[T] {
    override def isEmpty: Boolean = false

    /**
      * Easy problems
      */
    override def isLeaf: Boolean = left == End && right == End

    override def collectLeaves: List[Tree[T]] = {
      @tailrec
      def collectLeavesTailRec(remaining: List[Tree[T]], acc: List[Tree[T]]): List[Tree[T]] = {
        if (remaining.isEmpty) acc
        else if (remaining.head.isLeaf) collectLeavesTailRec(remaining.tail, remaining.head :: acc)
        else if (remaining.head.isEmpty) collectLeavesTailRec(remaining.tail, acc)
        else {
          val curr = remaining.head
          collectLeavesTailRec(curr.left :: curr.right :: remaining.tail, acc)
        }
      }

      collectLeavesTailRec(List(this), Nil)
    }

    override def leafCount: Int = {
      collectLeaves.length
    }

    override val size = 1 + left.size + right.size

    /**
      * Medium difficulty problems
      */
    def size2: Int = {
      @tailrec
      def sizeTailRec(remaining: List[Tree[T]], acc: Int): Int = {
        if (remaining.isEmpty) acc
        else {
          val curr = remaining.head
          if (curr.isEmpty) sizeTailRec(remaining.tail, acc)
          else if (!curr.left.isEmpty && !curr.right.isEmpty)  sizeTailRec(curr.left :: curr.right :: remaining.tail, 1 + acc)
          else if (!curr.left.isEmpty) sizeTailRec(curr.left :: remaining.tail, 1 + acc)
          else sizeTailRec(curr.right :: remaining.tail, 1 + acc)
        }
      }
      sizeTailRec(List(this), 0)
    }

    override def collectNodes(level: Int): List[Tree[T]] = {
      @tailrec
      def collectNodesTailRec(currentLevel: Int, acc: List[Tree[T]]): List[Tree[T]] = {
        if (acc.isEmpty) List()
        else if (currentLevel == level) acc
        else {
          val expanded = acc.flatMap { node =>
            if (!node.isEmpty) List(node.left, node.right)
            else List()
          }
          collectNodesTailRec(currentLevel + 1, expanded)
        }
      }

      def collectNodesTailRec2(currentLevel: Int, acc: List[Tree[T]]): List[Tree[T]] = {
        if (acc.isEmpty) List()
        else if (currentLevel == level) acc
        else {
          val expanded = for {
            node <- acc
            child <- List(node.left, node.right) if(!node.isEmpty)
          } yield child

          collectNodesTailRec2(currentLevel + 1, expanded)
        }
      }
      collectNodesTailRec2(0, List(this))
    }
  }

  val tree = Node(1,
    Node(2,
      Node(3, End, End),
      Node(4,
        End,
        Node(5, End, End),
      )
    ),
    Node(6,
      Node(7, End, End),
      Node(8, End, End)
    )
  )

  val tree10x = Node(10,
    Node(20,
      Node(30, End, End),
      Node(40,
        End,
        Node(50, End, End),
      )
    ),
    Node(60,
      Node(70, End, End),
      Node(80, End, End)
    )
  )

  val tree10xExtra = Node(10,
    Node(20,
      Node(30, End, End),
      Node(40,
        End,
        End
      )
    ),
    Node(60,
      Node(70, End, End),
      Node(80, End, End)
    )
  )

  println("------collectLeaves")
  println(tree.collectLeaves.map(_.value))
  println("------size")
  println(tree.size)
  val largeTree = (1 to 100000).foldLeft[Tree[Int]](End)((tree, number) => Node(number, tree, End))
  println(largeTree.size)
  println("-------collectNodes")
  println(largeTree.collectNodes(0))

}
