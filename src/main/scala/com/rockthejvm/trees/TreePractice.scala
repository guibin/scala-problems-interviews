package com.rockthejvm.trees

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
//    def size: Int

    // nodes at a given level
//    def collectNodes(level: Int): List[Tree[T]]

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


}
