/*
    @author: Alina Khairullina, Nataliia Pidhorodetska
    @version: 2020.05.24
 */

package list.implementation

import list.traits.IntList

/**
  * A companion object for the singly linked list.
  * This enables creating lists list this: val list = SinglyLinkedIntList(1,2,3)
  * which results in Cons(1,Cons(2,Cons(3,Empty))))
  */
object SinglyLinkedIntList {


  /** The apply function is a special function in scala.
    * It can be invoked with SinglyLinkedIntList.apply(args) or simply SinglyLinkedIntList(args).
    * This particular implementation of it is also a variadic function, i.e.
    * a function which accepts one or more arguments of the same type (integers) as parameters.
    */
  //inside this method xs is of type Seq[int]
  def apply(xs: Int*): SinglyLinkedIntList = xs match {
    case Seq() => Empty
    //: _* results in the sequence being passed as multiple parameters - (1,2,3) instead of Seq[Int]{1,2,3}
    case _ => Cons(xs.head, SinglyLinkedIntList(xs.tail: _*))
  }
}
//
abstract class SinglyLinkedIntList extends IntList {

  override def prefix(other: IntList): IntList = other match {
    case Empty => this
    case Cons(h, t) => Cons(h, this.prefix(t))
  }

  override def size: Int = this match {
    case Empty => 0
    case Cons(_, t) => 1 + t.size
  }

  override def map(mapFunc: Int => Int): IntList = this match {
    case Empty => Empty
    case Cons(h, t) => Cons(mapFunc(h), t.map(mapFunc))
  }

  override def filter(filterFunc: Int => Boolean): IntList = this match {
    case Empty => Empty
    case Cons(h, t) => {
      if (filterFunc(h)) Cons(h, t.filter(filterFunc))
      else t.filter(filterFunc)
    }
  }

  //foldLeft starts on the left side—the first item—and iterates to the right
  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match {
    case Empty => initial
    case Cons (h, t) => t.foldLeft(reduceFunc(initial, h))(reduceFunc)
  }

  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int =
    tail.foldLeft(head)(reduceFunc)

  //Second variant for reduceLeft
  /*
  override def reduceLeft (reduceFunc: (Int, Int) => Int): Int =
    tail.tail.foldLeft(reduceFunc(head, tail.head))(reduceFunc)
  */

 override def forAll(predicateFunc: Int => Boolean): Boolean = this.size match {
    case 0 => true
    case _ => {
      if (!predicateFunc(head)) false
      else tail.forAll (predicateFunc)
    }
  }

  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match {
    case Empty => initial
    case Cons (h, t) => reduceFunc(initial, t.foldRight(h)(reduceFunc))
  }


  override def reduceRight(reduceFunc: (Int, Int) => Int): Int =
    tail.foldRight(head)(reduceFunc)


  override def insertionSort: IntList = this match {
    case Empty => Empty
    case Cons (h, Empty) => Cons (h, Empty)
    case Cons (h, t) => {
      if (h > t.head) Cons (t.head, Cons (h, t.tail.insertionSort).insertionSort).insertionSort
      else Cons (h, t.insertionSort)
    }
  }

  override def insertSorted(elem: Int): IntList =
    Cons (elem, this).insertionSort

  override def foldLeft[A](initial: A)(reduceFunc: (A, Int) => A): A = this match {
    case Empty => initial
    case Cons (h, t) => t.foldLeft(reduceFunc(initial, h))(reduceFunc)
  }

}