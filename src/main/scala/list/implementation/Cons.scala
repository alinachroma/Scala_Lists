/*
    @author: Alina Khairullina, Nataliia Pidhorodetska
    @version: 2020.05.24
 */

package list.implementation

import list.traits.IntList

case class Cons(head: Int, tail: IntList) extends SinglyLinkedIntList {
  override def isEmpty = false

  override def get(index: Int): Int = index match {
    case 0 => head
    case i => tail.get(i - 1)
  }

  override def append(elem: Int): IntList = Cons(head, tail.append(elem))


  override def contains(elem: Int): Boolean =
    if (head == elem) true
    else tail.contains(elem)

  /*
override def contains(elem: Int): Boolean = head match {
case elem => true
case _ => tail.contains(elem)
}
   */


  override def prepend(elem: Int): IntList = Cons(elem, this)

  override def delete(elem: Int): IntList =
    if (head == elem) tail
    else Cons(head, tail.delete(elem))

  override def deleteAll(elem: Int): IntList =
    if (head == elem) tail.deleteAll(elem)
    else Cons(head, tail.deleteAll(elem))
}