package dev.dimuzio
package lists

import scala.annotation.tailrec

/**
 * User: patricio
 * Date: 7/7/21
 * Time: 10:25
 */

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def headOption: Option[T]
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def headOption: Option[Nothing] = None

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {

  override def isEmpty: Boolean = false

  override def headOption: Option[T] = Some(head)

  override def apply(index: Int): T = {
    /*
    Complexity of this algorithm
    O(min(N, index))
     */
    @tailrec
    def helper(remaining: RList[T], count: Int = 0): T = {
      if (count == index) remaining.head
      //else if (remaining.isEmpty) throw new NoSuchElementException // no needed cause head empty throw the exception
      else helper(remaining.tail, count + 1)
    }
    if(index < 0) throw new NoSuchElementException
    helper(this)
  }

  override def toString: String = {
    @tailrec
    def toStringTailReq(remaining: RList[T], str: String = ""): String = {
      if (remaining.isEmpty) str
      else if (remaining.tail.isEmpty) str + remaining.head
      else toStringTailReq(remaining.tail, str + remaining.head + ", ")
    }
    s"[${toStringTailReq(this)}]"
  }

  override def length: Int = {
    /*
    O(N)
     */
    @tailrec
    def aux(list: RList[T], value: Int = 0): Int = {
      if (list.isEmpty) value
      else aux(list.tail, value + 1)
    }
    aux(this)
  }

  override def reverse: RList[T] = {
    /*
    O(N)
     */
    @tailrec
    def aux(list: RList[T], reversed: RList[T] = RNil): RList[T] = {
      if(list.isEmpty) reversed
      else aux(list.tail, reversed.::(list.head))
    }
    aux(this)
  }
}

object RList {
  def from[T](iterable: Iterable[T]):RList[T] = {
    @tailrec
    def aux(iterable: Iterable[T], acc: RList[T] = RNil): RList[T] = {
      if (iterable.isEmpty) acc
      else aux(iterable.tail, iterable.head :: acc)
    }
    aux(iterable).reverse
  }
}

object ListProblems extends App {

  //val aSmallList = ::(1,::(2,::(3, ::(4, RNil)))).::(0)
  val aSmallList = 1 :: 2 :: 3 :: 4 :: RNil
  val aSmallList2 = 1 :: RNil
  val aLargeList = RList.from(0 to 50000)
  println(aSmallList(2))
  println(aLargeList(5348))
  println(aLargeList(15200))
  println(aSmallList.length)
  println(aSmallList.reverse)
  println(aLargeList.reverse)
  println(aLargeList.length)
  println(aSmallList2.length)
  println(RList.from(0 to 500))

}
