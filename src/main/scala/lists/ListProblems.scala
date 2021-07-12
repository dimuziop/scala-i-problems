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
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def headOption: Option[Nothing] = None

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

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

}

object ListProblems extends App {

  //val aSmallList = ::(1,::(2,::(3, ::(4, RNil)))).::(0)
  val aSmallList = 1 :: 2 :: 3 :: 4 :: RNil
  println(aSmallList(2))

}
