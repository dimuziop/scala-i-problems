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
  /**
   * Easy problems
   */
  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]
  def ++[S >: T](anotherList: RList[S]): RList[S]
  def removeAt(index: Int): RList[T]

  // the big three
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  /**
   * Medium difficulty
   */
  // run -length encoding
  def rle: RList[(T, Int)]
  def duplicateEach(times: Int): RList[T]
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

  // Appends list
  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  /**
   * Medium difficulty
   */
  override def rle: RList[(Nothing, Int)] = RNil

  override def duplicateEach(times: Int): RList[Nothing] = RNil
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

  override def ++[S >: T](anotherList: RList[S]): RList[S] = { // head :: (tail ++ anotherList) // stack recursive
    /*
    Complexity: O(M+N)
     */
    @tailrec
    def aux(list: RList[S], concatenated: RList[S]): RList[S] = {
      if (list.isEmpty) concatenated
      else aux(list.tail, list.head :: concatenated)
    }
    aux(this.reverse, anotherList)
  }

  /*
  Complexity: O(N)
   */
  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def aux(list: RList[T], newList: RList[T] = RNil, currentPosition: Int = 0): RList[T] = {
      if(currentPosition < index && list.isEmpty) newList.reverse
      else if (currentPosition == index) newList.reverse ++ list.tail
      else aux(list.tail, list.head :: newList, currentPosition + 1)
    }
    if (index < 0) return this
    aux(this)
  }

  // Complexity O(N)
  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def aux(list: RList[T], newList: RList[S] = RNil): RList[S] = {
      if (list.isEmpty) newList.reverse
      else aux(list.tail, f(list.head) :: newList)
    }
    aux(this)
  }

  // sum of all the length of f(x) = Z
  //Complexity: O(Z^2)
  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def aux(list: RList[T], newList: RList[S] = RNil): RList[S] = {
      if (list.isEmpty) newList.reverse
      else aux(list.tail, f(list.head).reverse ++ newList)
    }
    aux(this)
  }

  // Complexity O(N)
  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def aux(list: RList[T], newList: RList[T] = RNil): RList[T] = {
      if(list.isEmpty) newList.reverse
      else aux(list.tail, if (f(list.head)) list.head :: newList else newList)
    }
    aux(this)
  }

  /**
   * Medium difficulty
   */
    /*
    Complexity: O(N)
     */
  override def rle: RList[(T, Int)] = {
    @tailrec
    def aux(remainingList: RList[T], lastTuple: (T, Int), newList: RList[(T, Int)] = RNil) : RList[(T, Int)] = {
      if (remainingList.isEmpty && lastTuple._2 == 0) newList.reverse
      else if (remainingList.isEmpty) (lastTuple :: newList).reverse
      else if (remainingList.head == lastTuple._1) aux(remainingList.tail, lastTuple._1 -> (lastTuple._2 + 1), newList) // could be lastTuple.copy()
      else aux(remainingList.tail, remainingList.head -> 1, lastTuple :: newList)
    }

    aux(this.tail, this.head -> 1)
  }

  //Complexity: O(N * K)
  override def duplicateEach(times: Int): RList[T] = {
    @tailrec
    def aux(value: T, acc: RList[T] = RNil, current: Int = 0): RList[T] = {
      if (current == times) acc
      else aux(value, value :: acc, current + 1)
    }

    this.flatMap(x => aux(x))
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

  def testEasyFunctions(): Unit = {//val aSmallList = ::(1,::(2,::(3, ::(4, RNil)))).::(0)
  val aSmallList = 1 :: 2 :: 3 :: 4 :: RNil
  val aSmallList2 = 1 :: 2 :: RNil
  val aLargeList = RList.from(0 to 5000000)
  /*println(aSmallList(2))
  println(aLargeList(5348))
  println(aLargeList(15200))
  println(aSmallList.length)
  println(aSmallList.reverse)
  println(aLargeList.reverse)
  println(aLargeList.length)
  println(aSmallList2.length)
  println(RList.from(0 to 500))*/

  //println(aSmallList ++ aSmallList2)

  println(aSmallList)
  println(aSmallList.removeAt(10))

  println(aSmallList.map(x => x * 2))
  println(aSmallList.filter(x => x % 2 == 0))
  val time = System.currentTimeMillis()
  //aLargeList.flatMap(x => x :: x *2 :: RNil) // 3 secs
  aLargeList.map(_ + 2) // 1sec
  println("time:" + (System.currentTimeMillis() - time))
}

  def testMediumDifficultyFunctions(): Unit = {
    println((1 :: 1 :: 1 :: 2 :: 2 :: 3 :: RNil).rle)
    println((1 :: 2 :: 3 :: 4 :: 5 :: RNil).duplicateEach(3))
    println((1 :: 2 :: 3 :: 4 :: 5 :: RNil).duplicateEach(1))
    println((1 :: 2 :: 3 :: 4 :: 5 :: RNil).duplicateEach(0))
    println(RList.from(1 to 15000).duplicateEach(5))
  }

  testMediumDifficultyFunctions()


}
