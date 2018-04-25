package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  //from the course:
  lazy val genMap: Gen[Map[Int,Int]] = oneOf(
    const(Map.empty[Int,Int]),
    for {
      k <- arbitrary[Int]
      v <- arbitrary[Int]
      m <- oneOf(const(Map.empty[Int,Int]), genMap)
    } yield m.updated(k, v)
  )

  //origen: lazy val genHeap: Gen[H] = ???
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)
  )


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //from the course
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }



  //my own properties
  property("minfrom1") = forAll { a: Int =>
    if (a <= Int.MinValue) {
      val h = insert(a, empty)
      val h2 = insert(Int.MinValue, h)
      val min = findMin(h2)
      min == Int.MinValue || min == a
    } else {
      val aMinus1 = a -1
      val h = insert(a, empty)
      val h2 = insert(aMinus1, h)
      findMin(h2) == aMinus1
    }
  }

  //my own properties
  property("minfrom2") = forAll { (a: Int, b: Int) =>
    val minVal = a min b
    val h = insert(a, empty)
    val h2 = insert(b, h)
    findMin(h2) == minVal
  }

  //my own properties
  property("insertanddelete") = forAll { a: Int =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    h2 == empty
  }

  //my own properties
  property("sortedsequence") = forAll { anyHeap: H =>
    if (isEmpty(anyHeap))
      true
    else {
      val min = findMin(anyHeap)
      val h = deleteMin(anyHeap)
      isSorted(min, h)
    }
  }

  def isSorted(minmin: Int, h: H): Boolean = {
    if (isEmpty(h)) true
    else {
      val min = findMin(h)
      val h2 = deleteMin(h)
      if (isTheMin(minmin, min)) isSorted(min, h2) else false
    }
  }

  def isTheMin(minmin: Int, min: Int): Boolean = ord.compare(minmin, min) < 1


  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  //my own properties
  property("min_of_melding") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2))
      meld(h1, h2) == empty
    else if (!isEmpty(h1) && !isEmpty(h2)) {
      val minH1 = findMin(h1)
      val minH2 = findMin(h2)
      val h3 = meld(h1, h2)
      val minH3 =findMin(h3)
      minH3 == minH1 || minH3 == minH2
    } else if (!isEmpty(h1)) {
      val minH1 = findMin(h1)
      val h3 = meld(h1, h2)
      val minH3 =findMin(h3)
      minH3 == minH1
    } else { // (isEmpty(h2)) {
      val minH2 = findMin(h2)
      val h3 = meld(h2, h1)
      val minH3 =findMin(h3)
      minH3 == minH2
    }
  }

  //my own properties
  property("insert_delete_insert_delete") = forAll { (h: H) =>
    if (isEmpty(h)) {
      val h2 = insert(Int.MaxValue, h)
      val h3 = insert(Int.MinValue, h2)
      val minH3 = findMin(h3)
      val h4 = deleteMin(h3)
      val minH4 = findMin(h4)
      val h5 = deleteMin(h4)
      minH3 == Int.MinValue && !isEmpty(h4) && minH4 == Int.MaxValue && isEmpty(h5)
    } else {
      val h2 = insert(Int.MinValue, h)
      val minH2 = findMin(h2)
      val h3 = deleteMin(h2)
      minH2 == Int.MinValue && !isEmpty(h3)
    }
  }

  //my own properties
  property("this_is_it") = forAll { (h: H) =>
    if (isEmpty(h)) thisIsIt(insert(Int.MaxValue, h)) else thisIsIt(h)
  }

  def thisIsIt(h:H):Boolean = {
    val h2 = insert(Int.MaxValue, h)
    val h3 = deleteMin(h2)
    val h4 = insert(Int.MinValue, h2)
    !isEmpty(h3) && findMin(meld(h,meld(h3, h4))) == Int.MinValue && thisShouldBeEmpty(h)
  }

  //my own properties
  property("delete_until_empty") = forAll {(h: H) => thisShouldBeEmpty(h)}

  def thisShouldBeEmpty(h:H):Boolean = if (isEmpty(h)) true else thisShouldBeEmpty(deleteMin(h))

  //my own properties
  property("insert_0,1,2...") = forAll {(h: H) =>
    val h2 = insertBatch(insert(Int.MinValue,h), 1000)
    findMin(h2) == Int.MinValue
  }

  def insertBatch(h:H, count:Int):H =
    if (count == 0) h else insertBatch(insert(Int.MinValue + count, h), count -1 )

  //1.390.270.103 did not equal 540.375.547
  //2.147.483.647
  //my own properties
  property("insert_and_delete_001") = forAll { (h: H) =>
    val iterations = 1000
    if (isEmpty(h)) {
      val h0 = insert(Int.MinValue, h)
      val h2 = insertBatch2(h0, iterations)
      val h3 = deleteBatch2(h2, iterations)
      h3 == h0 && (deleteMin(h0) == h)
    } else {
      val h2 = insertBatch2(h, iterations)
      val h3 = deleteBatch2(h2, iterations)
      h3 == h
    }
  }

  def insertBatch2(h:H, count:Int):H =
    if (count == 0) h else insertBatch2(insert(Int.MinValue, h), count - 1 )

  def deleteBatch2(h:H, count:Int):H =
    if (count == 0) h else deleteBatch2(deleteMin(h), count - 1 )


  //my own properties
  property("two_integers") = forAll { (x: Int, y:Int) =>
    val h = insert(x, insert(y, empty))
    val minVal = findMin(h)
    val h2 = deleteMin(h)
    val h3 = deleteMin(h2)
    minVal == (x min y) && findMin(h2) == (x max y) && h3 == empty
  }

  //my own properties
  property("three_integers") = forAll { (x: Int, y:Int, z:Int) =>
    val list = List(x, y, z)
    val h = insert(x, insert(y, insert(z, empty)))
    val minVal = findMin(h)
    val h2 = deleteMin(h)//1
    val minVal2 = findMin(h2)
    val h3 = deleteMin(h2)//2
    val minVal3 = findMin(h3)
    val h4 = deleteMin(h3)//3
    val list2 = withoutFirst(list)(e => e == minVal)
    val list3 = withoutFirst(list2)(e => e == minVal2)
    minVal == (x min y min z) && h4 == empty && list2.min == minVal2 && list3.min == minVal3
  }

  def withoutFirst[A](xs: List[A])(p: A => Boolean) = {
    var found = false
    xs.filter(x => found || !p(x) || { found=true; false })
  }

}
