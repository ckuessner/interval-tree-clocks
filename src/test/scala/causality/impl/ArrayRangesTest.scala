package de.tu_darmstadt.stg.daimpl
package causality.impl

import causality.impl.Defs.Time

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.language.implicitConversions

class ArrayRangesTest extends AnyFlatSpec {

  "intersect" should "work" in {
    var left  = ArrayRanges(Seq((1, 2)))
    var right = ArrayRanges(Seq((1, 2)))

    (left intersect right) shouldEqual ArrayRanges(Seq((1, 2)))
    (right intersect left) shouldEqual ArrayRanges(Seq((1, 2)))

    right = ArrayRanges(Seq((2, 3)))

    (left intersect right) shouldEqual ArrayRanges.empty
    (right intersect left) shouldEqual ArrayRanges.empty

    left = ArrayRanges.empty
    right = ArrayRanges.empty

    (left intersect right) shouldEqual ArrayRanges.empty
    (right intersect left) shouldEqual ArrayRanges.empty

    left = ArrayRanges(Seq((0, 10), (15, 20)))
    (left intersect right) shouldEqual ArrayRanges.empty
    (right intersect left) shouldEqual ArrayRanges.empty

    right = ArrayRanges(Seq((0, 10)))
    (left intersect right) shouldEqual ArrayRanges(Seq((0, 10)))
    (right intersect left) shouldEqual ArrayRanges(Seq((0, 10)))

    right = ArrayRanges(Seq((2, 8)))
    (left intersect right) shouldEqual ArrayRanges(Seq((2, 8)))
    (right intersect left) shouldEqual ArrayRanges(Seq((2, 8)))

    right = ArrayRanges(Seq((2, 3), (4, 6)))
    (left intersect right) shouldEqual ArrayRanges(Seq((2, 3), (4, 6)))
    (right intersect left) shouldEqual ArrayRanges(Seq((2, 3), (4, 6)))

    right = ArrayRanges(Seq((2, 3), (4, 12)))
    (left intersect right) shouldEqual ArrayRanges(Seq((2, 3), (4, 10)))
    (right intersect left) shouldEqual ArrayRanges(Seq((2, 3), (4, 10)))

    right = ArrayRanges(Seq((2, 3), (4, 12)))
    (left intersect right) shouldEqual ArrayRanges(Seq((2, 3), (4, 10)))
    (right intersect left) shouldEqual ArrayRanges(Seq((2, 3), (4, 10)))

    right = ArrayRanges(Seq((2, 3), (4, 16), (18, 22)))
    (left intersect right) shouldEqual ArrayRanges(Seq((2, 3), (4, 10), (15, 16), (18, 20)))
    (right intersect left) shouldEqual ArrayRanges(Seq((2, 3), (4, 10), (15, 16), (18, 20)))

    right = ArrayRanges(Seq((10,15)))
    (left intersect right) shouldEqual ArrayRanges.empty
    (right intersect left) shouldEqual ArrayRanges.empty
  }

  private implicit def iteratorConv(range: Iterator[Int]): Iterator[Time] = range.map(i => i)

  "from" should "work" in {
    ArrayRanges.from(Seq[Time](1, 2, 3).iterator) shouldEqual ArrayRanges(Seq((1, 4)))
    ArrayRanges.from(Seq[Time](3, 2, 1).iterator) shouldEqual ArrayRanges(Seq((1, 4)))

    ArrayRanges.from(Seq[Time](1, 3).iterator) shouldEqual ArrayRanges(Seq((1, 2), (3, 4)))
    ArrayRanges.from(Seq[Time](3, 1).iterator) shouldEqual ArrayRanges(Seq((1, 2), (3, 4)))

    ArrayRanges.from(((1 to 3) ++ (5 to 8)).iterator) shouldEqual ArrayRanges(Seq((1, 4), (5, 9)))
    ArrayRanges.from(((5 to 8) ++ (1 to 3)).iterator) shouldEqual ArrayRanges(Seq((1, 4), (5, 9)))
  }

  it should "work with duplicates" in {
    ArrayRanges.from(Seq[Time](1,1).iterator) shouldEqual ArrayRanges(Seq((1, 2)))
    ArrayRanges.from(Seq[Time](1,3,3).iterator) shouldEqual ArrayRanges(Seq((1, 2),(3,4)))
    ArrayRanges.from(Seq[Time](1,3,3,3,4,4,5,8,8,10).iterator) shouldEqual ArrayRanges(Seq((1, 2),(3,6),(8,9),(10,11)))
  }

  "subtract" should "result in empty range for complete overlap" in {
    (ArrayRanges(Seq((1,10))) subtract ArrayRanges(Seq((1,10)))) shouldEqual ArrayRanges.empty
    (ArrayRanges(Seq((1,10))) subtract ArrayRanges(Seq((0,10)))) shouldEqual ArrayRanges.empty
    (ArrayRanges(Seq((1,10))) subtract ArrayRanges(Seq((0,11)))) shouldEqual ArrayRanges.empty

    (ArrayRanges(Seq((1,5), (6,10))) subtract ArrayRanges(Seq((1,5), (6,10)))) shouldEqual ArrayRanges.empty
    (ArrayRanges(Seq((1,5), (6,10))) subtract ArrayRanges(Seq((1,10)))) shouldEqual ArrayRanges.empty
    (ArrayRanges(Seq((1,5), (6,10))) subtract ArrayRanges(Seq((0,10)))) shouldEqual ArrayRanges.empty
    (ArrayRanges(Seq((1,5), (6,10))) subtract ArrayRanges(Seq((0,11)))) shouldEqual ArrayRanges.empty
  }

  it should "work overlap on left but not right" in {
    (ArrayRanges(Seq((1,10))) subtract ArrayRanges(Seq((0,5)))) shouldEqual ArrayRanges(Seq((5,10)))
    (ArrayRanges(Seq((1,10), (11,20))) subtract ArrayRanges(Seq((0,5)))) shouldEqual ArrayRanges(Seq((5,10), (11,20)))
    (ArrayRanges(Seq((1,10), (11,20))) subtract ArrayRanges(Seq((11,19)))) shouldEqual ArrayRanges(Seq((1,10), (19,20)))
    (ArrayRanges(Seq((1,10), (11,20))) subtract ArrayRanges(Seq((10,19)))) shouldEqual ArrayRanges(Seq((1,10), (19,20)))
    (ArrayRanges(Seq((1,10), (11,20), (21,22))) subtract ArrayRanges(Seq((11,19)))) shouldEqual ArrayRanges(Seq((1,10), (19,20), (21,22)))
  }

  it should "work overlap on right but not left" in {
    (ArrayRanges(Seq((1,10))) subtract ArrayRanges(Seq((5,10)))) shouldEqual ArrayRanges(Seq((1,5)))
    (ArrayRanges(Seq((1,10))) subtract ArrayRanges(Seq((5,12)))) shouldEqual ArrayRanges(Seq((1,5)))
    (ArrayRanges(Seq((1,10), (11,20))) subtract ArrayRanges(Seq((0,5)))) shouldEqual ArrayRanges(Seq((5,10), (11,20)))
    (ArrayRanges(Seq((1,10), (11,20))) subtract ArrayRanges(Seq((11,19)))) shouldEqual ArrayRanges(Seq((1,10), (19,20)))
    (ArrayRanges(Seq((1,10), (11,20))) subtract ArrayRanges(Seq((10,19)))) shouldEqual ArrayRanges(Seq((1,10), (19,20)))
    (ArrayRanges(Seq((1,10), (11,20), (21,22))) subtract ArrayRanges(Seq((11,19)))) shouldEqual ArrayRanges(Seq((1,10), (19,20), (21,22)))
  }

  it should "work for combinations of overlap" in {
    (ArrayRanges(Seq((0,2), (5,10), (11,20))) subtract ArrayRanges(Seq((0,5)))) shouldEqual ArrayRanges(Seq((5,10), (11,20)))
    (ArrayRanges(Seq((0,2), (5,10), (11,20))) subtract ArrayRanges(Seq((0,6)))) shouldEqual ArrayRanges(Seq((6,10), (11,20)))
    (ArrayRanges(Seq((0,2), (5,10), (11,20))) subtract ArrayRanges(Seq((6,19)))) shouldEqual ArrayRanges(Seq((0,2), (5,6), (19,20)))
    (ArrayRanges(Seq((0,2), (5,10), (11,20))) subtract ArrayRanges(Seq((6,22)))) shouldEqual ArrayRanges(Seq((0,2),(5,6)))
  }

  it should "work with no overlap" in {
    var left = ArrayRanges(Seq((5,10), (11,20)))
    (left subtract ArrayRanges(Seq((0,5)))) shouldEqual left
    (left subtract ArrayRanges(Seq((0,4)))) shouldEqual left
    (left subtract ArrayRanges(Seq((20,25)))) shouldEqual left

    left = ArrayRanges(Seq((0,2), (5,10), (11,20)))
    (left subtract ArrayRanges(Seq((2,5)))) shouldEqual left
    (left subtract ArrayRanges(Seq((2,3)))) shouldEqual left
    (left subtract ArrayRanges(Seq((3,5)))) shouldEqual left
    (left subtract ArrayRanges(Seq((10,11)))) shouldEqual left

  }

  it should "work if left is empty" in {
    val left = ArrayRanges.empty
    val right = ArrayRanges(Seq((1,5)))
    (left subtract right) shouldEqual ArrayRanges.empty
  }

  it should "work if right is empty" in {
    var left = ArrayRanges(Seq((1,5)))
    val right = ArrayRanges.empty
    (left subtract right) shouldEqual left

    left = ArrayRanges(Seq((0,20), (25,30)))
    (left subtract right) shouldEqual left
  }

  it should "work if both are empty" in {
    (ArrayRanges.empty subtract ArrayRanges.empty) shouldEqual ArrayRanges.empty
  }

}
