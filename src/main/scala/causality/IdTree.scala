package de.tu_darmstadt.stg.daimpl
package causality

import causality.IdTree.splitNormalizedIdTree

import scala.annotation.{tailrec, targetName}
import scala.math.Ordering.Implicits.infixOrderingOps

enum IdTree:
  case Leaf(value: 0 | 1)
  case Branch(left: IdTree, right: IdTree)

  /** Normalizes and splits this id into two non-overlapping ids such that both ids are child ids.
    *
    * @return
    *   A tuple of non-overlapping child ids.
    */
  def split: (IdTree, IdTree) = {
    splitNormalizedIdTree(this.normalize)
  }

  @targetName("add")
  def +(otherId: IdTree): Option[IdTree] = (this, otherId) match
    case (Leaf(0), otherId) => Some(otherId)
    case (id, Leaf(0))      => Some(id)
    case (Branch(l1, r1), Branch(l2, r2)) =>
      for {
        l <- l1 + l2
        r <- r1 + r2
      } yield Branch(l, r).normalize
    // Cannot join two IdTrees that aren't disjoint
    case (Leaf(1), _) => None
    case (_, Leaf(1)) => None

object IdTree {
  val seed: IdTree      = Leaf(1)
  val anonymous: IdTree = Leaf(0)

  /** Splits the id without normalization.
    *
    * If the id is in normal form, the resulting ids can be incorrect. Example: Branch(Branch(1, 1), Branch(0,0)) will
    * be split into Branch(Branch(1,1),0) and Branch(0, Branch(0,0)) which is equivalent to Branch(1,0) and Leaf(0) (the
    * anonymous stamp).
    *
    * @param id
    *   The normalized id to be split
    * @return
    *   Assuming the ids are normalized, two non-overlapping child ids of the id
    */
  def splitNormalizedIdTree(id: IdTree): (IdTree, IdTree) =
    id match
      case Leaf(0) => (Leaf(0), Leaf(0))
      case Leaf(1) => (Branch(Leaf(1), Leaf(0)), Branch(Leaf(0), Leaf(1)))
      case Branch(Leaf(0), i) =>
        val (i1, i2) = splitNormalizedIdTree(i)
        (Branch(Leaf(0), i1), Branch(Leaf(0), i2))
      case Branch(i, Leaf(0)) =>
        val (i1, i2) = splitNormalizedIdTree(i)
        (Branch(i1, Leaf(0)), Branch(i2, Leaf(0)))
      case Branch(i1, i2) =>
        (Branch(i1, Leaf(0)), Branch(Leaf(0), i2))

  given Conversion[Int, Leaf] = {
    case 0 => Leaf(0)
    case 1 => Leaf(1)
  }

  given Conversion[(Leaf, Leaf), Branch] = Branch.apply

  extension (id: IdTree)
    def min: 0 | 1 = id match {
      case IdTree.Leaf(value) => value
      case IdTree.Branch(left, right) =>
        val lMin: 0 | 1 = left.min
        val rMin: 0 | 1 = right.min
        if lMin <= rMin then lMin else rMin
    }

    def max: 0 | 1 = id match {
      case IdTree.Leaf(value) => value
      case IdTree.Branch(left, right) =>
        val lMax: 0 | 1 = left.max
        val rMax: 0 | 1 = right.max
        if lMax >= rMax then lMax else rMax
    }

  given NormalForm[IdTree] with
    extension (tree: IdTree)
      def normalize: IdTree = tree match
        case l @ Leaf(_)                                => l
        case Branch(l @ Leaf(i1), Leaf(i2)) if i1 == i2 => l
        case Branch(l, r) =>
          (l.normalize, r.normalize) match
            case (Leaf(lNormVal), Leaf(rNormVal)) if lNormVal == rNormVal => Leaf(lNormVal)
            case (lNorm, rNorm)                                           => Branch(lNorm, rNorm)

      def isNormalized: Boolean = tree match
        case Leaf(_)                  => true
        case Branch(Leaf(l), Leaf(r)) => l != r
        case Branch(bLeft, bRight)    => bLeft.isNormalized && bRight.isNormalized

  given PartialOrdering[IdTree] with {
    override def lteq(x: IdTree, y: IdTree): Boolean = (x, y) match {
      case (Leaf(l), Leaf(r))               => l <= r
      case (Leaf(l), r)                     => l <= r.min
      case (l, Leaf(r))                     => l.max <= r
      case (Branch(l1, l2), Branch(r1, r2)) => lteq(l1, r1) && lteq(l2, r2)
    }

    override def tryCompare(x: IdTree, y: IdTree): Option[Int] = (x, y) match {
      case (Leaf(l), Leaf(r)) => Some(l.compare(r))
      case (Branch(l1, l2), Branch(r1, r2)) =>
        for {
          l1Comp <- tryCompare(l1, r1)
          l2Comp <- tryCompare(l2, r2)
          comp <-
            if l1Comp == l2Comp
            then Some(l1Comp)
            else if l1Comp == 0 then Some(l2Comp)
            else if l2Comp == 0 then Some(l1Comp)
            else None
        } yield comp
      case (l @ Leaf(_), Branch(r1, r2))   => tryCompare(Branch(l, l), Branch(r1, r2))
      case (l @ Branch(_, _), r @ Leaf(_)) => tryCompare(l, Branch(r, r))
    }
  }
}
