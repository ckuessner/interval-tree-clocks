package de.tu_darmstadt.stg.daimpl
package causality

import causality.DotStore._

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
sealed trait DotStore[D] {
  def dots(dotStore: D): Set[Dot]

  def bottom: D
}

object DotStore {
  type Dot = LamportClock
  type DotSet = Set[Dot]
  type DotFun[V] = Map[Dot, V]
  type DotMap[K, V] = Map[K, V]

  def apply[D](implicit dotStore: DotStore[D]): DotStore[D] = dotStore

  implicit def dotSetDotStore: DotStore[DotSet] = new DotStore[DotSet] {
    override def dots(dotStore: DotSet): Set[Dot] = dotStore

    override def bottom: DotSet = Set.empty
  }

  // Todo: V should be a SemiLattice according to paper
  implicit def dotFunDotStore[V]: DotStore[DotFun[V]] = new DotStore[DotFun[V]] {
    override def dots(dotStore: DotFun[V]): Set[Dot] = dotStore.keySet

    override def bottom: DotFun[V] = Map.empty
  }

  implicit def dotMapDotStore[K, V: DotStore]: DotStore[DotMap[K, V]] = new DotStore[DotMap[K, V]] {
    override def dots(dotStore: DotMap[K, V]): Set[Dot] =
      dotStore.values.flatMap(DotStore[V].dots(_)).toSet

    override def bottom: DotMap[K, V] = Map.empty
  }
}

object DotSetPartialOrdering extends PartialOrdering[DotSet] {
  override def tryCompare(x: DotSet, y: DotSet): Option[Int] = {
    val unionOfXandY = x union y
    if (unionOfXandY.size == x.size) { // x contains all elements of union of x and y
      if (unionOfXandY.size == y.size) return Some(0) // x = y
      else return Some(1) // y doesn't contain all elements of x => x > y
    } else if (unionOfXandY.size == y.size) return Some(1) // y contains all elements of union of x and y but x doesn't
    else return None
  }

  override def lteq(x: DotSet, y: DotSet): Boolean = {
    x.forall(y.contains)
  }
}