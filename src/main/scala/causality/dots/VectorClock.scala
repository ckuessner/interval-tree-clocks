package de.tu_darmstadt.stg.daimpl
package causality.dots

import causality.dots.Defs.{Id, Time}
import util.MapHelper.max

import scala.math.PartialOrdering

case class VectorClock(timestamps: Map[Id, Time] = Map()) {
  def merged(other: VectorClock): VectorClock = VectorClock(max(timestamps, other.timestamps))

  def merged(other: Map[Id, Time]): VectorClock = VectorClock(max(timestamps, other))

  def merged(other: Dot): VectorClock = merged(Map(other.replicaId -> other.time))

  def advance(replicaId: Id): VectorClock = VectorClock(
    timestamps = timestamps + (replicaId -> (timestamps.getOrElse(replicaId, 0L) + 1L))
  )

  def timeOf(replicaId: Id): Time = timestamps.getOrElse(replicaId, 0)

  def clockOf(replicaId: Id): Dot = Dot(timeOf(replicaId), replicaId)

  def contains(timestamp: Dot): Boolean = timestamps.getOrElse(timestamp.replicaId, 0L) >= timestamp.time
}

object VectorClock {
  given VectorClockOrdering: PartialOrdering[VectorClock] with {
    override def tryCompare(x: VectorClock, y: VectorClock): Option[Int] = {
      if (x.timestamps.isEmpty) return Some(0)
      if (x.timestamps.keySet != y.timestamps.keySet) return None

      val clockPairs  = x.timestamps.keySet.map(key => (x.timestamps(key), y.timestamps(key)))
      val comparisons = clockPairs map { case (x, y) => x compare y }

      if (comparisons.max < 0) return Some(-1)
      if (comparisons.min > 0) return Some(1)
      if (comparisons.min == 0 && comparisons.max == 0) return Some(0)
      return None
    }

    override def lteq(x: VectorClock, y: VectorClock): Boolean = {
      if (x.timestamps.isEmpty) return true
      val anyXGreaterY = x.timestamps.exists { case (key, xTimeStampForKey) =>
        xTimeStampForKey > y.timestamps.getOrElse(key, 0L)
      }

      !anyXGreaterY
    }
  }
}
