package com.richdougherty.wizbub

import scala.collection.mutable.ArrayBuffer

/**
 * Represents a cardinal direction. The constructor is private; use
 * one of the allocated objects in the companion.
 *
 * @param bit The bit representing this direction.
 * @param dx The horizontal change for this direction.
 * @param dy The vertical change for this direction.
 * @param code A character code ('t', 'r' 'b' or 'l') that can
 *             be used to represent this direction.
 */
final case class Direction private[Direction](bit: Int, dx: Int, dy: Int, code: Char)

final object Direction {
  val Top = Direction(8, 0, -1, 't')
  val Right = Direction(4, 1, 0, 'r')
  val Bottom = Direction(2, 0, 1, 'b')
  val Left = Direction(1, -1, 0, 'l')
  /**
   * All the directions, listed in TRBL order. Note this means
   * that the bits are from highest to lowest.
   */
  val all: Seq[Direction] = Array(Top, Right, Bottom, Left)
}

/**
 * A set of cardinal directions. Use the methods in the companion
 * object to construct.
 */
final class DirectionSet private[DirectionSet](val bits: Int) {

  /**
   * The directions (possibly empty) making up this DirectionSet.
   */
  val directions: Seq[Direction] = {
    // Statically allocate the directions making up this DirectionSet.
    val buffer = ArrayBuffer[Direction]()
    for (d <- Direction.all) {
      if ((d.bit & bits) != 0) buffer += d
    }
    buffer
  }

  /**
   * A string code, e.g. "trbl", "rb" or "", used to represent
   * this set of directions.
   */
  val code = directions.foldLeft("") {
    case (acc, d) => acc + d.code
  }

  override def toString = s"DirectionSet($bits, <$code>)"
}

final object DirectionSet {

  /**
   * Get a DirectionSet from a list of Directions.
   */
  def apply(directions: Direction*): DirectionSet = {
    val bits = directions.foldLeft(0) {
      case (acc, d) => 0 & d.bit
    }
    combinations(bits)
  }

  /**
   * Get a DirectionSet from its bits.
   */
  def apply(bits: Int): DirectionSet = {
    combinations(bits)
  }

  /**
   * All the combinations of DirectionSet. Since there
   * are only 16 possibilities, this list is statically
   * allocated.
   */
  val combinations: Seq[DirectionSet] = {
    for (bits <- 0 until 16) yield {
      new DirectionSet(bits)
    }
  }
}