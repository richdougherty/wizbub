package com.richdougherty.wizbub

import scala.collection.mutable.ArrayBuffer

/**
 * Represents a cardinal direction. The constructor is private; use
 * one of the allocated objects in the companion.
 *
 * @param index The order of this direction.
 * @param dx The horizontal change for this direction.
 * @param dy The vertical change for this direction.
 * @param code A character code ('t', 'r' 'b' or 'l') that can
 *             be used to represent this direction.
 */
final case class Direction private[Direction](val index: Int, dx: Int, dy: Int, code: Char) {
  val bit = 1 << (3 - index)
  import Direction.all.{ length => DirCount }
  def rotate(steps: Int) = {
    val i = Math.floorMod(index + steps, Direction.all.length)
    Direction.all(i)
  }
  def clockwise: Direction = rotate(1)
  def anticlockwise: Direction = rotate(-1)
  def principal: Compass.Principal = Compass.Principal.all(index * 2)
}

final object Direction {
  val Top = Direction(0, 0, -1, 't')
  val Right = Direction(1, 1, 0, 'r')
  val Bottom = Direction(2, 0, 1, 'b')
  val Left = Direction(3, -1, 0, 'l')
  /**
   * All the directions, listed in TRBL order. Note this means
   * that the bits are from highest to lowest.
   */
  val all: Array[Direction] = Array(Top, Right, Bottom, Left)
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

  override def toString = code
}

final object DirectionSet {

  /**
   * Get a DirectionSet from a list of Directions.
   */
  def apply(directions: Direction*): DirectionSet = {
    val bits = directions.foldLeft(0) {
      case (acc, d) => acc | d.bit
    }
    val ds = combinations(bits)
    ds
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
  val combinations: Array[DirectionSet] = {
    (for (bits <- 0 until (1 << Direction.all.size)) yield {
      new DirectionSet(bits)
    }).toArray
  }

}

object Compass {

  final case class Principal private[Principal](val index: Int, val dirSet: DirectionSet) {
    val bit: Int = 1 << (7 - index)

    val dx = dirSet.directions.foldLeft(0) {
      case (acc, dir) => acc + dir.dx
    }
    val dy = dirSet.directions.foldLeft(0) {
      case (acc, dir) => acc + dir.dy
    }
    val code = dirSet.directions.foldLeft("") {
      case (acc, dir) => acc + dir.code
    }

    def rotate(steps: Int) = {
      val i = Math.floorMod(index + steps, Principal.all.length)
      Principal.all(i)
    }
    def clockwise: Principal = rotate(1)
    def anticlockwise: Principal = rotate(-1)
  }

  /**
   * One of the principal compass directions, N, NE, E, SE, S, etc.
   */
  final object Principal {

    val all = new Array[Principal](8)

    private def createPrincipal(index: Int, dirSet: DirectionSet): Principal = {
      val pp = new Principal(index, dirSet)
      all(index) = pp
      pp
    }

    val Top = createPrincipal(0, DirectionSet(Direction.Top))
    val TopRight = createPrincipal(1, DirectionSet(Direction.Top, Direction.Right))
    val Right = createPrincipal(2, DirectionSet(Direction.Right))
    val RightBottom = createPrincipal(3, DirectionSet(Direction.Right, Direction.Bottom))
    val Bottom = createPrincipal(4, DirectionSet(Direction.Bottom))
    val BottomLeft = createPrincipal(5, DirectionSet(Direction.Bottom, Direction.Left))
    val Left =  createPrincipal(6, DirectionSet(Direction.Left))
    val TopLeft = createPrincipal(7, DirectionSet(Direction.Top, Direction.Left))

    def fromBit(bit: Int): Principal = all(7 - bit)
  }

  final class PrincipalSet private[PrincipalSet](val bits: Int) {

    /**
     * The principal points in this set. The set can be empty if there are no points.
     */
    val points: Array[Principal] = {
      // Statically allocate the directions making up this Set
      val buffer = ArrayBuffer[Principal]()
      for (p <- Principal.all) {
        if ((p.bit & bits) != 0) buffer += p
      }
      buffer.toArray
    }

    override val toString = points.map(_.code).mkString("<", ":", ">")

    def contains(p: Principal) = (bits & p.bit) != 0
  }

  object PrincipalSet {
    /**
     * All the combinations of PrincipalSet. Since there
     * are only 256 possibilities, this list is statically
     * allocated.
     */
    val combinations: Array[PrincipalSet] = {
      (for (bits <- 0 until (1 << Principal.all.size)) yield {
        new PrincipalSet(bits)
      }).toArray
    }

    def apply(bits: Int): PrincipalSet = combinations(bits)

    def apply(points: Principal*): PrincipalSet = {
      val bits = points.foldLeft(0) {
        case (acc, p) => acc | p.bit
      }
      combinations(bits)
    }

  }

}