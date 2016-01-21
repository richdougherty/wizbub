package com.richdougherty.wizbub

class World {

}

final object WorldSlice {
  final val SIZE = 256
}

class WorldSlice {
  import WorldSlice.SIZE
  /** Use a single-dimensional array here so we only need a single bounds check. */
  private val entities = new Array[Entity](SIZE * SIZE)
  /** Convert entity coordinates into a location in the entities array. */
  private def entityIndex(x: Int, y: Int): Int = x * SIZE + y
  /** Access to the entities array. */
  def apply(x: Int, y: Int): Entity = entities(entityIndex(x, y))
  def update(x: Int, y: Int, e: Entity) = entities(entityIndex(x, y)) = e
}

object Entity {
  type Id = Int
}

trait Entity

final class GroundEntity(val tile: DawnLikeTile) extends Entity {
  /** The entity (if any) on top of this piece of grass */
  var aboveEntity: Entity = null
}

final class PlayerEntity(val tile: DawnLikeTile) extends Entity