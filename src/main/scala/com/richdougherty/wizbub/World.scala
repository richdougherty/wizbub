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
  def getOrNull(x: Int, y: Int): Entity = {
    val i = entityIndex(x, y)
    if (0 <= i && i < entities.length) entities(i) else null
  }
  def update(x: Int, y: Int, e: Entity) = entities(entityIndex(x, y)) = e
}

object Entity {
  type Id = Int
  class IdGenerator {
    private var nextId = 0
    def freshId(): Int = {
      val id = nextId
      nextId += 1
      id
    }
  }
}

sealed abstract class Entity(final val id: Entity.Id) {
  final def equals(that: Entity) = this.id == that.id
  final override def equals(that: AnyRef) = that match {
    case e: Entity => equals(e)
    case _ => false
  }
}

object GroundEntity {
  sealed trait Kind
  /** Plain old dirt. */
  case object Dirt extends Kind
  /** Grass growing on top of dirt. */
  case object Grass extends Kind
  /** Dirt that was once grass, but has now been cut. */
  case object CutGrass extends Kind
}

final class GroundEntity(id: Entity.Id, var kind: GroundEntity.Kind) extends Entity(id) {
  /** The entity (if any) on top of this piece of ground */
  var aboveEntity: Entity = null
}

final class PlayerEntity(id: Entity.Id, val playerNumber: Int) extends Entity(id)

final class WallEntity(id: Entity.Id) extends Entity(id)
final class TreeEntity(id: Entity.Id) extends Entity(id)