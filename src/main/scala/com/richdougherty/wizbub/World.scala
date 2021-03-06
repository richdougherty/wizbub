package com.richdougherty.wizbub

class World {

}

final object WorldSlice {
  final val SIZE = 64
}

class WorldSlice {
  import WorldSlice.SIZE
  /** Use a single-dimensional array here so we only need a single bounds check. */
  private val entities = new Array[Entity](SIZE * SIZE)
  /** Convert entity coordinates into a location in the entities array. */
  private def entityIndex(x: Int, y: Int): Int = x * SIZE + y
  /** Access to the entities array. */
  def inBounds(x: Int, y: Int): Boolean = { 0 <= x && x < WorldSlice.SIZE && 0 <= y && y < WorldSlice.SIZE }
  def apply(x: Int, y: Int): Entity = entities(entityIndex(x, y))
  def getOrNull(x: Int, y: Int): Entity = {
    if (inBounds(x, y)) apply(x, y) else null
  }
  def update(x: Int, y: Int, e: Entity) = entities(entityIndex(x, y)) = e
  def cell(x: Int, y: Int): Entity.Cell = new Entity.Cell {
    override def get: Entity = getOrNull(x, y)
    override def set(e: Entity) = update(x, y, e)
  }
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
  trait Cell {
    def get: Entity
    def set(e: Entity): Unit
  }
}

sealed abstract class Entity(final val id: Entity.Id) {
  final def equals(that: Entity) = this.id == that.id
  final override def equals(that: AnyRef) = that match {
    case e: Entity => equals(e)
    case _ => false
  }
  var drawable: Drawable = null
}

object GroundEntity {
  sealed trait Kind
  /** Plain old dirt. */
  case object Dirt extends Kind
  /** Grass growing on top of dirt. */
  case object Grass extends Kind
  /** Dirt that was once grass, but has now been cut. */
  case object CutGrass extends Kind
  /** Stone floor. */
  case object Stone extends Kind
}

final class GroundEntity(id: Entity.Id, var kind: GroundEntity.Kind) extends Entity(id) with Entity.Cell {
  /** The entity (if any) on top of this piece of ground */
  var aboveEntity: Entity = null
  override def get: Entity = aboveEntity
  override def set(e: Entity) = aboveEntity = e
}

final class PlayerEntity(id: Entity.Id, val playerNumber: Int, var hp: Int) extends Entity(id) {
  var countDown: Int = 0
}

final class WallEntity(id: Entity.Id) extends Entity(id)
final class TreeEntity(id: Entity.Id) extends Entity(id)
final class DoorEntity(id: Entity.Id, var open: Boolean) extends Entity(id) with Entity.Cell {
  // The entity, if any, inside the doorway
  var inEntity: Entity = null
  override def get: Entity = inEntity
  override def set(e: Entity) = inEntity = e
}

object PortalEntity {
  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
}
final class PortalEntity(
                          id: Entity.Id,
                          var direction: PortalEntity.Direction,
                          var onEntity: Entity = null) extends Entity(id) with Entity.Cell {
  override def get: Entity = onEntity
  override def set(e: Entity) = onEntity = e
}