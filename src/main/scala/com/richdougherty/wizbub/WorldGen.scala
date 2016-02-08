package com.richdougherty.wizbub

import squidpony.squidgrid.mapping.DungeonGenerator

class WorldSliceGenerator {

  def generate: WorldSlice = {
    val dg = new DungeonGenerator(WorldSlice.SIZE, WorldSlice.SIZE)
    val charArray: Array[Array[Char]] = dg.generate()

    val worldSlice = new WorldSlice
    var playerPlaced: Boolean = false
    for (x <- 0 until WorldSlice.SIZE; y <- 0 until WorldSlice.SIZE) {
      val char = charArray(x)(y)
      val entity: Entity = char match {
        case '#' => // Wall
          val ground = new GroundEntity(-1, GroundEntity.Dirt)
          ground.aboveEntity = new WallEntity(-1)
          ground
        case '.' => // Floor
          val ground = new GroundEntity(-1, GroundEntity.Dirt) // TODO
          // If we haven't placed the player yet, put them on this bit of floor
          if (!playerPlaced) {
            ground.aboveEntity = new PlayerEntity(-1, playerNumber = 0)
            playerPlaced = true
          }
          ground
        case '~' => // Deep water
          new GroundEntity(-1, GroundEntity.Dirt) // TODO
        case ',' => // Shallow water
          new GroundEntity(-1, GroundEntity.Dirt) // TODO
        case '^' => // Trap
          new GroundEntity(-1, GroundEntity.Dirt) // TODO
        case '+' => // Horizontal door
          val ground = new GroundEntity(-1, GroundEntity.Dirt)
          ground.aboveEntity = new DoorEntity(-1, open = false)
          ground
        case '/' => // Vertical door
          val ground = new GroundEntity(-1, GroundEntity.Dirt)
          ground.aboveEntity = new DoorEntity(-1, open = false)
          ground
        case unknown => throw new AssertionError(s"Got invalid tile type from generator: $unknown")

      }
      worldSlice(x, y) = entity
    }
    worldSlice
  }
}
