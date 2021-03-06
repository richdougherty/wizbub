package com.richdougherty.wizbub

import squidpony.squidgrid.mapping.DungeonGenerator

class WorldSliceGenerator {

  def generate: WorldSlice = {
    val generator = new DungeonGenerator(WorldSlice.SIZE, WorldSlice.SIZE)
    val charArray: Array[Array[Char]] = generator
      .addGrass(30)
      .addDoors(50, true)
      .generate()

    val worldSlice = new WorldSlice
    var playerPlaced: Boolean = false
    var snakePlaced: Boolean = false

    for (x <- 0 until WorldSlice.SIZE; y <- 0 until WorldSlice.SIZE) {
      val char = charArray(x)(y)

      def createEmptyGround(kind: GroundEntity.Kind): GroundEntity = {
        val ground = new GroundEntity(-1, kind) // TODO
        // If we haven't placed the player yet, put them on this bit of floor
        if (!playerPlaced) {
          ground.aboveEntity = new PlayerEntity(-1, playerNumber = 0, 5)
          playerPlaced = true
        }
        if (x > 20 && y > 20 && !snakePlaced) {
          ground.aboveEntity = new PlayerEntity(-1, playerNumber = 2, 5)
          snakePlaced = true
        }
        ground
      }

      val entity: Entity = char match {
        case '#' => // Wall
          val ground = new GroundEntity(-1, GroundEntity.Stone)
          ground.aboveEntity = new WallEntity(-1)
          ground
        case '.' => // Floor
          createEmptyGround(GroundEntity.Stone)
        case '"' => // Floor
          createEmptyGround(GroundEntity.Grass)
        case '~' => // Deep water
          new GroundEntity(-1, GroundEntity.Dirt) // TODO
        case ',' => // Shallow water
          new GroundEntity(-1, GroundEntity.Dirt) // TODO
        case '^' => // Trap
          new GroundEntity(-1, GroundEntity.Dirt) // TODO
        case '+' => // Horizontal door
          val ground = new GroundEntity(-1, GroundEntity.Stone)
          ground.aboveEntity = new DoorEntity(-1, open = false)
          ground
        case '/' => // Vertical door
          val ground = new GroundEntity(-1, GroundEntity.Stone)
          ground.aboveEntity = new DoorEntity(-1, open = false)
          ground
        case unknown => throw new AssertionError(s"Got invalid tile type from generator: $unknown")
      }
      worldSlice(x, y) = entity
    }

    worldSlice(generator.stairsUp.x, generator.stairsUp.y) = new PortalEntity(-1, PortalEntity.Up)
    worldSlice(generator.stairsDown.x, generator.stairsDown.y) = new PortalEntity(-1, PortalEntity.Down)

    worldSlice
  }
}
