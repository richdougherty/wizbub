package com.richdougherty.wizbub

import java.io._

import scala.concurrent.{Future, ExecutionContext}

class WorldPickler(implicit exec: ExecutionContext) {

  def pickle(worldSlice: WorldSlice, out: DataOutput): Unit = {
    for (x <- 0 until WorldSlice.SIZE; y <- 0 until WorldSlice.SIZE) {
      def writeEntity(e: Entity): Unit = e match {
        case ground: GroundEntity =>
          out.writeByte(0)
          ground.kind match {
            case GroundEntity.Dirt => out.writeByte(0)
            case GroundEntity.Grass => out.writeByte(1)
            case GroundEntity.CutGrass => out.writeByte(2)
          }
          if (ground.aboveEntity == null) {
            out.writeBoolean(false) // false
          } else {
            out.writeBoolean(true)
            writeEntity(ground.aboveEntity)
          }
        case player: PlayerEntity =>
          out.writeByte(1)
          out.writeInt(player.playerNumber)
        case _: WallEntity => out.writeByte(2)
        case _: TreeEntity => out.writeByte(3)
        case door: DoorEntity =>
          out.writeByte(4)
          out.writeBoolean(door.open)
        case unknown => throw new IOException(s"Unexpected entity type when pickling ($x, $y): $unknown")
      }
      writeEntity(worldSlice(x, y))
    }
  }

  def unpickle(worldSlice: WorldSlice, in: DataInput): Unit = {
    for (x <- 0 until WorldSlice.SIZE; y <- 0 until WorldSlice.SIZE) {
      def readEntity(): Entity = in.readByte() match {
        case 0 =>
          val ground = new GroundEntity(-1, GroundEntity.Dirt)
          ground.kind = in.readByte() match {
            case 0 => GroundEntity.Dirt
            case 1 => GroundEntity.Grass
            case 2 => GroundEntity.CutGrass
          }
          if (in.readBoolean()) ground.aboveEntity = readEntity()
          ground
        case 1 =>
          val playerNumber = in.readInt()
          new PlayerEntity(-1, playerNumber)
        case 2 => new WallEntity(-1)
        case 3 => new TreeEntity(-1)
        case 4 =>
          val open = in.readBoolean()
          new DoorEntity(-1, open)
      }
      try {
        worldSlice(x, y) = readEntity()
      } catch {
        case _: Exception => throw new IOException(s"Error when unpickling ($x, $y) from save file")
      }

    }
  }

  def writeToFile(worldSlice: WorldSlice): Future[Unit] = {
    // Synchronously pickle the slice state into a byte array.
    val baos = new ByteArrayOutputStream()
    try pickle(worldSlice, new DataOutputStream(baos)) finally baos.close()
    // Asynchronously write the byte array to a file
    Future {
      val dir = ApplicationDataDir.get
      if (!dir.exists()) {
        dir.mkdir()
      }
      val savefile = new File(dir, "savefile")
      val out = new FileOutputStream(savefile)
      try baos.writeTo(out) finally out.close()
    }
  }

  def readFromFile(worldSlice: WorldSlice): Unit = {
    val dir = ApplicationDataDir.get
    val savefile = new File(dir, "savefile")
    if (savefile.exists()) {
      val in = new FileInputStream(savefile)
      try unpickle(worldSlice, new DataInputStream(in)) finally in.close()
    }
  }

}

object ApplicationDataDir {
  def get: File = {
    System.getProperty("os.name") match {
      case "Mac OS X" =>
        new File(System.getProperty("user.home") + "/Library/Application Support/Wizbub")
      case w if w.startsWith("Windows") =>
        new File(System.getenv("APPDATA") + "\\Wizbub")
      case _ =>
        // FIXME: Handle Android
        new File(System.getenv("user.home") + "/.wizbub")
    }
  }
}