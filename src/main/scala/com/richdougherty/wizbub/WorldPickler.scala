package com.richdougherty.wizbub

import java.io._

object WorldPickler {

  def pickle(worldSlice: WorldSlice, out: OutputStream): Unit = {
    for (x <- 0 until WorldSlice.SIZE; y <- 0 until WorldSlice.SIZE) {
      val byte: Byte = worldSlice(x, y) match {
        case ground: GroundEntity =>
          ground.kind match {
            case GroundEntity.Dirt => 0
            case GroundEntity.Grass => 1
          }
        case wall: WallEntity => 2
      }
      out.write(byte)
    }
  }

  def unpickle(worldSlice: WorldSlice, in: InputStream): Unit = {
    for (x <- 0 until WorldSlice.SIZE; y <- 0 until WorldSlice.SIZE) {
      val entity: Entity = in.read() match {
        case 0 => new GroundEntity(-1, GroundEntity.Dirt)
        case 1 => new GroundEntity(-1, GroundEntity.Grass)
        case 2 => new WallEntity(-1)
      }
      worldSlice(x, y) = entity
    }
  }

  def writeToFile(worldSlice: WorldSlice): Unit = {
    val dir = ApplicationDataDir.get
    if (!dir.exists()) { dir.mkdir() }
    val savefile = new File(dir, "savefile")
    val out = new FileOutputStream(savefile)
    try pickle(worldSlice, out) finally out.close()
  }

  def readFromFile(worldSlice: WorldSlice): Unit = {
    val dir = ApplicationDataDir.get
    val savefile = new File(dir, "savefile")
    if (savefile.exists()) {
      val in = new FileInputStream(savefile)
      try unpickle(worldSlice, in) finally in.close()
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