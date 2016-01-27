package com.richdougherty.wizbub

import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureRegion}
import com.badlogic.gdx.utils.Disposable

class DawnLikeTile(frames: Array[TextureRegion]) {
  def draw(batch: SpriteBatch, x: Float, y: Float): Unit = {
    val frameIndex = ((System.currentTimeMillis / 300) % frames.length).toInt
    val frame: TextureRegion = frames(frameIndex)
    // Draw the tile with width and height of 1 because we use a camera that
    // renders using world coordinates.
    batch.draw(frame, x, y, 1f, 1f)
  }
}

class DawnLikeAtlas(name: String, textures: Array[Texture]) extends Disposable {
  private val framesSplitIntoRegions: Array[Array[Array[TextureRegion]]] = {
    textures.map { texture =>
      val textureRegions: Array[Array[TextureRegion]] = TextureRegion.split(texture, 16, 16)
      // Flip each TextureRegion because we render tiles in world coordinates
      for (row <- textureRegions; region <- row) {
        region.flip(false, true)
      }
      textureRegions
    }
  }
  def apply(x: Int, y: Int): DawnLikeTile = {
    val frames: Array[TextureRegion] = framesSplitIntoRegions.map(_(y)(x))
    new DawnLikeTile(frames)
  }
  def dispose(): Unit = textures.foreach(_.dispose())
}

object DawnLikeAtlas {
  def loadStatic(folder: String, file: String): DawnLikeAtlas = {
    val texture = new Texture(s"dawnlike/$folder/$file.png")
    new DawnLikeAtlas(s"$folder:$file", Array(texture))
  }
  def loadAnimated(folder: String, file: String): DawnLikeAtlas = {
    val textures: Array[Texture] = (0 to 1).map { i =>
      new Texture(s"dawnlike/$folder/$file$i.png")
    }.toArray
    new DawnLikeAtlas(s"$folder:$file", textures)
  }
}