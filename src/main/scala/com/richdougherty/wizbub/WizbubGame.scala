package com.richdougherty.wizbub

import com.badlogic.gdx.ApplicationAdapter
import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{OrthographicCamera, Color, GL20, Texture}
import com.badlogic.gdx.graphics.g2d.BitmapFont
import com.badlogic.gdx.graphics.g2d.Sprite
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g2d.TextureRegion
import com.badlogic.gdx.utils.Disposable


class DawnLikeTile(frames: Array[TextureRegion]) {
  def draw(batch: SpriteBatch, x: Float, y: Float): Unit = {
    val frameIndex = ((System.currentTimeMillis / 300) % frames.length).toInt
    val frame: TextureRegion = frames(frameIndex)
    batch.draw(frame, x, y, 1f, 1f)
  }
}

class DawnLikeAtlas(name: String, textures: Array[Texture]) extends Disposable {
  private val framesSplitIntoRegions: Array[Array[Array[TextureRegion]]] = {
    textures.map(TextureRegion.split(_, 16, 16))
  }
  println(s"Atlas $name split into (${framesSplitIntoRegions(0).length}, ${framesSplitIntoRegions(0)(0).length}) regions")
  def apply(x: Int, y: Int): DawnLikeTile = {
    val frames: Array[TextureRegion] = framesSplitIntoRegions.map(_(x)(y))
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

class WizbubGame extends ApplicationAdapter {
  private var camera: OrthographicCamera = null
  private var batch: SpriteBatch = null
  private var playerAtlas: DawnLikeAtlas = null
  private var player0Sprite: DawnLikeTile = null
  private var player1Sprite: DawnLikeTile = null
  private var floorAtlas: DawnLikeAtlas = null
  private var grassSprite: DawnLikeTile = null

  override def create(): Unit =  {
    resizeCamera(Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    batch = new SpriteBatch()
    playerAtlas = DawnLikeAtlas.loadAnimated("Characters", "Player")
    player0Sprite = playerAtlas(0, 0)
    player1Sprite = playerAtlas(0, 6)
    floorAtlas = DawnLikeAtlas.loadStatic("Objects", "Floor")
    grassSprite = floorAtlas(7, 8)
  }

  override def render(): Unit = {
    Gdx.gl.glClearColor(0, 0, 0, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
    camera.update()
    batch.begin()
    batch.setProjectionMatrix(camera.combined)
    for (x <- 0 until 16; y <- 0 until 16) {
      grassSprite.draw(batch, x, y)
    }
    player0Sprite.draw(batch, 1f, 1f)
    player1Sprite.draw(batch, 2f, 2f)
    batch.end()
  }

  override def resize(width: Int, height: Int): Unit = {
    resizeCamera(width, height)
  }

  private def resizeCamera(width: Int, height: Int): Unit = {
    val scale = 16f / Math.min(height, width)
    camera = new OrthographicCamera(width * scale, height * scale)
    camera.translate(camera.viewportWidth/2, camera.viewportHeight/2)
  }

}