package com.richdougherty.wizbub

import com.badlogic.gdx.ApplicationAdapter
import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{OrthographicCamera, Color, GL20, Texture}
import com.badlogic.gdx.graphics.g2d.BitmapFont
import com.badlogic.gdx.graphics.g2d.Sprite
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g2d.TextureRegion
import com.badlogic.gdx.utils.Disposable


class DawnLikeSprite(sprites: Array[Sprite]) {
  def draw(batch: SpriteBatch): Unit = {
    val spriteIndex = ((System.currentTimeMillis / 300) % sprites.length).toInt
    val sprite = sprites(spriteIndex)
    sprite.draw(batch)
  }
  def translate(x: Float, y: Float): Unit = {
    sprites.foreach(_.translate(x, y))
  }
}

class DawnLikeAtlas(name: String, textures: Array[Texture]) extends Disposable {
  private val regions: Array[Array[Array[TextureRegion]]] = {
    textures.map(TextureRegion.split(_, 16, 16))
  }
  println(s"Atlas $name split into (${regions(0).length}, ${regions(0)(0).length}) regions")
  def apply(x: Int, y: Int): DawnLikeSprite = {
    val sprites: Array[Sprite] = regions.map { regionsAtFrame =>
      val regionAtFrame = regionsAtFrame(x)(y)
      val sprite = new Sprite(regionAtFrame)
      sprite.setSize(1f, 1f)
      sprite
    }
    new DawnLikeSprite(sprites)
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
  private var player0Sprite: DawnLikeSprite = null
  private var player1Sprite: DawnLikeSprite = null
  private var floorAtlas: DawnLikeAtlas = null
  private var grassSprite: DawnLikeSprite = null

  override def create(): Unit =  {
    resizeCamera(Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    batch = new SpriteBatch()
    playerAtlas = DawnLikeAtlas.loadAnimated("Characters", "Player")
    player0Sprite = playerAtlas(0, 0)
    player0Sprite.translate(1f, 1f)
    player1Sprite = playerAtlas(0, 6)
    player1Sprite.translate(2f, 2f)
    floorAtlas = DawnLikeAtlas.loadStatic("Objects", "Floor")
    grassSprite = floorAtlas(7, 8)
    grassSprite.translate(1f, 1f)
  }

  override def render(): Unit = {
    Gdx.gl.glClearColor(0, 0, 0, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
    camera.update()
    batch.begin()
    batch.setProjectionMatrix(camera.combined)
    grassSprite.draw(batch)
    player0Sprite.draw(batch)
    player1Sprite.draw(batch)
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
