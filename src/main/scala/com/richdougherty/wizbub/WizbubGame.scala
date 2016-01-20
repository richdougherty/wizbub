package com.richdougherty.wizbub

import com.badlogic.gdx.ApplicationAdapter
import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.graphics.Texture
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

class DawnLikeAtlas(textures: Array[Texture]) extends Disposable {
  private val regions: Array[Array[Array[TextureRegion]]] = {
    textures.map(TextureRegion.split(_, 16, 16))
  }
  def apply(x: Int, y: Int): DawnLikeSprite = {
    val sprites: Array[Sprite] = regions.map { regionsAtFrame =>
      val regionAtFrame = regionsAtFrame(x)(y)
      val sprite = new Sprite(regionAtFrame)
      sprite.scale(2f)
      sprite
    }
    new DawnLikeSprite(sprites)
  }
  def dispose(): Unit = textures.foreach(_.dispose())
}

object DawnLikeAtlas {
  def load(folder: String, file: String): DawnLikeAtlas = {
    val textures: Array[Texture] = (0 to 1).map { i =>
      new Texture(s"dawnlike/$folder/$file$i.png")
    }.toArray
    new DawnLikeAtlas(textures)
  }
}

class WizbubGame extends ApplicationAdapter {
  private var batch: SpriteBatch = null
  private var playerAtlas: DawnLikeAtlas = null
  private var player0Sprite: DawnLikeSprite = null
  private var player1Sprite: DawnLikeSprite = null

  override def create(): Unit =  {
    batch = new SpriteBatch()
    playerAtlas = DawnLikeAtlas.load("Characters", "Player")
    player0Sprite = playerAtlas(0, 0)
    player0Sprite.translate(100f, 100f)
    player1Sprite = playerAtlas(0, 6)
    player1Sprite.translate(200f, 200f)
  }

  override def render(): Unit = {
    Gdx.gl.glClearColor(0, 0, 0, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
    batch.begin()

    player0Sprite.draw(batch)
    player1Sprite.draw(batch)

    batch.end()
  }
}
