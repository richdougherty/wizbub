package com.richdougherty.wizbub.dawnlike

import com.richdougherty.wizbub.DawnLikeAtlas
import com.richdougherty.wizbub.DawnLikeTile
import com.richdougherty.wizbub.dawnlike.index._

import com.badlogic.gdx.utils.Disposable

import scala.collection.immutable.{Seq, Map}

final class DawnLikeTiles extends Disposable {

  private val index = Index.readFromFile()
  private val atlases: Map[(String, String), DawnLikeAtlas] = {
    (for {
      directory <- index.directories
      tileset <- directory.tilesets
    } yield {
      val loadAtlas = if (tileset.animated) {
        DawnLikeAtlas.loadAnimated(_, _)
      } else {
        DawnLikeAtlas.loadStatic(_, _)
      }
      val atlas = loadAtlas(directory.name, tileset.name)
      ((directory.name, tileset.name), atlas)
    }).toMap
  }

  override def dispose(): Unit = {
    atlases.values.foreach(_.dispose())
  }

  def apply(directory: String, tileset: String, x: Int, y: Int): DawnLikeTile = {
    atlases((directory, tileset))(x, y)
  }

  def findTile(attrs: (String, String)*): DawnLikeTile = {
    val ref = index.findTile(attrs: _*)
    apply(ref.directory, ref.tileset, ref.tile.x, ref.tile.y)
  }

}