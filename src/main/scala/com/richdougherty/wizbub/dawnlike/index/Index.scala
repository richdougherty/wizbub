package com.richdougherty.wizbub.dawnlike.index

import java.io.IOException
import java.util.IllegalFormatException

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.files.FileHandle
import com.richdougherty.wizbub.DawnLikeTile

import scala.collection.immutable.Map
import scala.collection.immutable.Seq
import org.json4s._
import org.json4s.jackson.JsonMethods

final case class Index(directories: Seq[Directory]) {

  def findTile(queries: TileQuery*): Index.Ref = {
    val tiles = findTiles(queries: _*)
    if (tiles.size != 1) throw new RuntimeException(s"Expected 1 tile matching ${queries.mkString}, got ${tiles.size}")
    tiles.head
  }
  def findTiles(queries: TileQuery*): Seq[Index.Ref] = {
    val allQuery = TileQuery.All(queries: _*)
    for {
      directory <- directories
      tileset <- directory.tilesets
      tile <- tileset.tiles
      if allQuery.matches(tile.attributes)
    } yield {
      Index.Ref(directory.name, tileset.name, tile)
    }
  }
}
final case class Directory(name: String, tilesets: Seq[Tileset])
final case class Tileset(name: String, width: Int, height: Int, animated: Boolean, tiles: Seq[Tile])
final case class Tile(x: Int, y: Int, attributes: Map[String, Seq[String]])

object Index {
  final case class Ref(directory: String, tileset: String, tile: Tile)

  def readFromFile(): Index = {
    val file = Gdx.files.internal("dawnlike/index.json")
    val jsonString = file.readString("utf-8")
    JsonMethods.parse(jsonString) match {
      case rootObject: JObject =>
        Index(rootObject.obj.map {
          case (dirName, dirObject: JObject) =>
            Directory(
              dirName,
              dirObject.obj.map {
                case (
                    tilesetName,
                    JObject(List(
                      JField("width", width: JInt),
                      JField("height", height: JInt),
                      JField("animated", animated: JBool),
                      JField("tiles", tileArray: JArray)
                    ))
                ) =>
                  Tileset(
                    tilesetName,
                    width.num.toInt,
                    height.num.toInt,
                    animated.value,
                    tileArray.arr.map {
                      case JObject(List(
                        JField("y", y: JInt),
                        JField("x", x: JInt),
                        JField("attrs", attrsObject: JObject)
                      )) =>
                        Tile(
                          x.num.toInt,
                          y.num.toInt,
                          attrsObject.obj.map {
                            case (attrName, attrValue) =>
                              val attrStrings: Seq[String] = attrValue match {
                                case JString(s) =>
                                  Seq(s)
                                case attrArray: JArray =>
                                  attrArray.arr.map {
                                    case JString(s) => s
                                    case _ => throw new IOException("Attribute list values must be JSON strings")
                                  }
                                case _ => throw new IOException("Attribute values must be JSON strings or arrays")
                              }
                              (attrName, attrStrings)
                          }.toMap
                        )
                    }
                  )
                case _ => throw new IOException("Tileset entry must be a JSON object")
              })
          case _ => throw new IOException("Directory entry must be a JSON object")
        })
      case _ => throw new IOException("JSON root must be an object")
    }
  }
}

/**
 * A query that can be used to search for a tile.
 */
sealed trait TileQuery {
  def matches(attrs: Map[String, Seq[String]]): Boolean
}
object TileQuery {
  final case class All(queries: TileQuery*) extends TileQuery {
    def matches(attrs: Map[String, Seq[String]]): Boolean = {
      queries.foldLeft(true) {
        case (result, query) => result && query.matches(attrs)
      }
    }
  }
  final case class AttrContains(name: String, value: String) extends TileQuery {
    def matches(attrs: Map[String, Seq[String]]): Boolean = {
      attrs.get(name) match {
        case None => false
        case Some(attrValues) => attrValues.contains(value)
      }
    }
  }
  final case class NoAttr(name: String) extends TileQuery {
    def matches(attrs: Map[String, Seq[String]]): Boolean = {
      !attrs.contains(name)
    }
  }
}
