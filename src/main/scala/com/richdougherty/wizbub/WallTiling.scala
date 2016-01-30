package com.richdougherty.wizbub

import com.richdougherty.wizbub.Compass.{PrincipalSet, Principal}
import com.richdougherty.wizbub.dawnlike.DawnLikeTiles
import com.richdougherty.wizbub.dawnlike.index.TileQuery
import com.richdougherty.wizbub.dawnlike.index.TileQuery.{AttrContains, NoAttr}

class WallTiling(dawnLikeTiles: DawnLikeTiles) {

  private val tiles: Array[DawnLikeTile] = {

    // Load tiles and store them in a map keyed on
    // the DirectionSet of their joins.
    val baseQuery = TileQuery.All(AttrContains("wall", "stone"), AttrContains("color", "sky/peppermint"))
    val fillTile = dawnLikeTiles.findTile(baseQuery, NoAttr("wall_dirs"))
    import Direction._
    val joinTileDirections: Seq[DirectionSet] = Seq(
      DirectionSet(Right, Bottom),
      DirectionSet(Right, Left),
      DirectionSet(Bottom, Left),
      DirectionSet(Right, Bottom, Left),
      DirectionSet(Top, Bottom),
      DirectionSet(),
      DirectionSet(Top, Right, Bottom),
      DirectionSet(Top, Right, Bottom, Left),
      DirectionSet(Top, Bottom, Left),
      DirectionSet(Top, Right),
      DirectionSet(Top, Left),
      DirectionSet(Top, Right, Left)
    )
    val joinTiles: Map[DirectionSet, DawnLikeTile] = (joinTileDirections.map { dirs =>
      val tile = dawnLikeTiles.findTile(baseQuery, AttrContains("wall_dirs", dirs.code))
      (dirs, tile)
    }).toMap

    // Iterate through all PrincipalPointSets and pick the best wall tile
    // for it.
    for (ps <- PrincipalSet.combinations) yield {
      val wallJoins = WallTiling.calculateWallJoinDirections(ps)
      val tile = wallJoins match {
        case null => fillTile
        case ds => joinTiles(ds)
      }
      tile
    }
  }

  def apply(ps: PrincipalSet) = tiles(ps.bits)

}

object WallTiling {

  import Compass._

  private[wizbub] def calculateWallJoinDirections(adjoiningWalls: PrincipalSet): DirectionSet = {

    // Get a list of possible joins.
    val possibleJoinDirections: Seq[Direction] = Direction.all.filter { dir: Direction =>
      val front: Principal = dir.principal

      if (adjoiningWalls.contains(front)) {
        val sides: Seq[Principal] = Seq(front.rotate(-2), front.rotate(-1), front.rotate(1), front.rotate(2))
        val adjoiningSideCount = sides.filter(side => adjoiningWalls.contains(side)).size
        adjoiningSideCount < sides.size
      } else false
    }

    // Choose the final join directions based on the possible joins found
    possibleJoinDirections.size match {
      case 0 =>
        val adjoiningSize = adjoiningWalls.points.size
        // No possible join directions. This square is either fully joined, not joined at all, or
        // only joined on diagonals.
        if (adjoiningSize == 8) {
          // Joined on all sides
          null
        } else {
          // Joined on no sides or only joined diagonally
          assert(adjoiningSize <= 4, s"Found more than 4 adjoining points on wall with no joins: $adjoiningSize, $adjoiningWalls")
          DirectionSet()
        }
      case 1 =>
        // We don't have tiles for 1-length joins, so extend the wall to make it join on
        // the opposite side. As a special case, a top join is treated as no join, because
        // the tile looks better that way.
        import Direction._
        possibleJoinDirections.head match {
          case Top => DirectionSet()
          case Bottom => DirectionSet(Top, Bottom)
          case Left | Right => DirectionSet(Left, Right)
        }
      case 2 | 3 | 4 =>
        // There are tiles for all combinations of 2, 3 and 4 directions.
        DirectionSet(possibleJoinDirections: _*)
    }
  }

}
