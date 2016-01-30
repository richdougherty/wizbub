package com.richdougherty.wizbub

import com.richdougherty.wizbub.Compass.{Principal, PrincipalSet}
import org.scalatest._

class WallTilingTest extends WordSpec with Matchers {

  "Wall tiling" should {
    "work out tile with no surrounding walls" in {
      val points = PrincipalSet()
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet())
    }
    "work out tile when full surrounded by walls" in {
      val points = PrincipalSet(Principal.all: _*)
      WallTiling.calculateWallJoinDirections(points) should be (null)
    }
    "work out a wall at the center of a simple T-intersection" in {
      val points = PrincipalSet(Principal.Top, Principal.Right, Principal.Bottom)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Top, Direction.Right, Direction.Bottom))
    }
    "work out a wall on the middle line of a T-intersection" in {
      val points = PrincipalSet(Principal.TopLeft, Principal.Top, Principal.TopRight)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet())
    }
    "work out a wall at the center of a T-intersection filled on one side" in {
      val points = PrincipalSet(
        Principal.TopLeft, Principal.Top, Principal.TopRight,
        Principal.Right, Principal.Left,
        Principal.Bottom
      )
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Left, Direction.Bottom, Direction.Right))
    }
    "work out a + join" in {
      val points = PrincipalSet(Principal.Top, Principal.Right, Principal.Bottom, Principal.Left)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Top, Direction.Right, Direction.Bottom, Direction.Left))
    }
    "work out a + join filled on two corners" in {
      val points = PrincipalSet(Principal.Top, Principal.Right, Principal.RightBottom, Principal.TopLeft, Principal.Bottom, Principal.Left)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Top, Direction.Right, Direction.Bottom, Direction.Left))
    }
    "work out a simple corner" in {
      val points = PrincipalSet(Principal.Left, Principal.Bottom)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Left, Direction.Bottom))
    }
    "work out a corner filled on the other sides" in {
      val points = PrincipalSet(
        Principal.Top, Principal.TopRight,
        Principal.Right, Principal.Left,
        Principal.BottomLeft, Principal.Bottom, Principal.RightBottom
      )
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Left, Direction.Top))
    }
    "work out a thin diagonal" in {
      val points = PrincipalSet(Principal.TopLeft, Principal.RightBottom)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet())
    }
    "work out a thick diagonal" in {
      val points = PrincipalSet(Principal.TopLeft, Principal.Top, Principal.Right, Principal.RightBottom)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Top, Direction.Right))
    }
    "work out a filled diagonal" in {
      val points = PrincipalSet(Principal.RightBottom, Principal.Bottom, Principal.BottomLeft, Principal.Left, Principal.TopLeft)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Bottom, Direction.Left))
    }
    "work out a horizontal line" in {
      val points = PrincipalSet(Principal.Left, Principal.Right)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Left, Direction.Right))
    }
    "work out a vertical line" in {
      val points = PrincipalSet(Principal.Top, Principal.Bottom)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Top, Direction.Bottom))
    }
    "work out a wall connected in one horizontal direction" in {
      val points = PrincipalSet(Principal.Left)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Left, Direction.Right))
    }
    "work out a wall connected in one vertical direction" in {
      val points = PrincipalSet(Principal.Bottom)
      WallTiling.calculateWallJoinDirections(points) should be (DirectionSet(Direction.Top, Direction.Bottom))
    }
  }

}
