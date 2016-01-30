package com.richdougherty.wizbub

import com.richdougherty.wizbub.Compass.{PrincipalSet, Principal}
import org.scalatest._

class CompassTest extends WordSpec with Matchers {

  "Direction" should {
    "have top, right, bottom and left values" in {
      Direction.all should be (Seq(Direction.Top, Direction.Right, Direction.Bottom, Direction.Left))
    }
    "have a value for the Top direction" in {
      val d = Direction.Top
      d.code should be ('t')
      d.bit should be (8)
    }
    "have a value for the Right direction" in {
      val d = Direction.Right
      d.code should be ('r')
      d.bit should be (4)
    }
    "be able to be rotated" in {
      Direction.Top.rotate(0) should be (Direction.Top)
      Direction.Top.rotate(1) should be (Direction.Right)
      Direction.Top.rotate(2) should be (Direction.Bottom)
      Direction.Top.rotate(3) should be (Direction.Left)
      Direction.Top.rotate(4) should be (Direction.Top)

      Direction.Left.rotate(0) should be (Direction.Left)
      Direction.Left.rotate(-1) should be (Direction.Bottom)
      Direction.Left.rotate(-2) should be (Direction.Right)
      Direction.Left.rotate(-3) should be (Direction.Top)
      Direction.Left.rotate(-4) should be (Direction.Left)

      Direction.Top.clockwise should be (Direction.Right)
      Direction.Right.clockwise should be (Direction.Bottom)
      Direction.Bottom.clockwise should be (Direction.Left)
      Direction.Left.clockwise should be (Direction.Top)

      Direction.Top.anticlockwise should be (Direction.Left)
      Direction.Right.anticlockwise should be (Direction.Top)
      Direction.Bottom.anticlockwise should be (Direction.Right)
      Direction.Left.anticlockwise should be (Direction.Bottom)
    }
    "each have a Principal" in {
      Direction.Top.principal should be (Principal.Top)
      Direction.Right.principal should be (Principal.Right)
      Direction.Bottom.principal should be (Principal.Bottom)
      Direction.Left.principal should be (Principal.Left)
    }
  }

  "DirectionSets" should {
    "be empty when with bits of 0x0" in {
      val ds = DirectionSet(0x0)
      ds.code should be ("")
      ds.bits should be (0)
      ds.directions.isEmpty should be (true)
    }
    "have 4 values with bits of 0xf" in {
      val ds = DirectionSet(0xf)
      ds.code should be ("trbl")
      ds.bits should be (0xf)
      ds.directions.size should be (4)
    }
    "be able to be created from lists of Directions" in {
      DirectionSet(Direction.Top).code should be ("t")
      DirectionSet(Direction.Top, Direction.Right).code should be ("tr")
      DirectionSet(Direction.Bottom, Direction.Right).code should be ("rb")
      DirectionSet(Direction.Top, Direction.Left).code should be ("tl")
    }
    "have 16 combinations" in {
      DirectionSet.combinations.size should be (16)
    }
  }

  "Principals" should {
    "have a value for the Top point" in {
      val p = Principal.Top
      p.code should be ("t")
      p.dirSet should be (DirectionSet(Direction.Top))
      p.bit should be (128)
    }
    "be able to be rotated" in {
      Principal.Top.rotate(-2) should be (Principal.Left)
      Principal.Top.rotate(-1) should be (Principal.TopLeft)
      Principal.Top.rotate(0) should be (Principal.Top)
      Principal.Top.rotate(1) should be (Principal.TopRight)
      Principal.Top.rotate(2) should be (Principal.Right)

      Principal.Top.clockwise should be (Principal.TopRight)
      Principal.TopLeft.clockwise should be (Principal.Top)
      Principal.Top.anticlockwise should be (Principal.TopLeft)
      Principal.TopLeft.anticlockwise should be (Principal.Left)
    }
  }
  "PrincipalSets" should {
    "be empty when with bits of 0x0" in {
      val ps = PrincipalSet(0x0)
      ps.toString should be("<>")
      ps.bits should be(0)
      ps.points.isEmpty should be(true)
    }
    "have 8 values with bits of 0xff" in {
      val ps = PrincipalSet(0xff)
      ps.toString should be("<t:tr:r:rb:b:bl:l:tl>")
      ps.bits should be(0xff)
      ps.points.size should be(8)
    }
    "be able to be created from list of Principals" in {
      val ps = PrincipalSet(Principal.all: _*)
      ps.toString should be("<t:tr:r:rb:b:bl:l:tl>")
      ps.bits should be(0xff)
      ps.points.size should be(8)
    }
    "have 256 combinations" in {
      PrincipalSet.combinations.size should be (256)
    }
  }
}
