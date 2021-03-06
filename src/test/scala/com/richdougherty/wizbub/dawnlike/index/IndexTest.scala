package com.richdougherty.wizbub.dawnlike.index

import com.badlogic.gdx.LifecycleListener
import com.badlogic.gdx.backends.headless.{HeadlessApplication, HeadlessApplicationConfiguration}
import com.richdougherty.wizbub.ScopedApplicationListener
import com.richdougherty.wizbub.dawnlike.index.TileQuery.{AttrContains, NoAttr}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}

import scala.collection.immutable
import scala.concurrent.Promise
import scala.util.Try

class IndexTest extends WordSpec with Matchers with ScalaFutures {

  implicit override val patienceConfig =
    PatienceConfig(timeout = Span(2, Seconds), interval = Span(5, Millis))

  def withIndex[A](body: Index => A): Unit = {
    val config = new HeadlessApplicationConfiguration()
    config.renderInterval = -1f // Don't call render method
    val result = Promise[Index]
    val appDone = Promise[Unit]
    lazy val app: HeadlessApplication = new HeadlessApplication(ScopedApplicationListener.listener(new ScopedApplicationListener {
      result.complete(Try {
        Index.readFromFile()
      })
      app.exit()
    }), config)
    app.addLifecycleListener(new LifecycleListener {
      override def pause(): Unit = ()
      override def resume(): Unit = ()
      override def dispose(): Unit = appDone.success(())
    })
    whenReady(result.future)(body)
    appDone.future.futureValue
  }

  "The DawnLike tile Index read from disk" should {
    "have 5 directories" in withIndex { index: Index =>
      index.directories.size should be (5)
    }
    "be able to find the Nethack 'neanderthal' tile in the right place" in withIndex { index: Index =>
      index.findTile(AttrContains("nethack", "neanderthal")) should be (Index.Ref(
        "Characters", "Player", Tile(0, 0, Map("nethack" -> immutable.Seq("neanderthal")))
      ))
    }
    "be able to find different ground tiles" in withIndex { index: Index =>
      index.findTile(AttrContains("ground", "grass"), AttrContains("color", "leaf"), AttrContains("edge_dirs", "tl")) should be (Index.Ref(
        "Objects", "Floor", Tile(7, 6,
          Map(
            "ground" -> List("grass"), "color" -> List("leaf"),
            "edge" -> List("dirt"), "edge_color" -> List("ocher"), "edge_dirs" -> List("tl")
          )
        )
      ))
      index.findTile(AttrContains("ground", "grass"), AttrContains("color", "leaf"), NoAttr("edge_dirs")) should be (Index.Ref(
        "Objects", "Floor", Tile(8, 7,
          Map(
            "ground" -> List("grass"), "color" -> List("leaf")
          )
        )
      ))
    }
  }

}
