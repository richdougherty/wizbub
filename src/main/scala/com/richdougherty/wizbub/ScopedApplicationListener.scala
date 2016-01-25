package com.richdougherty.wizbub

import com.badlogic.gdx.ApplicationListener
import com.badlogic.gdx.utils.Disposable

import scala.collection.mutable.ArrayBuffer

class ScopedApplicationListener extends Disposable {

  private val managedDisposals = ArrayBuffer[Disposable]()

  protected def disposeLater[D <: Disposable](d: D): D = {
    managedDisposals :+ d
    d
  }

  def resize(width: Int, height: Int): Unit = ()

  def pause(): Unit = ()

  def render(): Unit = ()

  def resume(): Unit = ()

  override def dispose(): Unit = {
    for (d <- managedDisposals.reverseIterator) {
      d.dispose()
    }
  }

}

object ScopedApplicationListener {
  def listener(create: => ScopedApplicationListener): ApplicationListener = {
    new ScopedApplicationListenerWrapper(() => create)
  }

  private class ScopedApplicationListenerWrapper(createFunction: () => ScopedApplicationListener) extends ApplicationListener {
    private var scoped: ScopedApplicationListener = null

    override def create(): Unit = { scoped = createFunction() }

    override def resize(width: Int, height: Int): Unit = { scoped.resize(width, height) }

    override def dispose(): Unit = { scoped.dispose() }

    override def pause(): Unit = { scoped.pause() }

    override def render(): Unit = { scoped.render() }

    override def resume(): Unit = { scoped.resume() }
  }
}