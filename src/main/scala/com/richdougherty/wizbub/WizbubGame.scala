package com.richdougherty.wizbub

import com.badlogic.gdx._
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera}

class WizbubGame extends ApplicationAdapter {

  private var camera: OrthographicCamera = null
  private var worldViewWidth: Int = -1
  private var worldViewHeight: Int = -1
  private var batch: SpriteBatch = null
  private var playerAtlas: DawnLikeAtlas = null
  private var player0Tile: DawnLikeTile = null
  private var player1Tile: DawnLikeTile = null
  private var floorAtlas: DawnLikeAtlas = null
  private var grassTile: DawnLikeTile = null
  private var dirtTile: DawnLikeTile = null

  private val idGenerator = new Entity.IdGenerator
  private var worldSlice: WorldSlice = null
  private var player0Entity: Entity = null
  private var player0X: Int = 1
  private var player0Y: Int = 1

  private var grassTiles: Array[DawnLikeTile] = null

  private def grassTileIndex(topDirt: Boolean, rightDirt: Boolean, bottomDirt: Boolean, leftDirt: Boolean): Int = {
    var i = 0
    if (topDirt) i += 8
    if (rightDirt) i += 4
    if (bottomDirt) i += 2
    if (leftDirt) i += 1
    i
  }

  override def create(): Unit =  {
    resizeCamera(Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    batch = new SpriteBatch()
    playerAtlas = DawnLikeAtlas.loadAnimated("Characters", "Player")
    player0Tile = playerAtlas(0, 0)
    player1Tile = playerAtlas(0, 6)
    floorAtlas = DawnLikeAtlas.loadStatic("Objects", "Floor")

    grassTiles = new Array[DawnLikeTile](16)
    grassTiles(0) = floorAtlas(7, 8)
    grassTiles(1) = floorAtlas(7, 7)
    grassTiles(2) = floorAtlas(8, 8)
    grassTiles(3) = floorAtlas(8, 7)
    grassTiles(4) = floorAtlas(7, 9)
    grassTiles(5) = floorAtlas(7, 10)
    grassTiles(6) = floorAtlas(8, 9)
    grassTiles(7) = floorAtlas(8, 10)
    grassTiles(8) = floorAtlas(6, 8)
    grassTiles(9) = floorAtlas(6, 7)
    grassTiles(10) = floorAtlas(7 ,12)
    grassTiles(11) = floorAtlas(7, 11)
    grassTiles(12) = floorAtlas(6, 9)
    grassTiles(13) = floorAtlas(6, 10)
    grassTiles(14) = floorAtlas(7, 13)
    grassTiles(15) = floorAtlas(6, 12)

    grassTile = floorAtlas(7, 8)
    dirtTile = floorAtlas(19, 1)

    worldSlice = new WorldSlice
    for (x <- 0 until WorldSlice.SIZE; y <- 0 until WorldSlice.SIZE) {
      worldSlice(x, y) = new GroundEntity(idGenerator.freshId(), GroundEntity.Grass)
    }
    player0Entity = new PlayerEntity(idGenerator.freshId(), player0Tile)
    val player1Entity = new PlayerEntity(idGenerator.freshId(), player1Tile)
    worldSlice(player0X, player0Y).asInstanceOf[GroundEntity].aboveEntity = player0Entity
    worldSlice(3, 3).asInstanceOf[GroundEntity].aboveEntity = player1Entity

    // Hacky support for moving player0 with arrow keys
    Gdx.input.setInputProcessor(new InputAdapter {
      import Input.Keys
      override def keyDown(keycode: Int): Boolean = {
        def move(dx: Int, dy: Int): Boolean = {
          val newX = player0X + dx
          val newY = player0Y + dy
          worldSlice(player0X, player0Y) match {
            case oldGround: GroundEntity if  oldGround.aboveEntity == player0Entity =>
              worldSlice(newX, newY) match {
                case newGround: GroundEntity if newGround.aboveEntity == null =>
                  oldGround.aboveEntity = null
                  newGround.aboveEntity = player0Entity
                  player0X = newX
                  player0Y = newY
                  true
                case _ => false
              }
            case _ => throw new IllegalStateException("Player entity missing")
          }
        }
        def changeGroundKind(newKind: GroundEntity.Kind): Boolean = {
          worldSlice(player0X, player0Y) match {
            case ground: GroundEntity =>
              ground.kind = newKind
              true
            case _ => false
          }
        }
        keycode match {
          case Keys.RIGHT => move(1, 0)
          case Keys.LEFT => move(-1, 0)
          case Keys.UP => move(0, -1)
          case Keys.DOWN => move(0, 1)
          case Keys.G => changeGroundKind(GroundEntity.Grass)
          case Keys.D => changeGroundKind(GroundEntity.Dirt)
          case _ => false
        }
      }
    })
  }

  override def render(): Unit = {
    Gdx.gl.glClearColor(0, 0, 0, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
    camera.update()
    batch.begin()
    batch.setProjectionMatrix(camera.combined)
    for (worldX <- 0 until worldViewWidth; worldY <- 0 until worldViewHeight) {
      val sceneX = worldX
      val sceneY = worldViewHeight - worldY - 1
      def renderEntity(entity: Entity): Unit = entity match {
        case null => ()
        case ground: GroundEntity =>
          val tile = ground.kind match {
            case GroundEntity.Grass =>
              def isNearbyGroundDirt(dx: Int, dy: Int): Boolean = {
                val x = worldX + dx
                val y = worldY + dy
                if (x < 0 || x >= WorldSlice.SIZE || y < 0 || y >= WorldSlice.SIZE) {
                  false // Asume not-dirt if outside world bounds
                } else {
                  val nearbyGround = worldSlice(x, y).asInstanceOf[GroundEntity]
                  nearbyGround.kind == GroundEntity.Dirt
                }
              }
              val i = grassTileIndex(
                isNearbyGroundDirt(0, -1),
                isNearbyGroundDirt(1, 0),
                isNearbyGroundDirt(0, 1),
                isNearbyGroundDirt(-1, 0)
              )
              grassTiles(i)
            case GroundEntity.Dirt => dirtTile
          }
          tile.draw(batch, sceneX, sceneY)
          renderEntity(ground.aboveEntity)
        case player: PlayerEntity =>
          player.tile.draw(batch, sceneX, sceneY)
      }
      renderEntity(worldSlice(worldX, worldY))
    }
    batch.end()
  }

  override def resize(width: Int, height: Int): Unit = {
    resizeCamera(width, height)
  }

  private def resizeCamera(width: Int, height: Int): Unit = {
    val scale = 16f / Math.min(height, width)
    val cameraWidth = width * scale
    val cameraHeight = height * scale
    camera = new OrthographicCamera(cameraWidth, cameraHeight)
    camera.translate(camera.viewportWidth/2, camera.viewportHeight/2)
    worldViewWidth = Math.ceil(cameraWidth).toInt
    worldViewHeight = Math.ceil(cameraHeight).toInt
  }

}
