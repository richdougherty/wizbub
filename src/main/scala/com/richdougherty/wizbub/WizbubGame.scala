package com.richdougherty.wizbub

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx._
import com.badlogic.gdx.graphics.g2d.{GlyphLayout, BitmapFont, SpriteBatch}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera}
import com.richdougherty.wizbub.GroundEntity.CutGrass
import com.richdougherty.wizbub.dawnlike.DawnLikeTiles
import com.richdougherty.wizbub.dawnlike.index.TileQuery.{NoAttr, AttrContains}

class WizbubGame extends ScopedApplicationListener {

  // TILES //

  val dawnLikeTiles = disposeLater { new DawnLikeTiles() }

  private val playerTiles = Array(
    dawnLikeTiles.findTile(AttrContains("nethack", "neanderthal")),
    dawnLikeTiles.findTile(AttrContains("nethack", "archeologist"))
  )

  private val grassTiles: Array[DawnLikeTile] = new Array[DawnLikeTile](16)
  for (directions <- DirectionSet.combinations) {
    grassTiles(directions.bits) = {
      val edgeQuery = if (directions.bits == 0) {
        NoAttr("edge_dirs")
      } else {
        AttrContains("edge_dirs", directions.code)
      }
      dawnLikeTiles.findTile(AttrContains("ground", "grass"), AttrContains("color", "leaf"), edgeQuery)
    }
  }

  private val dirtTile: DawnLikeTile = dawnLikeTiles.findTile(AttrContains("ground", "dirt"), AttrContains("color", "ocher"), NoAttr("edge_dirs"))
  private val wallTile: DawnLikeTile = dawnLikeTiles("Objects", "Wall", 3, 3)
  private val treeTile: DawnLikeTile = dawnLikeTiles("Objects", "Tree", 3, 3)


  // MODEL //

  private val idGenerator = new Entity.IdGenerator
  private val worldSlice: WorldSlice = new WorldSlice

  // Generate default world
  for (x <- 0 until WorldSlice.SIZE; y <- 0 until WorldSlice.SIZE) {
    worldSlice(x, y) = new GroundEntity(idGenerator.freshId(), GroundEntity.Grass)
  }
  worldSlice(1, 1).asInstanceOf[GroundEntity].aboveEntity = new PlayerEntity(idGenerator.freshId(), 0)
  worldSlice(3, 3).asInstanceOf[GroundEntity].aboveEntity = new PlayerEntity(idGenerator.freshId(), 1)
  // If there's a save file overwrite the world with its contents
  WorldPickler.readFromFile(worldSlice)

  // HACK: Scan the world to get the location of Player 0
  private var player0X: Int = -1
  private var player0Y: Int = -1
  for (x <- 0 until WorldSlice.SIZE; y <- 0 until WorldSlice.SIZE) {
    worldSlice(x, y) match {
      case ground: GroundEntity =>
        ground.aboveEntity match {
          case player: PlayerEntity if player.playerNumber == 0 =>
            player0X = x
            player0Y = y
          case _ => ()
        }
      case _ => ()
    }
  }

  // DISPLAY //

  private val worldCamera = new OrthographicCamera()

  private val batch: SpriteBatch = disposeLater { new SpriteBatch() }

  private val uiCamera = new OrthographicCamera()
  private val font = disposeLater { new BitmapFont() }

  // INPUT //

  sealed trait Menu
  case object Top extends Menu
  case object BuildWall extends Menu
  case object PlantTree extends Menu

  object DirectionKey {
    def unapply(keycode: Int): Option[Direction] = keycode match {
      case Keys.UP => Some(Direction.Top)
      case Keys.RIGHT => Some(Direction.Right)
      case Keys.DOWN => Some(Direction.Bottom)
      case Keys.LEFT => Some(Direction.Left)
      case _ => None
    }
  }

  var currentMenu: Menu = null
  val menuMessageGlyphs = new GlyphLayout()

  def setCurrentMenu(m: Menu): Unit = {
    val message = m match {
      case Top => "Arrows = move, D = dirt, G = grass, T = plant tree, W = build wall"
      case BuildWall => "Esc = back, arrows = build wall"
      case PlantTree => "Esc = back, arrows = plant tree"
    }
    currentMenu = m
    menuMessageGlyphs.setText(font, message)
  }
  setCurrentMenu(Top)

  // Hacky support for moving player0 with arrow keys
  Gdx.input.setInputProcessor(new InputAdapter {
    import Input.Keys
    override def keyDown(keycode: Int): Boolean = {
      def changeGroundKind(newKind: GroundEntity.Kind): Boolean = {
        worldSlice(player0X, player0Y) match {
          case ground: GroundEntity =>
            ground.kind = newKind
            WorldPickler.writeToFile(worldSlice)
            true
          case _ => false
        }
      }
      currentMenu match {
        case Top =>
          keycode match {
            case DirectionKey(dir) =>
              val newX = player0X + dir.dx
              val newY = player0Y + dir.dy
              if (0 <= newX && newX < WorldSlice.SIZE && 0 <= newY && newY < WorldSlice.SIZE) {
                worldSlice(player0X, player0Y) match {
                  case oldGround: GroundEntity =>
                    assert(oldGround.aboveEntity != null)
                    oldGround.aboveEntity match {
                      case player: PlayerEntity if player.playerNumber == 0 =>
                        worldSlice(newX, newY) match {
                          case newGround: GroundEntity if newGround.aboveEntity == null =>
                            newGround.aboveEntity = oldGround.aboveEntity
                            oldGround.aboveEntity = null
                            player0X = newX
                            player0Y = newY
                            worldCamera.position.x = player0X + 0.5f
                            worldCamera.position.y = player0Y + 0.5f
                            worldCamera.update()
                          case _ => ()
                        }
                      case illegal => throw new IllegalStateException(s"Expected player 0 at ${(player0X, player0Y)}: $illegal")
                    }
                  case illegal => throw new IllegalStateException(s"Expected player 0 to be standing on a ground tile at ${(player0X, player0Y)}: $illegal")
                }
              }
              true
            case Keys.G => changeGroundKind(GroundEntity.Grass)
            case Keys.D => changeGroundKind(GroundEntity.Dirt)
            case Keys.W =>
              setCurrentMenu(BuildWall)
              true
            case Keys.T =>
              setCurrentMenu(PlantTree)
              true
            case _ => false
          }
        case BuildWall =>
          keycode match {
            case Keys.ESCAPE =>
              setCurrentMenu(Top)
              true
            case DirectionKey(dir) =>
              val newX = player0X + dir.dx
              val newY = player0Y + dir.dy
              worldSlice(newX, newY) match {
                case ground: GroundEntity if ground.aboveEntity == null =>
                  if (ground.kind == GroundEntity.Grass) ground.kind = CutGrass // Building a wall cuts the grass
                  ground.aboveEntity = new WallEntity(idGenerator.freshId())
                  WorldPickler.writeToFile(worldSlice)
                  setCurrentMenu(Top)
                  true
                case _ => false
              }
            case _ => false
          }
          case PlantTree =>
            keycode match {
              case Keys.ESCAPE =>
                setCurrentMenu(Top)
                true
              case DirectionKey(dir) =>
                val newX = player0X + dir.dx
                val newY = player0Y + dir.dy
                worldSlice(newX, newY) match {
                  case ground: GroundEntity if ground.aboveEntity == null =>
                    ground.aboveEntity = new TreeEntity(idGenerator.freshId())
                    WorldPickler.writeToFile(worldSlice)
                    setCurrentMenu(Top)
                    true
                  case _ => false
                }
              case _ => false
            }
      }
    }
  })

  private def grassTileIndex(topDirt: Boolean, rightDirt: Boolean, bottomDirt: Boolean, leftDirt: Boolean): Int = {
    var i = 0
    if (topDirt) i += 8
    if (rightDirt) i += 4
    if (bottomDirt) i += 2
    if (leftDirt) i += 1
    i
  }

  override def render(): Unit = {
    Gdx.gl.glClearColor(0, 0, 0, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
    batch.begin()

    // Draw world objects

    batch.setProjectionMatrix(worldCamera.combined)

    val sceneLeft = Math.floor(worldCamera.position.x - worldCamera.viewportWidth/2).toInt
    val sceneRight = Math.ceil(worldCamera.position.x + worldCamera.viewportWidth/2).toInt
    val sceneTop = Math.floor(worldCamera.position.y - worldCamera.viewportHeight/2).toInt
    val sceneBottom = Math.ceil(worldCamera.position.y + worldCamera.viewportHeight/2).toInt
    for (sceneX <- sceneLeft to sceneRight; sceneY <- sceneTop to sceneBottom) {
      if (0 <= sceneX && sceneX < WorldSlice.SIZE && 0 <= sceneY && sceneY < WorldSlice.SIZE) {
        def renderGround(kind: GroundEntity.Kind): Unit = {
          val tile = kind match {
            case GroundEntity.Grass =>
              def isNearbyGroundDirt(dir: Direction): Boolean = {
                val x = sceneX + dir.dx
                val y = sceneY + dir.dy
                if (x < 0 || x >= WorldSlice.SIZE || y < 0 || y >= WorldSlice.SIZE) {
                  false // Assume not-dirt if outside world bounds
                } else {
                  worldSlice(x, y) match {
                    case ground: GroundEntity =>
                      ground.kind == GroundEntity.Dirt
                    case _ =>
                      false
                  }
                }
              }
              var directionBits = 0
              for (d <- Direction.all) {
                if (isNearbyGroundDirt(d)) directionBits |= d.bit
              }
              grassTiles(directionBits)
            case GroundEntity.Dirt => dirtTile
            case GroundEntity.CutGrass => dirtTile
          }
          tile.draw(batch, sceneX, sceneY)
        }
        def renderEntity(entity: Entity): Unit = entity match {
          case null => ()
          case ground: GroundEntity =>
            renderGround(ground.kind)
            renderEntity(ground.aboveEntity)
          case _: WallEntity =>
            wallTile.draw(batch, sceneX, sceneY)
          case _: TreeEntity =>
            treeTile.draw(batch, sceneX, sceneY)
          case player: PlayerEntity =>
            playerTiles(player.playerNumber).draw(batch, sceneX, sceneY)
        }
        renderEntity(worldSlice(sceneX, sceneY))
      }
    }

    // Draw UI objects

    batch.setProjectionMatrix(uiCamera.combined)

    font.draw(batch, menuMessageGlyphs, uiCamera.viewportWidth/2 - menuMessageGlyphs.width/2, font.getData.lineHeight + 10)

    batch.end()
  }

  override def resize(width: Int, height: Int): Unit = {
    // The camera is scaled so it holds 16 world tiles in its smallest
    // dimension. Each camera unit square corresponds to a single tile.
    val worldZoom = 2f
    val pixelsPerTile = 16f * worldZoom
    worldCamera.viewportWidth = width / pixelsPerTile
    worldCamera.viewportHeight = height / pixelsPerTile
    worldCamera.position.x = player0X + 0.5f
    worldCamera.position.y = player0Y + 0.5f
    worldCamera.up.y = -1
    worldCamera.direction.z = 1
    worldCamera.update()

    // The UI camera uses the same scale as the enclosing viewport
    uiCamera.viewportWidth = width
    uiCamera.viewportHeight = height
    uiCamera.position.x = uiCamera.viewportWidth/2
    uiCamera.position.y = uiCamera.viewportHeight/2
    uiCamera.update()
  }

}
