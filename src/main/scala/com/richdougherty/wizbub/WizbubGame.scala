package com.richdougherty.wizbub

import java.util.concurrent.{Executors, TimeUnit}

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx._
import com.badlogic.gdx.graphics.g2d.{BitmapFont, GlyphLayout, SpriteBatch}
import com.badlogic.gdx.graphics.{FPSLogger, GL20, OrthographicCamera}
import com.richdougherty.wizbub.Compass.{Principal, PrincipalSet}
import com.richdougherty.wizbub.GroundEntity.CutGrass
import com.richdougherty.wizbub.dawnlike.DawnLikeTiles
import com.richdougherty.wizbub.dawnlike.index.TileQuery.{AttrContains, NoAttr}

import scala.concurrent.{ExecutionContext, Future}

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

  private val wallTiles = new WallTiling(dawnLikeTiles)
  private val horzClosedDoorTile = dawnLikeTiles("Objects", "Door", 0, 0).singleFrameTile(0)
  private val horzOpenDoorTile = dawnLikeTiles("Objects", "Door", 0, 0).singleFrameTile(1)
  private val vertClosedDoorTile = dawnLikeTiles("Objects", "Door", 1, 0).singleFrameTile(0)
  private val vertOpenDoorTile = dawnLikeTiles("Objects", "Door", 1, 0).singleFrameTile(1)
  private val treeTile: DawnLikeTile = dawnLikeTiles("Objects", "Tree", 3, 3)
  class ForestDrawable extends Drawable {
    override def draw(batch: SpriteBatch, x: Float, y: Float): Unit = {
      treeTile.draw(batch, x - 0.5f, y - 0.5f)
      treeTile.draw(batch, x, y)
    }
  }
  private val forestDrawable: Drawable = new ForestDrawable

  // THREAD POOLS //

  val threadPoolExecutor = Executors.newCachedThreadPool()
  addDisposeLogic {
    threadPoolExecutor.shutdown()
    threadPoolExecutor.awaitTermination(30, TimeUnit.SECONDS)
  }
  val threadPoolExecutionContext = ExecutionContext.fromExecutor(threadPoolExecutor)

  // MODEL //

  val worldPickler = new WorldPickler()(threadPoolExecutionContext)

  private val idGenerator = new Entity.IdGenerator
  private val worldSlice: WorldSlice = new WorldSlice

  // Generate default world
  for (x <- 0 until WorldSlice.SIZE; y <- 0 until WorldSlice.SIZE) {
    worldSlice(x, y) = new GroundEntity(idGenerator.freshId(), GroundEntity.Grass)
  }
  worldSlice(1, 1).asInstanceOf[GroundEntity].aboveEntity = new PlayerEntity(idGenerator.freshId(), 0)
  worldSlice(3, 3).asInstanceOf[GroundEntity].aboveEntity = new PlayerEntity(idGenerator.freshId(), 1)
  // If there's a save file overwrite the world with its contents
  worldPickler.readFromFile(worldSlice)

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
  private val fpsLogger = new FPSLogger()

  // INPUT //

  sealed trait Menu
  case object Top extends Menu
  case object ActionMenu extends Menu
  case class BuildMenu(desc: String, create: () => Entity, clearsGround: Boolean) extends Menu

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
      case ActionMenu => s"Esc = back, arrows = pick action"
      case BuildMenu(desc, _, _) => s"Esc = back, arrows = $desc"
    }
    currentMenu = m
    menuMessageGlyphs.setText(font, message)
  }
  setCurrentMenu(Top)

  trait SaveFileState
  case object NotWriting extends SaveFileState
  case class Writing(writeToFileFuture: Future[Unit]) extends SaveFileState {
    var pendingWrite: Boolean = false
  }
  var saveFileState: SaveFileState = NotWriting

  def scheduleSave(): Unit = saveFileState match {
    case NotWriting =>
      val f = worldPickler.writeToFile(worldSlice)
    case w: Writing =>
      w.pendingWrite = true
  }

  object BuildKey {
    def unapply(keycode: Int): Option[BuildMenu] = keycode match {
      case Keys.W => Some(BuildMenu("build wall", () => new WallEntity(-1), clearsGround = true))
      case Keys.T => Some(BuildMenu("plant tree", () => new TreeEntity(-1), clearsGround = false))
      case Keys.O => Some(BuildMenu("build door", () => new DoorEntity(-1, open=false), clearsGround = false))
      case _ => None
    }
  }

  // Hacky support for moving player0 with arrow keys
  Gdx.input.setInputProcessor(new InputAdapter {
    import Input.Keys
    override def keyDown(keycode: Int): Boolean = {
      def invalidateCachedEntityDrawables(x: Int, y: Int): Unit = {
        def invalidateEntity(entity: Entity): Unit = entity match {
          case null => ()
          case ground: GroundEntity =>
            entity.drawable = null
            invalidateEntity(ground.aboveEntity)
          case _ =>
            entity.drawable = null
        }
        for (dx <- -2 to 2; dy <- -2 to 2) {
          invalidateEntity(worldSlice.getOrNull(x + dx, y + dy))
        }
      }
      def changeGroundKind(newKind: GroundEntity.Kind): Boolean = {
        worldSlice(player0X, player0Y) match {
          case ground: GroundEntity =>
            ground.kind = newKind
            invalidateCachedEntityDrawables(player0X, player0Y)
            scheduleSave()
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
              worldSlice.getOrNull(player0X, player0Y) match {
                case oldGround: GroundEntity =>
                  assert(oldGround.aboveEntity != null)
                  oldGround.aboveEntity match {
                    case player: PlayerEntity if player.playerNumber == 0 =>
                      worldSlice.getOrNull(newX, newY) match {
                        case newGround: GroundEntity =>
                          newGround.aboveEntity match {
                            case null =>
                              newGround.aboveEntity = oldGround.aboveEntity
                              oldGround.aboveEntity = null
                              invalidateCachedEntityDrawables(player0X, player0Y)
                              player0X = newX
                              player0Y = newY
                              worldCamera.position.x = player0X + 0.5f
                              worldCamera.position.y = player0Y + 0.5f
                              worldCamera.update()
                            case door: DoorEntity if !door.open =>
                              door.open = true
                              invalidateCachedEntityDrawables(newX, newY)
                            case door: DoorEntity if door.open =>
                              // TODO: Move player through door.
                            case _ => ()
                          }
                        case _ => ()
                      }
                    case illegal => throw new IllegalStateException(s"Expected player 0 at ${(player0X, player0Y)}: $illegal")
                  }
                case illegal => throw new IllegalStateException(s"Expected player 0 to be standing on a ground tile at ${(player0X, player0Y)}: $illegal")
              }
              true
            case Keys.A =>
              setCurrentMenu(ActionMenu)
              true
            case Keys.G => changeGroundKind(GroundEntity.Grass)
            case Keys.D => changeGroundKind(GroundEntity.Dirt)
            case BuildKey(buildMenu) =>
              setCurrentMenu(buildMenu)
              true
            case _ => false
          }
        case ActionMenu =>
          keycode match {
            case Keys.ESCAPE =>
              setCurrentMenu(Top)
              true
            case DirectionKey(dir) =>
              val newX = player0X + dir.dx
              val newY = player0Y + dir.dy
              worldSlice(newX, newY) match {
                case ground: GroundEntity =>
                  ground.aboveEntity match {
                    case door: DoorEntity =>
                      door.open = !door.open
                      invalidateCachedEntityDrawables(newX, newY)
                      setCurrentMenu(Top)
                    case _ => ()
                  }
                case _ => ()
              }
              true
            case _ => false
          }
        case BuildMenu(_, buildEntity, clearsGround) =>
          keycode match {
            case Keys.ESCAPE =>
              setCurrentMenu(Top)
              true
            case DirectionKey(dir) =>
              val newX = player0X + dir.dx
              val newY = player0Y + dir.dy
              worldSlice(newX, newY) match {
                case ground: GroundEntity if ground.aboveEntity == null =>
                  if (clearsGround && ground.kind == GroundEntity.Grass) ground.kind = CutGrass // Building a wall cuts the grass
                  ground.aboveEntity = buildEntity()
                  invalidateCachedEntityDrawables(player0X, player0Y)
                  scheduleSave()
                  setCurrentMenu(Top)
                case _ => ()
              }
              true
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

    saveFileState match {
      case NotWriting => ()
      case w: Writing =>
        if (w.writeToFileFuture.isCompleted) {
          saveFileState = NotWriting
          if (w.pendingWrite) {
            scheduleSave()
          }
        }
    }

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

      def getNearbyEntity(dx: Int, dy: Int): Entity = worldSlice.getOrNull(sceneX + dx, sceneY + dy)
      def getEntityInDir(dir: Direction): Entity = worldSlice.getOrNull(sceneX + dir.dx, sceneY + dir.dy)
      def filterCardinalPoints(f: Entity => Boolean): DirectionSet = {
        var directionBits = 0
        for (dir <- Direction.all) {
          val dirMatches: Boolean = f(getNearbyEntity(dir.dx, dir.dy))
          if (dirMatches) directionBits |= dir.bit
        }
        DirectionSet(directionBits)
      }
      def filterPrincipalPoints(f: Entity => Boolean): PrincipalSet = {
        var bits = 0
        for (p <- Principal.all) {
          val matches: Boolean = f(getNearbyEntity(p.dx, p.dy))
          if (matches) bits |= p.bit
        }
        PrincipalSet(bits)
      }

      def renderCachedOrElse(entity: Entity)(getDrawable: => Drawable): Unit = {
        if (entity.drawable == null) {
          entity.drawable = getDrawable
          assert(entity.drawable != null)
        }
        entity.drawable.draw(batch, sceneX, sceneY)
      }
      def renderGround(kind: GroundEntity.Kind): Drawable = {
        kind match {
          case GroundEntity.Grass =>
            val surroundingDirt: DirectionSet = filterCardinalPoints {
              case ground: GroundEntity =>  ground.kind == GroundEntity.Dirt
              case _ => false
            }
            grassTiles(surroundingDirt.bits)
          case GroundEntity.Dirt => dirtTile
          case GroundEntity.CutGrass => dirtTile
        }
      }
      def entityJoinsWall(entity: Entity): Boolean = entity match {
        case ground: GroundEntity =>
          ground.aboveEntity match {
            case _: WallEntity => true
            case _: DoorEntity => true
            case _ => false
          }
        case _ => false
      }
      def renderEntity(entity: Entity): Unit = entity match {
        case null => ()
        case ground: GroundEntity =>
          renderCachedOrElse(ground) { renderGround(ground.kind) }
          renderEntity(ground.aboveEntity)
        case wall: WallEntity =>
          renderCachedOrElse(wall) {
            val surroundingWalls: PrincipalSet = filterPrincipalPoints(entityJoinsWall)
            wallTiles(surroundingWalls)
          }
        case tree: TreeEntity =>
          renderCachedOrElse(tree) {
            // If there's are trees above and to the left then draw a tree between
            // this tree and that one. This will fill in the gap between the trees
            // and create a 'forest'.
            def isTree(dx: Int, dy: Int): Boolean = getNearbyEntity(dx, dy) match {
              case ground: GroundEntity =>
                ground.aboveEntity match {
                  case _: TreeEntity => true
                  case _ => false
                }
              case _ => false
            }
            if (isTree(-1, 0) && isTree(-1, -1) && isTree(0, -1)) {
              forestDrawable
            } else {
              treeTile
            }
          }
          // Draw the main tree
          treeTile.draw(batch, sceneX, sceneY)
        case door: DoorEntity =>
          renderCachedOrElse(door) {
            def joinInDir(dir: Direction): Boolean = entityJoinsWall(getEntityInDir(dir))
            if (joinInDir(Direction.Left) || joinInDir(Direction.Right) || !(joinInDir(Direction.Top) || joinInDir(Direction.Bottom))) {
              if (door.open) horzOpenDoorTile else horzClosedDoorTile
            } else {
              if (door.open) vertOpenDoorTile else vertClosedDoorTile
            }
          }
        case player: PlayerEntity =>
          renderCachedOrElse(player) { playerTiles(player.playerNumber) }
      }
      renderEntity(worldSlice.getOrNull(sceneX, sceneY))
    }

    // Draw UI objects

    batch.setProjectionMatrix(uiCamera.combined)

    font.draw(batch, menuMessageGlyphs, uiCamera.viewportWidth/2 - menuMessageGlyphs.width/2, font.getData.lineHeight + 10)

    batch.end()

    // Log a message about the game's frame rate once per second
    fpsLogger.log()
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
