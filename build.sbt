name := "wizbub"
scalaVersion := "2.11.7"
// resolvers += Resolver.sonatypeRepo("public")
libraryDependencies ++= Seq(
  "com.badlogicgames.gdx" % "gdx" % "1.8.0",
  "org.json4s" %% "json4s-jackson" % "3.3.0",
  // Test dependencies
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "com.badlogicgames.gdx" % "gdx-backend-headless" % "1.8.0" % "test",
  // Desktop dependencies
  "com.badlogicgames.gdx" % "gdx-backend-lwjgl" % "1.9.2",
  "com.badlogicgames.gdx" % "gdx-platform" % "1.9.2" classifier("natives-desktop")
)

// Fork and set working directory so libGDX can find assets
fork in run := true
baseDirectory in run := file("src/main/assets")
fork in Test := true
baseDirectory in Test := file("src/main/assets")

javaOptions in run ++= {
  if (sys.props("os.name") == "Mac OS X") {
    Seq(
      "-Xdock:name=Wizbub",
      "-Xdock:icon=dawnlike/Derived/icon128.png"
    )
  } else Seq.empty
}