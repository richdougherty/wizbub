name := "wizbub"
scalaVersion := "2.12.1"
// resolvers += Resolver.sonatypeRepo("public")
libraryDependencies ++= Seq(
  "com.badlogicgames.gdx" % "gdx" % "1.9.5",
  "org.json4s" %% "json4s-jackson" % "3.5.0",
  "com.squidpony" % "squidlib" % "3.0.0-b6",
  // Test dependencies
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.badlogicgames.gdx" % "gdx-backend-headless" % "1.9.5" % "test",
  // Desktop dependencies
  "com.badlogicgames.gdx" % "gdx-backend-lwjgl" % "1.9.5",
  "com.badlogicgames.gdx" % "gdx-platform" % "1.9.5" classifier("natives-desktop")
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