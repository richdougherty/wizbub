name := "wizbub"
scalaVersion := "2.11.7"
// resolvers += Resolver.sonatypeRepo("public")
libraryDependencies ++= Seq(
  "com.badlogicgames.gdx" % "gdx" % "1.8.0",
  // Desktop dependencies
  "com.badlogicgames.gdx" % "gdx-backend-lwjgl" % "1.8.0",
  "com.badlogicgames.gdx" % "gdx-platform" % "1.8.0" classifier("natives-desktop")
)
fork in run := true
baseDirectory in run := file("src/main/assets")
javaOptions in run ++= {
  if (sys.props("os.name") == "Mac OS X") {
    Seq(
      "-Xdock:name=Wizbub",
      "-Xdock:icon=dawnlike/Derived/icon128.png"
    )
  } else Seq.empty
}