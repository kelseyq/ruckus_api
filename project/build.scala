import sbt._
import Keys._

object RuckusBuild extends sbt.Build {
  lazy val root =
    Project("ruckus", file("."),
      settings = Defaults.defaultSettings
    ) dependsOn(gamedayOrig)

  // git-dependencies don't work on Heroku so we use submodules
  // don't forget:
  // git submodule init
  // git submodule update

  lazy val dispatch = file("lib/dispatch")

  lazy val gamedayOrig = Project(
    "gameday-fork",
    file("lib/gameday-fork")
  ) dependsOn (dispatch)
}