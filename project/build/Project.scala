import sbt._
import de.element34.sbteclipsify._

class Project(info: ProjectInfo) extends DefaultProject(info) with Eclipsify {
  override def artifactID = "etoffe"
  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.4.1"
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
}