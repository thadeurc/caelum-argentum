import sbt._ 
 
class ArgentumProject(info: ProjectInfo) extends DefaultProject(info) with IdeaProject { 
  val scalatest = "org.scalatest" % "scalatest" % "1.2"
}
