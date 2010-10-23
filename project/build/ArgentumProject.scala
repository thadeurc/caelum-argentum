import sbt._ 
 
class ArgentumProject(info: ProjectInfo) extends DefaultProject(info) { 
  val scalatest = "org.scalatest" % "scalatest" % "1.2"
}
