import mill._
import scalalib._

object `package` extends RootModule with ScalaModule {
  def scalaVersion: T[String] = "2.13.12"
  override def ivyDeps = Agg(
    ivy"org.chipsalliance::chisel:6.5.0",
    ivy"edu.berkeley.cs::chiseltest:6.0.0"
    // ivy"com.typesafe.scala-logging::scala-logging:3.9.5",
    // ivy"ch.qos.logback:logback-classic:1.3.5"

  )
  override def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-language:reflectiveCalls",
    "-feature",
    "-Xcheckinit",
    "-Xfatal-warnings",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
    "-Ymacro-annotations",
    "-opt:local"
    // "-g:none"
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"org.chipsalliance:::chisel-plugin::6.6.0"
  )

  def forkArgsGlobal = T { Seq(s"-Dproject.root=${T.workspace}", "-Dcustom.file=testData.csv") }
  override def forkArgs = forkArgsGlobal

  object test extends ScalaTests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.19", ivy"edu.berkeley.cs::chiseltest:6.0.0")
    def testFramework: T[String] = "org.scalatest.tools.Framework"

    override def forkArgs = forkArgsGlobal
  }
}
