/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.test


import scala.collection.JavaConversions._
import java.nio.file.Path
import java.util.Properties
import org.apache.maven.Maven
import org.apache.maven.cli.event.ExecutionEventLogger
import org.apache.maven.execution._
import org.codehaus.plexus.{DefaultContainerConfiguration, DefaultPlexusContainer, PlexusConstants}


/** A support mixin trait for dealing with Maven.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
trait MavenSupport {

  /** Constructs and configures a new Plexus Container, used by Maven */
  def plexusContainer = {
    val containerConfig = new DefaultContainerConfiguration()
      .setClassPathScanning(PlexusConstants.SCANNING_INDEX)
      .setAutoWiring(true)
      .setName("maven")
    val container = new DefaultPlexusContainer(containerConfig)

    container
  }

  /** Builds a Maven Request based on the specified POM file, execution listener, and goals.
    * In case an execution listener is not specified, the default is used ([[ExecutionEventLogger]]).
    *
    * @param pomFile  The fully qualified POM file.
    * @param executionListener  An optional execution listener. If none is specified, the default
    *                           ([[ExecutionEventLogger]]) will be used.
    * @param goals  Maven goals to be executed.
    */
  def buildMavenRequest(pomFile: Path,
                        executionListener: Option[ExecutionListener],
                        goals: String*): MavenExecutionRequest = {
    val userProperties = new Properties()
    val executionRequestPopulator = plexusContainer.lookup(classOf[MavenExecutionRequestPopulator])

    userProperties.setProperty("pushChanges", "false")

    val req = new DefaultMavenExecutionRequest()
      .setPom(pomFile.toFile)
      .setBaseDirectory(pomFile.getParent.toFile)
      .setGoals(goals)
      .setInteractiveMode(false)
      .setOffline(true)
      .setExecutionListener(executionListener.getOrElse(new ExecutionEventLogger))
      .setUserProperties(userProperties)

    executionRequestPopulator.populateDefaults(req)
  }

  /** Runs the specified Maven goals on the specified POM file, using the default Execution Listener
    * ([[ExecutionEventLogger]]).
    * Calling this method is equivalent to calling
    * `buildMavenRequest(pomFile: Path, executionListener: Option[ExecutionListener], goals: String*)`,
    * passing `None` as the `executionListener` parameter.
    *
    * @param pomFile  The fully qualified POM file.
    * @param goals  Maven goals to be executed.
    */
  def executeMaven(pomFile: Path, goals: String*): MavenExecutionResult = {
    executeMaven(pomFile, None, goals: _*)
  }

  /** Runs the specified Maven goals on the specified POM file, using the specified Execution Listener; if none is
    * specified, the method will be using the default execution listener ([[ExecutionEventLogger]]).
    *
    * @param pomFile  The fully qualified POM file.
    * @param executionListener  An optional execution listener. If none is specified, the default
    *                           ([[ExecutionEventLogger]]) will be used.
    * @param goals  Maven goals to be executed.
    */
  def executeMaven(pomFile: Path,
                   executionListener: Option[ExecutionListener],
                   goals: String*): MavenExecutionResult = {
    val mavenRequest = buildMavenRequest(pomFile, executionListener, goals: _*)
    val maven = plexusContainer.lookup(classOf[Maven])

    maven.execute(mavenRequest)
  }
}


/** The Companion Object, which allows for "static" import style of the trait's functionality, rather than having to
  * extend from it.
  * I.e., instead of
  * {{{
  *   class MyClass extends MySuperClass with MavenSupport {
  *     ...
  *   }
  * }}}
  * one can simply use:
  * {{{
  *   import com.wix.oss.ci.police.test.MavenSupport._
  *
  *   ...
  *
  *   class MyClass extends MySuperClass {
  *     ...
  *   }
  * }}}
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object MavenSupport extends MavenSupport
