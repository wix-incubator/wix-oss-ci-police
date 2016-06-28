/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.mojos


import org.apache.maven.plugin.{AbstractMojo, MojoExecutionException, MojoFailureException}
import org.apache.maven.plugins.annotations.{LifecyclePhase, Mojo, Parameter}
import org.apache.maven.project.MavenProject
import com.wix.oss.ci.police.handlers.CiPoliceHandler


/** The Mojo class, being triggered by Maven to execute the `wix-oss-ci-police` goal.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
@Mojo(
  name = "wix-oss-ci-police",
  defaultPhase = LifecyclePhase.VALIDATE)
class CiPoliceMojo extends AbstractMojo {

  @Parameter(defaultValue = "${project}", readonly = true)
  var project: MavenProject = _

  @Parameter(property = "ci-police.skip", defaultValue = "false")
  var skip: Boolean = _

  @Parameter(property = "isRelease", defaultValue = "false")
  var isRelease: Boolean = _


  @throws[MojoExecutionException]
  @throws[MojoFailureException]
  override def execute(): Unit = {
    val handler = new CiPoliceHandler(
      mavenProject = project,
      isRelease = isRelease,
      log = getLog,
      skip = skip)

    handler.execute()
  }
}
