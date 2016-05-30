/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.mojos


import org.apache.maven.plugin.AbstractMojo
import org.apache.maven.plugins.annotations.{Mojo, Parameter}
import org.apache.maven.project.MavenProject
import com.wix.oss.ci.police.handlers.CiPoliceHandler


/** The Mojo class, being triggered by Maven to execute the `wix-oss-ci-police` goal.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
@Mojo(name = "wix-oss-ci-police")
class CiPoliceMojo extends AbstractMojo {

  @Parameter(defaultValue = "${project}", readonly = true )
  var project: MavenProject = _

  @Parameter(property = "ci-police.skip", defaultValue = "false")
  var skip: Boolean = _


  override def execute(): Unit = {
    val handler = new CiPoliceHandler(
      project = project,
      log = getLog,
      skip = skip)

    handler.execute()
  }
}
