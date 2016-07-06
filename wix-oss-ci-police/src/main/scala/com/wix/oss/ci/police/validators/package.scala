/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police


import scala.util.Try
import scala.xml._
import java.nio.file.Path
import org.apache.maven.project.MavenProject


/** A package object introducing sub-module helpers.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
package object validators {

  /** An Extractor Object which indicates whether the given `MavenProject` is a sub-module of some other project.
    *
    * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
    */
  object IsSubModule {

    def unapply(mavenProject: MavenProject): Boolean = {
      getParentDirectory(mavenProject).exists { parentDir =>
        val pomFile = parentDir.resolve("pom.xml")

        Try(XML.loadFile(pomFile.toFile)).toOption.exists(pom =>
          moduleByName(pom, mavenProject.getArtifactId))
      }
    }

    def getParentDirectory(mavenProject: MavenProject): Option[Path] = {
      Option(mavenProject.getBasedir).flatMap(dir => Option(dir.getParentFile)).map(_.toPath)
    }

    val getModules: Elem => NodeSeq = pomXml => {
      pomXml \\ "modules" \ "module"
    }

    def moduleByName(pomXml: Elem, moduleName: String): Boolean = {
      getModules(pomXml).exists(module => module.text == moduleName)
    }
  }
}
