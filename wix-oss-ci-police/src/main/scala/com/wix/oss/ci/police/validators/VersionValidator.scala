/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import org.apache.maven.project.MavenProject
import com.wix.accord.NullSafeValidator
import com.wix.accord.ViolationBuilder._
import VersionValidator.effectiveVersion


/** A validator for Maven Project's `version`, to validate its format (`''X''.''X''.''X''-SNAPSHOT`).
  * It validates the ''effective'' version, i.e., the one from the project itself, and if not specified, the version
  * of the `parent`.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class VersionValidator extends NullSafeValidator[MavenProject] (
  mvnProj => effectiveVersion(mvnProj).exists(_.matches("""^\d+\.\d+\.\d+-SNAPSHOT$""")),
  _ -> "must be of the form X.X.X-SNAPSHOT"
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object VersionValidator {
  def haveValidVersion = new VersionValidator

  val effectiveVersion: MavenProject => Option[String] = mvnProj => {
    mvnProj.getParentArtifact
    (Option(mvnProj.getVersion), Option(mvnProj.getParent).map(_.getVersion)) match {
      case (None, Some(version))  => Some(version)
      case (verOpt, _)            => verOpt
    }
  }
}