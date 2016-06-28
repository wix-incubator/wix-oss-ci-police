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
class VersionValidator(isRelease: Boolean) extends NullSafeValidator[MavenProject] (
  mvnProject => effectiveVersion(mvnProject).exists(_.matches(
    if (isRelease) {
      """^\d+\.\d+\.\d+$"""
    } else {
      """^\d+\.\d+\.\d+-SNAPSHOT$"""
    })),
  _ -> s"must be of the form X.X.X${if (!isRelease) "-SNAPSHOT" else ""} for ${if (!isRelease) "non-" else ""}release execution"
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object VersionValidator {
  def haveValidVersion(isRelease: Boolean) = new VersionValidator(isRelease)

  val effectiveVersion: MavenProject => Option[String] = mvnProject => {
    (Option(mvnProject.getVersion), Option(mvnProject.getParent).map(_.getVersion)) match {
      case (None, Some(version))  => Some(version)
      case (verOpt, _)            => verOpt
    }
  }
}
