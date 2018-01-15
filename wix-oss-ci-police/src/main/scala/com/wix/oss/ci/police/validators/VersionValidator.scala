/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import com.wix.accord.ViolationBuilder._
import com.wix.accord.{NullSafeValidator, RuleViolation}
import com.wix.oss.ci.police.validators.VersionValidator.effectiveVersion
import org.apache.maven.project.MavenProject

import scala.annotation.tailrec


/** A validator for Maven Project's `version`, to validate its format (`''X''.''X''.''X''-SNAPSHOT` for Development
  * (RC), or `''X''.''X''.''X''` for Release).
  * It validates the ''effective'' version, i.e., the one from the project itself, and if not specified, the version
  * of the `parent` (recursively).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class VersionValidator(version: Version) extends NullSafeValidator[MavenProject] (
  mvnProject => effectiveVersion(mvnProject).exists(_.matches(version.validVersionRegex)),
  mvnProject => RuleViolation(
    effectiveVersion(mvnProject),
    s"must be of the form ${version.validVersionFormat} for ${version.executionType} execution")
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator, and a function for
  * getting the version of the `parent` (recursively).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object VersionValidator {
  def haveValidVersion(version: Version) = new VersionValidator(version)

  @tailrec
  val effectiveVersion: MavenProject => Option[String] = mvnProject => {
    (Option(mvnProject.getVersion), Option(mvnProject.getParent)) match {
      case (Some(version), _) => Some(version)
      case (_, Some(parent))  => effectiveVersion(parent)
      case _                  => None
    }
  }
}


/** A version base class. Defines the behavior and properties each extending object should specify.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
sealed abstract class Version {
  def validVersionRegex: String

  def validVersionFormat: String

  def executionType: String
}


/** The Companion Object, which defines two types of ''Version''-s:
  * - Development (RC), and
  * - Release
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object Version {

  /** A Development (RC) version, in which the version format have the "`-SNAPSHOT`" suffix (and so it is reflected
    * by the regular expression).
    *
    * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
    */
  case object Development extends Version {
    override val validVersionRegex = """^\d+\.\d+\.\d+-SNAPSHOT$"""
    override val validVersionFormat = "X.X.X-SNAPSHOT"
    override val executionType = "Development (RC)"
  }

  /** A Release version, in which the version format must not have the "`-SNAPSHOT`" suffix (and so it is reflected
    * by the regular expression).
    *
    * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
    */
  case object Release extends Version {
    override val validVersionRegex = """^\d+\.\d+\.\d+$"""
    override val validVersionFormat = "X.X.X"
    override val executionType = "Release"
  }
}
