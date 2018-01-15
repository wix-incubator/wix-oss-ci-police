/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import com.wix.accord.Descriptions.Description
import com.wix.accord.ViolationBuilder._
import com.wix.accord.{Descriptions, NullSafeValidator, RuleViolation, Validator}
import com.wix.oss.ci.police.validators.ScmValidator._
import org.apache.maven.model.Scm
import org.apache.maven.project.MavenProject


/** A validator for Maven Project's `scm`. It validates that the `url`, `connection` and `developerConnection` are of
  * the correct format, and that `tag` is not blank.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class ScmValidator extends NullSafeValidator[MavenProject] (
  {
    case HasScm(scm) =>
      Option(scm.getUrl).exists(_.matches(validUrlRegex)) &&
      Option(scm.getConnection).exists(_.matches(validConnectionRegex)) &&
      Option(scm.getDeveloperConnection).exists(_.matches(validDeveloperConnectionRegex)) &&
      !isBlank(scm.getTag)

    case IsSubModule() =>
      true

    case _ =>
      false
  },
  mvnProject => singleViolationToFailure(Option(mvnProject.getScm).map(scm =>
    RuleViolation(
      getInvalidValue(scm),
      getViolatedConstraint(scm),
      getViolationDescription(scm)))
    .getOrElse(RuleViolation(
      null,
      "SCM must not be null",
      Descriptions.Explicit("scm"))))
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator, plus constants and
  * helper methods for querying the violation.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object ScmValidator {
  val validUrlRegex = "https://github\\.com/wix/.+"
  val validConnectionRegex = "scm:git:git://github\\.com/wix/.+\\.git"
  val validDeveloperConnectionRegex = "scm:git:git@github\\.com:wix/.+\\.git"
  val isBlank: String => Boolean = value => Option(value).exists(_.trim == "")
  val haveValidScm: Validator[MavenProject] = new ScmValidator

  def getInvalidValue(scm: Scm): String = {
    scm match {
      case BlankUrl() | BlankConnection() | BlankDeveloperConnection() | BlankTag() => ""
      case HasInvalidUrl(url)                                                       => url
      case HasInvalidConnection(connection)                                         => connection
      case HasInvalidDeveloperConnection(developerConnection)                       => developerConnection
    }
  }

  def getViolatedConstraint(scm: Scm): String = {
    scm match {
      case BlankUrl() | BlankConnection() | BlankDeveloperConnection() | BlankTag() =>
        "must not be blank"

      case HasInvalidUrl(url) =>
        "must be of format https://github.com/wix/{project}"

      case HasInvalidConnection(connection) =>
        "must be of the format of scm:git:git://github.com/wix/{project}.git"

      case HasInvalidDeveloperConnection(developerConnection) =>
        "must be of the format of scm:git:git@github.com:wix/{project}.git"
    }
  }

  def getViolationDescription(scm: Scm): Description = {
    scm match {
      case BlankUrl() | HasInvalidUrl(_)                                 => Descriptions.Explicit("scm.url")
      case BlankConnection() | HasInvalidConnection(_)                   => Descriptions.Explicit("scm.connection")
      case BlankDeveloperConnection() | HasInvalidDeveloperConnection(_) => Descriptions.Explicit("scm.developerConnection")
      case BlankTag()                                                    => Descriptions.Explicit("scm.tag")
      case _                                                             => Descriptions.Explicit("")
    }
  }

  def getInvalidValue(value: String, validRegex: String): Option[String] = {
    if (value.matches(validRegex)) {
      None
    } else {
      Some(value)
    }
  }
}


/** Extractor Object that extracts SCM object out of the given Maven Project.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object HasScm {
  def unapply(mavenProject: MavenProject): Option[Scm] = {
    Option(mavenProject.getScm)
  }
}


/** Extractor Object that indicates whether the SCM's `url` is blank (missing or with blank value).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object BlankUrl {
  def unapply(scm: Scm): Boolean = {
    isBlank(scm.getUrl)
  }
}


/** Extractor Object that indicates whether the SCM's `connection` is blank (missing or with blank value).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object BlankConnection {
  def unapply(scm: Scm): Boolean = {
    isBlank(scm.getConnection)
  }
}


/** Extractor Object that indicates whether the SCM's `developerConnection` is blank (missing or with blank value).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object BlankDeveloperConnection {
  def unapply(scm: Scm): Boolean = {
    isBlank(scm.getDeveloperConnection)
  }
}


/** Extractor Object that indicates whether the SCM's `tag` is blank (missing or with blank value).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object BlankTag {
  def unapply(scm: Scm): Boolean = {
    isBlank(scm.getTag)
  }
}


/** Extractor Object that extracts the (invalid) SCM's `url` (i.e., not of the format
  * `https://github.com/wix/{project}`).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object HasInvalidUrl {
  def unapply(scm: Scm): Option[String] = {
    getInvalidValue(scm.getUrl, validUrlRegex)
  }
}


/** Extractor Object that extracts the (invalid) SCM's `connection` (i.e., not of the format
  * `scm:git:git://github.com/wix/{project}.git`).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object HasInvalidConnection {
  def unapply(scm: Scm): Option[String] = {
    getInvalidValue(scm.getConnection, validConnectionRegex)
  }
}


/** Extractor Object that extracts the (invalid) SCM's `developerConnection` (i.e., not of the format
  * `scm:git:git@github.com:wix/{project}.git`).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object HasInvalidDeveloperConnection {
  def unapply(scm: Scm): Option[String] = {
    getInvalidValue(scm.getDeveloperConnection, validDeveloperConnectionRegex)
  }
}
