/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import org.apache.maven.project.MavenProject
import com.wix.accord.{NullSafeValidator, RuleViolation}
import com.wix.accord.ViolationBuilder._


/** A validator for Maven Project's `url`, to validate its format (`https://github.com/wix/''{project}''`).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class ProjectUrlValidator extends NullSafeValidator[MavenProject] (
  {
    case HasProjectUrl(url) => url.matches("https://github\\.com/wix/.+")
    case IsSubModule()      => true
    case _                  => false
  },
  mvnProject => RuleViolation(
    mvnProject.getUrl,
    "must be of format https://github.com/wix/{project}",
    None)
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object ProjectUrlValidator {
  val haveValidProjectUrl = new ProjectUrlValidator
}


/** An Extractor Object which extracts the `url` from a given `MavenProject`.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object HasProjectUrl {
  def unapply(mavenProject: MavenProject): Option[String] = Option(mavenProject.getUrl)
}
