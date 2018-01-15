/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import com.wix.accord.ViolationBuilder._
import com.wix.accord.{Descriptions, NullSafeValidator, RuleViolation}
import com.wix.oss.ci.police.validators.LicenseValidator._
import org.apache.maven.model.License
import org.apache.maven.project.MavenProject

import scala.collection.JavaConversions._


/** A validator for Maven Project's `licenses`, to validate that holds a valid license.
  * A "valid license" is a license whose:
  * -  `name` is `modified BSD License`,
  * - `url` is of format `https://github.com/wix/''{project}''/blob/master/LICENSE.md`, and
  * - `distribution` is `repo`.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class LicenseValidator extends NullSafeValidator[MavenProject] (
  {
    case HasLicenses(licenses) => licenses.exists(validLicense)
    case IsSubModule() => true
    case _ => false
  },
  mavenProject => RuleViolation(
    s"[${
      mavenProject.getModel.getLicenses.map(license =>
        s"'name': '${license.getName}', 'url': '${license.getUrl}', 'distribution': '${license.getDistribution}'")
        .mkString(", ")
    }]",
    "must have a valid License (which 'name' is [modified BSD License], 'distribution' is [repo], and 'url' of format [https://github.com/wix/{project}/blob/master/LICENSE.md]",
    Descriptions.Explicit("licenses"))
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator, plus
  * helper function to validate a License.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object LicenseValidator {
  val haveValidLicense = new LicenseValidator

  val validUrlRegex = "https://raw\\.githubusercontent\\.com/wix/.+/master/LICENSE\\.md"

  def validLicense(license: License): Boolean = {
    license.getName == "modified BSD License" &&
    Option(license.getUrl).exists(_.matches(validUrlRegex)) &&
    license.getDistribution == "repo"
  }
}


/** Extractor Object that extracts the Licenses out of the given Maven Project.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object HasLicenses {
  def unapply(mavenProject: MavenProject): Option[Seq[License]] = {
    val licenses = mavenProject.getModel.getLicenses

    if (licenses.isEmpty) {
      None
    } else {
      Option(licenses)
    }
  }
}
