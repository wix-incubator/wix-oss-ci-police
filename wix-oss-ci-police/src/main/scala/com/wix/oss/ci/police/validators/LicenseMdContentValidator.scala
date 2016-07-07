/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import scala.io.Source
import java.nio.file.Files
import org.apache.maven.project.MavenProject
import com.wix.accord.{NullSafeValidator, RuleViolation}
import com.wix.accord.ViolationBuilder._
import LicenseMdContentValidator.{hasValidContent, hasLicenseMdFile}


/** A validator for Maven Project's `LICENSE.md` content, to validate it is of the certified version.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class LicenseMdContentValidator(licenseMdContentProvider: LicenseMdContentProvider) extends NullSafeValidator[MavenProject] (
  {
    case mavenProject if hasLicenseMdFile(licenseMdContentProvider, mavenProject) =>
      hasValidContent(licenseMdContentProvider, mavenProject)

    case IsSubModule() =>
      true

    case _ =>
      false
  },
  {
    case mavenProject if hasLicenseMdFile(licenseMdContentProvider, mavenProject) =>
      RuleViolation(
        licenseMdContentProvider.getLicenseMdContent(mavenProject),
        "is not the certified content of LICENSE.md file",
        None)
    case _ => RuleViolation(
      null,
      "LICENSE.md file is missing in project's root directory",
      None)
  }
)


/** The Companion Object, which introduces readable alternatives for instantiating the validator, plus constants and
  * helper methods for querying the content.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object LicenseMdContentValidator {
  val validLicenseMdContent =
    s"""Copyright (c) 2015, Wix.com Ltd.
        |All rights reserved.
        |
        |Redistribution and use in source and binary forms, with or without
        |modification, are permitted provided that the following conditions are met:
        |
        |* Redistributions of source code must retain the above copyright notice, this
        |  list of conditions and the following disclaimer.
        |
        |* Redistributions in binary form must reproduce the above copyright notice,
        |  this list of conditions and the following disclaimer in the documentation
        |  and/or other materials provided with the distribution.
        |
        |* Neither the name of Wix.com Ltd. nor the names of its
        |  contributors may be used to endorse or promote products derived from
        |  this software without specific prior written permission.
        |
        |THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
        |AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
        |IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
        |DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
        |FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
        |DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
        |SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
        |CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
        |OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
        |OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
     """.stripMargin
  val haveLicenseMdFileWithCertifiedContent: LicenseMdContentProvider => LicenseMdContentValidator =
    licenseMdContentProvider => new LicenseMdContentValidator(licenseMdContentProvider)


  def hasValidContent(licenseMdContentProvider: LicenseMdContentProvider, mavenProject: MavenProject): Boolean = {
    licenseMdContentProvider.getLicenseMdContent(mavenProject).contains(validLicenseMdContent)
  }

  def hasLicenseMdFile(licenseMdContentProvider: LicenseMdContentProvider, mavenProject: MavenProject): Boolean = {
    licenseMdContentProvider.getLicenseMdContent(mavenProject).isDefined
  }
}


/** A trait defining the operation for getting the content of LICENSE.md.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
trait LicenseMdContentProvider {
  def getLicenseMdContent(mavenProject: MavenProject): Option[String]
}


/** A concrete implementation of the [[LicenseMdContentProvider]] trait, which relies of the file system, to read the
  * content of LICENSE.md file.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class FileBasedLicenseMdContentProvider extends LicenseMdContentProvider {
  override def getLicenseMdContent(mavenProject: MavenProject): Option[String] = {
    val licenseMdPath = mavenProject.getBasedir.toPath.resolve("LICENSE.md")

    if (Files.exists(licenseMdPath)) {
      Option(Source.fromFile(licenseMdPath.toString).getLines().mkString("\n"))
    } else {
      None
    }
  }
}
