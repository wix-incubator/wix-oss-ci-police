/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import org.apache.maven.project.MavenProject
import com.wix.accord.ViolationBuilder._
import com.wix.accord.{Result, Success, Validator}
import com.wix.accord.dsl._
import com.wix.oss.ci.police.validators.GroupIdValidator.haveValidGroupId
import com.wix.oss.ci.police.validators.IssueManagementValidator.validIssueManagement
import com.wix.oss.ci.police.validators.LicenseMdContentValidator.haveLicenseMdFileWithCertifiedContent
import com.wix.oss.ci.police.validators.LicenseValidator.haveValidLicense
import com.wix.oss.ci.police.validators.NotBlankValidator.notBlank
import com.wix.oss.ci.police.validators.OrganizationValidator.validOrganization
import com.wix.oss.ci.police.validators.OwnersEmailValidator.haveValidWixDomainEmails
import com.wix.oss.ci.police.validators.ProjectUrlValidator.haveValidProjectUrl
import com.wix.oss.ci.police.validators.ScmValidator.haveValidScm
import com.wix.oss.ci.police.validators.VersionValidator.haveValidVersion


/** A validator for a Maven Project.
  * These validation reflect Maven requirements (e.g., non-blank `artifactId`), Sonatype requirements (e.g., the
  * existence of `scm`), and Wix requirements (e.g., `organization` details).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object CiPoliceValidator {
  /** Common validator for both Development (RC) and Release executions */
  val commonValidator = validator[MavenProject] { mavenProject =>
    mavenProject.getGroupId as "groupId" should haveValidGroupId
    mavenProject.getArtifactId as "artifactId" is notBlank
    mavenProject.getModel.getName as "name" is notBlank
    mavenProject.getDescription as "description" is notBlank
    mavenProject.getOrganization as "organization" is validOrganization
    mavenProject.getModel.getDevelopers as "developers" should haveValidWixDomainEmails
    nullSafe(
      mavenProject.getIssueManagement as "issueManagement" is validIssueManagement,
      "Issue Management")
  }

  /** Special validator for Release execution only.
    * This should be in ''addition'' to the `commonValidator`.
    */
  val releaseValidator = commonValidator and validator[MavenProject] { mavenProject =>
    mavenProject as "effective version" should haveValidVersion(Version.Release)
  }

  /** Special validator for Development (RC) execution only.
    * This should be in ''addition'' to the `commonValidator`.
    *
    * == NOTE ==
    * The reason for performing these validations in Development (RC) only, and skipping them in Release, is _not_
    * business/product, but rather technical (except for the `version` validation, which is different in Development
    * and Release).
    * The common to all these validations are that they are sub-module aware validations, i.e., should be skipped
    * in the validated project is a sub-module of some other top-level project.
    * There is no way of knowing where the current validated project is a sub-module, as it is a one direction path
    * from the "main" project to its sub-modules). The only way to figure it out is via the file system - go up the
    * files hierarchy, and find a pom file that declares you as its sub-module (in its `modules`).
    * As build of Release doesn't `clone`s the repo (but basically takes the artifact from `libs-snapshot` in
    * Artifactory and puts it in `libs-release`), figuring out whether the current project is a sub-module of another
    * is absolutely impossible. Hence, all these validations are skipped.
    */
  val developmentValidator: LicenseMdContentProvider => Validator[MavenProject] = licenseMdResolver => {
    commonValidator and validator[MavenProject] { mavenProject =>
      mavenProject as "effective version" should haveValidVersion(Version.Development)
      mavenProject as "project url" should haveValidProjectUrl
      mavenProject as "scm" should haveValidScm
      mavenProject as "licenses" should haveValidLicense
      mavenProject as "LICENSE.md" should haveLicenseMdFileWithCertifiedContent(licenseMdResolver)
    }
  }


  def getValidator(isRelease: Boolean, licenseMdContentProvider: LicenseMdContentProvider) = {
    if (isRelease)
      releaseValidator
    else
      developmentValidator(licenseMdContentProvider)
  }

  def nullSafe[T](validator: Validator[T], objectIdentifier: String): Validator[T] = {
    new Validator[T] {
      def apply(value: T): Result = {
        result(value != null, null.asInstanceOf[T] -> s"$objectIdentifier must not be null") match {
          case Success => validator(value)
          case other => other
        }
      }
    }
  }
}
