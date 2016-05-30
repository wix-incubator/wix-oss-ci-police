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
import com.wix.oss.ci.police.validators.LicenseValidator.haveValidLicense
import com.wix.oss.ci.police.validators.NotBlankValidator.notBlank
import com.wix.oss.ci.police.validators.OrganizationValidator.validOrganization
import com.wix.oss.ci.police.validators.OwnersEmailValidator.haveValidWixDomainEmails
import com.wix.oss.ci.police.validators.ScmValidator.validScm
import com.wix.oss.ci.police.validators.UrlValidator.validUrl
import com.wix.oss.ci.police.validators.VersionValidator.haveValidVersion


/** A validator for a Maven Project.
  * These validation reflect Maven requirements (e.g., non-blank `artifactId`), Sonatype requirements (e.g., the
  * existence of `scm`), and Wix requirements (e.g., `organization` details).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object CiPoliceValidator {
  implicit val mavenProjectValidator = validator[MavenProject] { mvnProj =>
    mvnProj as "effective groupId" should haveValidGroupId
    mvnProj.getArtifactId as "artifactId" is notBlank
    mvnProj as "effective version" should haveValidVersion
    mvnProj.getUrl as "url" is validUrl()
    mvnProj.getModel.getName as "name" is notBlank
    mvnProj.getDescription as "description" is notBlank
    mvnProj.getOrganization as "organization" is validOrganization
    mvnProj.getModel.getDevelopers as "developers" should haveValidWixDomainEmails
    nullSafe(mvnProj.getScm as "scm" is validScm, "SCM")
    nullSafe(mvnProj.getIssueManagement as "issueManagement" is validIssueManagement, "Issue Management")
    mvnProj.getModel.getLicenses as "licenses" should haveValidLicense
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
