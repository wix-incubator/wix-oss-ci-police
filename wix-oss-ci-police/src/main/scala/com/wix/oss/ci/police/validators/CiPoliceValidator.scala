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
  implicit val mavenProjectValidator = validator[(MavenProject, Boolean)] { mvnProjIsReleaseTuple =>
    mvnProjIsReleaseTuple._1.getGroupId as "groupId" should haveValidGroupId
    mvnProjIsReleaseTuple._1.getArtifactId as "artifactId" is notBlank
    mvnProjIsReleaseTuple._1 as "effective version" should haveValidVersion(mvnProjIsReleaseTuple._2)
    mvnProjIsReleaseTuple._1.getUrl as "url" is validUrl()
    mvnProjIsReleaseTuple._1.getModel.getName as "name" is notBlank
    mvnProjIsReleaseTuple._1.getDescription as "description" is notBlank
    mvnProjIsReleaseTuple._1.getOrganization as "organization" is validOrganization
    mvnProjIsReleaseTuple._1.getModel.getDevelopers as "developers" should haveValidWixDomainEmails
    nullSafe(mvnProjIsReleaseTuple._1.getScm as "scm" is validScm, "SCM")
    nullSafe(
      mvnProjIsReleaseTuple._1.getIssueManagement as "issueManagement" is validIssueManagement,
      "Issue Management")
    mvnProjIsReleaseTuple._1.getModel.getLicenses as "licenses" should haveValidLicense
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
