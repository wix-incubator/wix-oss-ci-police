/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import org.apache.maven.model.IssueManagement
import com.wix.accord.Validator
import com.wix.accord.dsl._
import UrlValidator.validUrl


/** A validator for Maven Project's `issueManagement`, to validate that the `url` is of the correct format
  * (`https://github.com/wix/{project}/issues`), and `system` is `GitHub Issues`.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object IssueManagementValidator {
  implicit val issueManagementValidator = validator[IssueManagement] { issueManagement =>
    issueManagement.getUrl as "issueManagement.url" is validUrl("/issues")
    issueManagement.getSystem as "issueManagement.system" is equalTo("GitHub Issues")
  }

  val validIssueManagement: Validator[IssueManagement] = issueManagementValidator
}
