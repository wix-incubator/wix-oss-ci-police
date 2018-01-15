/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import org.apache.maven.model.IssueManagement
import com.wix.accord.{NullSafeValidator, Validator}
import com.wix.accord.ViolationBuilder._
import com.wix.accord.dsl._
import IssueManagementUrlValidator.validIssueManagementUrl


/** A validator for Maven Project's `issueManagement`, to validate that the `url` is of the correct format
  * (`https://github.com/wix/{project}/issues`), and `system` is `GitHub Issues`.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object IssueManagementValidator {
  implicit val issueManagementValidator = validator[IssueManagement] { issueManagement =>
    issueManagement.getUrl as "url" is validIssueManagementUrl
    issueManagement.getSystem as "system" is equalTo("GitHub Issues")
  }

  val validIssueManagement: Validator[IssueManagement] = issueManagementValidator
}


/** A validator for Maven Project's `issueManagement`'s `url`. It validates the URL is of the format
  * `https://github.com/wix/{project}/issues`.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class IssueManagementUrlValidator extends NullSafeValidator[String] (
  url => url.matches(s"https://github\\.com/wix/.+/issues"),
  _ -> s"must be of format https://github.com/wix/{project}/issues"
)


/** The Companion Object, which introduces readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object IssueManagementUrlValidator {
  val validIssueManagementUrl = new IssueManagementUrlValidator
}
