/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import org.apache.maven.model.Organization
import com.wix.accord.NullSafeValidator
import com.wix.accord.ViolationBuilder._


/** A validator for Maven Project's `organization`, to validate that its `name` is `wix.com`, and `url` is
  * `http://wix.io`.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class OrganizationValidator extends NullSafeValidator[Organization] (
  org =>
    org.getName == "wix.com" && org.getUrl == "http://wix.io",
  _ -> "Organization name must be wix.com and url must be http://wix.io"
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object OrganizationValidator {
  val validOrganization = new OrganizationValidator
}
