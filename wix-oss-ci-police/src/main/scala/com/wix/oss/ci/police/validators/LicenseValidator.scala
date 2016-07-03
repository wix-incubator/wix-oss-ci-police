/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import scala.collection.JavaConversions._
import java.util.{List => JList}
import org.apache.maven.model.License
import com.wix.accord.NullSafeValidator
import com.wix.accord.ViolationBuilder._


/** A validator for Maven Project's `licenses`, to validate that holds a license whose `name` is
  * `Apache License, Version 2.0`, `url` is `http://www.apache.org/licenses/LICENSE-2.0.txt`, and `distribution` is
  * `repo`.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class LicenseValidator extends NullSafeValidator[JList[License]] (
  licenses => licenses.exists(license =>
    license.getName == "modified BSD License" &&
    license.getUrl == "https://github.com/wix/wix-oss-parents/wix-oss-superduper-license-certified-by-legal/LICENSE.md" &&
    license.getDistribution == "repo"),
  _ -> "must have a valid License"
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object LicenseValidator {
  val haveValidLicense = new LicenseValidator
}
