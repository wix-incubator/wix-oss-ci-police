/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import com.wix.accord.NullSafeValidator
import com.wix.accord.ViolationBuilder._


/** A validator for Maven Project's `groupId`, to validate its format (`com.wix` or `com.wix.''XXX''`).
  * As opposed to Maven requirement that groupId should be set only if different from the `parent`'s, Lifecycle
  * requires that a `groupId` must be set on each module.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class GroupIdValidator extends NullSafeValidator[String] (
  groupId => groupId.matches("""(^com\.wix$)|(^com\.wix\..+$)"""),
  _ -> "must be specified, and either be 'com.wix', or start with 'com.wix.'"
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object GroupIdValidator {
  def haveValidGroupId = new GroupIdValidator
}
