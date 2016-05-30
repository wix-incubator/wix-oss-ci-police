/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import com.wix.accord.NullSafeValidator
import com.wix.accord.ViolationBuilder._


/** A validator that validates that a given value is not blank, i.e., not `null`, and holds other characters than
  * blank spaces.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class NotBlankValidator extends NullSafeValidator[String](
  !_.trim.isEmpty,
  _ -> "must not be blank"
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object NotBlankValidator {
  def notBlank = new NotBlankValidator
}
