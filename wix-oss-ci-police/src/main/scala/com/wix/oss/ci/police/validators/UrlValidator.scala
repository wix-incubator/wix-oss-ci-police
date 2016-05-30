/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import com.wix.accord.NullSafeValidator
import com.wix.accord.ViolationBuilder._


/** A validator for Maven Project's `url`, to validate its format (`https://github.com/wix/''XXX''`).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class UrlValidator(suffix: String) extends NullSafeValidator[String] (
  url => url.matches(s"https://github\\.com/wix/.+$suffix"),
  _ -> s"must be of format https://github.com/wix/{project}$suffix"
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object UrlValidator {
  def validUrl(suffix: String = "") = new UrlValidator(suffix)
}