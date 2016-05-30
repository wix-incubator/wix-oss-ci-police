/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import org.apache.maven.model.Scm
import com.wix.accord.{NullSafeValidator, Validator}
import com.wix.accord.dsl._
import com.wix.accord.ViolationBuilder._
import NotBlankValidator.notBlank
import ScmConnectionValidator.validScmConnection
import ScmDeveloperConnectionValidator.validScmDeveloperConnection
import UrlValidator.validUrl


/** A validator for Maven Project's `scm`, to validate that the `url`, `connection` and `developerConnection` are of
  * the correct format, and that `tag` is not blank.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object ScmValidator {
  implicit val scmValidator = validator[Scm] { scm =>
    scm.getUrl as "scm.url" is validUrl()
    scm.getConnection as "scm.connection" is validScmConnection
    scm.getDeveloperConnection as "scm.developerConnection" is validScmDeveloperConnection
    scm.getTag as "scm.tag" is notBlank
  }

  val validScm: Validator[Scm] = scmValidator
}



/** A validator for `connection` element of Maven Project's `scm`, to validate that it is of the correct format.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class ScmConnectionValidator extends NullSafeValidator[String] (
  connection => connection.matches("scm:git:git://github\\.com/wix/.+\\.git"),
  _ -> "must be of the format of scm:git:git://github.com/wix/{project}.git"
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object ScmConnectionValidator {
  def validScmConnection = new ScmConnectionValidator
}



/** A validator for `developerConnection` element of Maven Project's `scm`, to validate that it is of the correct
  * format.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class ScmDeveloperConnectionValidator extends NullSafeValidator[String] (
  devConn => devConn.matches("scm:git:git@github\\.com:wix/.+\\.git"),
  _ -> "must be of the format of scm:git:git@github.com:wix/{project}.git"
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object ScmDeveloperConnectionValidator {
  def validScmDeveloperConnection = new ScmDeveloperConnectionValidator
}
