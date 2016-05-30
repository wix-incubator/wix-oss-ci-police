/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import scala.collection.JavaConversions._
import java.util.{List => JList}
import org.apache.maven.model.Developer
import com.wix.accord.NullSafeValidator
import com.wix.accord.ViolationBuilder._


/** A validator for Maven Project's `developers`, to validate that owner is specified (at least one), and that
  * onwers' email address is at Wix domain.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class OwnersEmailValidator extends NullSafeValidator[JList[Developer]] (
  developers => developers.filter(developer =>
    developer.getRoles.exists(role => role.toLowerCase == "owner")
  ).exists(owner =>
    owner.getEmail.matches(
      "^[A-Za-z0-9[\\Q!#$%&'*+-/=?^_`{|}~\\E]]+\\.?[A-Za-z0-9[\\Q!#$%&'*+-/=?^_`{|}~\\E]]+@wix\\.com$")),
  _ -> "owners email must be valid and at wix.com domain"
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object OwnersEmailValidator {
  def haveValidWixDomainEmails = new OwnersEmailValidator
}
