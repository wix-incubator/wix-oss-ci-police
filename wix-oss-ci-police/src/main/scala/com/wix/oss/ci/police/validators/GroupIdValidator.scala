/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import org.apache.maven.project.MavenProject
import com.wix.accord.NullSafeValidator
import com.wix.accord.ViolationBuilder._
import GroupIdValidator.effectiveGroupId


/** A validator for Maven Project's `groupId`, to validate its format (`com.wix` or `com.wix.''XXX''`)
  * It validates the ''effective'' group ID, i.e., the one from the project itself, and if not specified, the group ID
  * of the `parent`.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class GroupIdValidator extends NullSafeValidator[MavenProject] (
  mvnProj => effectiveGroupId(mvnProj).exists(_.matches("""(^com\.wix$)|(^com\.wix\..+$)""")),
  _ -> "does not have to be specified if inherited from parent, but if specified, must be either 'com.wix' or start with 'com.wix.'"
)


/** The Companion Object, which introduce readable alternatives for instantiating the validator.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object GroupIdValidator {
  def haveValidGroupId = new GroupIdValidator

  val effectiveGroupId: MavenProject => Option[String] = mvnProj => {
    (Option(mvnProj.getGroupId), Option(mvnProj.getParent).map(_.getGroupId)) match {
      case (None, Some(id)) => Some(id)
      case (idOpt, _)       => idOpt
    }
  }
}
