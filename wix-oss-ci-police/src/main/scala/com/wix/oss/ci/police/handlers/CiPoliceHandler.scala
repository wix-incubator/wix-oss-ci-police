/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.handlers


import scala.runtime.BoxedUnit
import java.util.{Collection => JCollection}
import org.apache.maven.plugin.MojoFailureException
import org.apache.maven.plugin.logging.Log
import org.apache.maven.project.MavenProject
import com.wix.accord.{Failure, Success, _}
import com.wix.oss.ci.police.CiPoliceViolationException
import com.wix.oss.ci.police.validators.CiPoliceValidator.mavenProjectValidator


/** The handler of the Open Source Software CI Police Mojo.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class CiPoliceHandler(mavenProject: MavenProject, isRelease: Boolean, log: Log, skip: Boolean) {

  @throws[MojoFailureException]
  def execute(): Unit = {
    if (skip) {
      log.info(s"skip=[$skip]. Doing nothing.")
    } else {
      log.info(s"Identified ${if (isRelease) {"release"} else "development (RC)"} execution")

      validate(mavenProject -> isRelease) match {
        case Success =>
          log.info("POM validation passed")

        case Failure(violations) =>
          for (v <- violations)
            yield log.error(s"Validation error: ${v.description.get} ${v.value match {
              case Missing() => "is missing"
              case e => s"[$e] (${v.constraint})"
            }}")

          throw CiPoliceViolationException(violations)
      }
    }
  }
}


/** An Extractor Object, used for indicating if a value is missing.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object Missing {
  def unapply(v: Any): Boolean = {
    v == null ||
      v.isInstanceOf[BoxedUnit] ||
      (v.isInstanceOf[Traversable[_]] && v.asInstanceOf[Traversable[_]].isEmpty) ||
      (v.isInstanceOf[JCollection[_]] && v.asInstanceOf[JCollection[_]].isEmpty)
  }
}
