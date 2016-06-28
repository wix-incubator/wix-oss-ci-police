/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police


import org.apache.maven.plugin.MojoFailureException
import com.wix.accord.Violation


/** A [[MojoFailureException]] class, thrown by the CI Police plugin, to specify some violation(s) was/were
  * encountered.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
case class CiPoliceViolationException private(violations: Set[Violation],
                                              message: String,
                                              cause: Throwable) extends MojoFailureException(message, cause)


/** The Companion Object, which introduces alternative means to construct [[CiPoliceViolationException]] instances.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object CiPoliceViolationException {
  private val createMessage: Set[Violation] => String = violations => violations mkString ", "


  def apply(violations: Set[Violation]): CiPoliceViolationException =
    new CiPoliceViolationException(violations, createMessage(violations), null)

  def apply(cause: Throwable): CiPoliceViolationException =
    new CiPoliceViolationException(Set.empty[Violation], Option(cause).map(_.toString).orNull, cause)

  def apply(): CiPoliceViolationException =
    new CiPoliceViolationException(Set.empty[Violation], null, null)
}
