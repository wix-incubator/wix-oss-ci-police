/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.maven.execution.listener


import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.tools.nsc.io.Path
import scala.util.Try
import java.nio.file.{Path => JPath}
import org.apache.maven.cli.CLIReportingUtils._
import org.apache.maven.cli.event.ExecutionEventLogger
import org.apache.maven.execution._
import com.wix.accord.{GroupViolation, RuleViolation, Violation}
import com.wix.oss.ci.police.CiPoliceViolationException
import com.wix.oss.ci.police.handlers.Missing
import ITLoggingExecutionEventListener.{maxProjectName, mb}



/** This class is basically a hack to a problem where logging stops after the first execution (even if the tests are
  * defined as `sequential`).
  * This hack includes writing to the log file, the very same records as the standard `ExecutionEventListener`
  * ([[ExecutionEventLogger]]) does, without using any logging framework. This is true for development (RC) execution
  * (e.g., `mvn clean install`), where in the case of a release execution (e.g.,
  * `mvn release:prepare release:perform`), it falls back to the default behavior (didn't verify that, but assuming the
  * release plugin takes care of these loggings).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class ITLoggingExecutionEventListener(logFile: JPath)(implicit isRelease: Boolean) extends ExecutionEventLogger {
  val log = Path(logFile.toString).createFile()

  override def projectDiscoveryStarted(event: ExecutionEvent): Unit = {
    if (isRelease)
      super.projectDiscoveryStarted(event)
    else
      log.writeAll("")
  }

  override def sessionEnded(event: ExecutionEvent): Unit = {
    if (isRelease) {
      super.sessionEnded(event)
    } else {
      val session = event.getSession
      val mavenExecutionResult = session.getResult

      logCiPoliceViolationException(mavenExecutionResult)
      session.getProjects.tail.headOption.foreach(_ => logReactorSummary(session))
      logSummary(mavenExecutionResult)
      logStatistics(session.getRequest)

      log.appendAll("[INFO] ------------------------------------------------------------------------\n")
    }
  }

  private def logCiPoliceViolationException(mavenExecutionResult: MavenExecutionResult): Unit = {
    val sessionExceptions = mavenExecutionResult.getExceptions
    val ciPoliceException = sessionExceptions.headOption.flatMap { lifecycleExecutionException =>
      val ciPoliceException = lifecycleExecutionException.getCause
      Try(ciPoliceException.getClass.getMethod("violations")).toOption.map { violationsMember =>
        val violations = castViolationsAcrossClassLoader(violationsMember.invoke(ciPoliceException))

        CiPoliceViolationException(violations)
      }
    }

    ciPoliceException.foreach(excpetion => excpetion.violations.foreach(v =>
      log.appendAll(s"[ERROR] Validation error: ${v.description.get} ${
        v.value match {
          case Missing() => "is missing"
          case e => s"[$e] (${v.constraint})"
        }
      }\n")))
  }

  private def logReactorSummary(session: MavenSession): Unit = {
    val result = session.getResult

    log.appendAll("[INFO] ------------------------------------------------------------------------\n")
    log.appendAll(s"[INFO] Reactor Summary:\n")
    log.appendAll(s"[INFO]\n")

    session.getProjects foreach { project =>
      log.appendAll(s"${s"[INFO] ${project.getName} ".padTo(maxProjectName, ".").mkString} ")

      Option(result.getBuildSummary(project)) match {
        case None =>
          log.appendAll("SKIPPED\n")

        case Some(buildResult) =>
          log.appendAll(s"${
            buildResult match {
              case success: BuildSuccess => "SUCCESS"
              case failure: BuildFailure => "FAILURE"
            }
          } [${formatDuration(buildResult.getTime).reverse.padTo(9, " ").mkString.reverse}]\n")
      }
    }
  }

  private def logSummary(mavenExecutionResult: MavenExecutionResult): Unit = {
    log.appendAll("[INFO] ------------------------------------------------------------------------\n")
    log.appendAll(s"[INFO] BUILD ${mavenExecutionResult.getExceptions.headOption.fold("SUCCESS")(_ => "FAILURE")}\n")
    log.appendAll("[INFO] ------------------------------------------------------------------------\n")
  }

  private def logStatistics(request: MavenExecutionRequest): Unit = {
    val finish = System.currentTimeMillis
    val time = finish - request.getStartTime.getTime
    val wallClock = if (request.getDegreeOfConcurrency > 1) " (Wall Clock)" else ""
    val runtime = Runtime.getRuntime

    log.appendAll(s"[INFO] Total time: ${formatDuration(time)} $wallClock\n")
    log.appendAll(s"[INFO] Finished at: ${formatTimestamp(finish)}\n")

    System.gc()

    log.appendAll(s"[INFO] Final Memory: ${(runtime.totalMemory() - runtime.freeMemory()) / mb}M/${runtime.totalMemory / mb}M\n")
  }

  /** The exceptions in Maven Execution Result are generated by some `ClassRealm` class loader. There are therefore
    * cannot be simply read or casted.
    * The first attempt was to serialize those exceptions and then deserialize them back, in the current class loader.
    * But this had failed ([[Violation]] is polymorphic, and specific types cannot be bound, because they are actually
    * different classes [different class loaders]).
    * No escaping - reflection to the rescue, and... yey!!! back to merry old Java programming style!!! N-O-T
    *
    * @param violations Totally useless [[Set]] of [[Violation]]-s. Defined as [[AnyRef]] because not recognized as a
    *                   [[Set]] (different class loaders).
    *
    * @return A [[Set]] of [[Violation]]-s; a useful [[Set]] of [[Violation]]-s.
    */
  private def castViolationsAcrossClassLoader(violations: AnyRef): Set[Violation] = {
    val iteratorMethod = violations.getClass.getMethod("iterator")
    val iterator = iteratorMethod.invoke(violations)
    val hasNextMethod = iterator.getClass.getMethod("hasNext")
    val nextMethod = iterator.getClass.getMethod("next")
    val violationBuff = mutable.Buffer[Violation]()

    while (hasNextMethod.invoke(iterator).toString.toBoolean) {
      val violation = nextMethod.invoke(iterator)
      val violationClass = violation.getClass
      val valueMember = violationClass.getMethod("value")
      val constraintMember = violationClass.getMethod("constraint")
      val descriptionMember = violationClass.getMethod("description")
      val childrenMemberOpt = Try(violationClass.getClass.getMethod("children")).toOption

      val value = valueMember.invoke(violation)
      val constraint = constraintMember.invoke(violation).asInstanceOf[String]
      val descriptionOpt = descriptionMember.invoke(violation)
      val isEmptyMethod = descriptionOpt.getClass.getMethod("isEmpty")
      val isEmpty = isEmptyMethod.invoke(descriptionOpt).toString.toBoolean
      val description = Option(if (isEmpty) {
        null
      } else {
        val getMethod = descriptionOpt.getClass.getMethod("get")

        getMethod.invoke(descriptionOpt).toString
      })
      val children = childrenMemberOpt.map(child => castViolationsAcrossClassLoader(child.invoke(violation)))

      violationBuff += childrenMemberOpt.fold[Violation](RuleViolation(value, constraint, description)) { _ =>
        GroupViolation(value, constraint, description, children.getOrElse(Set.empty[Violation]))
      }
    }

    violationBuff.toSet
  }
}


/** The companion object, which introduces some constants.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object ITLoggingExecutionEventListener {
  val mb = 1024 * 1024
  val maxProjectName = 52
}
