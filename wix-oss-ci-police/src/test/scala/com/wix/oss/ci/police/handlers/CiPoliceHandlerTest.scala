/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.handlers


import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import java.io.{ByteArrayOutputStream, PrintStream}
import javax.xml.bind.ValidationException
import org.apache.maven.plugin.logging.Log
import org.specs2.matcher.{Expectable, Matcher}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import com.wix.oss.ci.police.test.MavenElementsBuilder._


/** The Unit-Test for the [[CiPoliceHandler]] class.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class CiPoliceHandlerTest extends SpecWithJUnit {

  val regexEscape:String => String = in => {
    in.replaceAll("\\.", "\\\\.")
  }


  trait Ctx extends Scope {
    val infoBuffer = new ListBuffer[String]
    val warnBuffer = new ListBuffer[String]
    val errorBuffer = new ListBuffer[String]
    val debugBuffer = new ListBuffer[String]
    val log = new BufferLog(
      debugBuffer = debugBuffer,
      infoBuffer = infoBuffer,
      warnBuffer = warnBuffer,
      errorBuffer = errorBuffer)


    def succeedWithInfoRecords(records: String*): Matcher[Unit] = {
      not(throwA[Throwable]) and
        haveInfoRecords(records: _*) ^^ { (_: Unit) => log }
    }

    def failWithErrors(records: String*): Matcher[Unit] = {
      throwA[ValidationException] and
        haveErrorRecords(records: _*) ^^ { (_: Unit) => log }
    }

    def haveInfoRecords(records: String*): Matcher[Log] = {
      new Matcher[Log] {
        def apply[L <: Log](l: Expectable[L]) = {
          result(
            records.forall(record => infoBuffer.exists(_.matches(record))),
            s"${l.description} info record(s) [${records mkString ", "}] was (were) written",
            s"${l.description} info record(s) ['${records mkString ", "}] was (were) not written",
            l
          )
        }
      }
    }

    def haveErrorRecords(records: String*): Matcher[Log] = {
      new Matcher[Log] {
        def apply[L <: Log](l: Expectable[L]) = {
          result(
            records.forall(record => errorBuffer.exists(_.matches(record))),
            s"${l.description} error record(s) ['${records mkString ", "}] was (were) written",
            s"${l.description} error record(s) ['${records mkString ", "}] was (were) not written",
            l
          )
        }
      }
    }
  }


  "execute" should {
    "do nothing (except for logging), if the 'skip' flag is on" in new Ctx {
      val ciPoliceHandler = new CiPoliceHandler(
        project = mavenProject(),
        log = log,
        skip = true)

      ciPoliceHandler.execute() must succeedWithInfoRecords("""^skip=\[true\]\. Doing nothing\.$""")
    }

    "log a success record, if successfully validated POM" in new Ctx {
      val ciPoliceHandler = new CiPoliceHandler(
        project = mavenProject(),
        log = log,
        skip = false)

      ciPoliceHandler.execute() must succeedWithInfoRecords("^POM validation passed$")
    }

    "log an error record, if POM validation failed" in new Ctx {
      val invalidGroupId = "com.wixxiw"
      val someArtifactId = "wix-some-project"
      val someVersion = "3.33.333-SNAPSHOT"
      val invalidUrl = "https://some.invalid.url"
      val ciPoliceHandler = new CiPoliceHandler(
        project = mavenProject(
          groupId = Some(invalidGroupId),
          artifactId = Some(someArtifactId),
          version = Some(someVersion),
          url = Some(invalidUrl)),
        log = log,
        skip = false)

      ciPoliceHandler.execute() must failWithErrors(
        s"^Validation error: groupId \\[${regexEscape(invalidGroupId)}\\] \\(must be specified, and either be 'com\\.wix', or start with 'com\\.wix\\.'\\)$$",
        s"^Validation error: url \\[${regexEscape(invalidUrl)}\\] \\(must be of format https://github\\.com/wix/\\{project\\}\\)$$")
    }
  }
}



/** A [[Log]] implementation that buffers records.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
case class BufferLog(debugBuffer: mutable.Buffer[String],
                     infoBuffer: mutable.Buffer[String],
                     warnBuffer: mutable.Buffer[String],
                     errorBuffer: mutable.Buffer[String]) extends Log {

  override def warn(content: CharSequence): Unit = warnBuffer += content.toString

  override def warn(throwable: Throwable): Unit = printStackTrace(warnBuffer, throwable)

  override def warn(content: CharSequence, throwable: Throwable): Unit = {
    warn(content)
    warn(throwable)
  }

  override def error(content: CharSequence): Unit = errorBuffer += content.toString

  override def error(throwable: Throwable): Unit = printStackTrace(errorBuffer, throwable)

  override def error(content: CharSequence, throwable: Throwable): Unit = {
    error(content)
    error(throwable)
  }

  override def debug(content: CharSequence): Unit = debugBuffer += content.toString

  override def debug(throwable: Throwable): Unit = printStackTrace(debugBuffer, throwable)

  override def debug(content: CharSequence, throwable: Throwable): Unit = {
    debug(content)
    debug(throwable)
  }

  override def info(content: CharSequence): Unit = infoBuffer += content.toString

  override def info(throwable: Throwable): Unit = printStackTrace(infoBuffer, throwable)

  override def info(content: CharSequence, throwable: Throwable): Unit = {
    info(content)
    info(throwable)
  }

  override def isErrorEnabled: Boolean = true

  override def isInfoEnabled: Boolean = true

  override def isDebugEnabled: Boolean = true

  override def isWarnEnabled: Boolean = true

  private def printStackTrace(buff: mutable.Buffer[String], error: Throwable) = {
    val outStream = new ByteArrayOutputStream
    val printer = new PrintStream(outStream)

    error.printStackTrace(printer)
    val stackTrace = new String(outStream.toByteArray, "UTF-8")

    outStream.close()
    printer.close()

    buff += stackTrace
  }
}
