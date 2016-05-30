/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police


import scala.xml.{Elem, XML}
import java.io.{ByteArrayOutputStream, File, FileFilter, PrintStream}
import java.nio.file.Files
import org.apache.commons.io.filefilter.WildcardFileFilter
import org.apache.maven.cli.MavenCli
import org.specs2.matcher.Matcher
import org.specs2.matcher.Matchers._
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import CiPoliceIT.{buildFailureLogMessage, buildSuccessLogMessage}
import com.wix.oss.ci.police.mojos.CiPoliceMojo


/** The IT of the OSS CI Police.
  * It runs Maven against valid and invalid POMs, asserting success or failure based on the Maven output.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class CiPoliceIT extends SpecWithJUnit {
  val outStream = new ByteArrayOutputStream
  val ciPoliceVersion = {
    val ciPoliceFile = new File(classOf[CiPoliceMojo].getProtectionDomain.getCodeSource.getLocation.getFile)
    val ciPoliceFilename = if (ciPoliceFile.isFile) {
      ciPoliceFile.getName
    } else {
      ciPoliceFile.getParentFile.listFiles(
        new WildcardFileFilter("wix-oss-ci-police-*-SNAPSHOT.jar"): FileFilter).head.getName
    }

    ciPoliceFilename.drop("wix-oss-ci-police-".length).dropRight(".jar".length)
  }


  def hasNoErrorRecords: Matcher[ByteArrayOutputStream] = {
    beNone ^^ { (stream: ByteArrayOutputStream) =>
      val errorLogs = filterConcreteErrorLogs(stream)
      errorLogs.headOption.map(_ => errorLogs.mkString("\n", "\n", "\n")) aka "logged error messages" }
  }

  def haveBuildStatusLogMessage(statusMessage: Matcher[String]): Matcher[ByteArrayOutputStream] = {
    statusMessage ^^ { (stream: ByteArrayOutputStream) =>
      filterBuildStatusRecords(stream).mkString("\n") aka "build status" }
  }

  def stream2Strings(stream: ByteArrayOutputStream): Seq[String] = {
    new String(stream.toByteArray, "UTF-8").split("\n").toSeq
  }

  def filterConcreteErrorLogs(stream: ByteArrayOutputStream): Seq[String] = {
    stream2Strings(stream).filter(_.startsWith("[ERROR] - Validation error:"))
  }

  def filterBuildStatusRecords(stream: ByteArrayOutputStream): Seq[String] = {
    stream2Strings(stream).filter(_.startsWith("[INFO]")).reverse.slice(4, 7)
  }


  sequential


  trait Ctx extends Scope {
    val embeddedMaven = new MavenCli()
    outStream.reset()


    def mavenExecution(pom: Elem): Int = {
      val pomDir = Files.createTempDirectory("generated_pom_").toFile
      val pomFile = new File(pomDir, "pom.xml")

      XML.save(pomFile.getCanonicalPath, pom)

      System.setProperty("maven.multiModuleProjectDirectory", pomDir.getCanonicalPath)
      embeddedMaven.doMain(Array("clean", "verify"), pomDir.getCanonicalPath, new PrintStream(outStream), System.err)
    }

    def completeSuccessfully: Matcher[Int] = {
      hasNoErrorRecords ^^ { (_: Int) => outStream aka "logged error records" } and
        haveBuildStatusLogMessage(===(buildSuccessLogMessage)) ^^ { (_: Int) =>
          outStream aka "BUILD SUCCESS log records" } and
        ===(0) ^^ { (exitCode: Int) => exitCode aka s"exit code" }
    }

    def explodeWith(errorLogs: Matcher[Seq[String]]): Matcher[Int] = {
      errorLogs ^^ { (_: Int) => filterConcreteErrorLogs(outStream).map(_.drop(10)) aka "errors"} and
      haveBuildStatusLogMessage(===(buildFailureLogMessage)) ^^ { (_: Int) =>
        outStream aka "BUILD FAILURE log records" } and
        ===(1) ^^ { (_: Int) aka s"exit code"}
    }
  }


  "CI Police" should {
    "allow Maven execution to complete successfully, if POM validation passes" in new Ctx {
      val pomXml =
        <project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"
                 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                 xmlns="http://maven.apache.org/POM/4.0.0">
          <modelVersion>4.0.0</modelVersion>

          <groupId>com.wix</groupId>
          <artifactId>wix-os-something</artifactId>
          <version>1.0.0-SNAPSHOT</version>
          <description>bla bla bla</description>
          <name>some sexy name</name>
          <url>https://github.com/wix/some-test-project</url>

          <developers>
            <developer>
              <name>kuki buki</name>
              <email>kuki.buki@wix.com</email>
              <roles>
                <role>owner</role>
              </roles>
            </developer>
          </developers>

          <organization>
            <name>wix.com</name>
            <url>http://wix.io</url>
          </organization>

          <scm>
            <url>https://github.com/wix/test-project</url>
            <connection>scm:git:git://github.com/wix/test-project.git</connection>
            <developerConnection>scm:git:git@github.com:wix/test-project.git</developerConnection>
            <tag>HEAD</tag>
          </scm>

          <issueManagement>
            <url>https://github.com/wix/test-project/issues</url>
            <system>GitHub Issues</system>
          </issueManagement>

          <licenses>
            <license>
              <name>Apache License, Version 2.0</name>
              <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
              <distribution>repo</distribution>
            </license>
          </licenses>

          <build>
            <plugins>
              <plugin>
                <groupId>com.wix</groupId>
                <artifactId>wix-oss-ci-police</artifactId>
                <version>{ciPoliceVersion}</version>
                <executions>
                  <execution>
                    <id>wix-oss-ci-police</id>
                    <phase>validate</phase>
                    <goals>
                      <goal>wix-oss-ci-police</goal>
                    </goals>
                  </execution>
                </executions>
              </plugin>
            </plugins>
          </build>
        </project>

      mavenExecution(pomXml) must completeSuccessfully
    }

    "explodeWith Maven execution, if POM validation fails" in new Ctx {
      val invalidUrl = "https://github.com/wixxiw/some-test-project"
      val pomXml =
        <project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"
                 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                 xmlns="http://maven.apache.org/POM/4.0.0">
          <modelVersion>4.0.0</modelVersion>

          <groupId>com.wix</groupId>
          <artifactId>wix-os-something</artifactId>
          <version>1.0.0-SNAPSHOT</version>
          <description>bla bla bla</description>
          <name>some sexy name</name>
          <url>{invalidUrl}</url>

          <developers>
            <developer>
              <name>kuki buki</name>
              <email>kuki.buki@wix.com</email>
              <roles>
                <role>owner</role>
              </roles>
            </developer>
          </developers>

          <scm>
            <url>https://github.com/wix/test-project</url>
            <connection>scm:git:git://github.com/wix/test-project.git</connection>
            <developerConnection>scm:git:git@github.com:wix/test-project.git</developerConnection>
            <tag>HEAD</tag>
          </scm>

          <issueManagement>
            <url>https://github.com/wix/test-project/issues</url>
            <system>GitHub Issues</system>
          </issueManagement>

          <licenses>
            <license>
              <name>Apache License, Version 2.0</name>
              <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
              <distribution>repo</distribution>
            </license>
          </licenses>

          <build>
            <plugins>
              <plugin>
                <groupId>com.wix</groupId>
                <artifactId>wix-oss-ci-police</artifactId>
                <version>{ciPoliceVersion}</version>
                <executions>
                  <execution>
                    <id>wix-oss-ci-police</id>
                    <phase>validate</phase>
                    <goals>
                      <goal>wix-oss-ci-police</goal>
                    </goals>
                  </execution>
                </executions>
              </plugin>
            </plugins>
          </build>
        </project>

      mavenExecution(pomXml) must explodeWith(
        containAllOf(Seq(
          s"Validation error: url [$invalidUrl] (must be of format https://github.com/wix/{project})",
          s"Validation error: organization is missing")))
    }
  }
}

object CiPoliceIT {
  val buildSuccessLogMessage =
    """[INFO] - ------------------------------------------------------------------------
      |[INFO] - BUILD SUCCESS
      |[INFO] - ------------------------------------------------------------------------""".stripMargin
  val buildFailureLogMessage =
    """[INFO] - ------------------------------------------------------------------------
      |[INFO] - BUILD FAILURE
      |[INFO] - ------------------------------------------------------------------------""".stripMargin
}