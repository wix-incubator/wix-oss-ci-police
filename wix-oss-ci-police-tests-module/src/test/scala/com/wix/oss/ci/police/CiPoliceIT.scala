/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police


import scala.collection.JavaConversions._
import scala.io.Source
import scala.xml.{Elem, XML}
import java.io._
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import java.util.Properties
import org.apache.maven.Maven
import org.apache.maven.execution._
import org.codehaus.plexus.{DefaultContainerConfiguration, DefaultPlexusContainer, PlexusConstants}
import org.specs2.matcher.Matcher
import org.specs2.matcher.Matchers._
import org.specs2.mutable.{BeforeAfter, SpecWithJUnit}
import org.specs2.specification.Scope
import com.wix.oss.ci.police.maven.execution.listener.ITLoggingExecutionEventListener
import com.wix.oss.ci.police.mojos.CiPoliceMojo
import CiPoliceIT.{buildFailureLogMessage, buildSuccessLogMessage}


/** The IT of the OSS CI Police.
  * It runs Maven against valid and invalid POMs, asserting success or failure based on the Maven output.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class CiPoliceIT extends SpecWithJUnit {

  val invalidUrl = "https://github.com/wixxiw/some-test-project"


  def extractCiPoliceVersion(implicit isRelease: Boolean): String = {
    val ciPoliceFile = new File(classOf[CiPoliceMojo].getProtectionDomain.getCodeSource.getLocation.getFile)
    val ciPoliceFilename = if (ciPoliceFile.isFile) {
      ciPoliceFile.getName
    } else {
      ciPoliceFile.getParentFile.listFiles.map(_.getName).filter(file =>
        file.matches(s"wix-oss-ci-police-\\d+\\.\\d+\\.\\d+(-SNAPSHOT)?\\.jar")).head
    }

    val version = ciPoliceFilename.drop("wix-oss-ci-police-".length).dropRight(".jar".length)

    if (isRelease && version.endsWith("-SNAPSHOT")) {
      version.dropRight("-SNAPSHOT".length)
    } else if (!isRelease && !version.endsWith("-SNAPSHOT")) {
      s"$version-SNAPSHOT"
    } else {
      version
    }
  }

  def deleteProjectDirectory(projectDir: Path): Unit = {
    Files.walkFileTree(projectDir, new SimpleFileVisitor[Path]() {
      override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(dir: Path, exception: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    })
  }

  def simulateGitProjectDirectory(projectDir: Path): Unit = {
    val gitDir = Files.createDirectory(projectDir.resolve(".git"))

    Files.createDirectory(gitDir.resolve("objects"))
    Files.createDirectory(gitDir.resolve("refs"))

    Files.write(
      gitDir.resolve("HEAD"),
      "ref: refs/heads/master".getBytes("UTF-8"))
  }


  def completeSuccessfully: Matcher[MavenResult] = {
    ===(0) ^^ { (_: MavenResult).exitCode aka "exit code" } and
      hasNoValidationErrorRecords ^^ { (_: MavenResult).log aka "logged error records" } and
      haveBuildStatusLogMessage(===(buildSuccessLogMessage)) ^^ { (_: MavenResult).log aka
        "BUILD SUCCESS log records" }
  }

  def explodeWith(errorLogs: Matcher[Seq[String]]): Matcher[MavenResult] = {
    be_<(0) ^^ { (_: MavenResult).exitCode aka "exit code" } and
      errorLogs ^^ { (result: MavenResult) =>
        filterValidationErrorRecords(result.log).map(_.drop(8)) aka "errors" } and
      haveBuildStatusLogMessage(===(buildFailureLogMessage)) ^^ { (_: MavenResult).log aka
        "BUILD FAILURE log records" }
  }

  def hasNoValidationErrorRecords: Matcher[Seq[String]] = {
    beEmpty[Seq[String]] ^^ { (log: Seq[String]) => filterValidationErrorRecords(log) aka "logged error messages" }
  }

  def haveBuildStatusLogMessage(statusMessage: Matcher[Seq[String]]): Matcher[Seq[String]] = {
    statusMessage ^^ { (log: Seq[String]) =>
      filterBuildStatusRecords(log) aka "build status" }
  }

  def readFile(filename: Path): Seq[String] = {
    Source.fromFile(filename.toString).getLines().foldLeft(Seq.empty[String]){ _ :+ _ }
  }

  def filterValidationErrorRecords(log: Seq[String]): Seq[String] = {
    log.filter(_.startsWith("[ERROR] Validation error:"))
  }

  def filterBuildStatusRecords(log: Seq[String]): Seq[String] = {
    log.filter(_.startsWith("[INFO]")).takeRight(7).take(3)
  }


  sequential


  trait Ctx extends Scope with BeforeAfter {
    val projectDir = Files.createTempDirectory("generated_pom_")
    val logFile = projectDir.resolve("log.log")
    val plexusContainer = {
      val containerConfig = new DefaultContainerConfiguration()
        .setClassPathScanning(PlexusConstants.SCANNING_INDEX)
        .setAutoWiring(true)
        .setName("maven")
      val container = new DefaultPlexusContainer(containerConfig)

      container
    }

    override def before: Unit = simulateGitProjectDirectory(projectDir)

    override def after: Unit = deleteProjectDirectory(projectDir)


    def mavenExecution(pom: Elem, goals: String*)(implicit isRelease: Boolean): MavenResult = {
      def buildMavenRequest(pomFile: File): MavenExecutionRequest = {
        val userProperties = new Properties()
        val executionRequestPopulator = plexusContainer.lookup(classOf[MavenExecutionRequestPopulator])

        userProperties.setProperty("pushChanges", "false")

        val req = new DefaultMavenExecutionRequest()
          .setPom(pomFile)
          .setBaseDirectory(pomFile.getParentFile)
          .setGoals(goals)
          .setInteractiveMode(false)
          .setExecutionListener(new ITLoggingExecutionEventListener(logFile))
          .setUserProperties(userProperties)

        executionRequestPopulator.populateDefaults(req)
      }

      val pomDir = projectDir.toFile
      val pomFile = new File(pomDir, "pom.xml")
      val maven = plexusContainer.lookup(classOf[Maven])
      val mavenRequest = buildMavenRequest(pomFile)

      XML.save(pomFile.getCanonicalPath, pom)

      val result = maven.execute(mavenRequest)

      MavenResult(exitCode = -result.getExceptions.length, log = readFile(logFile))
    }
  }


  "CI Police, for development (RC) execution" should {
    implicit val isRelease = false
    val ciPoliceVersion = extractCiPoliceVersion

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
                <version>
                  {ciPoliceVersion}
                </version>
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

      mavenExecution(pomXml, "clean", "verify") must completeSuccessfully
    }

    "explodeWith Maven execution, if POM validation fails" in new Ctx {
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

      mavenExecution(pomXml, "clean", "verify") must explodeWith(
        containAllOf(Seq(
          s"Validation error: url [$invalidUrl] (must be of format https://github.com/wix/{project})",
          s"Validation error: organization is missing")))
    }
  }


  "CI Police, for release execution" should {
    implicit val isRelease = true
    val ciPoliceVersion = extractCiPoliceVersion

    "allow Maven release execution to complete successfully, if POM validation passes" in new Ctx {
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
                <artifactId>maven-release-plugin</artifactId>
                <version>2.5.3</version>
                <dependencies>
                  <dependency>
                    <groupId>org.apache.maven.scm</groupId>
                    <artifactId>maven-scm-provider-gitexe</artifactId>
                    <version>1.8.1</version>
                  </dependency>
                </dependencies>
                <configuration>
                  <preparationGoals>clean verify -DisRelease=true --log-file={logFile.toString}</preparationGoals>
                </configuration>
              </plugin>

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

      mavenExecution(pomXml, "release:clean", "release:prepare") must completeSuccessfully
    }

    "explode Maven execution, if POM validation fails" in new Ctx {
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
                <artifactId>maven-release-plugin</artifactId>
                <version>2.5.3</version>
                <dependencies>
                  <dependency>
                    <groupId>org.apache.maven.scm</groupId>
                    <artifactId>maven-scm-provider-gitexe</artifactId>
                    <version>1.8.1</version>
                  </dependency>
                </dependencies>
                <configuration>
                  <preparationGoals>clean verify -DisRelease=true --log-file={logFile.toString}</preparationGoals>
                </configuration>
              </plugin>

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

      mavenExecution(pomXml, "release:clean", "release:prepare") must explodeWith(
        containAllOf(Seq(
          s"Validation error: url [$invalidUrl] (must be of format https://github.com/wix/{project})",
          s"Validation error: organization is missing")))
    }
  }
}


/** The Companion Object, defining the log records of successful execution, and of a failed execution.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object CiPoliceIT {
  val buildSuccessLogMessage = Seq(
    "[INFO] ------------------------------------------------------------------------",
    "[INFO] BUILD SUCCESS",
    "[INFO] ------------------------------------------------------------------------")
  val buildFailureLogMessage = Seq(
    "[INFO] ------------------------------------------------------------------------",
    "[INFO] BUILD FAILURE",
    "[INFO] ------------------------------------------------------------------------")
}


case class Goals(acutalMavenGoals: Seq[String])

object Goals {
  val `clean verify` = new Goals(Seq(
    "clean",
    "verify"))
  val `release:clean release:prepare` = new Goals(Seq(
//    "--batch-mode",
//    "--offline",
    "release:clean",
    "release:prepare" //,
    /*
//    "-DremoteTagging=false",
//    "-DsuppressCommitBeforeTag=true",
    // "-DdryRun=true",
    // "-DignoreSnapshots=true",
//    "-Dtag=ci-police-test-333",
*/

//        "-DpushChanges=false",
//    "-DdevelopmentVersion=33.33.33-SNAPSHOT",
//    "-DreleaseVersion=3.3.3" //,
    /*
      "-DpreparationGoals=clean verify -DisRelease=true"*/))
}

case class MavenResult(exitCode: Int, log: Seq[String])


