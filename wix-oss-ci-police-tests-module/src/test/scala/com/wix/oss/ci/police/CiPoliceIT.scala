/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police


import scala.collection.JavaConversions._
import scala.io.Source
import scala.xml.{Elem, Node, NodeSeq, XML}
import scala.xml.transform.{RewriteRule, RuleTransformer}
import java.io._
import java.nio.file._
import org.specs2.matcher.Matcher
import org.specs2.matcher.Matchers._
import org.specs2.mutable.{BeforeAfter, SpecWithJUnit}
import org.specs2.specification.{BeforeAfterAll, Scope}
import com.wix.oss.ci.police.maven.execution.listener.ITLoggingExecutionEventListener
import com.wix.oss.ci.police.mojos.CiPoliceMojo
import com.wix.oss.ci.police.test.FilesSupport._
import com.wix.oss.ci.police.test.MavenResult
import com.wix.oss.ci.police.test.MavenSupport._
import CiPoliceIT.{buildFailureLogMessage, buildSuccessLogMessage}


/** The IT of the OSS CI Police.
  * It runs Maven against valid and invalid POMs, asserting success or failure based on the Maven output.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class CiPoliceIT extends SpecWithJUnit with BeforeAfterAll {

  val invalidUrl = "https://github.com/wixxiw/some-test-project"
  val tmpReleaseVersionProjectDir = Files.createTempDirectory("generated_ci_police_directory_")


  override def beforeAll: Unit = createReleaseLikeInstallation(tmpReleaseVersionProjectDir)

  override def afterAll: Unit = deleteDirectory(tmpReleaseVersionProjectDir)


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

  def simulateGitProjectDirectory(projectDir: Path): Unit = {
    val gitDir = Files.createDirectory(projectDir.resolve(".git"))

    Files.createDirectory(gitDir.resolve("objects"))
    Files.createDirectory(gitDir.resolve("refs"))

    Files.write(
      gitDir.resolve("HEAD"),
      "ref: refs/heads/master".getBytes("UTF-8"))
  }

  def createReleaseLikePom(inDirectory: Path,
                           basedOn: Path): Unit = {
    val origPom = XML.loadFile(basedOn.toFile)
    val topLevelPom = XML.loadFile(basedOn.getParent.getParent.resolve("pom.xml").toFile)
    val topLevelPomParentElement = topLevelPom \\ "parent"
    val origVersionElement = (origPom \\ "project" \ "version").head
    val replaceParentElement = new RewriteRule {
      override def transform(node: Node): NodeSeq = {
        node match {
          case elem: Elem if elem.label == "parent" => topLevelPomParentElement
          case elem                                 => elem
        }
      }
    }
    val replaceSnapshotVersion = new RewriteRule {
      override def transform(node: Node): NodeSeq = {
        node match {
          case e if e == origVersionElement => <version>{e.text.stripSuffix("-SNAPSHOT")}</version>
          case e                            => e
        }
      }
    }
    val releaseLikePomXML = new RuleTransformer(
      replaceParentElement,
      replaceSnapshotVersion).transform(origPom).head

    XML.save(inDirectory.resolve("pom.xml").toString, releaseLikePomXML)
  }

  def createReleaseLikeInstallation(releaseVersionProjectDir: Path): Unit = {
    val ciPoliceHomeDirectory =
      new File(classOf[CiPoliceMojo].getProtectionDomain.getCodeSource.getLocation.getFile) // classes directory
        .getParentFile // target directory
        .getParentFile // ci-police module directory
        .toPath

    createReleaseLikePom(releaseVersionProjectDir, ciPoliceHomeDirectory.resolve("pom.xml"))
    copyDirectory(
      ciPoliceHomeDirectory.resolve("src").resolve("main"),
      releaseVersionProjectDir.resolve("src").resolve("main"))

    executeMaven(releaseVersionProjectDir.resolve("pom.xml"), "install")
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

    override def before: Unit = simulateGitProjectDirectory(projectDir)

    override def after: Unit = deleteDirectory(projectDir)


    def mavenExecution(pom: Elem, goals: String*)(implicit isRelease: Boolean): MavenResult = {
      val pomFile = projectDir.resolve("pom.xml")

      XML.save(pomFile.toString, pom)

      val result = executeMaven(pomFile, Option(new ITLoggingExecutionEventListener(logFile)), goals: _*)

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
          <packaging>pom</packaging>
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

    "explode Maven execution while printing the errors encountered, if POM validation fails" in new Ctx {
      val pomXml =
        <project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"
                 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                 xmlns="http://maven.apache.org/POM/4.0.0">
          <modelVersion>4.0.0</modelVersion>

          <groupId>com.wix</groupId>
          <artifactId>wix-os-something</artifactId>
          <version>1.0.0-SNAPSHOT</version>
          <packaging>pom</packaging>
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
          <packaging>pom</packaging>
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

    "explode Maven execution while printing the errors encountered, if POM validation fails" in new Ctx {
      val pomXml =
        <project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"
                 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                 xmlns="http://maven.apache.org/POM/4.0.0">
          <modelVersion>4.0.0</modelVersion>

          <groupId>com.wix</groupId>
          <artifactId>wix-os-something</artifactId>
          <version>1.0.0-SNAPSHOT</version>
          <packaging>pom</packaging>
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
