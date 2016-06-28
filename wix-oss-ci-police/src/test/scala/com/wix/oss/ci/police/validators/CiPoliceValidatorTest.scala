/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.validators


import scala.collection.JavaConversions._
import java.util.{List => JList}
import org.apache.maven.model.{Developer, License}
import org.apache.maven.project.MavenProject
import org.specs2.mutable.SpecWithJUnit
import com.wix.accord.{validate => accordValidate}
import com.wix.accord.specs2.ResultMatchers
import com.wix.oss.ci.police.test.MavenElementsBuilder._
import CiPoliceValidator.mavenProjectValidator


/** The Unit Test class for the [[CiPoliceValidator]] object.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class CiPoliceValidatorTest extends SpecWithJUnit with ResultMatchers {

  val blank = "   "
  val invalidUrl = "https://github.com/wixxiw/kuki-buki"
  val someReleaseFlag = false

  def validate(project: MavenProject, isRelease: Boolean = someReleaseFlag) = accordValidate(project -> isRelease)

  "Maven Project's 'groupId'" should {
    "be accepted, if equals to 'com.wix'" in {
      val mvnProject = mavenProject(
          groupId = Some("com.wix"))

      validate(mvnProject) must succeed
    }

    "be accepted, if starts with 'com.wix.'" in {
      val mvnProject = mavenProject(
        groupId = Some("com.wix.kukibuki"))

      validate(mvnProject) must succeed
    }

    "be rejected, if does not start with 'com.wix.' and not equal to 'com.wix'" in {
      val invalidGroupId = "kuki.buki"
      val mvnProject= mavenProject(
        groupId = Some(invalidGroupId))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidGroupId,
        constraint = "must be specified, and either be 'com.wix', or start with 'com.wix.'",
        description = "groupId"))
    }

    "be rejected if missing, and no 'groupId' is specified in the 'parent'" in {
      val mvnProject = mavenProject(
        groupId = None,
        parent = Some(mavenParent(groupId = None)))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "is a null",
        description = "groupId"))
    }
  }


  "Maven Project's 'artifactId'" should {
    "be accepted, if not blank" in {
      val mvnProject = mavenProject(
        artifactId = Some("some-artifact-id"))

      validate(mvnProject) must succeed
    }

    "be rejected, if missing" in {
      val mvnProject = mavenProject(
        artifactId = None)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "is a null",
        description = "artifactId"))
    }

    "be rejected, if blank" in {
      val mvnProject = mavenProject(
        artifactId = Some(blank))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = blank,
        constraint = "must not be blank",
        description = "artifactId"))
    }
  }


  "Maven Project's 'version', in a non-release execution" should {
    "be accepted, if satisfies the format of '{number}.{number}.{number}-SNAPSHOT'" in {
      val mvnProject = mavenProject(
          version = Some("3.33.333-SNAPSHOT"))

      validate(mvnProject, isRelease = false) must succeed
    }

    "be accepted if missing, but a 'parent' is specified whose 'version' satisfies the format of '{number}.{number}.{number}-SNAPSHOT'" in {
      val mvnProject = mavenProject(
          version = None,
          parent = Some(mavenParent(version = Some("3.33.333-SNAPSHOT"))))

      validate(mvnProject, isRelease = false) must succeed
    }

    "be rejected, if does not satisfy the format of '{number}.{number}.{number}-SNAPSHOT'" in {
      val invalidVersion = "kuki.buki.333-SNAPSHOT"
      val mvnProject = mavenProject(
          version = Some(invalidVersion))

      validate(mvnProject, isRelease = false) must failWith(RuleViolationMatcher(
        value = mvnProject,
        constraint = "must be of the form X.X.X-SNAPSHOT for non-release execution",
        description = "effective version"))
    }

    "be rejected if missing 'version', and no 'version' is specified in the 'parent'" in {
      val mvnProject = mavenProject(
          version = None,
          parent = Some(mavenParent(version = None)))

      validate(mvnProject, isRelease = false) must failWith(RuleViolationMatcher(
        value = mvnProject,
        constraint = "must be of the form X.X.X-SNAPSHOT for non-release execution",
        description = "effective version"))
    }

    "be rejected if missing, and the 'version' specified in the 'parent' does not satisfy the format of '{number}.{number}.{number}-SNAPSHOT'" in {
      val mvnProject = mavenProject(
          version = None,
          parent = Some(mavenParent(version = Some("1.0-SNAPSHOT"))))

      validate(mvnProject, isRelease = false) must failWith(RuleViolationMatcher(
        value = mvnProject,
        constraint = "must be of the form X.X.X-SNAPSHOT for non-release execution",
        description = "effective version"))
    }
  }


  "Maven Project's 'version', in a release execution" should {
    "be accepted, if satisfies the format of '{number}.{number}.{number}'" in {
      val mvnProject = mavenProject(
          version = Some("3.33.333"))

      validate(mvnProject, isRelease = true) must succeed
    }

    "be accepted if missing, but a 'parent' is specified whose 'version' satisfies the format of '{number}.{number}.{number}'" in {
      val mvnProject = mavenProject(
          version = None,
          parent = Some(mavenParent(version = Some("3.33.333"))))

      validate(mvnProject, isRelease = true) must succeed
    }

    "be rejected, if does not satisfy the format of '{number}.{number}.{number}'" in {
      val invalidVersion = "kuki.buki.333"
      val mvnProject = mavenProject(
          version = Some(invalidVersion))

      validate(mvnProject, isRelease = true) must failWith(RuleViolationMatcher(
        value = mvnProject,
        constraint = "must be of the form X.X.X for release execution",
        description = "effective version"))
    }

    "be rejected if missing 'version', and no 'version' is specified in the 'parent'" in {
      val mvnProject = mavenProject(
          version = None,
          parent = Some(mavenParent(version = None)))

      validate(mvnProject, isRelease = true) must failWith(RuleViolationMatcher(
        value = mvnProject,
        constraint = "must be of the form X.X.X for release execution",
        description = "effective version"))
    }

    "be rejected if missing, and the 'version' specified in the 'parent' does not satisfy the format of '{number}.{number}.{number}' in release execution" in {
      val mvnProject = mavenProject(
          version = None,
          parent = Some(mavenParent(version = Some("1.0"))))

      validate(mvnProject, isRelease = true) must failWith(RuleViolationMatcher(
        value = mvnProject,
        constraint = "must be of the form X.X.X for release execution",
        description = "effective version"))
    }
  }


  "Maven Project's 'name'" should {
    "be accepted if not blank" in {
      val mvnProject = mavenProject(
          name = Some("some name"))

      validate(mvnProject) must succeed
    }

    "be rejected if missing" in {
      val mvnProject = mavenProject(
          name = None)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "is a null",
        description = "name"))
    }

    "be rejected if blank" in {
      val mvnProject = mavenProject(
          name = Some(blank))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = blank,
        constraint = "must not be blank",
        description = "name"))
    }
  }


  "Maven Project's 'url'" should {
    "be accepted, if starts with 'https://github.com/wix/'" in {
      val mvnProject = mavenProject(
          url = Some("https://github.com/wix/kuki-buki"))

      validate(mvnProject) must succeed
    }

    "be rejected, if not of the format https://github.com/wix/{project}" in {
      val mvnProject = mavenProject(
          url = Some(invalidUrl))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidUrl,
        constraint = "must be of format https://github.com/wix/{project}",
        description = "url"))
    }
  }


  "Maven Project's 'description'" should {
    "be accepted if not blank" in {
      val mvnProject = mavenProject(
          description = Some("some description"))

      validate(mvnProject) must succeed
    }

    "be rejected if missing" in {
      val mvnProject = mavenProject(
          description = None)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "is a null",
        description = "description"))
    }

    "be rejected if blank" in {
      val mvnProject = mavenProject(
          description = Some(blank))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = blank,
        constraint = "must not be blank",
        description = "description"))
    }
  }


  "Maven Project's 'developers'" should {
    "be accepted, if owners has a valid Wix domain email address" in {
      val mvnProject = mavenProject(
          developers = Seq(
            mavenDeveloper(email = Some("shuki@wiiiiiiiix.com"), roles = Seq("developer")),
            mavenDeveloper(email = Some("kuki@wix.com"), roles = Seq("owner"))))

      validate(mvnProject) must succeed
    }

    "be rejected if there is no owner" in {
      val noOwners = Seq(
        mavenDeveloper(email = Some("kuki.buki@wix.com"), roles = Seq("developer")),
        mavenDeveloper(email = Some("shuki.tuki@wix.com"), roles = Seq("developer")))
      val mvnProject = mavenProject(
          developers = noOwners)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = noOwners: JList[Developer],
        constraint = "owners email must be valid and at wix.com domain",
        description = "developers"))
    }

    "be rejected if the owner's email address is not at Wix domain" in {
      val ownerNonWixDomainAddress = Seq(
        mavenDeveloper(email = Some("suki@wix.com"), roles = Seq("developer")),
        mavenDeveloper(email = Some("kuki@wixxiw.com"), roles = Seq("owner")))
      val mvnProject = mavenProject(
          developers = ownerNonWixDomainAddress)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = ownerNonWixDomainAddress: JList[Developer],
        constraint = "owners email must be valid and at wix.com domain",
        description = "developers"))
    }

    "be rejected if the owner's email address is not valid" in {
      val ownerInvalidEmailAddress = Seq(
        mavenDeveloper(email = Some(".invalid@wix.com"), roles = Seq("owner)")))
      val mvnProject = mavenProject(
          developers = ownerInvalidEmailAddress)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = ownerInvalidEmailAddress: JList[Developer],
        constraint = "owners email must be valid and at wix.com domain",
        description = "developers"))
    }

    "be rejected if missing" in {
      val mvnProject = mavenProject(
          developers = Nil)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = Nil: JList[Developer],
        constraint = "owners email must be valid and at wix.com domain",
        description = "developers"))
    }
  }


  "Maven Project's 'scm'" should {
    "be accepted if: " +
      "*   'url' is of the format https://github.com/wix/{project} " +
      "*   'connection' is of the format scm:git:git://github.com/wix/{project}.git " +
      "*   'developerConnection' is of the format scm:git:git@github.com:wix/{project}.git " +
      "*   'tag' is not blank" in {
      val mvnProject = mavenProject(
          scm = Some(mavenScm(
            url = Some("https://github.com/wix/kuki-buki"),
            connection = Some("scm:git:git://github.com/wix/kuki-buki.git"),
            developerConnection = Some("scm:git:git@github.com:wix/kuki-buki.git"),
            tag = Some("HEAD"))))

      validate(mvnProject) must succeed
    }

    "be rejected if scm is missing" in {
      val mvnProject = mavenProject(
          scm = None)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "SCM must not be null",
        description = "scm"))
    }

    "be rejected if 'url' is not of the format https://github.com/wix/{project}" in {
      val mvnProject = mavenProject(
          scm = Some(mavenScm(url = Some(invalidUrl))))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidUrl,
        constraint = "must be of format https://github.com/wix/{project}",
        description = "scm"))
    }

    "be rejected if 'connection' is not of the format scm:git:git://github.com/wix/{project}.git" in {
      val invalidConnection = "scm:git:git://github.com/wix/kuki-buki"
      val mvnProject = mavenProject(
          scm = Some(mavenScm(connection = Some(invalidConnection))))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidConnection,
        constraint = "must be of the format of scm:git:git://github.com/wix/{project}.git",
        description = "scm"))
    }

    "be rejected if 'developerConnection' is not of the format scm:git:git@github.com:wix/{project}.git" in {
      val invalidDevConnection = "scm:git:git@github.com/wixxiw/kuki-buki.git"
      val mvnProject = mavenProject(
          scm = Some(mavenScm(developerConnection = Some(invalidDevConnection))))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidDevConnection,
        constraint = "must be of the format of scm:git:git@github.com:wix/{project}.git",
        description = "scm"))
    }

    "be rejected if 'tag' is blank" in {
      val mvnProject = mavenProject(
          scm = Some(mavenScm(tag = Some(blank))))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = blank,
        constraint = "must not be blank",
        description = "scm"))
    }
  }


  "Maven Project's 'issueManagement'" should {
    "be accepted if: " +
      "*   'url' is of the format https://github.com/wix/{project}/issues " +
      "*   'system' is GitHub Issues" in {
      val mvnProject = mavenProject(
          issueManagement = Some(mavenIssueManagement(
            url = Some("https://github.com/wix/kuki-buki/issues"),
            system = Some("GitHub Issues"))))

      validate(mvnProject) must succeed
    }

    "be rejected if missing" in {
      val mvnProject = mavenProject(
          issueManagement = None)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "Issue Management must not be null",
        description = "issueManagement"))
    }

    "be rejected if 'url' is not of the format https://github.com/wix/{project}/issues" in {
      val invalidIssuesUrl = s"$invalidUrl/issues"
      val mvnProject = mavenProject(
          issueManagement = Some(mavenIssueManagement(url = Some(invalidIssuesUrl))))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidIssuesUrl,
        constraint = "must be of format https://github.com/wix/{project}/issues",
        description = "issueManagement"))
    }

    "be rejected if 'system' is not GitHub Issues" in {
      val invalidSystem = "not GitHub Issues"
      val mvnProject = mavenProject(
          issueManagement = Some(mavenIssueManagement(system = Some(invalidSystem))))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidSystem,
        constraint = "does not equal GitHub Issues",
        description = "issueManagement"))
    }
  }


  "Maven Project's 'licenses'" should {
    "be accepted if holds at least one license which: " +
      "*   'name' is Apache License" +
      "*   'url' is http://www.apache.org/licenses/LICENSE-2.0.txt" +
      "*   'distribution' is repo" in {
      val mvnProject = mavenProject(
          licenses = Seq(
            mavenLicense(
              name = Some("NOT Apache License"),
              url = Some("http://www.apache.org/licenses/LICENSE-2.0.txt"),
              distribution = Some("repo")),
            mavenLicense(
              name = Some("Apache License"),
              url = Some("NOT http://www.apache.org/licenses/LICENSE-2.0.txt"),
              distribution = Some("repo")),
            mavenLicense(
              name = Some("Apache License"),
              url = Some("http://www.apache.org/licenses/LICENSE-2.0.txt"),
              distribution = Some("NOT repo")),
            mavenLicense(
              name = Some("Apache License, Version 2.0"),
              url = Some("http://www.apache.org/licenses/LICENSE-2.0.txt"),
              distribution = Some("repo"))))

      validate(mvnProject) must succeed
    }

    "be rejected if missing" in {
      val mvnProject = mavenProject(
          licenses = Nil)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = Nil: JList[License],
        constraint = "must have a valid License",
        description = "licenses"))
    }

    "be rejected if 'name' is not Apache License, Version 2.0" in {
      val invalidLicense = Seq(mavenLicense(name = Some("NOT Apache License, Version 2.0")))
      val mvnProject = mavenProject(
          licenses = invalidLicense)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidLicense: JList[License],
        constraint = "must have a valid License",
        description = "licenses"))
    }

    "be rejected if 'url' is not http://www.apache.org/licenses/LICENSE-2.0.txt" in {
      val invalidUrl = Seq(mavenLicense(url = Some("http://www.apache.org/licenses/NOT-LICENSE-2.0.txt")))
      val mvnProject = mavenProject(
          licenses = invalidUrl)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidUrl: JList[License],
        constraint = "must have a valid License",
        description = "licenses"))
    }

    "be rejected if 'distribution' is not repo" in {
      val invalidDistribution = Seq(mavenLicense(distribution = Some("NOT repo")))
      val mvnProject = mavenProject(
          licenses = invalidDistribution)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidDistribution: JList[License],
        constraint = "must have a valid License",
        description = "licenses"))
    }
  }


  "Maven Project's 'organization'" should {
    "be accepted if :" +
      "*   'name' is wix.com" +
      "*   'url' is http://wix.io" in {
      val mvnProject = mavenProject(
          organization = Some(mavenOrganization(
            name = Some("wix.com"),
            url = Some("http://wix.io"))))

      validate(mvnProject) must succeed
    }

    "be rejected if missing" in {
      val mvnProject = mavenProject(
          organization = None)

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "is a null",
        description = "organization"))
    }

    "be rejected if 'name' is not wix.com" in {
      val invalidName = mavenOrganization(name = Some("NOT wix.com"))
      val mvnProject = mavenProject(
          organization = Some(invalidName))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidName,
        constraint = "Organization name must be wix.com and url must be http://wix.io",
        description = "organization"))
    }

    "be rejected if 'url' is not http://wix.io" in {
      val invalidOrganizationUrl = mavenOrganization(url = Some("NOT http://wix.io"))
      val mvnProject = mavenProject(
          organization = Some(invalidOrganizationUrl))

      validate(mvnProject) must failWith(RuleViolationMatcher(
        value = invalidOrganizationUrl,
        constraint = "Organization name must be wix.com and url must be http://wix.io",
        description = "organization"))
    }
  }
}
