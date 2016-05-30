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
import org.specs2.mutable.SpecWithJUnit
import com.wix.accord._
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

  "Maven Project's 'groupId'" should {
    "be accepted, if equals to 'com.wix'" in {
      val pom = mavenProject(
        groupId = Some("com.wix"))

      validate(pom) must succeed
    }

    "be accepted, if starts with 'com.wix.'" in {
      val pom = mavenProject(
        groupId = Some("com.wix.kukibuki"))

      validate(pom) must succeed
    }

    "be accepted if missing, but a 'parent' is specified whose 'groupId' of 'com.wix'" in {
      val pom = mavenProject(
        groupId = None,
        parent = Some(mavenParent(groupId = Some("com.wix"))))

      validate(pom) must succeed
    }

    "be accepted if missing, but a 'parent' is specified, whose 'groupId' that starts with 'com.wix.'" in {
      val pom = mavenProject(
        groupId = None,
        parent = Some(mavenParent(groupId = Some("com.wix.parent"))))

      validate(pom) must succeed
    }

    "be rejected, if does not start with 'com.wix.' and not equal to 'com.wix'" in {
      val pom = mavenProject(
        groupId = Some("kuki.buki"))

      validate(pom) must failWith(RuleViolationMatcher(
        value = pom,
        constraint = "does not have to be specified if inherited from parent, but if specified, must be either 'com.wix' or start with 'com.wix.'",
        description = "effective groupId"))
    }

    "be rejected if missing, and a 'parent' is specified whose 'groupId' does not start with 'com.wix.' and is not equal to 'com.wix'" in {
      val pom = mavenProject(
        groupId = None,
        parent = Some(mavenParent(groupId = Some("com.kuki.buki"))))

      validate(pom) must failWith(RuleViolationMatcher(
        value = pom,
        constraint = "does not have to be specified if inherited from parent, but if specified, must be either 'com.wix' or start with 'com.wix.'",
        description = "effective groupId"))
    }

    "be rejected if missing, and no 'groupId' is specified in the 'parent'" in {
      val pom = mavenProject(
        groupId = None,
        parent = Some(mavenParent(groupId = None)))

      validate(pom) must failWith(RuleViolationMatcher(
        value = pom,
        constraint = "does not have to be specified if inherited from parent, but if specified, must be either 'com.wix' or start with 'com.wix.'",
        description = "effective groupId"))
    }
  }


  "Maven Project's 'artifactId'" should {
    "be accepted, if not blank" in {
      val pom = mavenProject(
        artifactId = Some("some-artifact-id"))

      validate(pom) must succeed
    }

    "be rejected, if missing" in {
      val pom = mavenProject(
        artifactId = None)

      validate(pom) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "is a null",
        description = "artifactId"))
    }

    "be rejected, if blank" in {
      val pom = mavenProject(
        artifactId = Some(blank))

      validate(pom) must failWith(RuleViolationMatcher(
        value = blank,
        constraint = "must not be blank",
        description = "artifactId"))
    }
  }


  "Maven Project's 'version'" should {
    "be accepted, if satisfies the format of '{number}.{number}.{number}-SNAPSHOT'" in {
      val pom = mavenProject(
        version = Some("3.33.333-SNAPSHOT"))

      validate(pom) must succeed
    }

    "be accepted if missing, but a 'parent' is specified whose 'version' satisfies the format of '{number}.{number}.{number}-SNAPSHOT'" in {
      val pom = mavenProject(
        version = None,
        parent = Some(mavenParent(version = Some("3.33.333-SNAPSHOT"))))

      validate(pom) must succeed
    }

    "be rejected, if does not satisfy the format of '{number}.{number}.{number}-SNAPSHOT'" in {
      val invalidVersion = "kuki.buki.333-SNAPSHOT"
      val pom = mavenProject(
        version = Some(invalidVersion))

      validate(pom) must failWith(RuleViolationMatcher(
        value = pom,
        constraint = "must be of the form X.X.X-SNAPSHOT",
        description = "effective version"))
    }

    "be rejected if missing 'version', and no 'version' is specified in the 'parent'" in {
      val pom = mavenProject(
        version = None,
        parent = Some(mavenParent(version = None)))

      validate(pom) must failWith(RuleViolationMatcher(
        value = pom,
        constraint = "must be of the form X.X.X-SNAPSHOT",
        description = "effective version"))
    }

    "be rejected if missing, and the 'version' specified in the 'parent' does not satisfy the format of '{number}.{number}.{number}-SNAPSHOT'" in {
      val pom = mavenProject(
        version = None,
        parent = Some(mavenParent(version = Some("1.0-SNAPSHOT"))))

      validate(pom) must failWith(RuleViolationMatcher(
        value = pom,
        constraint = "must be of the form X.X.X-SNAPSHOT",
        description = "effective version"))
    }
  }


  "Maven Project's 'name'" should {
    "be accepted if not blank" in {
      val pom = mavenProject(
        name = Some("some name"))

      validate(pom) must succeed
    }

    "be rejected if missing" in {
      val pom = mavenProject(
        name = None)

      validate(pom) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "is a null",
        description = "name"))
    }

    "be rejected if blank" in {
      val pom = mavenProject(
        name = Some(blank))

      validate(pom) must failWith(RuleViolationMatcher(
        value = blank,
        constraint = "must not be blank",
        description = "name"))
    }
  }


  "Maven Project's 'url'" should {
    "be accepted, if starts with 'https://github.com/wix/'" in {
      val pom = mavenProject(
        url = Some("https://github.com/wix/kuki-buki"))

      validate(pom) must succeed
    }

    "be rejected, if not of the format https://github.com/wix/{project}" in {
      val pom = mavenProject(
        url = Some(invalidUrl))

      validate(pom) must failWith(RuleViolationMatcher(
        value = invalidUrl,
        constraint = "must be of format https://github.com/wix/{project}",
        description = "url"))
    }
  }


  "Maven Project's 'description'" should {
    "be accepted if not blank" in {
      val pom = mavenProject(
        description = Some("some description"))

      validate(pom) must succeed
    }

    "be rejected if missing" in {
      val pom = mavenProject(
        description = None)

      validate(pom) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "is a null",
        description = "description"))
    }

    "be rejected if blank" in {
      val pom = mavenProject(
        description = Some(blank))

      validate(pom) must failWith(RuleViolationMatcher(
        value = blank,
        constraint = "must not be blank",
        description = "description"))
    }
  }


  "Maven Project's 'developers'" should {
    "be accepted, if owners has a valid Wix domain email address" in {
      val pom = mavenProject(
        developers = Seq(
          mavenDeveloper(email = Some("shuki@wiiiiiiiix.com"), roles = Seq("developer")),
          mavenDeveloper(email = Some("kuki@wix.com"), roles = Seq("owner"))))

      validate(pom) must succeed
    }

    "be rejected if there is no owner" in {
      val noOwners = Seq(
        mavenDeveloper(email = Some("kuki.buki@wix.com"), roles = Seq("developer")),
        mavenDeveloper(email = Some("shuki.tuki@wix.com"), roles = Seq("developer")))
      val pom = mavenProject(
        developers = noOwners)

      validate(pom) must failWith(RuleViolationMatcher(
        value = noOwners: JList[Developer],
        constraint = "owners email must be valid and at wix.com domain",
        description = "developers"))
    }

    "be rejected if the owner's email address is not at Wix domain" in {
      val ownerNonWixDomainAddress = Seq(
        mavenDeveloper(email = Some("suki@wix.com"), roles = Seq("developer")),
        mavenDeveloper(email = Some("kuki@wixxiw.com"), roles = Seq("owner")))
      val pom = mavenProject(
        developers = ownerNonWixDomainAddress)

      validate(pom) must failWith(RuleViolationMatcher(
        value = ownerNonWixDomainAddress: JList[Developer],
        constraint = "owners email must be valid and at wix.com domain",
        description = "developers"))
    }

    "be rejected if the owner's email address is not valid" in {
      val ownerInvalidEmailAddress = Seq(
        mavenDeveloper(email = Some(".invalid@wix.com"), roles = Seq("owner)")))
      val pom = mavenProject(
        developers = ownerInvalidEmailAddress)

      validate(pom) must failWith(RuleViolationMatcher(
        value = ownerInvalidEmailAddress: JList[Developer],
        constraint = "owners email must be valid and at wix.com domain",
        description = "developers"))
    }

    "be rejected if missing" in {
      val pom = mavenProject(
        developers = Nil)

      validate(pom) must failWith(RuleViolationMatcher(
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
      val pom = mavenProject(
        scm = Some(mavenScm(
          url = Some("https://github.com/wix/kuki-buki"),
          connection = Some("scm:git:git://github.com/wix/kuki-buki.git"),
          developerConnection = Some("scm:git:git@github.com:wix/kuki-buki.git"),
          tag = Some("HEAD"))))

      validate(pom) must succeed
    }

    "be rejected if scm is missing" in {
      val pom = mavenProject(
        scm = None)

      validate(pom) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "SCM must not be null",
        description = "scm"))
    }

    "be rejected if 'url' is not of the format https://github.com/wix/{project}" in {
      val pom = mavenProject(
        scm = Some(mavenScm(url = Some(invalidUrl))))

      validate(pom) must failWith(RuleViolationMatcher(
        value = invalidUrl,
        constraint = "must be of format https://github.com/wix/{project}",
        description = "scm"))
    }

    "be rejected if 'connection' is not of the format scm:git:git://github.com/wix/{project}.git" in {
      val invalidConnection = "scm:git:git://github.com/wix/kuki-buki"
      val pom = mavenProject(
        scm = Some(mavenScm(connection = Some(invalidConnection))))

      validate(pom) must failWith(RuleViolationMatcher(
        value = invalidConnection,
        constraint = "must be of the format of scm:git:git://github.com/wix/{project}.git",
        description = "scm"))
    }

    "be rejected if 'developerConnection' is not of the format scm:git:git@github.com:wix/{project}.git" in {
      val invalidDevConnection = "scm:git:git@github.com/wixxiw/kuki-buki.git"
      val pom = mavenProject(
        scm = Some(mavenScm(developerConnection = Some(invalidDevConnection))))

      validate(pom) must failWith(RuleViolationMatcher(
        value = invalidDevConnection,
        constraint = "must be of the format of scm:git:git@github.com:wix/{project}.git",
        description = "scm"))
    }

    "be rejected if 'tag' is blank" in {
      val pom = mavenProject(
        scm = Some(mavenScm(tag = Some(blank))))

      validate(pom) must failWith(RuleViolationMatcher(
        value = blank,
        constraint = "must not be blank",
        description = "scm"))
    }
  }


  "Maven Project's 'issueManagement'" should {
    "be accepted if: " +
      "*   'url' is of the format https://github.com/wix/{project}/issues " +
      "*   'system' is GitHub Issues" in {
      val pom = mavenProject(
        issueManagement = Some(mavenIssueManagement(
          url = Some("https://github.com/wix/kuki-buki/issues"),
          system = Some("GitHub Issues"))))

      validate(pom) must succeed
    }

    "be rejected if missing" in {
      val pom = mavenProject(
        issueManagement = None)

      validate(pom) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "Issue Management must not be null",
        description = "issueManagement"))
    }

    "be rejected if 'url' is not of the format https://github.com/wix/{project}/issues" in {
      val invalidIssuesUrl = s"$invalidUrl/issues"
      val pom = mavenProject(
        issueManagement = Some(mavenIssueManagement(url = Some(invalidIssuesUrl))))

      validate(pom) must failWith(RuleViolationMatcher(
        value = invalidIssuesUrl,
        constraint = "must be of format https://github.com/wix/{project}/issues",
        description = "issueManagement"))
    }

    "be rejected if 'system' is not GitHub Issues" in {
      val invalidSystem = "not GitHub Issues"
      val pom = mavenProject(
        issueManagement = Some(mavenIssueManagement(system = Some(invalidSystem))))

      validate(pom) must failWith(RuleViolationMatcher(
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
      val pom = mavenProject(
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

      validate(pom) must succeed
    }

    "be rejected if missing" in {
      val pom = mavenProject(
        licenses = Nil)

      validate(pom) must failWith(RuleViolationMatcher(
        value = Nil: JList[License],
        constraint = "must have a valid License",
        description = "licenses"))
    }

    "be rejected if 'name' is not Apache License, Version 2.0" in {
      val invalidLicense = Seq(mavenLicense(name = Some("NOT Apache License, Version 2.0")))
      val pom = mavenProject(
        licenses = invalidLicense)

      validate(pom) must failWith(RuleViolationMatcher(
        value = invalidLicense: JList[License],
        constraint = "must have a valid License",
        description = "licenses"))
    }

    "be rejected if 'url' is not http://www.apache.org/licenses/LICENSE-2.0.txt" in {
      val invalidUrl = Seq(mavenLicense(url = Some("http://www.apache.org/licenses/NOT-LICENSE-2.0.txt")))
      val pom = mavenProject(
        licenses = invalidUrl)

      validate(pom) must failWith(RuleViolationMatcher(
        value = invalidUrl: JList[License],
        constraint = "must have a valid License",
        description = "licenses"))
    }

    "be rejected if 'distribution' is not repo" in {
      val invalidDistribution = Seq(mavenLicense(distribution = Some("NOT repo")))
      val pom = mavenProject(
        licenses = invalidDistribution)

      validate(pom) must failWith(RuleViolationMatcher(
        value = invalidDistribution: JList[License],
        constraint = "must have a valid License",
        description = "licenses"))
    }
  }


  "Maven Project's 'organization'" should {
    "be accepted if :" +
      "*   'name' is wix.com" +
      "*   'url' is http://wix.io" in {
      val pom = mavenProject(
        organization = Some(mavenOrganization(
          name = Some("wix.com"),
          url = Some("http://wix.io"))))

      validate(pom) must succeed
    }

    "be rejected if missing" in {
      val pom = mavenProject(
        organization = None)

      validate(pom) must failWith(RuleViolationMatcher(
        value = null,
        constraint = "is a null",
        description = "organization"))
    }

    "be rejected if 'name' is not wix.com" in {
      val invalidName = mavenOrganization(name = Some("NOT wix.com"))
      val pom = mavenProject(
        organization = Some(invalidName))

      validate(pom) must failWith(RuleViolationMatcher(
        value = invalidName,
        constraint = "Organization name must be wix.com and url must be http://wix.io",
        description = "organization"))
    }

    "be rejected if 'url' is not http://wix.io" in {
      val invalidOrganizationUrl = mavenOrganization(url = Some("NOT http://wix.io"))
      val pom = mavenProject(
        organization = Some(invalidOrganizationUrl))

      validate(pom) must failWith(RuleViolationMatcher(
        value = invalidOrganizationUrl,
        constraint = "Organization name must be wix.com and url must be http://wix.io",
        description = "organization"))
    }
  }
}
