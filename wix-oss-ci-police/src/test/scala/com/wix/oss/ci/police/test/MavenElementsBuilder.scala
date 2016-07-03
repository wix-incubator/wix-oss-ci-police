/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.test


import scala.collection.JavaConversions._
import org.apache.maven.model._
import org.apache.maven.project.MavenProject


/** A utility object for constructing Maven elements (e.g., project, scm).
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object MavenElementsBuilder {

  def mavenProject(artifactId: Option[String] = Some("wix-oss-kuki-buki"),
                   groupId: Option[String] = Some("com.wix"),
                   version: Option[String] = Some("1.0.0-SNAPSHOT"),
                   name: Option[String] = Some("project-name"),
                   description: Option[String] = Some("some description"),
                   url: Option[String] = Some("https://github.com/wix/some-wix-project"),
                   parent: Option[Parent] = Some(mavenParent()),
                   developers: Seq[Developer] = Seq(mavenDeveloper()),
                   organization: Option[Organization] = Some(mavenOrganization()),
                   scm: Option[Scm] = Some(mavenScm()),
                   issueManagement: Option[IssueManagement] = Some(mavenIssueManagement()),
                   licenses: Seq[License] = Seq(mavenLicense())): MavenProject = {
    val model = new Model()

    artifactId      foreach model.setArtifactId
    groupId         foreach model.setGroupId
    version         foreach model.setVersion
    name            foreach model.setName
    description     foreach model.setDescription
    url             foreach model.setUrl
    parent          foreach model.setParent
    organization    foreach model.setOrganization
    scm             foreach model.setScm
    issueManagement foreach model.setIssueManagement

    model.setDevelopers(developers)
    model.setLicenses(licenses)

    new MavenProject(model)
  }

  def mavenParent(artifactId: Option[String] = Some("wix-oss-some-parent"),
                  groupId: Option[String] = Some("com.wix"),
                  version: Option[String] = Some("1.0.0"),
                  relativePath: Option[String] = None): Parent = {
    val parent = new Parent

    artifactId    foreach parent.setArtifactId
    groupId       foreach parent.setGroupId
    version       foreach parent.setVersion
    relativePath  foreach parent.setRelativePath

    parent
  }

  def mavenDeveloper(email: Option[String] = Some("some.one@wix.com"),
                     roles: Seq[String] = Seq("owner")): Developer = {
    val developer = new Developer

    email foreach developer.setEmail

    developer.setRoles(roles)

    developer
  }

  def mavenScm(url: Option[String] = Some("https://github.com/wix/kuki-buki"),
               connection: Option[String] = Some("scm:git:git://github.com/wix/kuki-buki.git"),
               developerConnection: Option[String] = Some("scm:git:git@github.com:wix/kuki-buki.git"),
               tag: Option[String] = Some("HEAD")): Scm = {
    val scm = new Scm()

    url                 foreach scm.setUrl
    connection          foreach scm.setConnection
    developerConnection foreach scm.setDeveloperConnection
    tag                 foreach scm.setTag

    scm
  }

  def mavenIssueManagement(url: Option[String] = Some("https://github.com/wix/kuki-buki/issues"),
                           system: Option[String] = Some("GitHub Issues")): IssueManagement = {
    val issueManagement = new IssueManagement

    url     foreach issueManagement.setUrl
    system  foreach issueManagement.setSystem

    issueManagement
  }

  def mavenLicense(name: Option[String] = Some("modified BSD License"),
                   url: Option[String] = Some("https://github.com/wix/wix-oss-parents/wix-oss-superduper-license-certified-by-legal/LICENSE.md"),
                   distribution: Option[String] = Some("repo")): License = {
    val license = new License

    name          foreach license.setName
    url           foreach license.setUrl
    distribution  foreach license.setDistribution

    license
  }

  def mavenOrganization(name: Option[String] = Some("wix.com"),
                        url: Option[String] = Some("http://wix.io")): Organization = {
    val organization = new Organization

    name  foreach organization.setName
    url   foreach organization.setUrl

    organization
  }
}
