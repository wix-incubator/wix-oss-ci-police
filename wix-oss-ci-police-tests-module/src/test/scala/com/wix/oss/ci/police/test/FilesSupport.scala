/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.oss.ci.police.test


import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._


/** A support mixin trait for dealing with files and directories.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
trait FilesSupport {

  /** Deletes the specified directory recursively */
  def deleteDirectory(directory: Path): Unit = {
    Files.walkFileTree(directory, new SimpleFileVisitor[Path]() {
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

  /** Copies the specified directory, recursively, to the destination directory. */
  def copyDirectory(source: Path, target: Path): Unit = {
    Files.walkFileTree(source, new SimpleFileVisitor[Path]() {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
        val relativePath = source.relativize(dir)

        Files.createDirectories(target.resolve(relativePath))

        FileVisitResult.CONTINUE
      }

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        val relativeFile = source.relativize(file)

        Files.copy(file, target.resolve(relativeFile), StandardCopyOption.REPLACE_EXISTING)

        FileVisitResult.CONTINUE
      }
    })
  }
}


/** The Companion Object, which allows for "static" import style of the trait's functionality, rather than having to
  * extend from it.
  * I.e., instead of
  * {{{
  *   class MyClass extends MySuperClass with FilesSupport {
  *     ...
  *   }
  * }}}
  * one can simply use:
  * {{{
  *   import com.wix.oss.ci.police.test.FilesSupport._
  *
  *   ...
  *
  *   class MyClass extends MySuperClass {
  *     ...
  *   }
  * }}}
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object FilesSupport extends FilesSupport
