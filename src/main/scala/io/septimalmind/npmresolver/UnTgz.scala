package io.septimalmind.npmresolver

import org.apache.commons.compress.archivers.tar.TarArchiveInputStream

import java.nio.file.Path

object UnTgz {

  import java.io.{FileInputStream, FileOutputStream, IOException}
  import java.util.zip.GZIPInputStream

  @throws[IOException]
  def extract(tgz: Path, base: Path): Unit = {
    val tin = new TarArchiveInputStream(
      new GZIPInputStream(new FileInputStream(tgz.toFile))
    )

    base.toFile.mkdirs()

    try {
      var tarEnt = tin.getNextEntry
      while ({
        tarEnt != null
      }) {
        val entryName = tarEnt.getName
        assert(entryName.startsWith("package/"))
        val fullPath = base.resolve(entryName.drop("package/".length))

        println(fullPath)
        if (tarEnt.isDirectory) {
          fullPath.toFile.mkdirs()
        } else {
          fullPath.getParent.toFile.mkdirs()

          val fos = new FileOutputStream(fullPath.toFile)
          try {
            tin.transferTo(fos)
          } finally {
            if (fos != null) fos.close()
          }
        }

        tarEnt = tin.getNextEntry
      }
    } finally {
      if (tin != null) tin.close()
    }
  }
}
