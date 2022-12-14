package io.septimalmind.npmresolver

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, FileInputStream, FileOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import izumi.functional.bio.{F, IO2}

import java.nio.file.Path

trait CompressionIO2[F[+_, +_]] {
  def deflate(bytes: Array[Byte]): F[Throwable, Array[Byte]]
  def inflate(bytes: Array[Byte]): F[Throwable, Array[Byte]]

  def deflate(file: File, target: Path): F[Throwable, Unit]

}

final class CompressionIO2Gzip[F[+_, +_]: IO2] extends CompressionIO2[F] {

  def deflate(bytes: Array[Byte]): F[Throwable, Array[Byte]] = {
    for {
      aos <- F.syncThrowable(new ByteArrayOutputStream())
      _ <- F.bracket(F.syncThrowable(new GZIPOutputStream(aos)))(gzos => F.sync(gzos.close())) {
        gzos =>
          F.syncThrowable(gzos.write(bytes))
      }
    } yield {
      aos.toByteArray
    }
  }

  def inflate(bytes: Array[Byte]): F[Throwable, Array[Byte]] = {
    for {
      ais <- F.syncThrowable(new ByteArrayInputStream(bytes))
      bytes <- F.bracket(F.syncThrowable(new GZIPInputStream(ais)))(gzis => F.sync(gzis.close())) {
        gzis =>
          F.syncThrowable(gzis.readAllBytes())
      }
    } yield {
      bytes
    }
  }

  override def deflate(file: File, target: Path): F[Throwable, Unit] = {
    for {
      aos <- F.syncThrowable(new FileOutputStream(target.toFile))
      _ <- F.bracket(F.syncThrowable(new GZIPOutputStream(aos)))(gzos => F.sync(gzos.close())) {
        gzos =>
          F.bracket(F.syncThrowable(new FileInputStream(file)))(is => F.sync(is.close())) {
            is =>
              val buffer = new Array[Byte](1024 * 64)
              var len    = is.read(buffer)

              F.sync {
                while (len != -1) {
                  gzos.write(buffer, 0, len)
                  len = is.read(buffer)
                }
              }
          }

      }
    } yield {}
  }
}
