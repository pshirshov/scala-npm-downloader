package io.septimalmind.npmresolver.caches

import io.circe.syntax.EncoderOps
import io.circe.{Codec, parser}
import io.septimalmind.npmresolver.caches.EtagCache.{
  Changed,
  Etag,
  Etagged,
  RefreshResult
}
import izumi.functional.bio._
import izumi.fundamentals.platform.files.IzFiles

import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.concurrent.duration.DurationInt

trait EtagCache[F[+_, +_]] {
  def fetch(
      key: String,
      refresh: (Path, Option[Etag]) => F[Throwable, RefreshResult]
  ): F[Throwable, Etagged]
}

object EtagCache {
  sealed trait RefreshResult
  case class Unchanged(etag: Etag) extends RefreshResult
  case class Changed(etag: Etag) extends RefreshResult

  case class Etag(etag: String)
  case class Etagged(tag: Etag, path: Path)

  case class Meta(tag: Etag)
  object Meta {
    implicit val ce: Codec[Etag] =
      io.circe.generic.semiauto.deriveCodec[Etag]

    implicit val c: Codec[Meta] =
      io.circe.generic.semiauto.deriveCodec[Meta]
  }

  class EtagCacheLocalFSImpl[F[+_, +_]: Async2: Fork2: Temporal2: BlockingIO2](
      root: java.nio.file.Path
  ) extends EtagCache[F] {
    override def fetch(
        key: String,
        refresh: (Path, Option[Etag]) => F[Throwable, RefreshResult]
    ): F[Throwable, Etagged] = {
      for {
        rootFile <- F.sync(root.resolve(key).toFile)
        _ <- F.sync(rootFile.mkdirs())
        _ <- F.ifThenElse(
          rootFile.exists() && rootFile.isDirectory && rootFile.canWrite
        )(F.unit, F.fail(new RuntimeException(s"root not writeable: $root")))
        metaPath <- F.pure(root.resolve(s"${key.toString}.meta"))
        lockPath <- F.pure(root.resolve(s"${key.toString}.lock"))
        blobPath <- F.pure(root.resolve(s"${key.toString}.data"))

        out <- F.bracket(
          F.sync(
            FileChannel.open(
              lockPath,
              StandardOpenOption.WRITE,
              StandardOpenOption.CREATE
            )
          )
        )(chan =>
          for {
            _ <- F.sync(chan.close())
            _ <- F.sync(lockPath.toFile.delete())
          } yield {}
        ) { chan =>
          for {
            _ <- F.retryWhileF(F.syncThrowable(chan.lock()))(e =>
              for {
                _ <- F.sync(println(s"can't lock on ${lockPath} ($e)"))
                _ <- F.sleep(1.second)
              } yield {
                true
              }
            )

            maybePrevTag <- F.ifThenElse(F.sync(metaPath.toFile.exists()))(
              F.sync(IzFiles.readString(metaPath))
                .flatMap(s => F.fromEither(parser.parse(s)))
                .flatMap(j => F.fromEither(j.as[Meta]))
                .map(m => Some(m.tag)),
              F.pure(None)
            )

            result <- refresh(blobPath, maybePrevTag)
            newTag <- result match {
              case t: Unchanged =>
                F.pure(t.etag)
              case t: Changed =>
                F.sync(
                  Files.writeString(metaPath, Meta(t.etag).asJson.noSpaces)
                ) *> F.pure(t.etag)
            }
          } yield {
            Etagged(newTag, blobPath)
          }
        }

      } yield {
        out
      }
    }
  }

}
