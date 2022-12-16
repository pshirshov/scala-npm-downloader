package io.septimalmind.npmresolver.caches

import io.septimalmind.npmresolver.Hashsum
import izumi.functional.bio.{F, _}

import java.nio.channels.FileChannel
import java.nio.file.{Path, StandardOpenOption}
import scala.concurrent.duration.DurationInt

trait BlobCache[F[+_, +_]] {
  def fetch(
      hashsum: Hashsum,
      orElse: Path => F[Throwable, Path]
  ): F[Throwable, Path]
}

object BlobCache {
  class BlobCacheLocalFSImpl[F[+_, +_]: Async2: Fork2: Temporal2: BlockingIO2](
      root: java.nio.file.Path
  ) extends BlobCache[F] {

    override def fetch(
        hashsum: Hashsum,
        orElse: Path => F[Throwable, Path]
    ): F[Throwable, Path] = {
      for {
        rootFile <- F.sync(root.toFile)
        _ <- F.sync(rootFile.mkdirs())
        _ <- F.ifThenElse(
          rootFile.exists() && rootFile.isDirectory && rootFile.canWrite
        )(F.unit, F.fail(new RuntimeException(s"root not writeable: $root")))
        blobPath <- F.pure(root.resolve(hashsum.toString))
        lockPath <- F.pure(root.resolve(s"${hashsum.toString}.lock"))

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
            out <- F
              .ifThenElse(blobPath.toFile.exists())(
                F.sync(println(s"Reusing ${blobPath}")) *> F.pure(blobPath),
                F.sync(println(s"Fetching ${blobPath}")) *> orElse(blobPath)
              )
          } yield {
            out
          }
        }

      } yield {
        out
      }
    }
  }

}
