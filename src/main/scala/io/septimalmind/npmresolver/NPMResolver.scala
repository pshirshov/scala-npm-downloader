package io.septimalmind.npmresolver

import fs2.io.file
import izumi.functional.bio._
import izumi.functional.bio.catz._
import org.http4s.EntityDecoder
import org.http4s.circe._
import org.http4s.client.Client

import java.nio.file.Path
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

object NPMResolver {
  case class Dist(npm: NPMDist, artifact: NPMArtifact)
  case class ToDownload(
      tarballs: List[Dist]
  )

}

class NPMResolver[F[+_, +_]: Async2: Fork2: Temporal2: BlockingIO2](
    client: Client[F[Throwable, *]],
    cache: BlobCache[F],
    entropy2: Entropy2[F]
) {
  type BIOTask[A] = F[Throwable, A]
  import NPMResolver._

  def download(
      toDownload: ToDownload,
      target: Path
  ): F[Throwable, List[Path]] = {
    F.parTraverse(toDownload.tarballs.distinct) { tarball =>
      for {
        path <- cache
          .fetch(
            Hashsum.Sha1(tarball.npm.shasum),
            p =>
              client
                .expect(tarball.npm.tarball)(
                  EntityDecoder.binFile[BIOTask](
                    fs2.io.file.Path.fromNioPath(p)
                  )
                )
                .map(_.toNioPath)
          )
        _ <- checkHash(path, tarball.npm.shasum)
        _ <- F.sync(
          UnTgz.extract(
            path,
            target.resolve("node_modules").resolve(tarball.artifact.name)
          )
        )
      } yield {
        path
      }

    }
  }

  def resolve(
      artifact: NPMArtifact
  ): F[Throwable, ToDownload] = {
    resolve(artifact, new ConcurrentHashMap())
  }

  def resolve(
      artifact: NPMArtifact,
      state: ConcurrentHashMap[NPMArtifact, Either[UUID, NPMDescriptor]]
  ): F[Throwable, ToDownload] = {
    for {
      token <- entropy2.nextTimeUUID()
      maybeDescriptor <- F.sync(
        state.computeIfAbsent(artifact, _ => Left(token))
      )
      descriptor <- maybeDescriptor match {
        case Right(value) =>
          F.pure(Some(value))

        case Left(t) if t == token =>
          for {
            url <- F.pure(s"https://registry.npmjs.org/${artifact.name}")
            _ <- F.sync(println(s"Download required: $artifact, $url"))
            descriptor <- client.expect[NPMDescriptor](
              url
            )(
              jsonOf[BIOTask, NPMDescriptor]
            )
            // the slippage still may happen here, we need a CHM over F
            _ <- F.sync(state.put(artifact, Right(descriptor)))
          } yield {
            Some(descriptor)
          }
        case Left(_) =>
          F.sync(println(s"Download already queued: $artifact")) *> F.pure(None)
      }

      out <- descriptor match {
        case Some(value) =>
          for {
            ver <- F.fromOption(
              new RuntimeException(s"Version not found: ${artifact.version}")
            )(value.versions.get(artifact.version))
            _ <- F.sync(println(s"Found descriptor for ${artifact}"))
            deps <- F.parTraverse(ver.dependencies.getOrElse(Map.empty)) {
              case (artifact, verspec) =>
                for {
                  ver <- versionToDownload(verspec)
                  out <- resolve(NPMArtifact(artifact, ver), state)
                } yield {
                  out
                }
            }
          } yield {
            ToDownload(
              List(Dist(ver.dist, artifact)) ++ deps.flatMap(_.tarballs)
            )
          }
        case None =>
          F.pure(ToDownload(List.empty)) // it's already in the queue
      }

    } yield {
      out
    }
  }

  private def checkHash(
      path: java.nio.file.Path,
      shasum: String
  ): F[Throwable, Unit] = {
    import izumi.fundamentals.platform.bytes.IzBytes._
    for {
      out <- fs2.io.file
        .Files[BIOTask]
        .readAll(file.Path.fromNioPath(path))
        .through(fs2.hash.sha1)
        .compile
        .toVector
      hex = out.toArray.toHex
      _ <- F.ifThenElse(hex == shasum)(
        F.unit,
        F.fail(
          new RuntimeException(s"Hash mismatch: got ${hex}, expected ${shasum}")
        )
      )
    } yield {}

  }

  private def versionToDownload(spec: String): F[Throwable, String] = {
    F.pure(if (spec.charAt(0).isDigit) {
      spec
    } else {
      spec.substring(1)
    })
  }
}
