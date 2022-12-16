package io.septimalmind.npmresolver

import fs2.io.file
import izumi.functional.bio._
import izumi.functional.bio.catz._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.{EntityDecoder, EntityTag, Uri}

import java.nio.file.Path
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

object NPMResolver {
  case class Dist(npm: NPMDist, artifact: NPMArtifact)
  case class ToDownload(
      tarballs: List[Dist]
  )

  case class DownloadedDescriptor(
      etag: Option[String],
      descriptor: NPMDescriptor
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

  private def resolve(
      artifact: NPMArtifact,
      state: ConcurrentHashMap[NPMArtifact, Either[UUID, DownloadedDescriptor]]
  ): F[Throwable, ToDownload] = {
    for {
      // this is just a local per-invokation cache to avoid visiting the same node multiple times
      token <- entropy2.nextTimeUUID()
      maybeDescriptor <- F.sync(
        state.computeIfAbsent(artifact, _ => Left(token))
      )
      descriptor <- maybeDescriptor match {
        case Right(value) =>
          F.pure(Some(value))

        case Left(t) if t == token =>
          for {
            descriptor <- doDownload(artifact)
            // The initiator token should make the slippages impossible
            // Though a CHM over F would be the right solution
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
            )(value.descriptor.versions.get(artifact.version))
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

  object clientF extends Http4sClientDsl[BIOTask]

  private def doDownload(
      artifact: NPMArtifact
  ): F[Throwable, DownloadedDescriptor] = {
    import clientF._
    import org.http4s.dsl.io._
    import org.http4s.headers._
    implicit val d: EntityDecoder[BIOTask, NPMDescriptor] =
      jsonOf[BIOTask, NPMDescriptor]

    for {
      url <- F.sync(
        Uri.unsafeFromString(
          s"https://registry.npmjs.org/${artifact.name}"
        )
      )
      maybeEtag <- F.pure(Option("nuthing"))
      req <- maybeEtag match {
        case Some(value) =>
          F.pure(GET(url, `If-None-Match`(EntityTag.apply(value))))
        case None =>
          F.pure(GET(url))
      }
      _ <- F.sync(println(s"Download required: $artifact, $req"))
      descriptor <- client.run(req).use { req =>
        for {
          etag <- F.pure(req.headers.get[ETag].map(_.tag.tag))
          body <- req.as[NPMDescriptor]
          // TODO: persist etags
          // TODO: fetch from local storage if we have an entry and it's not changed
        } yield {
          println(s"$url => $etag")
          DownloadedDescriptor(etag, body)
        }
      }
    } yield {
      descriptor
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
