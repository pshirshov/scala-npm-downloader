package io.septimalmind.npmresolver

import fs2.io.file
import io.circe.parser
import io.septimalmind.npmresolver.caches.EtagCache.Etag
import io.septimalmind.npmresolver.caches.{
  BlobCache,
  ConcurrentActionCache,
  EtagCache
}
import io.septimalmind.npmresolver.compression.CompressionIO2Gzip
import izumi.functional.bio._
import izumi.functional.bio.catz._
import izumi.fundamentals.platform.files.IzFiles
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.{EntityDecoder, EntityTag, Status, Uri}

import java.nio.file.{Path, Paths}

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

class NPMResolver[
    F[+_, +_]: Async2: Fork2: Temporal2: BlockingIO2: Primitives2
](
    client: Client[F[Throwable, *]],
    cache: BlobCache[F],
    etagCache: EtagCache[F],
    compr: CompressionIO2Gzip[F]
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
        _ <- compr.untgz(
          path,
          target.resolve("node_modules").resolve(tarball.artifact.name)
        ) { case (base, sub) =>
          val p = Paths.get(sub)
          assert(p.startsWith("package"))
          import scala.jdk.CollectionConverters._

          val tgt = p.iterator().asScala
          println(tgt.toList)
          F.sync(base.resolve(sub.drop("package/".length)))
        }
      } yield {
        path
      }

    }
  }

  def resolve(
      artifact: NPMArtifact
  ): F[Throwable, ToDownload] = {
    resolve(
      artifact,
      new ConcurrentActionCache[F, NPMArtifact, DownloadedDescriptor]
    )
  }

  private def resolve(
      artifact: NPMArtifact,
      cac: ConcurrentActionCache[F, NPMArtifact, DownloadedDescriptor]
  ): F[Throwable, ToDownload] = {
    for {
      descriptor <- cac.retrieve(artifact, a => doDownload(a))
      ver <- F.fromOption(
        new RuntimeException(s"Version not found: ${artifact.version}")
      )(descriptor.descriptor.versions.get(artifact.version))
      _ <- F.sync(println(s"Found descriptor for ${artifact}"))
      deps <- F.parTraverse(ver.dependencies.getOrElse(Map.empty)) {
        case (artifact, verspec) =>
          for {
            ver <- versionToDownload(verspec)
            out <- resolve(NPMArtifact(artifact, ver), cac)
          } yield {
            out
          }
      }
    } yield {
      ToDownload(
        List(Dist(ver.dist, artifact)) ++ deps.flatMap(_.tarballs)
      )
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
      //maybeEtag <- F.pure(Option("nuthing"))

      blobPath <- etagCache.fetch(
        artifact.name,
        (blobTarget, maybeTag) =>
          for {
            req <- maybeTag match {
              case Some(value) =>
                F.pure(
                  GET(url, `If-None-Match`(EntityTag.apply(value.etag)))
                )
              case None =>
                F.pure(GET(url))
            }
            _ <- F.sync(println(s"Download required: $artifact, $req"))
            path <- client.run(req).use { req =>
              for {
                etag <- F.fromOption(
                  new RuntimeException(s"No etag in npm response")
                )(req.headers.get[ETag].map(_.tag.tag))
                out <- F.ifThenElse(req.status == Status.NotModified)(
                  F.pure(EtagCache.Unchanged(Etag(etag))),
                  req.body
                    .through(
                      fs2.io.file
                        .Files[BIOTask]
                        .writeAll(fs2.io.file.Path.fromNioPath(blobTarget))
                    )
                    .compile
                    .drain *> F.pure(EtagCache.Changed(Etag(etag)))
                )
              } yield {
                println(s"$url => $out")
                out
              }
            }
          } yield {
            path
          }
      )

//      out <- Files[BIOTask]
//        .readAll(fs2.io.file.Path.fromNioPath(blobPath.path))
//        .through(byteArrayParser)
//        .through(decoder[BIOTask, NPMDescriptor])
//        .compile
//        .lastOrError

      out <- F
        .sync(IzFiles.readString(blobPath.path))
        .flatMap(s => F.fromEither(parser.parse(s)))
        .flatMap(j => F.fromEither(j.as[NPMDescriptor]))

    } yield {
      DownloadedDescriptor(Some(blobPath.tag.etag), out)
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
