package io.septimalmind.npmresolver

import fs2.io.file
import io.circe.Parser
import io.circe.jawn.JawnParser
import izumi.functional.bio._
import izumi.functional.bio.catz._
import org.http4s.EntityDecoder
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import zio._
import zio.blocking.Blocking
import zio.clock.Clock

import java.nio.file.{Files, Path}

case class Dist(npm: NPMDist, artifact: NPMArtifact)
case class ToDownload(
    tarballs: List[Dist]
)

class NPMResolver[F[+_, +_]: Async2: Fork2: Temporal2: BlockingIO2](
    client: Client[F[Throwable, *]]
) {
  type BIOTask[A] = F[Throwable, A]

  def checkHash(path: file.Path, shasum: String): F[Throwable, Unit] = {
    import izumi.fundamentals.platform.bytes.IzBytes._

    for {
      out <- fs2.io.file
        .Files[BIOTask]
        .readAll(path)
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

  def download(
      toDownload: ToDownload,
      target: Path
  ): F[Throwable, List[fs2.io.file.Path]] = {
    F.parTraverse(toDownload.tarballs) { tarball =>
      val name = tarball.npm.tarball.split('/').last

      for {
        path <- client.expect(tarball.npm.tarball)(
          EntityDecoder.binFile[BIOTask](
            fs2.io.file.Path.fromNioPath(target.resolve(name))
          )
        )
        _ <- checkHash(path, tarball.npm.shasum)
      } yield {
        path
      }

    }
  }

  def resolve(artifact: NPMArtifact): F[Throwable, ToDownload] = {
    for {
      url <- F.pure(s"https://registry.npmjs.org/${artifact.name}")
      descriptor <- client.expect[NPMDescriptor](url)(
        jsonOf[BIOTask, NPMDescriptor]
      )
      ver <- F.fromOption(
        new RuntimeException(s"Version not found: ${artifact.version}")
      )(descriptor.versions.get(artifact.version))
      _ <- F.sync(println(s"Found descriptor for ${artifact}"))
      deps <- F.parTraverse(ver.dependencies.getOrElse(Map.empty)) {
        case (artifact, verspec) =>
          for {
            ver <- versionToDownload(verspec)
            out <- resolve(NPMArtifact(artifact, ver))
          } yield {
            out
          }
      }
    } yield {
      ToDownload(List(Dist(ver.dist, artifact)) ++ deps.flatMap(_.tarballs))
    }
  }

  def versionToDownload(spec: String): F[Throwable, String] = {
    F.pure(if (spec.charAt(0).isDigit) {
      spec
    } else {
      spec.substring(1)
    })
  }
}

object Main {
  def main(args: Array[String]): Unit = {

    val a = F.sync(println("Hello world!.."))

    def resolve[F[+_, +_]: Async2: Fork2: Temporal2: BlockingIO2](
        client: Client[F[Throwable, *]]
    ) = {
      for {
        res <- F.pure(new NPMResolver[F](client))
        todo <- res.resolve(
          NPMArtifact("testy-mctestface", "1.0.5")
        )
        tars <- res.download(
          todo,
          Files.createTempDirectory(s"npm-${System.nanoTime()}")
        )
      } yield {
        (todo, tars)

      }

    }

    val run = {
      import zio.interop.catz._
      import zio.interop.catz.implicits._ // WTF

      EmberClientBuilder
        .default[Task]
        .build
    }

    val p = for {
      _ <- a
      clock <- ZIO.environment[Clock]
      blocking <- ZIO.environment[Blocking]
      resolved <- {
        implicit val c = clock
        implicit val b = blocking
        run.use(client => resolve[IO](client))
      }
    } yield {
      println(resolved)
    }
    zio.Runtime.default.unsafeRun(p)

  }
}
