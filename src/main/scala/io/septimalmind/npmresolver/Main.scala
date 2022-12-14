package io.septimalmind.npmresolver

import fs2.io.file
import io.circe.Parser
import io.circe.jawn.JawnParser
import izumi.functional.bio._
import izumi.functional.bio.catz._
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.http4s.EntityDecoder
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import zio._
import zio.blocking.Blocking
import zio.clock.Clock

import java.io.File
import java.nio.file.{Files, Path}
import java.util

case class Dist(npm: NPMDist, artifact: NPMArtifact)
case class ToDownload(
    tarballs: List[Dist]
)

object UnTgz {

  import java.io.FileInputStream
  import java.io.FileOutputStream
  import java.io.IOException
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

class NPMResolver[F[+_, +_]: Async2: Fork2: Temporal2: BlockingIO2](
    client: Client[F[Throwable, *]]
) {
  type BIOTask[A] = F[Throwable, A]
  private val compr = new CompressionIO2Gzip[F]()

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
        _ <- F.sync(
          UnTgz.extract(
            path.toNioPath,
            target.resolve("node_modules").resolve(tarball.artifact.name)
          )
        )
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

  private def checkHash(path: file.Path, shasum: String): F[Throwable, Unit] = {
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

  private def versionToDownload(spec: String): F[Throwable, String] = {
    F.pure(if (spec.charAt(0).isDigit) {
      spec
    } else {
      spec.substring(1)
    })
  }
}

object Main {
  def test(path: String) = {
    import org.graalvm.polyglot.Context
    val options = new util.HashMap[String, String]()
    println(path)
    options.put("js.commonjs-require", "true")
    options.put("js.commonjs-require-cwd", path)

    val cx = Context
      .newBuilder("js")
      .allowExperimentalOptions(true)
      .allowIO(true)
      .options(options)
      .build

    val module = cx.eval("js", "require('es-leftpad');")
    println(module)
  }

  def main(args: Array[String]): Unit = {

    def resolve[F[+_, +_]: Async2: Fork2: Temporal2: BlockingIO2](
        client: Client[F[Throwable, *]]
    ) = {
      for {
        res <- F.pure(new NPMResolver[F](client))
        todo <- res.resolve(
          NPMArtifact("testy-mctestface", "1.0.5")
        )
        out <- F.sync(Files.createTempDirectory(s"npm-${System.nanoTime()}"))
        tars <- res.download(
          todo,
          out
        )
      } yield {
        (out, todo, tars)
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
      clock <- ZIO.environment[Clock]
      blocking <- ZIO.environment[Blocking]
      resolved <- {
        implicit val c = clock
        implicit val b = blocking
        run.use(client => resolve[IO](client))
      }
    } yield {
      println(resolved)
      test(resolved._1.resolve("node_modules").toString)
    }
    zio.Runtime.default.unsafeRun(p)

  }
}
