package io.septimalmind.npmresolver

import io.septimalmind.npmresolver.caches.{BlobCache, EtagCache}
import izumi.functional.bio._
import izumi.functional.bio.catz._
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import zio._
import zio.blocking.Blocking
import zio.clock.Clock

import java.nio.file.{Files, Paths}
import java.util

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

    def resolve[
        F[+_, +_]: Async2: Fork2: Temporal2: BlockingIO2: Entropy2: Primitives2
    ](
        client: Client[F[Throwable, *]]
    ) = {
      for {
        cache <- F.pure(
          new BlobCache.BlobCacheLocalFSImpl[F](Paths.get("/tmp/npm-blobs"))
        )
        ecache <- F.pure(
          new EtagCache.EtagCacheLocalFSImpl[F](
            Paths.get("/tmp/npm-descriptors")
          )
        )
        res <- F.pure(
          new NPMResolver[F](client, cache, ecache)
        )
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
      //test(resolved._1.resolve("node_modules").toString)
    }
    zio.Runtime.default.unsafeRun(p)

  }
}
