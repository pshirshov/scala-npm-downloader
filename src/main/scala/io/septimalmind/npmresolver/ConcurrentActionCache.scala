package io.septimalmind.npmresolver

import izumi.functional.bio.{Entropy2, F, IO2, Primitives2, Promise2}

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

/** This is just a local per-invokation cache for graph traversals to avoid visiting the same node multiple times
  */
class ConcurrentActionCache[F[+_, +_]: IO2: Entropy2: Primitives2, K, V] {
  private val content =
    new ConcurrentHashMap[K, (UUID, Promise2[F, Throwable, V])]()

  def retrieve(
      key: K,
      compute: K => F[Throwable, V]
  ): F[Throwable, V] = {
    for {
      // this is just a local per-invokation cache to avoid visiting the same node multiple times
      token <- implicitly[Entropy2[F]].nextUUID()
      promise <- F.mkPromise[Throwable, V]
      maybeState <- F.sync(
        content.computeIfAbsent(key, _ => (token, promise))
      )
      out <- maybeState match {
        case (ptoken, promise) if ptoken == token =>
          for {
            value <- compute(key)
            _ <- promise.succeed(value)
          } yield {
            value
          }
        case (_, promse) =>
          for {
            value <- promse.await
          } yield {
            value
          }
      }
    } yield {
      out
    }
  }
}
