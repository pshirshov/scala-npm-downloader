package io.septimalmind.npmresolver

import izumi.functional.bio.{Entropy2, F, IO2}

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

/** This is just a local per-invokation cache for graph traversals to avoid visiting the same node multiple times
  */
class ConcurrentActionCache[F[+_, +_]: IO2: Entropy2, K, V] {
  private val content = new ConcurrentHashMap[K, Either[UUID, V]]()

  def retrieve(
      key: K,
      compute: K => F[Throwable, V]
  ): F[Throwable, ConcurrentActionCache.CacheResolution[V]] = {
    for {
      // this is just a local per-invokation cache to avoid visiting the same node multiple times
      token <- implicitly[Entropy2[F]].nextUUID()
      maybeState <- F.sync(
        content.computeIfAbsent(key, _ => Left(token))
      )
      out <- maybeState match {
        case Left(value) if value == token =>
          for {
            value <- compute(key)
            // The initiator token should make the slippages impossible
            // Though a CHM over F would be the right solution
            _ <- F.sync(content.put(key, Right(value)))
          } yield {
            ConcurrentActionCache.HaveV(value)
          }
        case Left(_) =>
          F.pure(ConcurrentActionCache.PendingTask[V]())
        case Right(value) =>
          F.pure(ConcurrentActionCache.HaveV(value))

      }
    } yield {
      out
    }
  }
}

object ConcurrentActionCache {
  sealed trait CacheResolution[V]
  case class HaveV[V](v: V) extends CacheResolution[V]
  case class PendingTask[V]() extends CacheResolution[V]
}
