import java.util.UUID
import java.time.LocalDateTime

object Utils {
  def genId(): String = UUID.randomUUID().toString
  def forM[A, B, C](list: List[A], f: A => Either[B, C]): Either[B, List[C]] =
    list.foldRight[Either[B, List[C]]](Right(Nil)) { (a, acc) =>
      for {
        results <- acc
        result <- f(a)
      } yield result :: results
    }
  def now() = LocalDateTime.now()

  def initOrPut[K, V](map: Map[K, List[V]], key: K, value: V): Map[K, List[V]] = {
    map + (key -> (value +: map.getOrElse(key, Nil)))
  }

  def initOrUpdate[K, V](map: Map[K, V], key: K, init: V, update: V => V): Map[K, V] = {
    map + (key -> map.get(key).map(update).getOrElse(init))
  }
}
