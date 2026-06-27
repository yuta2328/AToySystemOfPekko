import java.util.UUID
import java.time.LocalDateTime

object Utils {
  def genId(): String = UUID.randomUUID().toString
  def forM[A, B](list: List[A], f: A => Either[B, A]): Either[B, List[A]] =
    list.foldRight[Either[B, List[A]]](Right(Nil)) { (a, acc) =>
      for {
        results <- acc
        result <- f(a)
      } yield result :: results
    }
  def now() = LocalDateTime.now()
}
