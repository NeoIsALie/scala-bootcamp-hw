package typeclass

object TypeClass {
  trait HashCode[T] {
    def hash(entity: T): Int
  }

  object HashCode {
    def apply[F: HashCode]: HashCode[F] = implicitly[HashCode[F]]
  }

  object HashCodeSyntax {
    implicit class HashCodeOps[A: HashCode](x: A) {
      def hash: Int = HashCode[A].hash(x)
    }
  }

  object HashCodeInstances {
    implicit val stringHashCode: HashCode[String] = _.hashCode
  }
}

object AdvancedHomework {
  trait FlatMap[F[_]] {
    def flatMap[A, B](x: F[A])(f: A => F[B]): F[B]
  }

  object FlatMap {
    def apply[F[_]: FlatMap]: FlatMap[F] = implicitly[FlatMap[F]]
  }

  object FlatMapSyntax {
    implicit class FlatMapOps[F[_]: FlatMap, A](x: F[A]) {
      def >>=[B](f: A => F[B]): F[B] = FlatMap[F].flatMap(x)(f)
    }
  }

  object FlatMapInstances {
    implicit val optionFlatMap: FlatMap[Option] = new FlatMap[Option] {
      def flatMap[A, B](x: Option[A])(f: A => Option[B]): Option[B] = x.flatMap(f)
    }
  }
}
}