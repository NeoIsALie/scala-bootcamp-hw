package typeclass

object Task1 {
  final case class Money(amount: BigDecimal)

  // TODO: create Ordering instance for Money
  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
}

object Task2 {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  // TODO: create Show instance for User
  object Show{
    def apply[A: Show]: Show[A] = implicitly
  }

  // TODO: create syntax for Show so i can do User("1", "Oleg").show
  object ShowSyntax {
    implicit class ShowApply[A: Show] (a: A) {
      def show: String = Show[A].show(a)
    }
  }

  object ShowInstances {
    implicit val showUser: Show[User] = _.toString
  }
  object Task3 {
    type Error = String
    final case class User(id: String, name: String)

    trait Parse[T] {
      def parse(entity: String): Either[Error, T]
    }

    object Parse {
      def apply[F: Parse]: Parse[F] = implicitly[Parse[F]]
    }

    object ParseSyntax {
      implicit class ParseOps(inner: String) {
        def parse[A: Parse]: Either[Error, A] = Parse[A].parse(inner)
      }
    }

    object ParseInstances {
      implicit val parseUser: Parse[User] = (entity: String) =>
        entity.split(";").toList match {
          case id :: name :: Nil => Right(User(id, name))
          case _                 => Left(s"Cannot parse '$entity' as User")
        }
    }
  }

  object Task4 {
    trait Eq[T] {
      def eqv(lhs: T, rhs: T): Boolean
    }

    object Eq {
      def apply[F: Eq]: Eq[F] = implicitly[Eq[F]]
    }

    object EqSyntax {
      implicit class EqOps[A: Eq](lhs: A) {
        def ===(rhs: A): Boolean = Eq[A].eqv(lhs, rhs)
        def eqv(rhs: A): Boolean = Eq[A].eqv(lhs, rhs)
      }
    }

    object EqInstances {
      implicit def anyEq[A]: Eq[A] = _ == _
    }
  }
}


