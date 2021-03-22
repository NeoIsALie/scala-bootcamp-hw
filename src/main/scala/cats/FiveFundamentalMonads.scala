package cats

object FiveFundamentalMonads {

  trait Monad[M[_]] {
    def unit[A](a: A): M[A]

    def bind[A, B](ma: M[A])(amb: A => M[B]): M[B]
  }

  trait Monoid[A] {
    def mempty: A
    def mappend(x: A)(y: A): A
  }

  case class Identity[A](a: A)
  object Identity {
    implicit val identityMonad = new Monad[Identity] {
      def unit[A](a: A): Identity[A] = Identity(a)
      def bind[A, B](ma: Identity[A])(amb: A => Identity[B]): Identity[B] =
        amb(ma.a)
    }
  }

  sealed trait Maybe[+A]
  case class Just[A](a: A) extends Maybe[A]
  case object None extends Maybe[Nothing]
  object Maybe {
    implicit val maybeMonad = new Monad[Maybe] {
      def unit[A](a: A): Maybe[A] = Just(a)
      def bind[A, B](ma: Maybe[A])(amb: A => Maybe[B]): Maybe[B] =
        ma match {
          case Just(a) => amb(a)
          case None    => None
        }
    }
  }

  case class State[S, A](run: S => (S, A))
  object State {
    implicit def stateMonad[S] = new Monad[State[S, *]] {
      def unit[A](a: A): State[S, A] = State((s: S) => (s, a))

      def bind[A, B](ma: State[S, A])(amb: A => State[S, B]): State[S, B] = {
        State((s: S) =>
          ma.run(s) match {
            case (s, a) => amb(a).run(s)
          }
        )
      }
    }
  }

  case class Reader[R, A](run: R => A)

  object Reader {
    implicit def readerMonad[R] = new Monad[Reader[R, *]] {
      def unit[A](a: A): Reader[R, A] = Reader((_: R) => a)

      def bind[A, B](ma: Reader[R, A])(amb: A => Reader[R, B]): Reader[R, B] =
        Reader((r: R) => amb(ma.run(r)).run(r))
    }
  }

  case class Writer[W, A](run: (W, A))
  object Writer {
    implicit def readerMonad[W](implicit m: Monoid[W]) = new Monad[Writer[W, *]] {
      def unit[A](a: A): Writer[W, A] = Writer((m.mempty, a))

      def bind[A, B](ma: Writer[W, A])(amb: A => Writer[W, B]): Writer[W, B] = {
        val (wa, a) = ma.run
        val (wb, b) = amb(a).run
        val w = m.mappend(wa)(wb)
        Writer((w, b))
      }
    }
  }
}
