* Fifth day

Code from John
```scala

sealed trait Free[F[_], A] { self =>
      final def map[B](f: A => B): Free[F, B] = self.flatMap(f.andThen(Free.point[F, B](_)))

      final def flatMap[B](f: A => Free[F, B]): Free[F, B] = Free.FlatMap(self, f)

      final def <* [B](that: Free[F, B]): Free[F, A] =
        self.flatMap(a => that.map(_ => a))

      final def *> [B](that: Free[F, B]): Free[F, B] =
        self.flatMap(_ => that)

      final def fold[G[_]: Monad](interpreter: F ~> G): G[A] =
        self match {
          case Free.Return(value0)  => value0().point[G]
          case Free.Effect(fa)      => interpreter(fa)
          case Free.FlatMap(fa0, f) => fa0.fold(interpreter).flatMap(a0 => f(a0).fold(interpreter))
        }
    }
    object Free {
      case class Return[F[_], A](value0: () => A) extends Free[F, A] {
        lazy val value = value0()
      }
      case class Effect[F[_], A](effect: F[A]) extends Free[F, A]
      case class FlatMap[F[_], A0, A](fa0: Free[F, A0], f: A0 => Free[F, A]) extends Free[F, A]

      def point[F[_], A](a: => A): Free[F, A] = Return(() => a)
      def lift[F[_], A](fa: F[A]): Free[F, A] = Effect(fa)
    }

    sealed trait ConsoleF[A]
    final case object ReadLine extends ConsoleF[String]
    final case class PrintLine(line: String) extends ConsoleF[Unit]

    def readLine: Free[ConsoleF, String] = Free.lift[ConsoleF, String](ReadLine)
    def printLine(line: String): Free[ConsoleF, Unit] = Free.lift[ConsoleF, Unit](PrintLine(line))

    val program: Free[ConsoleF, String] =
      for {
        _    <- printLine("Good morning! What is your name?")
        name <- readLine
        _    <- printLine("Good to meet you, " + name + "!")
      } yield name

    import scalaz.zio.IO
    import scalaz.zio.interop.scalaz72._

    val programIO: IO[Nothing, String] =
      program.fold[IO[Nothing, ?]](new NaturalTransformation[ConsoleF, IO[Nothing, ?]] {
        def apply[A](consoleF: ConsoleF[A]): IO[Nothing, A] =
          consoleF match {
            case ReadLine => IO.sync(scala.io.StdIn.readLine())
            case PrintLine(line) => IO.sync(println(line))
          }
      })

    case class TestData(input: List[String], output: List[String])
    case class State[S, A](run: S => (S, A)) {
      def eval(s: S): A = run(s)._2
    }
    object State {
      implicit def MonadState[S]: Monad[State[S, ?]] =
        new Monad[State[S, ?]] {
          def point[A](a: => A): State[S, A] = State(s => (s, a))
          def bind[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
            State[S, B](s => fa.run(s) match {
              case (s, a) => f(a).run(s)
            })
        }

      def get[S]: State[S, S] = State(s => (s, s))
      def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
      def modify[S](f: S => S): State[S, Unit] =
        get[S].flatMap(s => set(f(s)))
    }

    val programState: State[TestData, String] =
      program.fold[State[TestData, ?]](new NaturalTransformation[ConsoleF, State[TestData, ?]] {
        def apply[A](consoleF: ConsoleF[A]): State[TestData, A] =
          consoleF match {
            case ReadLine =>
              for {
                data <- State.get[TestData]
                line = data.input.head
                _    <- State.set(data.copy(input = data.input.drop(1)))
              } yield line

            case PrintLine(line) =>
              State.modify[TestData](d => d.copy(output = line :: d.output))
          }
      })

    programState.eval(TestData("John" :: Nil, Nil))
```


** Loyalty point implementation from John

```scala

 import scalaz.zio._

    sealed abstract class DatabaseError extends Exception
    trait Source[E, A] {
      def fold[Z](z: Z)(f: (Z, A) => Z): IO[E, Z]
    }
    type Database[A] = IO[DatabaseError, A]
    type DatabaseSource[A] = Source[DatabaseError, A]
    type DatabaseDerived[A, B] = DatabaseSource[A] => Database[B]

    trait Number[A] {
      def zero: A
      def one: A
      def plus(l: A, r: A): A
      def minus(l: A, r: A): A
      def times(l: A, r: A): A
    }
    object Number {
      def apply[A](implicit N: Number[A]): Number[A] = N
    }
    implicit class NumberSyntax[A](l: A) {
      def + (r: A)(implicit N: Number[A]): A = N.plus(l, r)
      def - (r: A)(implicit N: Number[A]): A = N.minus(l, r)
      def * (r: A)(implicit N: Number[A]): A = N.times(l, r)
    }

    final case class Customer[AccountID, Num](
      name: String,
      email: String,
      account: Account[AccountID, Num]
    )
    final case class Account[AccountID, Num](
      id  : AccountID,
      txns: DatabaseSource[Transaction[AccountID, Num]])
    object Account {
      import Transaction._
      type TxnDerived[A, B] = DatabaseDerived[Transaction[A, B], B]

      def balance[A, B: Number] : TxnDerived[A, B] =
        _.fold[B](Number[B].zero) {
          case (balance, Redeem  (v, _)) => balance - v
          case (balance, Earn    (v, _)) => balance + v
          case (balance, Transfer(v, _)) => balance - v
        }
      def status[A, B] : TxnDerived[A, Status] =
        _.fold[Status](Status.Open) {
          case (status, _) => status
        }

      def tier[A, B: Number: Order](tiers: Map[B, Tier]) : TxnDerived[A, B] =
        ???

      sealed trait Status
      object Status {
        case object Open extends Status
        case object Closed extends Status
      }
      sealed trait Tier
      object Tier {
        case object Silver   extends Tier
        case object Gold     extends Tier
        case object Platinum extends Tier
      }
    }
    final case class Reward()
    final case class Purchase(id: java.util.UUID, description: String, quantity: Int)

    sealed trait Transaction[+AccountID, +Num]
    object Transaction {
      final case class Redeem   [Num](amount: Num, reward: Reward) extends Transaction[Nothing, Num]
      final case class Earn     [Num](amount: Num, purchase: Purchase) extends Transaction[Nothing, Num]
      final case class Transfer [AccountID, Num](amount: Num, recipient: AccountID) extends Transaction[AccountID, Num]
    }

```

