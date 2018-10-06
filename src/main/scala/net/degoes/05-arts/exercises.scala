// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.arts

import scalaz._
import Scalaz._

object exercises {

   object free {

    trait NaturalTransformation[F[_], G[_]] {
      def apply[A](fa: F[A]): G[A]
    }
    type ~> [F[_], G[_]] = NaturalTransformation[F, G]
    
     def headOption: List    ~> Option = new NaturalTransformation[List, Option] {
       def apply[A](fa:List[A]):Option[A] = 
        fa.headOption
     }
     def right[A]: Either[A, ?] ~> Option = ???
     def left[A]: Either[A, ?]  ~> Option = ???

     //def fromOption: Option ~> IO[Unit, ?] = ???


     sealed trait Free[F[_], A] { self =>
      
      final def map[B](f: A => B): Free[F, B] = self.flatMap(f.andThen(Free.point[F, B](_)))
      
      final def flatMap[B](f: A=> Free[F, B]):Free[F, B] = Free.FlatMap(self, f)

      final def <* [B](that:Free[F, B]): Free[F, A] = self.flatMap(a => that.map(_ => a))

      final def *> [B](that:Free[F, B]): Free[F, B] = self.flatMap(_ => that)
     
     final def fold[G[_]:Monad](interpreter: F ~> G):G[A] = 
       self match {
         case Free.Return(value0)   => value0().point[G]
         case Free.Effect(fa)       => interpreter(fa)
         case Free.FlatMap (fa0, f) => fa0.fold(interpreter).flatMap(a0 => f(a0).fold(interpreter))
       }

     }
     object Free {
       case class Return[F[_], A](value0:() => A) extends Free[F, A] {
         lazy val value = value0()
       }
       case class Effect[F[_], A](effect: F[A]) extends Free[F, A]
       case class FlatMap[F[_], A0, A](fa0:Free[F, A0], f: A0 => Free[F, A]) extends Free[F, A]

       def point[F[_], A](a: => A): Free[F, A] = Return(() => a)
       def lift[F[_], A](fa: F[A]): Free[F, A] = Effect(fa)
     
     }

     sealed trait ConsoleF[A]
     final case object ReadLine extends ConsoleF[String]
     final case class PrintLine(line:String) extends ConsoleF[Unit]

     def readLine: Free[ConsoleF, String] = Free.lift[ConsoleF, String](ReadLine)
     def printLine(line:String):Free[ConsoleF, Unit] = Free.lift[ConsoleF,Unit](PrintLine(line))


     val program: Free[ConsoleF, String] = 
     for {
      _    <- printLine("Goodmorning! What is your name?")
      name <- readLine
      _    <- printLine(s"Good to meet you, $name!")
     } yield name

     import scalaz.zio.IO
     import scalaz.zio.interop.scalaz72._
     
     val programIO: IO[Nothing, String] = 
     program.fold[IO[Nothing, ?]](new NaturalTransformation[ConsoleF, IO[Nothing,?]] {
          def apply[A](consoleF: ConsoleF[A]): IO[Nothing, A] = 
             consoleF match {
               case ReadLine       => IO.sync(scala.io.StdIn.readLine()) 
               case PrintLine(line) => IO.sync(println(line))
             }
     })

     case class TestData(input:List[String], output: List[String])
     case class State[S, A](run: S => (S, A))

     object State {

      implicit def MonadState[S]: Monad[State[S, ?]] = 
        new Monad[State[S, ?]] {
          def point[A](a: => A):State[S, A] = State(s => (s, a))
          def bind[A, B](fa:State[S, A])(f: A => State[S, B]): State[S, B] = ???
        }

       def get[S]: State[S, S] = State(s => (s, s))

       def set[S](s:S): State[S, Unit] = State(_ => (s, ()))

       def modify[S](f: S => S): State[S, Unit] = get[S].flatMap(s => set(f(s)))
     }       

     val programState: State[TestData, String] =  
     program.fold[State[TestData, ?]](new NaturalTransformation[ConsoleF, State[TestData, ?]]{
            def apply[A](consoleF: ConsoleF[A]): State[TestData, A] = 
            consoleF match {
              case ReadLine => 
                 for {
                   data <- State.get[TestData]
                   line = data.input.head
                  _     <- State.set(data.copy(input = data.input.drop(1))) 
                } yield line

                case PrintLine(line) => State.modify[TestData](d => d.copy(output = line :: d.output))
            }
          })



   }


  object design {

   /**
    Loyalty points managment

    ** Entities - Data
       * Customer
       * Loyalty points
       * Loyalty point account
       * Loyalty Points Issuer  -- places you can get points from
       * Loyalty Points Receiver --places you can spend points
       * Tiers ?
    
    ** Services - Functions/ Operations
      * Customer performs some action to earn loyalty points from an issuer
      * Customer spends loyalty points at a receiver
      * Customer transfer loyalty points to another account
      * Customer sees account details including loyalty point balance
      * Customer opens / closes loyalty point account
      * Customer signs up / opt-ins for loyalty point offers
      * Notification & transactional email

   */ 

   import scalaz.zio._

   sealed abstract class DatabaseError extends Exception

   type Database[A] = IO[DatabaseError, A]
   type DatabaseSource[A] = Source[DatabaseError, A]
   type DatabaseDerived[A, B] = DatabaseSource[A] => Database[B]

   trait Source[E, A] {
     def fold[Z](z: Z)(f: (Z, A) => Z): IO[E, Z]
   }
   final case class Customer[AccountID, Num](
     name: String,
     email: String,
     account: Account[AccountID, Num]
   )
   
   trait Number[A] {
     def zero: A
     def one: A
     def plus(l: A, r: A): A
     def minus(l: A, r: A): A
     def times(l: A, r: A): A
   }

   object Number {
     def apply[A](implicit N: Number[A]) = N
   }
   
   implicit class NumberSyntax[A](l: A) {
       def + (r: A)(implicit N: Number[A]):A = N.plus(l, r)
       def - (r: A)(implicit N: Number[A]):A = N.minus(l, r)
       def * (r: A)(implicit N: Number[A]):A = N.times(l, r)
    }
   
    
   sealed trait Transaction[+AccountID, +Num]
   object Transaction {
     final case class Redeem   [Num](amount: Num, reward: Reward)                 extends Transaction[Nothing, Num]
     final case class Earn     [Num](amount:Num, purchase: Purchase)             extends Transaction[Nothing, Num]
     final case class Transfer [AccountID, Num](amount: Num, account: AccountID) extends Transaction[AccountID, Num]
   }

   final case class Reward()
   final case class Purchase(id: java.util.UUID, description: String, quantity: Int)
   final case class Account[AccountID, Num](
     id:AccountID,
     txns:DatabaseSource[Transaction[AccountID,  Num]]
   )
   object Account {
     import Transaction._

     type TxnDerived[A, B] = DatabaseDerived[Transaction[A, B], B]

     def balance [A, B: Number]: TxnDerived[A, B] = 
         _.fold[B](Number[B].zero){
           case (balance, Redeem   (v, _)) => balance - v 
           case (balance, Earn     (v, _)) => balance + v 
           case (balance, Transfer (v, _)) => balance - v 
         }
     def status  [A, B] : TxnDerived[A, Status] = 
      _.fold[Status](Status.Open) {
           case (status, _) => status 
      }
     def tier   [A, B:Number: Order](tiers: Map[B, Tier])   : TxnDerived[A, B]   = ???
   }

   sealed trait Status
   
   object Status {
     case object Open   extends Status
     case object Closed extends Status
   }

   sealed trait Tier
     
     object Tier {
       case object Silver   extends Tier
       case object Gold     extends Tier
       case object Platinum extends Tier
     }

   trait Confirmation
   
   sealed trait LoyaltyError
   object LoyaltyError {
     case object InsufficentBalance extends LoyaltyError
     case object InvalidReward      extends LoyaltyError
     case object InvalidPurchase    extends LoyaltyError
     case object InvalidAccount     extends LoyaltyError
     case object ClosedAccount      extends LoyaltyError
   }


    trait LoyaltyTransactions[F[_]] {
     type UserAccountID = java.util.UUID
       def redeem(accountID: UserAccountID, points: Long, reward: Reward):F[Either[LoyaltyError, Confirmation]]

       def earn(accountID: UserAccountID, points: Long, purchase: Purchase):F[Either[LoyaltyError, Confirmation]]

       def transfer(sourceAccountID: UserAccountID, transferAccountID: UserAccountID, points: Long):F[Either[LoyaltyError, Confirmation]]
       
       def balance(account: UserAccountID): F[Long]
    }

    trait Batch[A]{
      def apply[F[_]: LoyaltyTransactions: Applicative]: F[A]
    }

   /*  atomic { new Batch[A] {
      def apply[F[_]: LoyaltyTransactions: Applicative]: F[A] = 
      LoyaltyTransactions[F].redeem(???) *>
      (LoyaltyTransactions[F].earn(???) |@|
      LoyaltyTransactions[F].transfer(???))((_,_))
     }} */
    

   //servicees
   trait LoyaltyProgram[F[_]] {
     type UserAccountID = java.util.UUID

     def atomic[A](batch:Batch[A]): F[A]

     def open: F[UserAccountID]

     def close(account: UserAccountID): F[UserAccountID]

     
   } 

   object LoyaltyProgram {

     private object internal {
       import java.util.UUID

      
    object Statement {
      final def point[A](a: => A): Statement[A] = Return[A](a)
    }
      sealed trait Statement[A] { self => 


      

      }
      final case class Return[A](value:A)                                                 extends Statement[A]
      final case class Balance[A](account:UUID)                                           extends Statement[Long]
      final case class Map[A0, A](statement: Statement[A0], f: A0 => A)                   extends Statement[A]
      final case class ZipWith[A, B, C](l: Statement[A],r: Statement[B], f: (A, B) => C)  extends Statement[C]
      final case class Earn(accountID: UUID, points: Long, purchase: Purchase)            extends Statement[Either[LoyaltyError, Confirmation]]
      final case class Redem(accountID: UUID, points: Long, reward: Reward)               extends Statement[Either[LoyaltyError, Confirmation]]
      final case class Transfer(sourceAccount: UUID, transferAccount:UUID,  points: Long) extends Statement[Either[LoyaltyError, Confirmation]]


      implicit val LoyaltyTransactionsInstance: LoyaltyTransactions[Statement] with Applicative[Statement] =
       new LoyaltyTransactions[Statement] with Applicative[Statement]{
        def point [A](a: => A): Statement[A] = ???
        def ap[A, B](fa: => Statement[A])(f: => Statement[A => B]): Statement[B] = ???
        def earn(accountID: UUID, points: Long, purchase: Purchase): Statement[Either[LoyaltyError, Confirmation]] = ???
        def redeem(accountID: UUID, points: Long, purchase: Reward): Statement[Either[LoyaltyError, Confirmation]] = ???
        def transfer(sourceAccount: UUID, transferAccount:UUID,  points: Long): Statement[Either[LoyaltyError, Confirmation]] = ???
        def balance(account: java.util.UUID): Statement[Long] = ???
       }
      
     }
     implicit val LoyaltyProgramIO: LoyaltyProgram[IO[Exception, ?]] = 
        new LoyaltyProgram[IO[Exception, ?]] {
          import internal._
          import java.sql.ResultSet
          import java.sql.{Statement => JStatement}
          type Query = String

          def interpret[A](statement: Statement[A]): (JStatement, ResultSet => IO[Exception, A]) = ???

          def atomic[A](batch: Batch[A]): IO[Exception, A] = {
            val statement: Statement[A] = batch[Statement]
            val (jstatement, processor) = interpret(statement)
            val resultSet = IO.syncException(jstatement.getResultSet())
            resultSet.flatMap(processor)
            ???
          }

          def balance(account: java.util.UUID): scalaz.zio.IO[Exception,Long] = ???
          def close(account: java.util.UUID): scalaz.zio.IO[Exception,java.util.UUID] = ??? 
          def open: scalaz.zio.IO[Exception,java.util.UUID] = ???
        }

   }
  }

  object fixpoint {

  object classic {
      sealed trait Json

      case object Null                     extends Json
      case class  Bool(boolean: Boolean)   extends Json
      case class  Str(string: String)      extends Json
      case class  Num(number: BigDecimal)  extends Json
      case class  Arr(array: List[Json])      extends Json
      case class  Obj(obj: Map[String, Json]) extends Json

      def renameField(old: String, newF: String): Json => Json = 
       (json: Json) =>  json match {
         case Null        => json 
         case Bool(value) => json
         case Str(value)  => json
         case Num(value)  => json
         case Arr(value)  => Arr(value.map(renameField(old, newF)))
         case Obj(map0)   =>  
          val map = map0.mapValues(renameField(old, newF))
          map.get(old).fold(json)(v2 => Obj(map + (newF -> v2) ))
       }
    }    

   
    object fixed {
      sealed trait JsonF[+A]
      case object Null extends JsonF[Nothing]
      case class Bool(value: Boolean) extends JsonF[Nothing]
      case class Str(value: String) extends JsonF[Nothing]
      case class Num(value: BigDecimal) extends JsonF[Nothing]
      case class Arr[A](value: List[A]) extends JsonF[A]
      case class Obj[A](value: Map[String, A]) extends JsonF[A]
      object JsonF {
        implicit val FunctorJsonF: Functor[JsonF] =
          new Functor[JsonF] {
            def map[A, B](fa: JsonF[A])(f: A => B): JsonF[B] = fa match {
              case Null => Null
              case Bool(v) => Bool(v)
              case Str(v) => Str(v)
              case Num(v) => Num(v)
              case Arr(v) => Arr(v.map(f))
              case Obj(map) => Obj(map.mapValues(f))
            }
          }
      }

      sealed trait ListF[+A, +B]
      case object Nil                         extends ListF[Nothing, Nothing]
      case class Cons[A, B](head: A, tail: B) extends ListF[A, B]

      object ListF {
        case object Nil                         extends ListF[Nothing, Nothing]
        case class Cons[A, B](head: A, tail: B) extends ListF[A, B]

        implicit def FunctorListF[A0]: Functor[ListF[A0, ?]] = 
           new Functor[ListF[A0, ?]] {
             def map[A, B](fa: ListF[A0, A])(f: A => B): ListF[A0, B] = fa match {
               case Nil              => Nil
               case Cons(head, tail) => Cons(head, f(tail))
             }
           }
           
       type ListR[A] = Fix[ListF[A, ?]]
       object ListR {
         def nil[A] : ListR[A] = Fix[ListF[A, ?]](Nil)
         def cons[A](head: A, tail: ListR[A]): ListR[A] = Fix[ListF[A, ?]](Cons(head, tail)) 
       }

       def foldRight[A, B](list: ListR[A], b: B)(f: (A, B) =>  B): B = 
          list.cata[B](_ match {
            case Nil        => b 
            case Cons(a, b) => f(a, b)
          })

      }
      final case class Fix[F[_]](unfix: F[Fix[F]]) { self =>
        def transformDown(f: F[Fix[F]] => F[Fix[F]])(implicit F: Functor[F]): Fix[F] =
          Fix[F](f(unfix).map(_.transformDown(f)))

        def transformUp(f: F[Fix[F]] => F[Fix[F]])(implicit F: Functor[F]): Fix[F] =
          Fix[F](f(unfix.map(_.transformUp(f))))
          
        def cata[A](f: F[A] => A): A = ???
      }

      type Json = Fix[JsonF]
      object Json {
        val null0: Json = Fix[JsonF](Null)
        def bool(v: Boolean): Json = Fix[JsonF](Bool(v))
        def str(v: String): Json = Fix[JsonF](Str(v))
        def num(v: BigDecimal): Json = Fix[JsonF](Num(v))
        def arr(v: List[Json]): Json = Fix[JsonF](Arr(v))
        def obj(v: Map[String, Json]): Json = Fix[JsonF](Obj(v))
      }
      // now we can add new types to the JsonF type with the Cofree
      // Cofree[JsonF, (JType, Int)]
      import Json._

      val Example =
        obj(Map(
          "address" -> obj(Map(
            "number" -> str("221B"),
            "street" -> str("Baker Street")
          )),
          "name" -> str("Sherlock Holmes")
        ))

      def renameField(old: String, newf: String): JsonF[Fix[JsonF]] => JsonF[Fix[JsonF]] =
        _ match {
          case Null => Null
          case j @ Bool(_) => j
          case j @ Str(_) => j
          case j @ Num(_) => j
          case j @ Arr(_) => j
          case j @ Obj(map) => map.get(old).fold(j)(v => Obj(map + (newf -> v)))
        }
      // 
      // def collectFields: Json => List[String] =
      //   (json: Json) => json match {
      //     case Null => Nil
      //     case Bool(value) => Nil
      //     case Str(value) => Nil
      //     case Num(value) => Nil
      //     case Arr(value) => value.flatMap(collectFields)
      //     case Obj(map0) => map0.keys.toList ++ map0.values.toList.flatMap(collectFields)
      //   }

      Example.transformDown(renameField("street", "street_name"))
    }


    sealed trait Json[+A]
    object Json {
      case object Null extends Json[Nothing]
      case class Bool[A](boolean: Boolean) extends Json[A]
      case class Str[A](string: String) extends Json[A]
      case class Num[A](number: BigDecimal) extends Json[A]
      case class Arr[A](array: List[A]) extends Json[A]
      case class Obj[A](obj: Map[String, A]) extends Json[A]

      implicit val TraverseJson: Traverse[Json] = ???
    }





  }

  object selectable {
    sealed trait Parser[+E, +A] { self =>
      import Parser._

      final def map[B](f: A => B): Parser[E, B] = Map[E, A, B](self, f)

      final def || [E1 >: E, B](that: Parser[E1, B]): Parser[E1, Either[A, B]] =
        Alternative(self, that)

      final def * : Parser[E, List[A]] = Repeat(self)

      final def ~ [E1 >: E, B](that: Parser[E1, B]): Parser[E1, (A, B)] = Zip(self, that)

      final def <~ [E1 >: E, B](that: Parser[E1, B]): Parser[E1, A] = (self ~ that).map(_._1)

      final def ~> [E1 >: E, B](that: Parser[E1, B]): Parser[E1, B] = (self ~ that).map(_._2)
    }
    object Parser {
      def fail[E](e: E): Parser[E, Nothing] = Fail(e)

      def char[E](e: E): Parser[E, Char] = Character(e)

      def select[E, A](cond: Parser[E, Boolean])(
        ifTrue: Parser[E, A], ifFalse: Parser[E, A]): Parser[E, A] =
        Select(cond, ifTrue, ifFalse)

      case class Fail[E](error: E) extends Parser[E, Nothing]
      case class Succeed[A](value: A) extends Parser[Nothing, A]
      case class Character[E](error: E) extends Parser[E, Char]
      case class Repeat[E, A](value: Parser[E, A]) extends Parser[E, List[A]]
      case class Alternative[E, A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, Either[A, B]]
      case class Zip[E, A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, (A, B)]
      case class Map[E, A0, A](value: Parser[E, A0], f: A0 => A) extends Parser[E, A]
      case class Select[E, A](
        condition: Parser[E, Boolean], ifTrue: Parser[E, A], ifFalse: Parser[E, A]) extends Parser[E, A]

      implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
        new Applicative[Parser[E, ?]] {
          def point[A](a: => A): Parser[E,A] = Succeed(a)
          def ap[A, B](fa: => Parser[E,A])(f: => Parser[E,A => B]): Parser[E,B] =
            Map[E, (A => B, A), B](Zip(f, fa), t => t._1(t._2))
        }
    }

    def compiler[E, A](parser: Parser[E, A]): String => Either[E, A] =
      (input: String) => {
        var index: Int = 0
        var error: E = null.asInstanceOf[E]
        var value: A = null.asInstanceOf[A]
        type Repr = () => Unit

        def compile0(parser: Parser[E, A]): Repr = ???

        compile0(parser)()

        if (error != null) Left(error) else Right(value)
      }
  }

  /* object hoas {
   /**
    FOAS:
     1. Embed DSL into programming language.
     2. With data structure (initial) or type class (final) 
   */ 
    
    trait Dsl[A] {
      // Problem 1: Dynamically type DSL
      // Problem 2: Reference values that have not been declared
      def int(v: Int): Int
      def plus(l: Int, r: Int): Int
      def minus(l: Int, r: Int): Int
      def times(l: Int, r: Int): Int
      def value(identifier: String): A 
      def let(identifier: String, value: A, body: A): A
    }
    object Dsl {
      def apply[A: Dsl] = implicitly[Dsl[A]]
    }
    implicit class DslSyntax[A](l: Int) {
      def + (r: Int)(implicit A: Dsl[A]): A = A.plus(l, r)
      def - (r: Int)(implicit A: Dsl[A]): A = A.minus(l, r)
      def * (r: Int)(implicit A: Dsl[A]): A = A.times(l, r)
    }


    def int[A: Dsl](v: Int) = Dsl[A].int(v)
    def value[A: Dsl](s: String) = Dsl[A].value(s)
    def let[A: Dsl](ident: String, value: A)(body: A): A = Dsl[A].let(ident, value, body) 
    
    def program[A: Dsl]: A = let("a", int(10))(value("a") * value("a"))

  } */


   object hoas2 {

     trait Dsl[Expr[_]] {
      // Problem 1: Dynamically type DSL
      // Problem 2: Reference values that have not been declared
      def int(v: Int): Expr[Int]
      def plus(l: Expr[Int], r: Expr[Int]):Expr[Int]
      def minus(l: Expr[Int], r: Expr[Int]): Expr[Int]
      def times(l: Expr[Int], r: Expr[Int]):Expr[Int]
      //def value(identifier: String): Expr[Int] 
      def let[A, B](value: Expr[A], body: Expr[A] => Expr[B]): Expr[B]
    }
    object Dsl {
      def apply[F[_]: Dsl] = implicitly[Dsl[F]]

      import scalaz.zio._
      implicit def DslIO[E]: Dsl[IO[E, ?]] = 
      new Dsl[IO[E, ?]] {
        def int(v: Int): IO[E, Int] = IO.now(v)
        def let[A, B](value: IO[E, A], body: IO[E, A] => IO[E, B]): IO[E, B] = value.flatMap(a => body(IO.now(a)))
        def minus(l: IO[E, Int], r: IO[E, Int]): IO[E, Int] = l.seqWith(r)(_ - _)
        def plus (l: IO[E, Int], r: IO[E, Int]): IO[E, Int] = l.seqWith(r)(_ + _)
        def times(l: IO[E, Int], r: IO[E, Int]): IO[E, Int] = l.seqWith(r)(_ * _)
      } 
    }
    implicit class DslIntSyntax[F[_]](l: F[Int]) {
      def + (r: F[Int])(implicit F: Dsl[F]): F[Int] = F.plus(l, r)
      def - (r: F[Int])(implicit F: Dsl[F]): F[Int] = F.minus(l, r)
      def * (r: F[Int])(implicit F: Dsl[F]): F[Int] = F.times(l, r)
    }


    def int[F[_]: Dsl](v: Int):F[Int] = Dsl[F].int(v)
   // def value[F[_]: Dsl](s: String) = Dsl[F].value(s)
    def let[F[_]: Dsl, A, B](value: F[A])(body: F[A] => F[B]): F[B] = Dsl[F].let(value, body) 
    
    def program[F[_]: Dsl]: F[Int] = 
                   let(int(1))(a =>
                    let(int(10))(b => a * a + b * b))

     
     
   }


  
}
