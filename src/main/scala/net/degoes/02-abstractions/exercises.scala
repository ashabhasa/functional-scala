// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.abstractions

import scalaz._
import Scalaz._
import net.degoes.abstractions.parser.Parser.point

import scala.language.higherKinds

object algebra {

  /**
    * Assoiciative lwa append(append(a, b), c) = append(a, append(b, c))
    */
  trait Semigroup[A] {
    def append(l: => A, r: => A): A
  }

  //
  // EXERCISE 1
  //
  // Define a semigroup for `NotEmpty` below.
  //
  case class NotEmpty[+A](head: A, tail: Option[NotEmpty[A]])

  implicit def NotEmptySemigroup[A]: Semigroup[NotEmpty[A]] =
    new Semigroup[NotEmpty[A]] {
      override def append(l: => NotEmpty[A], r: => NotEmpty[A]): NotEmpty[A] = l
    }
  val example1 = NotEmpty(1, None) |+| NotEmpty(2, None)

  //
  // EXERCISE 2
  //
  // Design a permission system for securing some resource, together with a
  // monoid for the permission data structure.
  //


  sealed abstract class Capability

  case object Read extends Capability

  case object Write extends Capability

  case object NoPermission extends Capability

  case class AccountId(value: String)

  case class Resource(value: String)


  case class Permission(value: Map[AccountId, Map[Resource, Set[Capability]]])

  implicit val MonoidPermission: Monoid[Permission] = new Monoid[Permission] {
    override def zero: Permission = Permission(mzero[Map[AccountId, Map[Resource, Set[Capability]]]])

    override def append(f1: Permission, f2: => Permission): Permission = Permission(f1.value |+| f2.value)
  }
  private val capabilities: Map[Resource, Set[Capability]] = Map(Resource("resource") -> Set(Read, Write))

  val example2 = mzero[Permission] |+| Permission(Map(AccountId("id1") -> capabilities))

  //
  // EXERCISE 3
  //
  // Define an instance of `Semigroup` for `(A, B)` when both `A` and
  // `B` form semigroups.
  //
  implicit def SemigroupTuple2[A: Semigroup, B: Semigroup]:Semigroup[(A, B)] = new Semigroup[(A, B)] {
    override def append(l: => (A, B), r: => (A, B)): (A, B) = ???
    //        (l._1 |+| r._1, l._2 |+| r._2)

  }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Monoid` for `NotEmpty` for any type `A`.
  //
  implicit def MonoidNotEmpty[A]: Monoid[NotEmpty[A]] = ???
}

object functor {
  //
  // EXERCISE 1
  //
  // Define an instance of `Functor` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val BTreeFunctor: Functor[BTree] =
    new Functor[BTree] {
      def map[A, B](fa: BTree[A])(f: A => B): BTree[B] =
        fa match {
          case Leaf(a)    => Leaf(f(a))
          case Fork(l, r) => Fork(map(l)(f), map(r)(f))
        }
    }


   implicit val OptionFunctor = new Functor[Option] {
     override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
       case None    =>  None
       case Some(a) => Some(f(a))
     }
   }
  //
  // EXERCISE 2
  //
  // Define an instance of `Functor` for `Nothing`.
  //
  implicit val NothingFunctor: Functor[Nothing] = new Functor[Nothing] {
    override def map[A, B](fa: Nothing)(f: A => B): Nothing = fa
  }

  //
  // EXERCISE 3
  //
  // Define an instance of `Functor` for Parser[E, ?].
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def ParserFunctor[E]: Functor[Parser[E, ?]] =
    new Functor[Parser[E, ?]] {
      def map[A, B](fa: Parser[E, A])(f: A => B): Parser[E, B] =
        Parser[E, B]((inout:String) =>  fa.run(inout) match {
          case Left(e)  => Left(e)
          case Right((inout, a)) => Right((inout, f(a)) )
        })
    }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Functor` for the following data type.
  //
  // ash: You cannot define it we need another function from B => A
  case class DataType[A](f: A => A)
  implicit val DataTypeFunctor: Functor[DataType] =
    new Functor[DataType] {
      def map[A, B](fa: DataType[A])(f: A => B): DataType[B] =
        ???
    }

  //
  // EXERCISE 5
  //
  // Define an instance of `Functor` for `FunctorProduct`.
  //
  case class FunctorProduct[F[_], G[_], A](l: F[A], r: G[A])
  implicit def FunctorProductFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorProduct[F, G, ?]] = new Functor[FunctorProduct[F, G, ?]]{
    override def map[A, B](fa: FunctorProduct[F, G, A])(f: A => B): FunctorProduct[F, G, B] =
      FunctorProduct(fa.l.map(f), fa.r.map(f))
  }

  //
  // EXERCISE 6
  //
  // Define an instance of `Functor` for `FunctorSum`.
  //
  case class FunctorSum[F[_], G[_], A](run: Either[F[A], G[A]])
  implicit def FunctorSumFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorSum[F, G, ?]] = new Functor[FunctorSum[F, G, ?]] {
    override def map[A, B](fa: FunctorSum[F, G, A])(f: A => B): FunctorSum[F, G, B] =
      FunctorSum(fa.run match {
        case Left(a)  => Left(a.map(f))
        case Right(a) => Right(a.map(f))
      })
  }

  //
  // EXERCISE 7
  //
  // Define an instance of `Functor` for `FunctorNest`.
  //
  case class FunctorNest[F[_], G[_], A](run: F[G[A]])

  implicit def FunctorNestFunctor[F[_] : Functor, G[_] : Functor]:
  Functor[FunctorNest[F, G, ?]] = new Functor[FunctorNest[F, G, ?]] {
    override def map[A, B](fga: FunctorNest[F, G, A])(f: A => B): FunctorNest[F, G, B] =
      FunctorNest[F, G, B](fga.run.map(_.map(f)))
  }

  def zip[E, A, B] (l: Parser[E, A], r: Parser[E, B]): Parser[E, (A, B)] =
    Parser[E, (A, B)]((input :String) => l.run(input) match {
      case Left(e)       => Left(e)
      case Right((_, a)) => r.run(input) match {
        case Left(e)       => Left(e)
        case Right((s, b)) => Right(s, (a, b))
      }
    })

  def zipOption[A, B](l: Option[A], r: Option[B]): Option[(A, B)] =
    (l, r) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _ => None
    }

  def zipWith[A, B, C](l: Option[A], r: Option[B])(f: ((A, B)) => C): Option[C] =
    zipOption(l, r).map(f)

  def zipList1[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, bs) =>
        zipList1(as, bs) ++ bs.map(b => (a, b))
      case (Nil, bs) => Nil
    }
  def zipList2[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, b :: bs) => ((a, b)) :: zipList2(as, bs)
      case _ => Nil
    }

  //
  // EXERCISE 8
  //
  // Define `Applicative` for `Option`.
  //
  implicit val OptionApplicative: Applicative[Option] =
    new Applicative[Option] {
      def point[A](a: => A): Option[A] = ???

      def ap[A, B](fa: => Option[A])(ff: => Option[A => B]): Option[B] =
        (fa, ff) match {
          case (None, _)          => None
          case (_, None)          => None
          case (Some(a), Some(f)) => Some(f(a))
        }
    }

  //
  // EXERCISE 9
  //
  // Implement `zip` in terms of the applicative composition using `|@|`.
  //
  // Bonus: Implement `ap2` in terms of `zip`.
  //
  val example1: Option[(Int, Int)] = (Option(3) |@| Option(5))((_, _))
  val example2: Option[(Int, String)] = zip(Option(3), Option("foo")) : Option[(Int, String)]
  def zip[F[_]: Applicative, A, B](l: F[A], r: F[B]): F[(A, B)] =
    (l |@| r)((_,_))
  def ap2[F[_]: Applicative, A, B](fa: F[A], fab: F[A => B]): F[B] =
    (fa |@| fab)((a, f) => f(a))

  //
  // EXERCISE 10
  //
  // Define an instance of `Applicative` for `Parser[E, ?]`.
  //
  implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
    new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E,A] =
        Parser((input:String) => Right(input -> a))

      def ap[A, B](fa: => Parser[E,A])(f: => Parser[E, A => B]): Parser[E,B] =
        Parser[E,B] ((input:String) => fa.run(input) match {
          case Left(e) => Left(e)
          case Right((input, a)) => f.run(input) match {
            case Left(e) => Left(e)
            case Right((input, f)) => Right(input -> f(a))
          }
        })
    }

  implicit def ApplicativeList: Applicative[List] = new Applicative[List] {
      def point[A](a: => A): List[A] = List(a)

      def ap[A, B](fa: => List[A])(f: => List[A => B]): List[B] = (fa, f) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (x::xs, f::fs) => f(x) :: ap(xs)(fs)

      }
    }

  //
  // EXERCISE 11
  //
  // Define an instance of `Monad` for `BTree`.
  //
  //  sealed trait BTree[+A]
  //  case class Leaf[A](a: A) extends BTree[A]
  //  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val MonadBTree: Monad[BTree] =
    new Monad[BTree] {
      def point[A](a: => A): BTree[A] =
        Leaf(a)

      def bind[A, B](fa: BTree[A])(f: A => BTree[B]): BTree[B] =
        fa match {
          case Leaf(a) => f(a)
          case Fork(l, r) => Fork[B](bind(l)(f), bind(r)(f))
        }
    }

  //
  // EXERCISE 12
  //
  // Define an instance of `Monad` for `Parser[E, ?]`.
  //
//  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def MonadParser[E]: Monad[Parser[E, ?]] =
    new Monad[Parser[E, ?]] {
      def point[A](a: => A): Parser[E,A] = Parser[E,A](input => Right(input -> a))

      def bind[A, B](fa: Parser[E,A])(f: A => Parser[E,B]): Parser[E,B] =
        Parser[E, B](input => fa.run(input) match {
          case Left(e)       => Left(e)
          case Right((s, a)) => f(a).run(s)
        })
    }

  implicit val MonadOption: Monad[Option] = new Monad[Option] {
    override def point[A](a: => A): Option[A] = Some(a)

    override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case None    => None
      case Some(a) => f(a)
    }
  }
}

object parser {

  //
  // EXERCISE 1
  //
  // Implement all missing methods for parser.
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)]) { self =>
    def ~ [E1 >: E, B](that: Parser[E1, B]): Parser[E1, (A, B)] =
      self.flatMap(a => that.map(b => (a, b)))
      // Parser[E1, (A, B)](input => self.run(input) match {
      //   case Left(e)           => Left(e)
      //   case Right((input, a)) => that.run(input) match {
      //     case Left(e)            => Left(e)
      //     case Right((input , b)) => Right((input, (a, b)))
      //   }
      // })

    def ~> [E1 >: E, B](that: => Parser[E1, B]): Parser[E1, B] = (self ~ that).map(_._2)

    def <~ [E1 >: E, B](that: => Parser[E1, B]): Parser[E1, A] = (self ~ that).map(_._1)

    def map[B](f: A => B): Parser[E, B] = flatMap(f.andThen(Parser.point[B](_)))

    def flatMap[E1 >: E, B](f: A => Parser[E1, B]): Parser[E1, B] = 
       Parser[E1, B](input => self.run(input) match {
         case Left(e)  => Left(e)
         case Right((input, a)) => f(a).run(input)
       })

    def orElse[E1 >: E, B](that: => Parser[E1, B]): Parser[E1, Either[A, B]] = 
      Parser[E1, Either[A, B]](input => self.run(input) match {
        case Right((input, a)) => Right((input, Left(a)))
        case Left (e)          => that.run(input) match {
          case Left(e)           => Left(e)
          case Right((input, b)) => Right((input, Right(b))) 
        }
      })

      def orElse1[E1 >: E, B](that: => Parser[E1, B]): Parser[E1, Either[A, B]] = {
        val self1: Parser[E1, Either[A, B]] = self.map(Left(_))
        val that1: Parser[E1, Either[A, B]] = that.map(Right(_))
        type Return = Either[E1, (String, Either[A, B])]
       Parser(input => self1.run(input).fold[Return](_ => that1.run(input), Right(_)))

      }

    def | [E1 >: E, A1 >: A](that: => Parser[E1, A1]): Parser[E1, A1] =
      (self orElse that).map(_.merge)

    def repeat:Parser[E, List[A]] = ((self.map(List(_)) | point(Nil)) ~ repeat).map(t => t._1 ++ t._2)

    def ? : Parser[E, Option[A]] = self.map(Some(_)) | Parser.point(None)
  }

  object Parser {
    def fail[E](e: E): Parser[E, Nothing] = Parser(input => Left(e))

    def point[A](a: A): Parser[Nothing, A] = Parser(input => Right(input -> a))

    def char[E](e: E): Parser[E, Char] = Parser(input =>
      if (input.length == 0) Left(e)
      else Right((input.drop(1), input.charAt(0))))

    def digit[E](e: E): Parser[E, Int] =
      for {
        c <- char(e)
        option = scala.util.Try(c.toString.toInt).toOption
        d <- option.fold[Parser[E, Int]](Parser.fail(e))(point)
      } yield d

    def literal[E](f: Char => E)(c0:Char): Parser[E, Char] =
      for {
        c <- char(f(c0))
        _ <- if(c != c0) Parser.point(f(0)) else Parser.point(())
      } yield c

    def whitespace:Parser[Nothing, Unit] = Parser(input => Right((input.dropWhile(_ == ' '), ())))
  }

  sealed trait Error
//  case object ExpectedChar extends Error
  case class ExpectedLiteral(char:Char) extends Error
  case object ExpectedDigit extends Error
//  case object ExpectedBrace extends Error

  val parser : Parser[Error, List[Int]] =
    for {
      _      <- Parser.literal(ExpectedLiteral)('[')
      digits <- (Parser.digit(ExpectedDigit) <~ Parser.literal(ExpectedLiteral)(',')).repeat
      _      <- Parser.literal(ExpectedLiteral)(']')
    } yield digits


}

object foldable {

  trait Foldable[F[_]] {
     def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

     def foldRight[A, B](fa: F[A], z: => B)(f: (A, B) => B): B
   }

  implicit val FoldList : Foldable[List] =  new Foldable[List] {
    def foldMap[A, B](fa: List[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
      case Nil     => mzero[B]
      case a :: as => f(a) |+| foldMap(as)(f)
    }

    def foldRight[A, B](fa: List[A], z: => B)(f: (A, B) => B): B = fa match {
      case Nil     => z
      case a :: as => f(a, foldRight(as, z)(f))
    }
  }

  //
  // EXERCISE 1
  //
  // Define an instance of `Foldable` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val FoldableBTree: Foldable[BTree] =
    new Foldable[BTree] {
      def foldMap[A, B](fa: BTree[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
        case Leaf(a)    => f(a)
        case Fork(l, r) => foldMap(l)(f) |+| foldMap(r)(f)
      }

      def foldRight[A, B](fa: BTree[A], z: => B)(f: (A, B) => B): B =
       fa match {
         case Leaf(a)    => f(a, z)
         case Fork(l, r) => 
         val  newZero = foldRight(l, z)(f)
         foldRight(r, newZero)(f)
       }
    }

  //
  // EXERCISE 2
  //
  // Try to define an instance of `Foldable` for `A => ?`.

  //Ash:There is no benefit in implementing this one
  implicit def FunctionFoldable[A0]: Foldable[A0 => ?] =  ???

  //
  // EXERCISE 3
  //
  // Define an instance of `Traverse` for `BTree`.
  //
  implicit val TraverseBTree: Traverse[BTree] =
    new Traverse[BTree] {
      def traverseImpl[G[_], A, B](fa: BTree[A])(f: A => G[B])(implicit F: Applicative[G]): G[BTree[B]] =
        fa match {
          case Leaf(a)    => F.map(f(a))(Leaf(_))
          case Fork(l, r) => (traverseImpl(l)(f) |@| traverseImpl(r)(f))(Fork(_,_))
        }
    }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Traverse` for `Parser[E, ?]`.
  //
  // PArser is not Traversable since it's not a data type but a function.
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def TraverseParser[E]: Traverse[Parser[E, ?]] = new Traverse[Parser[E, ?]] {
    override def traverseImpl[G[_], A, B](fa: Parser[E, A])(f: A => G[B])(implicit ev: Applicative[G]): G[Parser[E, B]] = ???
  }
}

object optics {



  sealed trait Country
  object Country {
    val usa: Prism[Country, Unit] =
      Prism[Country, Unit]({
        case USA => Some(())
        case _ => None
      }, _ => USA)
  }
  case object USA    extends Country
  case object UK     extends Country
  case object Poland extends Country

  case class Org(name: String, address: Address, site: Site)
  object Org {
    val site: Lens[Org, Site] =
      Lens[Org, Site](_.site, l => _.copy(site = l))
  }
  case class Address(
    number: String,
    street: String,
    postalCode: String,
    country: Country)
  case class Site(
    manager: Employee,
    address: Address,
    employees: Set[Employee])
  object Site {
    val manager: Lens[Site, Employee] =
      Lens[Site, Employee](_.manager, m => _.copy(manager = m))
  }
  case class Employee(
    name: String,
    dob: java.time.Instant,
    salary: BigDecimal,
    address: Address)
  object Employee {
    val salary: Lens[Employee, BigDecimal] =
      Lens[Employee, BigDecimal](_.salary, s => _.copy(salary = s))
  }

  lazy val org: Org = ???

  lazy val org2 =
    org.copy(site =
      org.site.copy(manager = org.site.manager.copy(
        salary = org.site.manager.salary * 0.95
      ))
    )

  //
  // EXERCISE 1
  //
  // Implement the `>>>` method of `Lens`.
  //
  final case class Lens[S, A](
    get: S => A,
    set: A => (S => S)
  ) { self =>
    def >>> [B](that: Lens[A, B]): Lens[S, B] =
      Lens[S, B](
        get = self.get.andThen(that.get),
        set =  b => s => ???/*self.set(that.get(self.get(s)(s)))*/)

    final def update(f: A => A): S => S =
      (s: S) => self.set(f(self.get(s)))(s)
  }

  //
  // EXERCISE 2
  //
  // Create a version of `org2` that uses lenses to update the salaries.
  //
  val org2_lens: Org =
    (Org.site >>> Site.manager >>> Employee.salary).
      update(_ * 0.95)(org)

  //
  // EXERCISE 3
  //
  // Implement `>>>` for `Prism`.
  //
  final case class Prism[S, A](
    get: S => Option[A],
    set: A => S) { self =>
    def >>> [B](that: Prism[A, B]): Prism[S, B] =
      Prism[S, B](s => get(s).flatMap(that.get), that.set.andThen(self.set))

    final def select(implicit ev: Unit =:= A): S =
      set(ev(()))
  }

  //
  // EXERCISE 4
  //
  // Implement `_Left` and `_Right`.
  //
  def _Left[A, B]: Prism[Either[A, B], A] =
    Prism[Either[A, B], A](_.fold(Some(_), _ => None), Left(_))
  def _Right[A, B]: Prism[Either[A, B], B] =
    Prism[Either[A, B], B](_.fold(_ => None, Some(_)), Right(_))

}
