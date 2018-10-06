**Second Day**  Abstractions

 - Abstract algebras

  Every time u hear abstraction we should think type class and vice versa

  - Semigroup
```scala
    /**
      * Associativity law: append(append(a, b), c) = append(a, append(b, c))
      */
    trait Semigroup[A]{
      def append(l: => A, r: => A): A
    }
```
    Examples
```
    (Int, +) is a semigroup

    (String, ++) is a semigroup

    (List[A], ++) is a semigroup
```
```scala
/*
  Left identity append(a, zero) == a

  Right identity append(zero, a) == a
*/
trait Monoid[A] extends Semigroup[A]{
   def zero: A
}

```

The identity law for `Optio`n it means that `Some` will be `Some` and `None` will be `None`. It means it wont change the structure of the datatype

For `lists` it means that we preserve tha order of the elements

Identity law for the `Future` says that we cannot change a `Succes` into a `Failure` and vice versa

-- composition law
```scala
f: B => C
g: A => C
f.compose(g): A => C

fa.map( g andThen f) == fa.map(g).map(f)
```

- Functor composition
`Functor` `F` , `G`


`Option` is a functor
`List` is a functor
`Vector` is a functor
`Map[K, ?]` is a functor

They all have type `* => *`

`Future` is not a container but it's a Functor none the less

`Function[A0, ?]` is a functor

What is a functor?
It operates on something that has structure `* => *`

Functor: `F[_]`
`F` - A sum type with different terms
We could also say that `F` is a language and `F[A]` is a program in `F`  (that is, whose instructions are terms in the `F` `sum` type) that may halt or it may produce one or more `A` value

`map` - Change the output of a program by replacing the value computed by the program `a` by `f(a)`, where `f: A => B`

```scala
sealed trait Option[+A]
case object None extends Option[A]
case class Some[A](value:A) extends Option[A]

val option: Option[A] = Some(12)


sealed trait List[+A]
case object Nil extends List[A] -- halt instruction
case class Cons[A](head:A, tails:List[A]) extends List[A]
```

`List` is either a `Nil` or a `Cons`


Example of functor

```scala
sealed trait BankTransaction[+A]
case class Return[A](value: A)                                    extends BankTransaction
case class Deposit[A](amount: BigDecimal, next: BigDecimal => A)  extends BankTransaction
case class Withdraw[A](amount: BigDecimal, next: BigDecimal => A) extends BankTransaction

object BankTransaction{
 implicit val FunctorBankTransaction: Functor[BankTransaction] = 
 new Functor[BankTransaction]{
  def map[A, B](fa: BankTransaction[A])(f: A => B): BankTransaction[B] =
    fa match {
    case Return(value)          => Return(f(a))
    case Deposit(amount, next)  => Deposit(amount, (b: BigDecimal) => next(b).map(f))
    case Withdraw(amount, next) => Withdraw(amount, (b: BigDecimal) => next(b).map(f))

    }
  }
}

val t: BankTransaction[BigDecimal] = ???
val t2: BankTransaction[String] = t1.map(d => d.toString)

val supplier1: BankTransaction[BigDecimal] = ???
val supplier2: BankTransaction[BigDecimal] = ???
val allSuppliers: BankTransaction[(BigDecimal, BigDecimal)] = ???
```

-- Other example

```scala
trait HttpHeader
val Headers = Parser[Exception, List[HttpHeader]] = ???
val Body    = Parser[Exception, String] = ???
val Content = Parser[Exception, (List[HttpHeaders], String)] = ???
```
Can we combine this using `map`?
No because it's not powerful enough

- There are more powerful functors
To combine things we need a more powerful functor

```scala
trait Apply[F[_]] extends Functor[F] {
  def zip[A, B](l:F[A], r:F[B]): F[(A , B)]
}
```

-- Implement the syntax class for zip

```scala
implicit class ZipSyntax[F[_], A]
```

Lets implement zip for BankTransfer 

```scala
object BankTransaction{
 implicit val FunctorBankTransaction: Functor[BankTransaction] = 
  new Functor[BankTransaction] with Zip[BankTransaction]{
    def map[A, B](fa: BankTransaction[A])(f: A => B): BankTransaction[B] =
      fa match {
       case Return(value)          => Return(f(a))
       case Deposit(amount, next)  => Deposit(amount, (b: BigDecimal) => next(b).map(f))
       case Withdraw(amount, next) => Withdraw(amount, (b: BigDecimal) => next(b).map(f))

    }
    def zip[A, B](l:BankTransaction[A], r:BankTransaction[B]): BankTransaction[(A , B)] =
     val nested = l.map(a => r.map(b => (a, b)))
     flatten(nested)

    def flatten[C](v: BankTransaction[BankTransaction[C]]) = v match {
         case Return(t: BankTransaction) => t
         case Deposit(amount, next)      => Deposit(amount, (b: BigDecimal) => flatten(next(b)))
         case Withdraw(amount, next)     => Withdraw(amount, (b: BigDecimal) => flatten(next(b)))
    }
 }
}
```

-- Applicatives
```scala
trait Applicatives[F[_]]
```

Why isn't `Map[K, ?]` an `Applicative`?

Because we cannot implement `point` for `Map`


The true nature of the `Applicative` type class is to combine a `product` of `Programs` in a `Program` of `products`


* Monads
```scala
trait Monad[F[_]] extends Applicative[F] {
  def   ap[A, B](fa: F[A])(f:F[A =>   B]):F[B]
  def bind[A, B](fa: F[A])(f:  A => F[B]):F[B]
}
```
In procedural programming our program consists of a number of statements each of which consists of a value

```
PROC PROGRAMM
  A1 : = DO_X
  A2 : = DO_Y(A1)
  A3 : = DO_Z(A2)
  A1 : = DO_X

  IF(A3 < 2) THEN
    DO_W(A, A2, A3)
  ELSE
    DO_U(A1, A2, A3)
  END
END
```
The sequential nature of the program is represented by a Monad

Code posted by John De Goes
```scala
object parser {
  //
  // EXERCISE 1
  //
  // Implement all missing methods for parser.
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)]) { self =>
    def ~ [E1 >: E, B](that: Parser[E1, B]): Parser[E1, (A, B)] =
      self.flatMap(a => that.map(b => (a, b)))

    def ~> [E1 >: E, B](that: Parser[E1, B]): Parser[E1, B] =
      (self ~ that).map(_._2)

    def <~ [E1 >: E, B](that: Parser[E1, B]): Parser[E1, A] =
      (self ~ that).map(_._1)

    def map[B](f: A => B): Parser[E, B] =
      flatMap(f.andThen(Parser.point[B](_)))

    def flatMap[E1 >: E, B](f: A => Parser[E1, B]): Parser[E1, B] =
      Parser[E1, B](input =>
        self.run(input) match {
          case Left(e) => Left(e)
          case Right((input, a)) => f(a).run(input)
        })

    def orElse[E1 >: E, B](that: Parser[E1, B]): Parser[E1, Either[A, B]] = {
      val self1 = self.map(Left(_))
      val that1 = that.map(Right(_))

      type Return = Either[E1, (String, Either[A, B])]

      Parser(i => self1.run(i).fold[Return](_ => that1.run(i), Right(_)))
    }

    def filter[E1 >: E](e0: E1)(f: A => Boolean): Parser[E1, A] =
      Parser(input =>
        self.run(input) match {
          case Left(e) => Left(e)
          case Right((input, a)) => if (f(a)) Right((input, a)) else Left(e0)
        })

    def | [E1 >: E, A1 >: A](that: Parser[E1, A1]): Parser[E1, A1] =
      (self orElse (that)).map(_.merge)

    def rep: Parser[E, List[A]] =
      ((self.map(List(_)) | Parser.point[List[A]](Nil)) ~ rep).map(t => t._1 ++ t._2)

    def ? : Parser[E, Option[A]] = self.map(Some(_)) | Parser.point(None)
  }
  object Parser {
    def fail[E](e: E): Parser[E, Nothing] =
      Parser(input => Left(e))

    def point[A](a: => A): Parser[Nothing, A] =
      Parser(input => Right((input, a)))

    def maybeChar: Parser[Nothing, Option[Char]] =
      Parser(input =>
        if (input.length == 0) Right((input, None))
        else Right((input.drop(1), Some(input.charAt(0)))))

    def char[E](e: E): Parser[E, Char] =
      Parser(input =>
        if (input.length == 0) Left(e)
        else Right((input.drop(1), input.charAt(0))))

    def digit[E](e: E): Parser[E, Int] =
      for {
        c <- char(e)
        option = scala.util.Try(c.toString.toInt).toOption
        d <- option.fold[Parser[E, Int]](Parser.fail(e))(point(_))
      } yield d

    def literal[E](f: Char => E)(c0: Char): Parser[E, Char] =
      for {
        c <- char(f(c0))
        _ <- if (c != c0) Parser.point(f(0)) else Parser.point(())
      } yield c

    def whitespace: Parser[Nothing, Unit] =
      Parser(input => Right((input.dropWhile(_ == ' '), ())))
  }

  // [1,2,3,]
  sealed trait Error
  case class ExpectedLit(char: Char) extends Error
  case object ExpectedDigit extends Error

  val parser: Parser[Error, List[Int]] =
    for {
      _       <- Parser.literal(ExpectedLit)('[')
      digits  <- (Parser.digit(ExpectedDigit) <~ Parser.literal(ExpectedLit)(',')).rep
      _       <- Parser.literal(ExpectedLit)(']')
    } yield digits
}
```

* Foldable

```scala
  trait Foldabla[F[_]] {
    def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

    def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B
  }
```

 Implement foldable instance for lists

```scala
  implicit val FoldList : Foldable[List] =  new Foldable[List] {
      override def foldMap[A, B](fa: List[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
        case Nil     => mzero[B]
        case a :: as => f(a) |+| foldMap(as)(f)
      }

      override def foldRight[A, B](fa: List[A], z: => B)(f: (A, B) => B): B = fa match {
        case Nil     => z
        case a :: as => f(a, foldRight(as, z)(f))
      }
    }
```
--
`Traverse` is more powerful that `Foldable`
If you have a `List[Option[A]]` and you want to have a `Option[List[A]]` we can use the `traverse typeclass`

So basically it can take a `F[G[A]]` and trasform it into a `G[F[A]]`

```scala
trait Traverse[F[_]]{
  def sequence[G[_]: Applicative, A](fga:F[G[A]]): G[F[A]]
}
```

*Optics
Are a very powerful structure that allows to manipulate nested data
They allow us to make terms in `Product` or `Sum` first class

There are two types of optics corresponding to `Products` and `Sum` types

A `Lens` has two type parameters

`Lens[S, A]`

`S` - type of the super structure (its a product type)
`A` - the type of the sub structure

```scala
final case class Lens[S, A] (
 get: S => A,
 set: A => (S => S)
)
```

`Prisms` are used to access terms of a `Sum` type

```scala
final case class Prism[S, A](
 get: S => Option[A]
  set: A => S
)
```
We can compose `Lenses` with `Lenses` and `Prisms` with `Prisms`
we can also compose `Lenses` with `Prisms` to get `Optionals`

Day two, Recap

The essence of `Traverse` is like that of a `for loop`, so `itereating` while doing `effects`


  * New types

   How to avoid `implicit` conflicts

```scala
   implicit val IntSemigroup1: Semigroup[Int]= (l, r) => l + r
   implicit val IntSemigroup2: Semigroup[Int]= (l, r) => l * r
```
   create simple wrapper types known also as new types

```scala
  class Add(val:Int) extends AnyVal
  object Add{def apply(value: Int) = new Add(value)}
  class Mul(val:Int) extends AnyVal
  object Mul{def apply(value: Int) = new Mul(value)}

  implicit val IntSemigroup1: Semigroup[Add]= (l, r) => l .value+ r.value
  implicit val IntSemigroup2: Semigroup[Mul]= (l, r) => l.value * r.value
```