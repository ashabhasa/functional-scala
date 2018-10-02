 1 - Day one - Scala essentials
  
Unit is a special type.
It contains only one value ()
def foo()  // empty parameter list
val thunk : () => Int = () => 42

Since it contains a single value it communicates no information
You can think of it as the absence of information


Nothing
Is the set that contains nothing, this means we can have no value of type Nothing.
It's very useful in functional programming, ex
``` Either[Nothing, Int] ```
The scala compiler knows that it cannot construct the left hand side of this type
```scala
val either = Either[Nothing, Int] = Right(42)
val int = either match {
case Left(n)  => n
case Right(i) => i
}
```

Types compose in two primary ways

1. Product composition (Product Types)
2. Sum composition (Sum types)

Any type composed from `sums` and `products` is called an algebraic data type (ADT)

* Product types
```
A = {a1, a2, a3}
|A| = 3
B = {b1, b2}
|B| = 2
A * B = {(a1, b1), (a2, b1), (a3, b1), (a1, b2), (a2,b2), (a3,b2)}

|A*B| = |A|*|B| = 6
```

Another way to think about `product composition` is:
To think of this sets as dimensions

A
^
|
|
|
|
|-----------------> B

One way to create product types in scala are tuples

```scala

(1, "true", true):(Int, String, Boolean)
val t: (Int, String, Boolean)
t._1 :Int
t._2:String
t._3:Boolean
```
Another way is to use case classes

```scala

case class Person(name:String, age:Int, isMinor:Boolean)
val p:Person = ???
p.name
p.age
p.isMinor
```

The type `Unit ()`

```math
Unit = { () }
|Unit| = 1

A * 1 ~ A
```

* Sum types
```
A = {a1, a2, a3}
|A| = 3
B = {b1, b2}
|B| = 2
A + B = {(A, a1), (A, a2), (A, a3), (B, b2), (B, b2), (B, b2)}

|A+B| = |A| + |B| = 5
```
In scala we can use Either  to represent Sum types

```scala
Either[String, Int]
Either[String, String]
```

Another more generic way is to use sealed traits:

```scala
// sealed abstract class for binary compatibility
sealed trait Food
case object Cake extends Food
case object Chicken extends Food
case object Cheese extends Food
case object Apple extends Food


| Either[Unit, A] | = 1 + |A|
|Option[A]|         = 1 + |A|

Either[Nothing, A] ~ Option[A]

type Option[A] = Either[Unit, A]
```

* Modeling domain models as ADT
```scala
type Url = String
def getUrl(url:Url):String = ???
```
- Solution

```scala
case class Url(protocol:Protocol,
               location:Location,
               port:Option[Int]
               hash: Option[Hash],
               path:Option[Path],
               query:Option[Query])
               {
               def toString: String = ???
               }
```
This is not good modeling because String is too big a set to model urls and not all strings are valid urls


* Functions




* Polymorfic

```scala
val square : Int => Int = x * x
val squareD: Double => Double = x * x

def squareP[A]: A => A = a * a
```
- Identity function

```scala
def identity[A](a:A):A = a

identity("")
identity(123)
identity(true)
```

identity is not a function it is a method, Scala uses some `magic` to convert a method to a function

```scala

def isOdd(i:Int) = i % 2 == 1
List[Int].filter(isOdd) ~ List[int].filter(x => isOdd(x))

```

Another workaround is to create a trait

```scala
trait Identity{
def apply[A](a:A):A = a
}

val identity:Identity = new Identity
```

Or using an object

```scala
object Identity{
  def apply[A](a:A):A = a
}

val identity:Identity = new Identity
```

* Higherkinded types

```scala
case class Person(name:String, age:Int)
```

The data defined above introduces in scope two different things

```scala
object Person {
  def apply(name: String, age: Int): Person
}

type Person
```

but its ok because they exist in different universes

That is why the following does not break because they are defined in different namespaces

```scala
trait Food
val Food = 1
```

A `type constructor` allows us to build types

```scala
val pers: Person = Person("Arber", 33)
```

The `data construsctor` takes two parameters whereas the `type constructor` doesn't take any arguments.
To `Person` `data constructor` we provide a string and an int and we get back a `Person`

For the `Person` `type constructor` we give no parameters

Person type constructor has kind  `*`

`*` = {x : x is a type in the Scala type system}

```scala

sealed trait Tree[+A]
case class Leaf[A](a: A) extends Tree[A]
case class Fork[A](left: Tree[A], right: Tree[A]) extends Tree[A]
```


This is a `sum` type, it means I can build a `Tree` either from a `Leaf` or from a `Fork`

```scala
val tree: Tree[Int] = Fork(Leaf(1), Fork(Leaf(2),Leaf(3)))
```

`Tree[A]` has one type parameter whereas `Person` had none, they both are `type constructors`,

`Person` is a type but `Tree[A]` is not.

We have two `data constructors` for Tree

```scala
Leaf: A => Tree[A]
Fork: (Tree[A], Tree[A]) => Tree[A]
```
 Tree Type constructor
 `* => *`  is a function that when given one type will give back another type
 ex: `Tree[Int]`

 This notation `(* => *)` describes the structure of the `Tree` `type constructor` we usually say that Tree has `kind` `* => *`


 Examples

```scala
 trait Foo[A, B ,C]
 [[*, *, *], *, *]
 trait Foo2[A[X, Y, Z], B, C]
 val foo2: Foo2[Tuple3, String, Boolean]

 trait Foo3[A[X, Y, Z], B, C[X, Y]]
```

 Note that in the last definition the `X`, `Y`  in type `A` are different from the ones in type `C` that is why to avoid any confusion it's better to write

```scala 
  trait Foo3[A[_, _, _], B, C[_, _]]
```
* Partial type application
`Map[*, *]`

```scala
type MapString = Map(String, *)
```

- Universal vs Existential types ( find out some more)

* Type classes
Every type class consists of a set of three things

- types
- operations on values of those types
- laws governing the behavior of the operations

Every type class instance is an implementation of the type class for a set of a given types

Dont put implicit in top level


Scalaz  encoding


*Day one recap*

What is a type?

What is a function?

What are the three properties of a function ?

Why are they important?

What are polymorphic functions?

Scala 2.x doesn't have any polymorphic functions we use methods to emulate them


What are ADT?

One of the goals of func prog is to make invalid data irrepresentable using ADTs?
This allows us to make this invalid data illegal in our application.

Higher kinds

`(* => *) => * `
this is a higher order kind


* Partial type application
 kind-projectors is plugin that greatly simplifies 
