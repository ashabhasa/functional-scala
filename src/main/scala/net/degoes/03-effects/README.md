*Day 3*

```scala
object notepad {

  def println(line:String):Unit = ???
  def readLine: String  = ???

  sealed trait Program[A]{ self =>

    import Program.{Return,WriteLine,ReadLine}

//    def map[B](f: A => B):Program[B]  = self match {
//      case Return(thunk)         => Return(() => f(thunk()))
//      case WriteLine(line, next) => WriteLine(line, next.map(f))
//      case ReadLine(next)        => ReadLine(input => next(input).map(f))
//    }

    def map[B](f: A => B): Program[B] =
      flatMap(f andThen (Program.point(_)))

    def zip[B](that: Program[B]): Program[(A, B)] =
      for {
        a <- self
        b <- that
      } yield (a, b)


    def flatMap[B](f: A => Program[B]): Program[B] = self match {
      case Return(thunk)         => f(thunk())
      case WriteLine(line, next) => WriteLine(line, next.flatMap(f))
      case ReadLine(next)        => ReadLine(input => next(input).flatMap(f))
    }
  }

  object Program {

    final case class Return[A](values: () => A) extends Program[A]
    final case class WriteLine[A](line: String, next: Program[A]) extends Program[A]
    final case class ReadLine[A](next: String => Program[A]) extends Program[A]

    def point[A] (a: => A): Program[A] = Return(() => a)
    def writeLine(line:String):Program[Unit] = WriteLine(line, point(()))
    def readLine:Program[String] = ReadLine(point(_))

  }


  import Program.{point,readLine,writeLine}


  val prog = for {
    _    <- writeLine("Hello World")
    _    <- writeLine("What is your name?")
    name <- readLine
    _    <- writeLine("Hello, "+ name + ", it is good to meet you!")
  } yield name

}
```

We are going to write an `interpreter` for our `program`. 
Its going to be `total` but not `deterministic`

```scala
  def interpret[A](program: Program[A]): A = program match {
    case Program.Return(thunk)         => thunk()
    case Program.WriteLine(line, next) => println(line); interpret(next)
    case Program.ReadLine(next)        => interpret(next(scala.io.StdIn.readLine()))
  }
```

  * ZIO

  `IO[E, A]` -> fail with some `E` or compute an `A`

  `IO.now` strictly evaluates its argument
  `IO.point` isn't strict


  Error recovery -> when we want to recover from an error
  we can do this using the `attempt` method
  or we can use `reedem`

  or using `orElse`


  Concurrency


  what are the drawbacks of

 ```scala
    def myPar[E, A, B](left: IO[E, A], right: IO[E, B]): IO[E, (A, B)] =
      for {
        l  <- left.fork
        r  <- right.fork
        lv <- l.join
        rv <- r.join

      } yield (lv, rv)
```
 If left takes a long time but the right one fails immediately when we join we
 have to wait a long time for the completion of the left fiber only to fail


 `IO.parTraverse(fibsToCompute)()` is an `effectful` for loop (done in parallel)


   * Resource handling

   The classical way to handle resources
   
  ```scala
      def processData(file:String):Unit = {
        var handle  =null
        try {
             handle = openFile(file)
          var data   = readFile(handle)
        } finally {
          if(handle != null) closeHandle(handle)
        }
      }
  ```

 The worst problem is that this works only for blocking io


 What happens with this?

 ```scala
 {
   try {
     try throw new Error("Primary error")
     finally throw new Error("Secondary Error")
   } catch {
     case e: Error => println(e)
   }
   println("ended")
 }
 ```

 It will print:
 ```
 java.lang.Error: Secondary Error
 ended
 ```

 The first exception was lost


 In Zio we use Bracket

 ```scala
  val acquire: IO[Exception, FileHandle]               = ???
  val release: FileHandle => IO[Nothing, Unit]         = ???
  val use    : FileHandle => IO[Exception, Result]     = ???
  acquire.bracket(release)(use)
```
     
`acquire` is uninterruptibile
*Beware* `finalizers` should be infallibile we express this using this type `IO[Nothing, Unit]`

*Promise*
 `Promise[E, A]` is a variable that can be set exactly once

* ZIO run time system (RTS)

What happens when the main functions throws an exception ?

```

```

*Schedules*

`Schedule[A, B]`

A `schedule` consumes values of type `A` and produces values of type `B`

```scala
  def makeRequest(input:Request): IO[Exception, Response] = ???

  val io2 = makeRequest(Request())

  // we are retrying this 3 times
  io2 orElse io2 orElse io2

  io2.retry(schedule)
  io2.retryOrElse(schedule, ___)

  io2.repeat(schedule)
  io2.repeatOrElse(schedule, __)
```
`Schedule` provides a flexible means for doing re try

`IO.repeat` feeds in the values from the succesful IO
`IO.retry` feeds in the errors from failed IO


*Day 3 recap*

We can't call `println` or `readline` in a function because its very hard to reason about the intersection of functional code and non functional code.

In functional we can use equational reasoning replacing expressions with its values.
Testing and refactoring is very easy in functional programs where as in non functional program are more difficult to test and refactor and reason abpout.

And understanding how a program that mixes functional and non functional code is not easy

`Schedules` are used  to `retry` failing actions or `repeat` successful actions

