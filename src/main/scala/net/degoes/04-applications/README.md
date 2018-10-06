*Day 4 - Applications*
We'll build an application

You should not do something that the user can do themselves

John-s first version
```scala
 case class CrawlState[E, A](visited: Set[URL], crawl: Crawl[E, A])
  object CrawlState {
    def visited[E: Monoid, A: Monoid](visited: Set[URL]): CrawlState[E, A] =
      CrawlState(visited, mzero[Crawl[E, A]])
    def crawled[E, A](crawl: Crawl[E, A]): CrawlState[E, A] =
      CrawlState(mzero[Set[URL]], crawl)

    implicit def MonoidCrawlState[E: Monoid, A: Monoid]: Monoid[CrawlState[E, A]] =
      new Monoid[CrawlState[E, A]] {
        def zero = CrawlState(mzero[Set[URL]], mzero[Crawl[E, A]])
        def append(l: CrawlState[E, A], r: => CrawlState[E, A]) =
          CrawlState(l.visited |+| r.visited, l.crawl |+| r.crawl)
      }
  }
  //
  // EXERCISE 1
  //
  // Implement the `crawlIO` function.
  //
  def crawlIO[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {
      type Acc = IO[Nothing, CrawlState[E, A]]

      def loop(seeds: Set[URL], acc: CrawlState[E, A]): Acc =
        seeds.foldLeft[Acc](IO.now(acc |+| CrawlState.visited(seeds))) {
          case (acc, seed) =>
            acc.flatMap(acc =>
              getURL(seed).redeem(
                err => IO.now(acc),
                html => {
                  val seeds2 = extractURLs(seed, html).toSet.flatMap(router) -- acc.visited

                  for {
                    crawl2  <-  processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
                    acc     <-  loop(seeds2, acc |+| CrawlState.crawled(crawl2))
                  } yield acc
                }
              )
            )
        }

      loop(seeds, mzero[CrawlState[E, A]]).map(_.crawl)
    }

```


    //////////////////

    My unfinished version
```scala

     case class CrawlState[E, A](visited:Set[URL], crwal:Crawl[E,A])

      object CrawlState {

        def visited[E: Monoid, A:Monoid](visited:Set[URL]):CrawlState[E,A] = ???
        def crawl[E, A](crawl:Crawl[E,A]):CrawlState[E,A] = ???


        implicit def MonoidCrawlState[E: Monoid, A:Monoid]:Monoid[CrawlState[E, A]] = new Monoid[CrawlState[E, A]] {
          override def zero = CrawlState(mzero[Set[URL]], mzero[Crawl[E,A]])

          override def append(l: CrawlState[E, A], r: => CrawlState[E, A]): CrawlState[E, A] =
            CrawlState(l.visited |+| r.visited, l.crwal |+| r.crwal)
        }
      }

      //
      // EXERCISE 1
      //
      // Implement the `crawlIO` function.
      //

     def crawlIO[E: Monoid, A: Monoid](
        seeds     : Set[URL],
        router    : URL => Set[URL],
        processor : (URL, String) => IO[E, A]): IO[Exception, Crawl[E, A]] = {
        type Acc = IO[Exception, CrawlState[E, A]]

        def loop(seeds: Set[URL], acc:CrawlState[E,A]): Acc =
          seeds.foldLeft[Acc](IO.now(acc |+| CrawlState.visited(seeds))){
            case (acc: Acc, seed:URL) =>
              for {
                acc0  <- acc
    //            (visited0, crawl0) = acc0
                html  <- getURL(seed)
                seeds2   = extractURLs(seed, html).toSet.flatMap(router) -- acc0.visited
                crawl2 <- processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
                acc   <- loop(seeds2, acc0 |+| CrawlState.crawl(crawl2))
                //            a    <- processor(seed, html).attempt {
    //              case Left(e)  => Crawl(e, mzero[A])
    //              case Right(a) => Crawl(mzero[E], a)
    //            }
              } yield acc

          }

        loop(seeds, mzero[CrawlState[E, A]]).map(_.crawl)
        ???
      }
```
      --------------------------------
 
 Retry if getUrl fails
 
 `retry(Schedule.spaced(10.seconds).jittered && recurs(5) *> Schedule.identity])`

 Ex2 parallelize the application

1 way is to use `parTraverse`

Ex 2.5 :  Make GetUrl async

```scala
def getURLAsync(url: URL): IO[Exception, String] = {

    def createRunnable(k: Callback[Nothing, ExitResult[Exception, String]]): Runnable =
      new Runnable {
        override def run(): Unit =
          try {
            k(ExitResult.Completed(ExitResult.Completed(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)))
          } catch {
            case e: Exception => k(ExitResult.Completed(ExitResult.Failed(e)))
          }
      }

    def completeRunnable(promise: Promise[Exception, String]): IO[Nothing, Unit] = {
      for {
        exitResult <- IO.async[Nothing, ExitResult[Exception, String]](k => blockingPool.submit(createRunnable(k)))
        _          <- promise.done(exitResult)
      } yield ()
    }

    for {
      promise <- Promise.make[Exception, String]
      _       <- completeRunnable(promise).fork
      html    <- promise.get
    } yield html
  }
```

  * Final tagless *

