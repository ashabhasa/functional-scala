// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.applications

import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._
import scalaz._
import Scalaz._
import net.degoes.applications.exercises.getURL

import scala.collection.JavaConverters._

object exercises extends App {

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
                                     seeds: Set[URL],
                                     router: URL => Set[URL],
                                     processor: (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {
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
                  crawl2 <- processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
                  acc <- loop(seeds2, acc |+| CrawlState.crawled(crawl2))
                } yield acc
              }
            )
          )
      }

    loop(seeds, mzero[CrawlState[E, A]]).map(_.crawl)
  }

  //This version while more clean it's less efficient, because we will visit some
  // urls twice in some cases
  // Using the Ref we now
  def crawlIOTraverse[E: Monoid, A: Monoid](
                                             seeds: Set[URL],
                                             router: URL => Set[URL],
                                             processor: (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {

    def processUrl(ref: Ref[CrawlState[E, A]], seed: URL, html: String): IO[Nothing, Unit] = {
      for {
        visited <- ref.get.map(_.visited)
        crawl <- processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
        _ <- ref.update(_ |+| CrawlState.crawled(crawl))
        seeds <- IO.now(extractURLs(seed, html).toSet.flatMap(router) -- visited)
        _ <- loop(seeds, ref)
      } yield ()

    }

    def loop(seeds: Set[URL], ref: Ref[CrawlState[E, A]]): IO[Nothing, Unit] =
      ref.update(_ |+| CrawlState.visited(seeds)) *>
        IO.traverse(seeds) { seed =>
          getURL(seed).redeem(_ => IO.unit,
            html => processUrl(ref, seed, html))
        }.void

    for {
      ref <- Ref(mzero[CrawlState[E, A]])
      _ <- loop(seeds, ref)
      state <- ref.get
    } yield state.crawl

  }


  //
  // EXERCISE 2
  //
  // Implement a version of the `crawlIO` function that works in parallel.
  //
  def crawlIOPar[E: Monoid, A: Monoid](
                                        seeds: Set[URL],
                                        router: URL => Set[URL],
                                        processor: (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {

    def processUrl(ref: Ref[CrawlState[E, A]], seed: URL, html: String): IO[Nothing, Unit] = {
      for {
        visited <- ref.get.map(_.visited)
        crawl <- processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
        _ <- ref.update(_ |+| CrawlState.crawled(crawl))
        seeds <- IO.now(extractURLs(seed, html).toSet.flatMap(router) -- visited)
        _ <- loop(seeds, ref)
      } yield ()

    }

    def loop(seeds: Set[URL], ref: Ref[CrawlState[E, A]]): IO[Nothing, Unit] =
      ref.update(_ |+| CrawlState.visited(seeds)) *>
        IO.parTraverse(seeds) { seed =>
          getURL(seed).redeem(_ => IO.unit,
            html => processUrl(ref, seed, html))
        }.void

    for {
      ref <- Ref(mzero[CrawlState[E, A]])
      _ <- loop(seeds, ref)
      state <- ref.get
    } yield state.crawl

  }

  //
  // EXERCISE 3
  //
  // Implement a version of the `crawlIOPar` that can be tested without having
  // to interact with the real world.
  //
  def crawlIO2[E: Monoid, A: Monoid](
                                      seeds: Set[URL],
                                      router: URL => Set[URL],
                                      processor: (URL, String) => IO[E, A],
                                      getURL: URL => IO[Exception, String] = getURL(_)): IO[Exception, Crawl[E, A]] = {

    def processUrl(ref: Ref[CrawlState[E, A]], seed: URL, html: String): IO[Nothing, Unit] = {
      for {
        visited <- ref.get.map(_.visited)
        crawl <- processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
        _ <- ref.update(_ |+| CrawlState.crawled(crawl))
        seeds <- IO.now(extractURLs(seed, html).toSet.flatMap(router) -- visited)
        _ <- loop(seeds, ref)
      } yield ()

    }

    def loop(seeds: Set[URL], ref: Ref[CrawlState[E, A]]): IO[Nothing, Unit] =
      ref.update(_ |+| CrawlState.visited(seeds)) *>
        IO.parTraverse(seeds) { seed =>
          getURL(seed).redeem(_ => IO.unit,
            html => processUrl(ref, seed, html))
        }.void

    for {
      ref <- Ref(mzero[CrawlState[E, A]])
      _ <- loop(seeds, ref)
      state <- ref.get
    } yield state.crawl

  }

  //
  // EXERCISE 4
  //
  // Create a type class for printLine and readLine
  //

  trait Console[F[_]] {
    def printLine(line: String): F[Unit]

    def readLine: F[String]
  }


  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F

    implicit def ConsoleIO[E]: Console[IO[E, ?]] = new Console[IO[E, ?]] {
      override def printLine(line: String): IO[E, Unit] = IO.sync(println(line))

      override def readLine: IO[E, String] = IO.sync(scala.io.StdIn.readLine)
    }
  }


  //
  // EXERCISE 5
  //
  // Implement helper methods that work with any F[_] that supports the
  // Console effect
  //
  def printLine[F[_] : Console](line: String): F[Unit] = Console[F].printLine(line)

  def readLine[F[_] : Console]: F[String] = Console[F].readLine

  //
  // EXERCISE 6
  //
  // Create an instance of the Console type class for IO[E, ?] for any E
  // by using IO.sync and the scala functions println
  // and scala.io.StdIn.readLine
  //


  //
  // EXERCISE 7
  //
  // Create an instance of the Random type class for IO[E, ?] for any E
  // by using IO.sync and  scala.util.Random.nextInt
  //

  trait Random[F[_]] {
    def nextInt(max: Int): F[Int]
  }

  object Random {
    def apply[F[_]](implicit F: Random[F]): Random[F] = F

    implicit def Random[F]: Random[IO[F, ?]] = new Random[IO[F, ?]] {
      override def nextInt(max: Int): IO[F, Int] = IO.sync(scala.util.Random.nextInt(max))
    }
  }

  def nextInt[F[_] : Random](max: Int): F[Int] = Random[F].nextInt(max)


  //
  // EXERCISE 8
  //
  // Create a game that is polymorphic in the effect type F[_], requiring
  // only the capability to perform Console and Random effects
  // by using IO.sync and  scala.util.Random.nextInt
  //

  def myGame[F[_] : Console : Random : Monad]: F[Unit] =
    for {
      _     <- printLine[F]("Welcome to purely functional Hangman: ")
      name  <- getName[F]
      word  <- chooseWord[F]
      state  = State(name, Set[Char](), word)
      _     <- render[F](state)
      _     <- gameLoop[F](state)
    } yield ()


  case class State(name: String, guesses: Set[Char], word: String) {
    def failures: Int = (guesses -- word.toSet).size
  }

  def playerLost(state: State): Boolean =
    state.guesses.size > state.word.length * 2

  def playerWon(state: State): Boolean =
    (state.word.toSet -- state.guesses).size == 0

  def gameLoop[F[_] : Console : Monad](state: State): F[State] = {

    def aaaaaa(state: State, choice:Char): F[State] = {
      if (playerLost(state))     printLine[F](s"You have been Hanged!! Sorry ${state.name}, please play again! ") *> state.point[F]
      else if (playerWon(state)) printLine[F](s"Congratulations! You are a winner, ${state.name} !") *> state.point[F]
      else (if (state.word.toSet.contains(choice))
             printLine[F](s"You guessed correctly!Keep at it, ${state.name} !")
            else  printLine[F](s"You guessed correctly! Keep at it, ${state.name} !"))*> gameLoop(state)
    }

    for {
      choice <- getChoice[F]
      state <- state.copy(guesses = state.guesses + choice).point[F]
      state <- render[F](state) *> aaaaaa(state, choice)
    } yield state

  }

  def render[F[_] : Console](state: State): F[Unit] = {
    val word = state.word.toList.map(c =>
      if (state.guesses.contains(c)) s" $c " else "   ").mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

//    val guesses = " Guesses: " + state.guesses.mkString(", ")
    val guesses = state.guesses.mkString(start = "Guesses: ", sep = ", ", end = "")
//    val text = word + "\n" + line + "\n\n" + guesses + "\n"
    val text = s"$word\n$line\n\n$guesses"
    printLine[F](text)
  }

  def getChoice[F[_] : Console : Monad]: F[Char] =
    for {
      guess <- readLine[F].flatMap { line0 =>
        val line = line0.trim
        line.headOption match {
          case Some(choice) if choice.isLetter && line.length == 1 => choice.point[F]
          case _ => printLine[F]("Please enter one character: ") *> getChoice[F]
        }
      }
    } yield guess

  def getName[F[_] : Console : Apply]: F[String] =
    printLine[F]("Please enter your name: ") *> readLine[F]


  def chooseWord[F[_] : Random : Functor]: F[String] = nextInt[F](Dictionary.length).map(Dictionary.apply)


  val Dictionary = List("aaron", "abelian", "ability", "about", "abstract", "abstract", "abstraction", "accurately",
    "adamek", "add", "adjacent", "adjoint", "adjunction", "adjunctions", "after", "after", "again",
    "ahrens", "albeit", "algebra", "algebra", "algebraic", "all", "all", "allegories", "almost",
    "already", "also", "american", "among", "amount", "ams", "an", "an", "analysis", "analytic",
    "and", "and", "andre", "any", "anyone", "apart", "apologetic", "appears", "applicability",
    "applications", "applications", "applied", "apply", "applying", "applying", "approach", "archetypical",
    "archetypical", "are", "areas", "argument", "arising", "aristotle", "arrowsmorphism", "article", "arxiv13026946",
    "arxiv13030584", "as", "as", "aspect", "assumed", "at", "attempts", "audience", "august", "awodey", "axiom",
    "axiomatic", "axiomatized", "axioms", "back", "barr", "barry", "basic", "basic", "be", "beginners", "beginning",
    "behind", "being", "benedikt", "benjamin", "best", "better", "between", "bicategories", "binary", "bodo", "book",
    "borceux", "both", "both", "bourbaki", "bowdoin", "brash", "brendan", "build", "built", "but", "but", "by", "called",
    "cambridge", "can", "cardinal", "carlos", "carnap", "case", "cases", "categorial", "categorical", "categorical",
    "categories", "categories", "categorification", "categorize", "category", "category", "cats", "catsters", "central",
    "certain", "changes", "charles", "cheng", "chicago", "chiefly", "chopin", "chris", "cite", "clash", "classes", "classical", "closed", "coend", "coin", "colimit", "colin", "collection", "collections", "comparing", "completion", "composed", "composition", "computational", "computer", "computing", "concept", "concepts", "concepts", "conceptual", "concrete", "confronted", "consideration", "considers", "consistently", "construction", "constructions", "content", "contents", "context", "context", "contexts", "continues", "continuous", "contrast", "contributed", "contributions", "cooper", "correctness", "costas", "count", "course", "cover", "covering", "current", "currently", "david", "decategorification", "deducing", "define", "defined", "defining", "definition", "definitions", "der", "derives", "described", "describing", "description", "descriptions", "detailed", "development", "dictum", "did", "different", "dimensions", "directed", "discovered", "discovery", "discuss", "discussed", "discussion", "discussion", "disparage", "disservice", "do", "does", "driving", "drossos", "duality", "dvi", "each", "easy", "ed", "edges", "edit", "edition", "eilenberg", "eilenbergmaclane", "elementary", "elementary", "elements", "elementwise", "elephant", "ellis", "else", "embedding", "embodiment", "embryonic", "emily", "end", "enthusiastic", "equations", "equivalence", "equivalences", "equivalences", "etc", "etcs", "eugenia", "even", "eventually", "everything", "evident", "example", "examples", "examples", "except", "excused", "exist", "exists", "exposure", "expressed", "expressiveness", "extension", "extra", "f", "fact", "fair", "families", "far", "feeds", "feeling", "finds", "finite", "first", "flourished", "focuses", "folklore", "follows", "fong", "for", "for", "force", "forced", "foremost", "form", "formalizes", "formulated", "forthcoming", "found", "foundation", "foundations", "foundations", "francis", "free", "freyd", "freydmitchell", "from", "functions", "functor", "functor", "functors", "fundamental", "further", "gabrielulmer", "general", "general", "generalized", "generalizes", "geometry", "geometry", "george", "geroch", "get", "gift", "give", "given", "going", "goldblatt", "grandis", "graph", "gray", "grothendieck", "ground", "group", "groupoid", "grp", "guide", "göttingen", "had", "handbook", "handful", "handle", "harper", "has", "have", "he", "here", "here", "herrlich", "higher", "higher", "higherdimensional", "highlevel", "hilberts", "his", "historical", "historically", "history", "history", "holistic", "holland", "home", "homomorphisms", "homotopy", "homotopy", "horizontal", "horst", "however", "i", "idea", "ideas", "ieke", "if", "if", "illustrated", "important", "in", "in", "inaccessible", "inadmissible", "include", "includes", "including", "indeed", "indexes", "infinite", "informal", "initial", "innocent", "instance", "instead", "instiki", "interacting", "internal", "intersection", "into", "introduce", "introduced", "introduces", "introducing", "introduction", "introduction", "introductory", "intuitions", "invitation", "is", "isbell", "isbn", "isomorphisms", "it", "it", "its", "itself", "ive", "j", "jaap", "jacob", "jiri", "johnstone", "joy", "jstor", "just", "kan", "kant", "kapulkin", "kashiwara", "kind", "kinds", "kleins", "kmorphisms", "ktransfors", "kℕ", "la", "lagatta", "lane", "language", "large", "last", "later", "later", "latest", "lauda", "lawvere", "lawveres", "lead", "leads", "least", "lectures", "led", "leinster", "lemma", "lemmas", "level", "library", "lifting", "likewise", "limit", "limits", "link", "linked", "links", "list", "literally", "logic", "logic", "logically", "logische", "long", "lurie", "mac", "maclane", "made", "major", "make", "manifest", "many", "many", "mappings", "maps", "marco", "masaki", "material", "mathct0305049", "mathematical", "mathematical", "mathematician", "mathematician", "mathematics", "mathematics", "mathematicsbrit", "may", "mclarty", "mclartythe", "means", "meet", "membership", "methods", "michael", "misleading", "mitchell", "models", "models", "moerdijk", "monad", "monadicity", "monographs", "monoid", "more", "morphisms", "most", "mostly", "motivation", "motivations", "much", "much", "music", "must", "myriads", "named", "natural", "natural", "naturally", "navigation", "ncategory", "necessary", "need", "never", "new", "nlab", "no", "no", "nocturnes", "nonconcrete", "nonsense", "nontechnical", "norman", "north", "northholland", "not", "notes", "notes", "nothing", "notion", "now", "npov", "number", "object", "objects", "obliged", "observation", "observing", "of", "on", "one", "online", "oosten", "operads", "opposed", "or", "order", "originally", "other", "other", "others", "out", "outside", "outside", "over", "packing", "page", "page", "pages", "paper", "paradigm", "pareigis", "parlance", "part", "particularly", "pdf", "pedagogical", "people", "perfect", "perhaps", "perpetrated", "perspective", "peter", "phenomenon", "phil", "philosopher", "philosophers", "philosophical", "philosophy", "physics", "physics", "pierce", "pierre", "played", "pleasure", "pointed", "poset", "possession", "power", "powered", "powerful", "pp", "preface", "prerequisite", "present", "preserving", "presheaf", "presheaves", "press", "prevail", "print", "probability", "problem", "proceedings", "process", "progression", "project", "proof", "property", "provide", "provides", "ps", "publicly", "published", "pure", "purloining", "purpose", "quite", "quiver", "rails", "rather", "reader", "realizations", "reason", "recalled", "record", "references", "reflect", "reflects", "rejected", "related", "related", "relation", "relation", "relations", "representable", "reprints", "reproduce", "resistance", "rests", "results", "reveals", "reverse", "revised", "revisions", "revisions", "rezk", "riehl", "robert", "role", "row", "ruby", "running", "same", "samuel", "saunders", "say", "scedrov", "schanuel", "schapira", "school", "sci", "science", "scientists", "search", "see", "see", "sense", "sep", "sequence", "serious", "set", "set", "sets", "sets", "sheaf", "sheaves", "shortly", "show", "shulman", "similar", "simon", "simple", "simplified", "simply", "simpson", "since", "single", "site", "situations", "sketches", "skip", "small", "so", "society", "some", "some", "sometimes", "sophisticated", "sophistication", "source", "space", "speak", "special", "specific", "specifically", "speculative", "spivak", "sprache", "stage", "standard", "statements", "steenrod", "stephen", "steps", "steve", "still", "stop", "strecker", "structural", "structuralism", "structure", "structures", "students", "study", "studying", "subjects", "such", "suggest", "summer", "supported", "supports", "symposium", "syntax", "tac", "taken", "talk", "tannaka", "tautological", "technique", "tend", "tends", "term", "terminology", "ternary", "tex", "textbook", "textbooks", "texts", "than", "that", "the", "the", "their", "their", "them", "themselves", "then", "theorem", "theorems", "theorems", "theoretic", "theoretical", "theories", "theorist", "theory", "theory", "there", "there", "these", "these", "they", "thinking", "this", "this", "thought", "through", "throughout", "thus", "time", "to", "tom", "tone", "too", "toolset", "top", "topics", "topoi", "topological", "topology", "topologyhomotopy", "topos", "topos", "toposes", "toposes", "transactions", "transformation", "transformations", "trinitarianism", "trinity", "triple", "triples", "trivial", "trivially", "true", "turns", "two", "two", "type", "typically", "uncountable", "under", "under", "understood", "unification", "unify", "unions", "univalent", "universal", "universal", "universes", "university", "use", "used", "useful", "using", "usual", "van", "variants", "various", "vast", "vect", "versatile", "video", "videos", "viewpoint", "views", "vol", "vol", "vs", "was", "way", "we", "wealth", "web", "wells", "were", "what", "when", "when", "where", "which", "while", "whole", "whose", "will", "willerton", "william", "willingness", "with", "witticism", "words", "working", "working", "would", "writes", "xfy", "xfygzxgfz", "xy", "yoneda", "york1964", "youtube")


  //
  // EXERCISE 9
  //
  // Extend the game in IO[E, ?]
  //
  val myGameIO: IO[Nothing, Unit] = myGame[IO[Nothing, ?]]



  //
  // EXERCISE 10
  //
  // Create a test data structure that can contain a buffer of lines
  // (to be read from the console), a log of the output (that has been
  // written to the console), and a list of "random" numbers
  //

  object  MyGameTestData {

    case class TestData(output:List[String], input:List[String], random:List[Int]){
      def render = "OUTPUT:\n"
    }


    // EXERCISE 11
    //
    // Implment the following dynamically-created instance.
    //
    //
    type GameEffects[F[_]] = Console[F] with Random[F] with Monad[F]
    final case class TestIO[+E, +A](run: IO[E, A])

//    def createTestInstance[E](ref: Ref[TestData]): GameEffects[IO[E, ?]] =
//      new Console[IO[E, ?]] with Random[IO[E, ?]] with Monad[IO[E, ?]] {
//        def point[A](a: => A): IO[E,A] = IO.point(a)
//        def bind[A, B](fa: IO[E,A])(f: A => IO[E,B]): IO[E,B] = fa.flatMap(f)
//        def printLine(line: String): IO[E,Unit] = ref.update(s => s.copy(output = line :: s.output)).void
//        def readLine: IO[E,String] = ref.modify(s => (s.input.head, s.copy(input = s.input.drop(1))))
//        def nextInt(max: Int): IO[E,Int] = ref.modify(s => (s.random.head, s.copy(random = s.random.drop(1))))
//      }

    def createTestInstance[E](ref: Ref[TestData]): GameEffects[TestIO[E, ?]] =
      new Console[TestIO[E, ?]] with Random[TestIO[E, ?]] with Monad[TestIO[E, ?]] {
        def point[A](a: => A): TestIO[E,A] = TestIO(IO.point(a))
        def bind[A, B](fa: TestIO[E,A])(f: A => TestIO[E,B]): TestIO[E,B] = TestIO(fa.run.flatMap(f.andThen(_.run)))
        def printLine(line: String): TestIO[E,Unit] = TestIO(ref.update(s => s.copy(output = line :: s.output)).void)
        def readLine: TestIO[E,String] = TestIO(ref.modify(s => (s.input.head, s.copy(input = s.input.drop(1)))))
        def nextInt(max: Int): TestIO[E,Int] = TestIO(ref.modify(s => (s.random.head, s.copy(random = s.random.drop(1)))))
      }


    // EXERCISE 12
    //
    // Implement the following runner function, which will run the game using
    // the provided set of input test data
    //
    //

    def testGame(testData: TestData):IO[Nothing, TestData] =
      for {
        ref  <- Ref(testData)

        _    <- {
          implicit val inst = createTestInstance(ref)
          myGame[TestIO[Nothing, ?]]
        }.run
        data <- ref.get
      }yield data

    val tst = TestData(output = List(), input = List("John", "a", "r", "o", "n"), random = List(0))
    val GameTest1: IO[Nothing, String] = testGame(tst).map(_.render)

  }



  final case class Crawl[E, A](error: E, value: A) {
    def leftMap[E2](f: E => E2): Crawl[E2, A] = Crawl(f(error), value)

    def map[A2](f: A => A2): Crawl[E, A2] = Crawl(error, f(value))
  }

  object Crawl {
    implicit def CrawlMonoid[E: Monoid, A: Monoid]: Monoid[Crawl[E, A]] =
      new Monoid[Crawl[E, A]] {
        def zero: Crawl[E, A] = Crawl(mzero[E], mzero[A])

        def append(l: Crawl[E, A], r: => Crawl[E, A]): Crawl[E, A] =
          Crawl(l.error |+| r.error, l.value |+| r.value)
      }
  }

  import io.lemonlabs.uri._

  final case class URL private(parsed: io.lemonlabs.uri.Url) {

    final def relative(page: String): Option[URL] =
      scala.util.Try(parsed.path match {
        case Path(parts) =>
          val whole = parts.dropRight(1) :+ page.dropWhile(_ == '/')

          parsed.withPath(UrlPath(whole))
      }).toOption.map(new URL(_))

    def url: String = parsed.toString

    override def hashCode(): Int = url.hashCode

    override def equals(that: Any): Boolean = that match {
      case a: URL => this.url == a.url
      case _ => false
    }
  }

  object URL {

    import io.lemonlabs.uri._

    def apply(url: String): Option[URL] =
      scala.util.Try(AbsoluteUrl.parse(url)).toOption match {
        case None => None
        case Some(parsed) => Some(new URL(parsed))
      }
  }

  def getURL(url: URL): IO[Exception, String] =
    IO.syncException(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)

  private val blockingPool = java.util.concurrent.Executors.newCachedThreadPool()

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
        _ <- promise.done(exitResult)
      } yield ()
    }

    for {
      promise <- Promise.make[Exception, String]
      _ <- completeRunnable(promise).fork
      html <- promise.get
    } yield html
  }

  //    IO.syncException(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)

  def extractURLs(root: URL, html: String): List[URL] = {
    val pattern = "href=[\"\']([^\"\']+)[\"\']".r

    scala.util.Try({
      val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toList

      for {
        m <- matches
        url <- URL(m).toList ++ root.relative(m).toList
      } yield url
    }).getOrElse(Nil)
  }


  object test {
    val Home = URL("http://scalaz.org").get
    val Index = URL("http://scalaz.org/index.html").get
    val ScaladocIndex = URL("http://scalaz.org/scaladoc/index.html").get
    val About = URL("http://scalaz.org/about").get

    val SiteIndex =
      Map(
        Home -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        Index -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        ScaladocIndex -> """<html><body><a href="index.html">Home</a><a href="/about">About</a></body></html>""",
        About -> """<html><body><a href="home.html">Home</a><a href="http://google.com">Google</a></body></html>"""
      )


    val getURL: URL => IO[Exception, String] =
      (url: URL) => SiteIndex.get(url).fold[IO[Exception, String]](IO.fail(new Exception(s"Could not connect to $url")))(IO.now)

    //    val ScalazRouter : URL => Set[URL]  = url => if (url.parsed.apexDomain == Some("scala.org")) Set(url) else Set()
    val ScalazRouter: URL => Set[URL] = url => Set(url)

    val Processor: (URL, String) => IO[Unit, List[(URL, String)]] =
      (url, string) => IO.now(List(url -> string))
  }

  // run crawler
//  def run(args: List[String]): IO[Nothing, ExitStatus] =
//    (for {
//      //      _ <- putStrLn("Hello World!")
//      crawl <- crawlIO2(Set(test.Home), test.ScalazRouter, test.Processor, test.getURL)
//
//            _ <- putStrLn(crawl.value.mkString("\n"))
//    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))


  //run game
  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- myGame
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))

  // run test game
//  def run(args: List[String]): IO[Nothing, ExitStatus] =
//    (for {
//      p <- MyGameTestData.GameTest1
//      _ <- putStrLn(p)
//    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))


}
