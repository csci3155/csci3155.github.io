// Modern Software Practices at Twitter
// By Alex Beal (alexlbeal@gmail.com, https://twitter.com/beala)
// Talk Delivered On November 10th, 2016
// Abstract: Most mainstream industry languages today are only superficially different, 
//   adding features to a mainly procedural core, such as an object system or limited type 
//   system. For me, the practical lesson of 3155 was that taking an interest in language
//   design can open up a whole new world of possibilities in terms of safety,
//   expressiveness, and modularity. At Twitter we use Scala, which is a language that
//   incorporates many of these modern features. In this talk, I'll take you on a tour
//   of some of the expressive abstractions that we use everyday on the job that have
//   their roots in modern language design.

object Option {
  case class User(screenName: String, realName: String)
  
  val jackUserId = 0
  val userDb: Map[Int, User] = Map(jackUserId -> User("@jack", "Jack"))
  
  def getUser(userId: Int): User = {
    if(userDb.contains(userId)) {
      userDb(userId)
    } else {
      // What goes here?
      // null
      // throw new RuntimeException(s"User with user id $userId doesn't exist.")
      ???
    }
  }

  // Instead, let's write a small library to solve this.
  //
  // Start by representing the problem domain as data.
  // 
  // We'll call the data type Option.
  // Option is a value that may or may not exist.

  sealed trait Option[+A]
  case class Some[A](a: A) extends Option[A]       // Value exists.
  case object None         extends Option[Nothing] // Value doesn't exist.

  // Constructing optional values.

  val aUser: Option[User] = Some(User("@Horse_ebooks", "Horse ebooks"))
  val noUser: Option[User] = None

  // Redefining getUser with our Option library.
  
  def getUserOption(userId: Int): Option[User] = {
    if(userDb.contains(userId)) {
      Some(userDb(userId))
    } else {
      None
    }
  }

  // Now we need some way of combining and manipulating Option values.

  // One thing we'd like to do is apply a function to the value
  // but only if it exists.

  def apply[A,B](o: Option[A])(f: A => B): Option[B] = {
    o match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  // Example usage:
  apply[Int, Int](Some(1))(_ + 1) // -> Some(2)
  apply[Int, Int](None)   (_ + 1) // -> None

  def describeName(userId: Int): Option[String] = {
    apply(getUserOption(userId)){
      case User(screenName, realName) => 
      s"$screenName's real name is $realName"
    }
  }

  // Aside: Notice how close this is to normal function application.
  def applyFunc[A,B](a: A)        (f: A => B): B = f(a)
  //  apply    [A,B](o: Option[A])(f: A => B): Option[B]

  // We need one more combinator.
  // To motivate the why, see what happens when we define a function
  // that takes a User and returns their first tweet.

  case class Tweet(text: String)
  val tweetDb: Map[User, Seq[Tweet]] = Map(
    User("@jack", "Jack") -> Seq(Tweet("just setting up my twttr"))
  )

  def getFirstTweet(user: User): Option[Tweet] = {
    if(tweetDb.contains(user)) {
      Some(tweetDb(user).head)
    } else {
      None
    }
  }

  // A => Option[B]
  // B => Option[C]
  // A => Option[Option[C]]
  def getFirstTweetFromId1(user: Int): Option[Option[Tweet]] = {
    apply(getUserOption(user)){ user =>
      getFirstTweet(user)
    }
  }

  // Similar to apply, but the function f is allowed to fail.
  // If anything fails, the entire chain fails.

  def andThen[A,B](o: Option[A])(f: A => Option[B]): Option[B] = {
    o match {
      case Some(a) => f(a)
      case None => None
    }
  }

  andThen(Some(1))(i => Some(i + 1)) // -> Some(2)
  andThen(Some(1))(i => None       ) // -> None
  andThen(None: Option[Int])(i => Some(i + 1)) // -> None
  andThen(None: Option[Int])(i => None       ) // -> None

  def getFirstTweetFromId2(user: Int): Option[Tweet] = {
    andThen(getUserOption(user)){ user =>
      getFirstTweet(user)
    }
  }

  // With only apply and andThen we've actually built up a
  // pretty flexible library for working with values that might not exist.
  // We can apply functions to values that might not exist, and we can
  // even apply functions that might fail to values that might not exist.
  // 
  // But we need one more thing: at some point we need to extract the value
  // at the end of the program.

  def printFirstTweet1(user: Int): Unit = {
    println(getFirstTweetFromId2(user))
  }

  // handleOption allows us to supply functions which deal with
  // both cases: Some or None
  def handleOption[A,B](o: Option[A])(ifNone: => B)(ifSome: A => B): B = {
    o match {
      case Some(a) => ifSome(a)
      case None => ifNone
    }
  }

  handleOption(Some(1))("No integer.")( i =>       (i + 1).toString) // -> "2"
  handleOption(None)   ("No integer.")((i: Int) => (i + 1).toString)
  //   -> "No interger."

  def printFirstTweet2(user: Int): Unit = {
    println {
      handleOption(getFirstTweetFromId2(user))("User not found."){
        case Tweet(text) => text
      }
    }
  }

  def getOrElseOption[A](o: Option[A], ifNone: => A): A = {
    handleOption(o)(ifNone)(identity)
  }

  //  apply[A,B](o: Option[A])(f: A => B): Option[B]
  def map  [A,B](o: Option[A])(f: A => B): Option[B] = {
    handleOption[A, Option[B]](o)(None)(a => Some(f(a)))
  }

  //  andThen[A,B](o: Option[A])(f: A => Option[B]): Option[B]
  def flatMap[A,B](o: Option[A])(f: A => Option[B]): Option[B] = {
    handleOption[A, Option[B]](o)(None)(a => f(a))
  }

  // handleOption is usually called fold.
  //
  // If you squint, it's actually similar to a fold you're
  // more familiar with: fold over Seq/List.
  //
  // In both cases we supply functions to handle each of the possible
  // cases. 
  // For Options that Some and None.
  // For List that's Cons and Nil.
  def fold[A,B](o: Option[A])(ifNone: => B)(ifSome: A => B): B = {
    handleOption(o)(ifNone)(ifSome)
  }

}

object Try {

  // Try is an Option that can carry around an Exception
  // in the None/failure case.
  sealed trait Try[+A]
  case class Return[A](a: A) extends Try[A]
  case class Throw(e: Exception) extends Try[Nothing]

  case class User(screenName: String, realName: String)
  
  val jackUserId = 0
  val alexUserId = 1
  val userDb: Map[Int, User] = Map(
    jackUserId -> User("@jack", "Jack"),
    alexUserId -> User("@beala", "Alex")
  )
  
  def getUser(userId: Int): Try[User] = {
    if(userDb.contains(userId)) {
      Return(userDb(userId))
    } else {
      Throw(new RuntimeException(s"No user with id: $userId"))
    }
  }

  def map[A,B](e: Try[A])(f: A => B): Try[B] = {
    e match {
      case Return(a) => Return(f(a))
      case Throw(e) => Throw(e)
    }
  }

  def describeName(userId: Int): Try[String] = {
    map(getUser(userId)){
      case User(screenName, realName) => 
        s"$screenName's real name is $realName"
    }
  }

  case class Tweet(text: String)
  val tweetDb: Map[User, Seq[Tweet]] = Map(
    User("@jack", "Jack") -> Seq(Tweet("just setting up my twttr"))
  )

  def getFirstTweet(user: User): Try[Tweet] = {
    if(tweetDb.contains(user)) {
      Return(tweetDb(user).head)
    } else {
      Throw(new RuntimeException(s"$user doesn't have any tweets"))
    }
  }

  def flatMap[A,B](e: Try[A])(f: A => Try[B]): Try[B] = {
    e match {
      case Return(a) => f(a)
      case Throw(t) => Throw(t)
    }
  }

  def getFirstTweetFromId(user: Int): Try[Tweet] = {
    flatMap(getUser(user)){ user =>
      getFirstTweet(user)
    }
  }

  def handleEither[A,B](t: Try[A])(isThrow: Exception => B)(isReturn: A => B): B = {
    t match {
      case Return(a) => isReturn(a)
      case Throw(e) => isThrow(e)
    }
  }

  def tryCatch[A](t: Try[A])(f: Exception => A): A = {
    handleEither(t)(f)(identity _)
  }

  def printFirstTweet(user: Int): Unit = {
    tryCatch {
      map(getFirstTweetFromId(user))(println)
    }{
        case e: Exception => println(s"Exception caught: $e")
    }
  }

  trait Either[+A,+B]
  case class Left[A](a: A) extends Either[A, Nothing]
  case class Right[B](b: B) extends Either[Nothing, B]
}

object Futures {
  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global

  // A Future[Int] is an Int that's produced asynchronously.
  def asyncInt: Future[Int] = Future {
    Thread.sleep(5000)
    10
  }
  // When asyncInt is called, the work is offloaded to another thread,
  // and a reference to the yet-to-be-computed result is returned immediately.

  // Key idea: Future[A] is an A that may or may not exist at this point in time.

  // Can we apply a function to a yet-to-be-computed value? Sure we can!

  def asyncIntPlusOne: Future[Int] = asyncInt.map(i => i + 1)

  case class User(screenName: String, realName: String)
  
  val jackUserId = 0
  val alexUserId = 1
  val userDb: Map[Int, User] = Map(
    jackUserId -> User("@jack", "Jack"),
    alexUserId -> User("@beala", "Alex")
  )

  def getUser(userId: Int): Future[User] = {
    if(userDb.contains(userId)) {
      Future(userDb(userId))
    } else {
      Future.failed(new RuntimeException(s"No user with id: $userId"))
    }
  }

  case class Tweet(text: String)
  val tweetDb: Map[User, Seq[Tweet]] = Map(
    User("@jack", "Jack") -> Seq(Tweet("just setting up my twttr"))
  )

  // flatMap(a: Future[A])(f: A => Future[B]): Future[B]
  def getFirstTweet(user: User): Future[Tweet] = {
    if(tweetDb.contains(user)) {
      Future(tweetDb(user).head)
    } else {
      Future.failed(new RuntimeException(s"$user doesn't have any tweets"))
    }
  }

  def getFirstTweetById(userId: Int): Future[Tweet] = {
    getUser(userId).flatMap(getFirstTweet)
  }

  def getFirstTweet2(user: User): Future[Tweet] = {
    Future(tweetDb.contains(user)).flatMap { hasUser: Boolean =>
      if(hasUser) {
        Future(tweetDb(user).head)
      } else {
        Future.failed(new RuntimeException(s"$user doesn't have any tweets"))
      }
    }
  }

}

object Monad {
  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global

  trait Monad[M[_]] {
    def map[A,B]    (m: M[A])(f: A => B   ): M[B]
    def flatMap[A,B](m: M[A])(f: A => M[B]): M[B]
    def unit[A]     (a: A): M[A]
  }

  val OptionMonad = new Monad[Option.Option] {
    def map[A,B](m: Option.Option[A])(f: A => B): Option.Option[B] = {
      Option.map(m)(f)
    }
    def flatMap[A,B](m: Option.Option[A])(f: A => Option.Option[B]): Option.Option[B] = {
      Option.flatMap(m)(f)
    }
    def unit[A](a: A): Option.Option[A] = Option.Some(a)
  }

  val TryMonad = new Monad[Try.Try] {
    def map[A,B](m: Try.Try[A])(f: A => B): Try.Try[B] = {
      Try.map(m)(f)
    }
    def flatMap[A,B](m: Try.Try[A])(f: A => Try.Try[B]): Try.Try[B] = {
      Try.flatMap(m)(f)
    }
    def unit[A](a: A): Try.Try[A] = Try.Return(a)
  }

  val FutureMonad = new Monad[Future] {
    def map[A,B](m: Future[A])(f: A => B): Future[B] = {
      m.map(f)
    }
    def flatMap[A,B](m: Future[A])(f: A => Future[B]): Future[B] = {
      m.flatMap(f)
    }
    def unit[A](a: A): Future[A] = Future(a)
  }

  val ListMonad = new Monad[List] {
    def map[A,B](m: List[A])(f: A => B): List[B] = {
      m.map(f)
    }
    def flatMap[A,B](m: List[A])(f: A => List[B]): List[B] = {
      m.flatMap(f)
    }
    def unit[A](a: A): List[A] = List(a)
  }

  def monadCompose[A,B,C,M[_]](g: B => M[C])(f: A => M[B])(m: Monad[M]): A => M[C] = {
    a => {
      m.flatMap(f(a)){ b =>
        m.flatMap(g(b)){ c =>
          m.unit(c)
        }
      }
    }
  }

  // Compare to normal function composition:
  //
  //  monadCompose[A,B,C,M[_]](g: B => M[C])(f: A => M[B])(m: Monad[M]): A => M[C]
  def compose     [A,B,C     ](g: B => C)   (f: A => B)                : A => C = {
    a => g(f(a))
  }

  import Try.{Try, Tweet}
  val getFirstTweetById: Int => Try[Tweet] =
    monadCompose(Try.getFirstTweet)(Try.getUser)(TryMonad)

  def sequence[A,M[_]](ms: List[M[A]])(m: Monad[M]): M[List[A]] = {
    ms match {
      case head +: tail => m.flatMap(head){ h =>
        m.flatMap(sequence(tail)(m)){ t =>
          m.unit(h +: t)
        }
      }
      case Nil => m.unit(Nil)
    }
  }

  sequence[Int, Option.Option](List(Option.Some(1), Option.Some(2)))(OptionMonad)
  sequence[Int, Option.Option](List(Option.Some(1), Option.None))(OptionMonad)

  sequence(
    List(
      Future{Thread.sleep(10000); 1},
      Future{Thread.sleep(10000); 2},
      Future{Thread.sleep(10000); 3}
    )
  )(FutureMonad)
  
  /*
  Three fundamental concepts of computation [Failure, Collections and Effects] 
  that are normally defined independently of each other actually all share this 
  similar structure. This unifying pattern can be abstracted out and reused to 
  build higher abstractions that work for all current and future 
  implementations. If you want a motivating reason for understanding monads, 
  this is it! These insights are the essence of what I wish I knew about monads 
  looking back.
  - Stephen Diehl, "What I Wish I Knew When Learning Haskell"
  */
}

object Lessons {
  /**
  How to abstract:

  1) Encode the semantics of your domain as data.
  2) Write small generic combinators for combining that data.
  3) Write a function that 'interprets' that data. For example, `fold`.
  3) Use those combinators to describe the solution.
  4) Run the interpreter to extract the solution.

  Suddenly everything looks like a DSL.


  Be generic:
  1) Generic abstractions allow us to reuse existing data in new ways.
        All of the things we can do with an Int we can also do with
        an Int that may or may not exist: Option[Int]
  2) Generic types constrain implementations.
        There's only a few ways you can implement 
        (Option[A], A => B) => Option[B]
  3) Very generic interfaces mean I don't have to repeat myself.
        The Monad interface means I only need to write `sequence` once
        for all monad instances.


  Always be on the lookout for better tools.
  See: Rust, Haskell, and Idris

  "Functional Programming in Scala" is a great book that covers these topics in depth.
  **/
}