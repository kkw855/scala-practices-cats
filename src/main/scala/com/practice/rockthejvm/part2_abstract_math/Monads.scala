package com.practice.rockthejvm.part2_abstract_math

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

//noinspection ScalaWeakerAccess,TypeAnnotation,ScalaUnusedSymbol
object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList   = List('a', 'b', 'c')
  // Exercise 1.1: how do you create all combinations of (number, char)?
  val combinationList: List[(Int, Char)] = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationListFor: List[(Int, Char)] = for {
    n <- numbersList
    c <- charsList
  } yield (n, c) // identical

  // options
  val numberOption = Option(2)
  val charOption   = Option('d')
  // Exercise 1.2: how do you create the combination of (number, char)?
  val combinationOption = numberOption.flatMap(n => charOption.map(c => (n, c)))
  val combinationOptionFor = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // futures
  implicit val ex: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42)
  val charFuture   = Future('Z')
  // Exercise 1.3: how do you create the combination of (number, char)?
  val combinationFuture = numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val combinationFutureFor = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
    Pattern consists of two fundamental operations
    - wrapping a value into a monadic value
    - the flatMap mechanism

    All these is a called a MONAD
   */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption    = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption =
    optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None) // None

  import cats.instances.list._
  val listMonad        = Monad[List]
  val aList            = listMonad.pure(3)                             // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4, 5)

  // Exercise 2: use a Monad[Future]
  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture     = futureMonad.pure(43)
  // future that will end up with Success(87)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x * 10))

  // specialized API
  def getPairList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))
  def getPairFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  // generalize
  def getPair[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  def main(args: Array[String]): Unit = {
    println(getPair(numbersList, charsList))
    println(getPair(numberOption, charOption))
    getPair(numberFuture, charFuture).foreach(println)
  }
}
