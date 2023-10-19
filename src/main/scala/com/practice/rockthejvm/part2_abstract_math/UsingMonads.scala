package com.practice.rockthejvm.part2_abstract_math

import scala.util.{Failure, Try}

//noinspection ScalaWeakerAccess,TypeAnnotation,ScalaUnusedSymbol
object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  val monadList      = Monad[List]       // fetch the implicit Monad[List]
  val aSimpleList    = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))
  // applicable to Option, Try, Future

  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T]   = Either[Throwable, T]
  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither     = loadingMonad.pure(45) // LoadingOr[Int] = Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n =>
    if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life...")
  )

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation =
    loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))
  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocationBetter =
    getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location    <- trackLocation(orderStatus)
  } yield location

  // the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_]: Monad](cfg: Map[String, String], payload: String)(implicit
      httpService: HttpService[M]
  ): M[String] =
    for {
      conn     <- httpService.getConnection(cfg)
      response <- httpService.issueRequest(conn, payload)
    } yield response

  /*
    Requirements:
    - if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
      otherwise the method will fail, according to the logic of the type M
      (for Try it will return a Failure, for Option it will return None, for Future it will be failed future, for Either it will return Left
    - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload is less then 20 characters
      otherwise the method will fail, according to the logic of the type M

      Exercise 1: provide a real implementation of httpService using Try, Option, Future, Either
   */

  import cats.syntax.applicative._
  import cats.instances.option._
  import cats.instances.try_._

  implicit object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      for {
        payload <- payload.pure[Option]
        if payload.length < 20
      } yield s"request ($payload) has been accepted"
  }

  val responseOption = OptionHttpService.getConnection(config).flatMap { conn =>
    OptionHttpService.issueRequest(conn, "Hello, HTTP service")
  }
  val responseOptionFor = for {
    connection <- OptionHttpService.getConnection(config)
    response   <- OptionHttpService.issueRequest(connection, "Hello, HTTP service")
  } yield response

  object HttpServiceTry extends HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] = {
      val connection =
        for {
          host <- cfg.get("host")
          port <- cfg.get("port")
        } yield Connection(host, port)

      connection match {
        case Some(value) => value.pure[Try]
        case None        => Failure(new Exception("Not Found"))
      }
    }

    override def issueRequest(connection: Connection, payload: String): Try[String] =
      for {
        payload <- payload.pure[Try]
        if payload.length < 20
      } yield s"Request ($payload) has been accepted"
  }

  // Exercise 2: implement another HttpService with LoadingOr or ErrorOr
  implicit object AggressiveHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (!cfg.contains("host") || !cfg.contains("port")) {
        Left(new RuntimeException("Connection could not be established: invalid configuration"))
      } else {
        Right(Connection(cfg("host"), cfg("port")))
      }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length >= 20) Left(new RuntimeException("Payload is too large"))
      else Right(s"Request ($payload) has been accepted")
  }

  val errorOrResponse: ErrorOr[String] = for {
    conn     <- AggressiveHttpService.getConnection(config)
    response <- AggressiveHttpService.issueRequest(conn, "Hello ErrorOr")
  } yield response

  object LoadingOrHttpService extends HttpService[LoadingOr] {
    override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] = {
      val connection =
        for {
          host <- cfg.get("host")
          port <- cfg.get("port")
        } yield Connection(host, port)

      connection match {
        case Some(value) => value.pure[LoadingOr]
        case None        => Left("Not Found")
      }
    }

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] =
      if (payload.length >= 20) Left("Very long")
      else Right(s"Request ($payload) has been accepted")
  }

  def main(args: Array[String]): Unit = {
    println(getResponse[Option](config, "Hello Option"))
    println(getResponse[ErrorOr](config, "Hello ErrorOr"))
  }
}
