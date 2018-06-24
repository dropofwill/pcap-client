package com.willpaul.pcapclient

import cats.effect.{Effect, IO}
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.client.blaze._
import org.http4s.Uri

import scala.concurrent.ExecutionContext.Implicits.global

object PCapClient {

  val BASE_URL = Uri.uri("https://home.personalcapital.com")
  val LOGIN = BASE_URL / "page" / "login" / "goHome"
  val CSRF_SELECTOR = raw"globals.csrf='([a-f0-9-]+)".r

  // def authorize[F[_]: Effect](
  //   implicit ec: ExecutionContext,
  //   email: String, password: String): F[String] = ???


  val httpClient = Http1Client[IO]().unsafeRunSync

  def status(status: String): IO[String] = {
    val target = Uri.uri("https://httpstat.us/") / status

    Http1Client[IO]().flatMap(_.expect[String](target))
    // httpClient.expect[String](target)
  }

  def csrf(): IO[String] = {
    Http1Client[IO]()
      .flatMap(_.expect[String](LOGIN))
      .flatMap(CSRF_SELECTOR.findFirstMatchIn(_)
        .map(found => IO.pure(found.group(1)))
        .getOrElse(IO.raiseError(new Exception("No CSRF found"))))
  }
}

