package com.willpaul.pcapclient

import cats.effect.{Effect, IO}

import io.circe.syntax._
import io.circe.generic.auto._

import org.http4s._
import org.http4s.dsl.io._
import org.http4s.client.blaze._
import org.http4s.client.dsl.io._
import org.http4s.Uri
import org.http4s.circe._

import scala.concurrent.ExecutionContext.Implicits.global

case class UserIdentity(
  username: String, csrf: String,
  apiClient: String = "WEB",
  bindDevice: String = "false",
  skipLinkAccount: String = "false",
  redirectTo: String = "",
  skipFirstUse: String = "",
  referrerId: String = "")

case class SmsChallenge(
  csrf: String,
  challengeType: String = "challengeSMS",
  challengeReason: String = "DEVICE_AUTH",
  challengeMethod: String = "OP",
  apiClient: String = "WEB",
  bindDevice: String = "false")

case class SmsAuth(
  code: String,
  csrf: String,
  challengeReason: String = "DEVICE_AUTH",
  challengeMethod: String = "OP",
  apiClient: String = "WEB",
  bindDevice: String = "false")

object PCapClient {

  val BASE_URL = Uri.uri("https://home.personalcapital.com")
  val PAGE_URL = BASE_URL / "page"
  val API_URL = BASE_URL / "api"

  val CSRF_URL = PAGE_URL / "login" / "goHome"
  val IDENTIFY_URL = API_URL / "login" / "identifyUser"
  val SMS_CHALLENGE_URL = API_URL / "credential" / "challengeSms"
  val SMS_AUTH_URL = API_URL / "credential" / "authenticateSms"

  val CSRF_SELECTOR = raw"globals.csrf='([a-f0-9-]+)".r

  def status(status: String): IO[String] = {
    val target = Uri.uri("https://httpstat.us/") / status

    Http1Client[IO]().flatMap(_.expect[String](target))
  }

  def initAuth(username: String): IO[(String,String)] = {
    for {
      csrfToken <- csrf()
      // authLevel <- identify(UserIdentity(username, csrfToken))
      res <- smsChallenge(SmsChallenge(csrfToken))
    } yield (csrfToken, res)
  }

  def completeAuth(csrf: String, code: String): IO[String] =
    for {
      res <- smsAuth(SmsAuth(code, csrf))
    } yield (res)

  def smsChallenge(sms: SmsChallenge): IO[String] = Http1Client[IO]()
    .flatMap(_.expect[String](
      POST(SMS_CHALLENGE_URL, sms.asJson)))

  def smsAuth(sms: SmsAuth): IO[String] = Http1Client[IO]()
    .flatMap(_.expect[String](
      POST(SMS_AUTH_URL, sms.asJson)))

  def identify(user: UserIdentity): IO[String] = Http1Client[IO]()
    .flatMap(_.expect[String](
      POST(IDENTIFY_URL, user.asJson)))

  def csrf(): IO[String] = Http1Client[IO]()
    .flatMap(_.expect[String](CSRF_URL))
    .flatMap(findCsrfToken(_)
      .map(IO.pure(_))
      .getOrElse(IO.raiseError(new Exception("No CSRF found"))))


  def findCsrfToken(rawPage: String): Option[String] =
    CSRF_SELECTOR.findFirstMatchIn(rawPage).map(_.group(1))
}

