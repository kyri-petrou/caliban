package example.federation

import example.federation.FederationData.characters.sampleCharacters
import example.federation.FederationData.episodes.sampleEpisodes

import caliban.Http4sAdapter
import cats.data.Kleisli
import org.http4s.StaticFile
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio._
import zio.interop.catz._

object FederatedApp extends CatsApp {
  type ExampleTask[A] = RIO[Any, A]

  val service1 =
    CharacterService
      .make(sampleCharacters)
      .memoize
      .flatMap(layer =>
        for {
          interpreter <- FederatedApi.Characters.api.interpreter.map(_.provideLayer(layer))
          _           <- BlazeServerBuilder[ExampleTask]
                           .bindHttp(8089, "localhost")
                           .withHttpApp(
                             Router[ExampleTask](
                               "/api/graphql" -> CORS.policy(Http4sAdapter.makeHttpService(interpreter)),
                               "/graphiql"    -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", None))
                             ).orNotFound
                           )
                           .resource
                           .toScopedZIO
                           .forever
        } yield ()
      )

  val service2 =
    EpisodeService
      .make(sampleEpisodes)
      .memoize
      .flatMap(layer =>
        for {
          interpreter <- FederatedApi.Episodes.api.interpreter.map(_.provideLayer(layer))
          _           <- BlazeServerBuilder[ExampleTask]
                           .bindHttp(8088, "localhost")
                           .withHttpApp(
                             Router[ExampleTask](
                               "/api/graphql" -> CORS.policy(Http4sAdapter.makeHttpService(interpreter)),
                               "/graphiql"    -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", None))
                             ).orNotFound
                           )
                           .resource
                           .toScopedZIO
                           .forever
        } yield ()
      )

  override def run =
    (service1 race service2).exitCode
}
