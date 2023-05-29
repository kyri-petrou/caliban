package caliban.interop.cats

import cats.Applicative
import cats.data.{ Chain, Kleisli }
import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{ IO, IOLocal, Ref }
import cats.syntax.flatMap._
import cats.syntax.functor._
import zio.test._
import zio.{ Runtime, Tag, Trace, ZEnvironment, ZIO }

object CatsInteropSpec extends ZIOSpecDefault {

  override def spec = suite("CatsInteropSpec")(
    suite("contextual interop: inject an environment") {
      case class SecurityContext(isAdmin: Boolean)
      case class LogContext(traceId: String)
      case class RootContext(security: SecurityContext, log: LogContext)

      val rootCtx = RootContext(SecurityContext(false), LogContext("external-trace-id"))
      val inner   = RootContext(SecurityContext(true), LogContext("internal-trace-id"))

      List(
        test("Kleisli contextual interop: inject an environment") {
          type Effect[A] = Kleisli[IO, RootContext, A]

          def main(inner: RootContext)(implicit runtime: Runtime[RootContext]) =
            Dispatcher.parallel[Effect].use { dispatcher =>
              program[Effect, RootContext](CatsInterop.contextual(dispatcher), inner)
            }

          for {
            contextual <-
              ZIO.succeed(
                main(inner)(Runtime.default.withEnvironment(ZEnvironment(rootCtx))).run(rootCtx).unsafeRunSync()
              )
          } yield assertTrue(contextual == List(rootCtx, inner, rootCtx))
        },
        test("IO contextual interop: inject an environment") {

          def main(inner: RootContext)(implicit runtime: Runtime[RootContext]): IO[List[RootContext]] =
            Dispatcher.parallel[IO].use { dispatcher =>
              implicit val local: IOLocal[RootContext]           =
                IOLocal(runtime.environment.get[RootContext]).unsafeRunSync()

              implicit val injectEnv: InjectEnv[IO, RootContext] = InjectEnv.forIO

              program[IO, RootContext](CatsInterop.contextual(dispatcher), inner)
            }

          for {
            contextual <-
              ZIO.succeed(
                main(inner)(Runtime.default.withEnvironment(ZEnvironment(rootCtx))).unsafeRunSync()
              )
          } yield assertTrue(contextual == List(rootCtx, inner, rootCtx))
        }
      )
    },
    test("plain interop: do not inject an environment") {
      case class Context(traceId: String)

      type Effect[A] = Kleisli[IO, Context, A]

      val rootCtx = Context("external-trace-id")
      val inner   = Context("internal-trace-id")

      def main(inner: Context)(implicit runtime: Runtime[Context]) =
        Dispatcher.parallel[Effect].use { dispatcher =>
          program[Effect, Context](CatsInterop.default(dispatcher), inner)
        }

      for {
        contextual <-
          ZIO.succeed(main(inner)(Runtime.default.withEnvironment(ZEnvironment(rootCtx))).run(rootCtx).unsafeRunSync())
      } yield assertTrue(contextual == List(rootCtx, rootCtx, rootCtx))
    }
  )

  private def program[F[_]: Async, R: Tag](interop: CatsInterop[F, R], inner: R)(implicit
    local: Local[F, R]
  ): F[List[R]] = {

    def snapshot(ref: Ref[F, Chain[R]]): F[Unit] =
      for {
        current <- local.ask
        _       <- ref.update(_.append(current))
      } yield ()

    for {
      ref <- Ref.of(Chain.empty[R])
      _   <- snapshot(ref)
      _   <- interop.toEffect(interop.fromEffect(snapshot(ref)).updateService[R](_ => inner))
      _   <- snapshot(ref)
      r   <- ref.get
    } yield r.toList
  }

  trait Local[F[_], R] {
    def ask: F[R]
  }

  object Local {
    implicit def localForKleisli[F[_]: Applicative, R]: Local[Kleisli[F, R, *], R] =
      new Local[Kleisli[F, R, *], R] {
        def ask: Kleisli[F, R, R] = Kleisli.ask
      }

    implicit def forIO[R](implicit ioLocal: IOLocal[R]): Local[IO, R] =
      new Local[IO, R] {
        def ask: IO[R] = ioLocal.get
      }
  }

}
