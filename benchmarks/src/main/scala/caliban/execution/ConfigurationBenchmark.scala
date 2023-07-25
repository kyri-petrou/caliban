package caliban.execution

import caliban._
import org.openjdk.jmh.annotations._
import zio.{ Scope => _, _ }

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ConfigurationBenchmark {

  trait S1
  trait S2
  trait S3
  trait S4
  trait S5
  trait S6
  trait S7
  trait S8
  trait S9
  trait S10

  type Env = S1 with S2 with S3 with S4 with S5 with S6 with S7 with S8 with S9 with S10
  private val runtime =
    Unsafe.unsafe(implicit u => Runtime.unsafe.fromLayer[Env](ZLayer.succeed(S())))

  def run[A](zio: RIO[Env, A]): A = Unsafe.unsafe(implicit u => runtime.unsafe.run(zio).getOrThrow())

  import NestedZQueryBenchmarkSchema._

  case class S() extends S1 with S2 with S3 with S4 with S5 with S6 with S7 with S8 with S9 with S10

  val simple100: GraphQLInterpreter[Env, CalibanError] =
    run(graphQL[Any, SimpleRoot, Unit, Unit](RootResolver(NestedZQueryBenchmarkSchema.simple100Elements)).interpreter)

  val simple100Wrapped =
    simple100
      .wrapExecutionWith(e => ZIO.scoped(Configurator.setQueryExecution(QueryExecution.Sequential) *> e))
      .wrapExecutionWith(e => ZIO.scoped(Configurator.setQueryExecution(QueryExecution.Sequential) *> e))
      .wrapExecutionWith(e => ZIO.scoped(Configurator.setQueryExecution(QueryExecution.Sequential) *> e))
      .execute(simpleQuery)

  val simple100Layered = simple100
    .provideSomeLayer[Env](ZLayer.scoped(Configurator.setQueryExecution(QueryExecution.Sequential)))
    .provideSomeLayer[Env](ZLayer.scoped(Configurator.setQueryExecution(QueryExecution.Sequential)))
    .provideSomeLayer[Env](ZLayer.scoped(Configurator.setQueryExecution(QueryExecution.Sequential)))
    .execute(simpleQuery)

  val simple10000: GraphQLInterpreter[Env, CalibanError] =
    run(graphQL[Any, SimpleRoot, Unit, Unit](RootResolver(NestedZQueryBenchmarkSchema.simple10000Elements)).interpreter)

  val simple10000Wrapped =
    simple10000
      .wrapExecutionWith(e => ZIO.scoped(Configurator.setQueryExecution(QueryExecution.Sequential) *> e))
      .wrapExecutionWith(e => ZIO.scoped(Configurator.setQueryExecution(QueryExecution.Sequential) *> e))
      .wrapExecutionWith(e => ZIO.scoped(Configurator.setQueryExecution(QueryExecution.Sequential) *> e))
      .execute(simpleQuery)

  val simple10000Layered = simple10000
    .provideSomeLayer[Env](ZLayer.scoped(Configurator.setQueryExecution(QueryExecution.Sequential)))
    .provideSomeLayer[Env](ZLayer.scoped(Configurator.setQueryExecution(QueryExecution.Sequential)))
    .provideSomeLayer[Env](ZLayer.scoped(Configurator.setQueryExecution(QueryExecution.Sequential)))
    .execute(simpleQuery)

  @Benchmark
  def wrappedConfigurator100(): Any =
    run(simple100Wrapped)

  @Benchmark
  def layeredConfigurator100(): Any =
    run(simple100Layered)

  @Benchmark
  def wrappedConfigurator10000(): Any =
    run(simple10000Wrapped)

  @Benchmark
  def layeredConfigurator10000(): Any =
    run(simple10000Layered)
}
