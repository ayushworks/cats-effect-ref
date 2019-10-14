import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.concurrent.Deferred
import cats.implicits._

// GOAL : Multiple readers, one or more  writer.
// Readers wait before reading on a shared variable. Writer/s updates the shared variable only once

object WriterReaderDeferred extends IOApp {

  type Signal = Deferred[IO, Int]

  def writer(id: Int, signal: Signal): IO[Unit] = {
    signal.complete(id) >> IO(println(s"writer $id : wrote $id"))
  }

  def reader(id: Int, signal: Signal): IO[Unit] = {
    signal.get.map(value => println(s"reader $id : read $value"))
  }

  def program(signal: Signal) = for {
    _ <- List.range(1, 10).traverse(id => reader(id, signal)).start
    _ <- List.range(1, 10).traverse(id => writer(id, signal)).handleErrorWith(_ => IO.pure(()))
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = for {
    signal <- Deferred[IO, Int]
    _ <- program(signal)
  } yield ExitCode.Success
}
