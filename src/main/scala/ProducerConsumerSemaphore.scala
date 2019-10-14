import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.concurrent.Semaphore
import cats.implicits._

//GOAL : producer-consumer problem using semaphore

object ProducerConsumerSemaphore extends IOApp {

  type Channel = Semaphore[IO]

  def produce(producerChannel: Channel, consumerChannel: Channel, list: List[Int]): IO[Unit] = {
    list match {
      case Nil => producerChannel.release >> consumerChannel.release
      case head:: tail =>
        producerChannel.acquire >> IO(println(s"produce $head")) >> consumerChannel.release >> produce(producerChannel, consumerChannel, tail)
    }
  }

  def consume(producerChannel: Channel, consumerChannel: Channel, list: List[Int]): IO[Unit] = {
    list match {
      case Nil => producerChannel.release >> consumerChannel.release
      case head:: tail =>
        consumerChannel.acquire >> IO(println(s"consume $head")) >> producerChannel.release >> consume(producerChannel, consumerChannel, tail)
    }
  }

  val program = for {
    channelX <- Semaphore[IO](1)
    channelY <- Semaphore[IO](0)
    producerTask = produce(channelX, channelY, (0 until 10).toList)
    consumerTask = consume(channelX, channelY, (0 until 10).toList)
    fp  <- producerTask.start
    fc  <- consumerTask.start
    _   <- fp.join
    sum <- fc.join
  } yield sum

  override def run(args: List[String]): IO[ExitCode] = program.map(_ => ExitCode.Success)
}
