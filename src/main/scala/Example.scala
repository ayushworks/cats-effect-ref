import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.util.{Failure, Success, Try}

//GOAL : prevent overloading of an underlying service if it fails beyond a threshold

object Example extends IOApp {

  case class WindowConfig(runCount: Int, errorCount: Int) {

    def addError: WindowConfig = WindowConfig(runCount+1, errorCount+1)

    def addRun: WindowConfig = WindowConfig(runCount+1, errorCount)

    def errorRate : Double = (errorCount.toDouble/runCount.toDouble)*100

    def isCircuitOpen(threshold: Double, sampleSize: Int): Boolean = {
      if(runCount>= sampleSize && errorRate>=threshold) true
      else false
    }

  }

  object WindowConfig {
    def newWindow : WindowConfig = WindowConfig(0,0)
  }

  case class CircuitState(open: Boolean, windowConfig: WindowConfig) {

    def resetWindow : CircuitState = CircuitState(false, WindowConfig.newWindow)

  }

  case class CircuitOpenException(msg: String) extends Throwable(msg)

  trait CircuitBreaker[A,B] {
    def run(a: A): IO[B]
  }

  object CircuitBreaker {
    def create[A,B](threshold: Double, sampleSize: Int)(f: A => B) : IO[CircuitBreaker[A,B]] =
      Ref[IO].of(CircuitState(false, WindowConfig.newWindow)).map {
        circuit =>

          (a: A) =>
            circuit.modify {
            case CircuitState(true, _) =>
              //reset to old state
              (CircuitState(false, WindowConfig.newWindow), IO(println("circuit open")) >> IO.raiseError[B](CircuitOpenException("circuit breaker error")))
            case CircuitState(false, windowConfig) =>   Try(f(a)) match {
              case Success(value) => (CircuitState(windowConfig.addRun.isCircuitOpen(threshold, sampleSize), windowConfig.addRun), IO.pure(value))
              case Failure(exception) => (CircuitState(windowConfig.addError.isCircuitOpen(threshold, sampleSize), windowConfig.addError), IO.raiseError[B](exception))
            }
          }.flatten
    }
  }

  // a random service that could fail
  def method(id: Int): String = {
    println("running method")

    if(id>=0) throw new RuntimeException("error in method")

    id.toString
  }

  def program(breaker: CircuitBreaker[Int, String]) = for {
    result <- List.range(0, 12).traverse(id => breaker.run(id).handleErrorWith(ex => IO(ex.getMessage)))
  } yield result

  override def run(args: List[String]): IO[ExitCode] = for {
    breaker <- CircuitBreaker.create(50, 5)(method) //after collecting 5 samples; if they error % is >= 50% then circuit breaker is open
    result <- program(breaker)
    res <- IO(println(result)).map(_ => ExitCode.Success)
  } yield res
}
