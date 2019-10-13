import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.util.{Failure, Success, Try}

//GOAL : count how many times a particular method was called and prevent system overload by stopping errors after certain threshold

object Example extends IOApp {

  case class WindowConfig(runCount: Int, errorCount: Int) {

    def addError: WindowConfig = WindowConfig(runCount+1, errorCount+1)

    def addRun: WindowConfig = WindowConfig(runCount+1, errorCount)

    def errorRate : Double = errorCount.toDouble/runCount.toDouble
  }

  object WindowConfig {
    def newWindow : WindowConfig = WindowConfig(0,0)
  }

  case class CircuitState(open: Boolean, windowConfig: WindowConfig) {

    def resetWindow : CircuitState = CircuitState(false, WindowConfig.newWindow)

    def isCircuitOpen(threshold: Double, sampleSize: Int): Boolean = {
      if(windowConfig.runCount>= sampleSize && windowConfig.errorRate>threshold) true
      else false
    }

  }

  case class CircuitOpenException(msg: String) extends Throwable(msg)

  trait CircuitBreaker[A] {
    def run: IO[A]
  }

  object CircuitBreaker {
    def create[A](threshold: Double, sampleSize: Int)(body: => A) : IO[CircuitBreaker[A]] =
      Ref[IO].of(CircuitState(false, WindowConfig.newWindow)).map {
        circuit =>

          new CircuitBreaker[A] {
            override def run: IO[A] = circuit.get.flatMap {
              state =>
                state.open match {
                  case true => circuit.modify(s => (s.resetWindow, ())) >> IO.raiseError(CircuitOpenException(s"circuit-breaker open"))
                  case _ =>
                    Try(body) match {
                      case Success(value) => circuit.modify(s => (s.copy(windowConfig = s.windowConfig.addRun), value))
                      case Failure(exception) =>   circuit.modify(s => (s.copy(windowConfig = s.windowConfig.addRun), ())) >> IO.raiseError[A](exception)
                    }
                }
            }
          }
    }
  }


  override def run(args: List[String]): IO[ExitCode] = ???
}
