package example

import zio.ZIO

case class Person(name: String, age: Int)

object Person {
  val peter: Person = Person("Peter", 25)
}

trait ZIOApp {

  def run: ZIO[Any]

  def main(args: Array[String]): Unit = {
    run.run {
      result => println(s"Result: $result")
    }
  }
}

object succeedNow extends ZIOApp {

  val peterZIO: ZIO[Person] =
    ZIO.succeedNow(Person.peter)

  def run: ZIO[Person] = peterZIO
}

object succeedNowOhUh extends ZIOApp {

  // not lazy
  val helloZIO: ZIO[Unit] =
    ZIO.succeedNow(println("Hello!"))

  def run: ZIO[Int] = ZIO.succeedNow(42)
}

object succeed extends ZIOApp {

  val helloZIO: ZIO[Unit] =
    ZIO.succeed(println("Hello!"))

  def run: ZIO[Unit] = helloZIO
}

object succeedAgain extends ZIOApp {

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  def run: ZIO[Unit] = printLine("Hui!")
}

object zip extends ZIOApp {

  val zippedZIO: ZIO[(Int, String)] =
    ZIO.succeed(42) zip ZIO.succeed("Hui")

  def run: ZIO[(Int, String)] = zippedZIO
}

object map extends ZIOApp {

  val zippedZIO: ZIO[(Int, String)] =
    ZIO.succeed(42) zip ZIO.succeed("Hui")

  val mappedZIO: ZIO[Person] =
    zippedZIO.map {
      case (int, string) => Person(string, int)
    }

  def run: ZIO[Person] = mappedZIO
}
