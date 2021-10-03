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

    Thread.sleep(5000)
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

object flatMap extends ZIOApp {

  val zippedZIO: ZIO[(Int, String)] =
    ZIO.succeed(42) zip ZIO.succeed("Hui")

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  val flatMappedZIO: ZIO[Unit] =
    zippedZIO.flatMap { tuple =>
      printLine(s"Tuple: $tuple")
    }

  def run: ZIO[Unit] = flatMappedZIO
}

object forComprehension extends ZIOApp {

  val zippedZIO: ZIO[(Int, String)] =
    ZIO.succeed(42) zip ZIO.succeed("Hui")

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  val flatMappedZIO = for {
    tuple <- zippedZIO
    _     <- printLine(s"Tuple: $tuple")
  } yield "Nice"

  def run = flatMappedZIO
}

object as extends ZIOApp {

  val zippedZIO: ZIO[(Int, String)] =
    ZIO.succeed(42) zip ZIO.succeed("Hui")

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  val flatMappedZIO =
    zippedZIO.flatMap { t =>
      printLine(s"Tuple: $t")
        .as("Nice")
    }

  def run = flatMappedZIO
}

object async extends ZIOApp {

  val asyncZIO: ZIO[Int] =
    ZIO.async[Int] { complete =>
      println("Async started")
      Thread.sleep(1000)
      complete(10)
    }

  def run = asyncZIO
}

object fork extends ZIOApp {

  val asyncZIO: ZIO[Int] =
    ZIO.async[Int] { complete =>
      println("Async started")
      Thread.sleep(1000)
      complete(scala.util.Random.nextInt(999))
    }

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  val forkedZIO = for {
    fiber1 <- asyncZIO.fork
    fiber2 <- asyncZIO.fork
    _      <- printLine("forked")
    int1   <- fiber1.join
    int2   <- fiber2.join
  } yield s"Fork($int1, $int2)"

  def run = forkedZIO
}
