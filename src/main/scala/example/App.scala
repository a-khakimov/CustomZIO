package example

import zio.ZIO

case class Person(name: String)

object Person {
  val peter: Person = Person("Peter")
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

  def run: ZIO[Unit] = ZIO.succeedNow(42)
}

object succeed extends ZIOApp {

  val helloZIO: ZIO[Unit] =
    ZIO.succeed(() => println("Hello!"))

  def run: ZIO[Unit] = ZIO.succeedNow(42)
}
