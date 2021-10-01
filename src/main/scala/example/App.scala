package example

import zio.ZIO

case class Person(name: String)

object Person {
  val peter: Person = Person("Peter")
}

trait ZIOApp {

  def run: ZIO[Any]

  def main(args: Array[String]) =
    println(s"Result: ${run.run}")
}

object succeedNow extends ZIOApp {

  val peterZIO: ZIO[Person] =
    ZIO.succeedNow(Person.peter)

  def run: ZIO[Person] = peterZIO

}
