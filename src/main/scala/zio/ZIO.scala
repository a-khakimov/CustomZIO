package zio

sealed trait ZIO[+A] {
  def run(callback: A => Unit): Unit
}

object ZIO {
  def succeedNow[A](value: A): ZIO[A] = ZIO.Succeed(value)

  case class Succeed[A](value: A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit = {
      callback(value)
    }
  }
}
