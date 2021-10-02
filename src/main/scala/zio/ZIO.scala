package zio

sealed trait ZIO[+A] { self =>

  def run(callback: A => Unit): Unit

  def zip[B](that: ZIO[B]): ZIO[(A, B)] =
    ZIO.Zip(self, that)
}

object ZIO {

  // call by name
  def succeed[A](f: => A): ZIO[A] =
    ZIO.Effect(() => f)

  def succeedNow[A](value: A): ZIO[A] = ZIO.Succeed(value)

  case class Succeed[A](value: A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit =
      callback(value)
  }

  case class Effect[A](f: () => A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit =
      callback(f())
  }

  case class Zip[A, B](left: ZIO[A], right: ZIO[B]) extends ZIO[(A, B)] {
    override def run(callback: ((A, B)) => Unit): Unit =
      left.run { a =>
        right.run { b =>
          callback(a, b)
        }
      }
  }
}
