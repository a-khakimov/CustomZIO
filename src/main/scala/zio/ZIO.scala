package zio

sealed trait ZIO[+A] { self =>

  def run(callback: A => Unit): Unit

  def zip[B](that: ZIO[B]): ZIO[(A, B)] =
    ZIO.Zip(self, that)

  def map[B](f: A => B): ZIO[B] =
    ZIO.Map(self, f)

  def flatMap[B](f: A => ZIO[B]): ZIO[B] =
    ZIO.FlatMap(self, f)

  def as[B](value: B): ZIO[B] =
    self.map(_ => value)
}

object ZIO {

  // call by name
  def succeed[A](value: => A): ZIO[A] =
    ZIO.Effect(() => value)

  def succeedNow[A](value: A): ZIO[A] =
    ZIO.Succeed(value)

  def async[A](register: (A => Any) => Any): ZIO[A] =
    ZIO.Async(register)

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

  case class Map[A, B](zio: ZIO[A], f: A => B) extends ZIO[B] {
    override def run(callback: B => Unit): Unit =
      zio.run { a =>
        callback(f(a))
      }
  }

  case class FlatMap[A, B](zio: ZIO[A], f: A => ZIO[B]) extends ZIO[B] {
    override def run(callback: B => Unit): Unit =
      zio.run { a =>
        f(a).run { b =>
          callback(b)
        }
      }
  }

  case class Async[A](register: (A => Any) => Any) extends ZIO[A] {
    override def run(callback: A => Unit): Unit =
      register(callback)
  }
}
