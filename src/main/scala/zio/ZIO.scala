package zio

import scala.concurrent.ExecutionContext

trait Fiber[+A] {
  def start(): Unit
  // 1. Early Completion
  // 2. Late Completion
  def join: ZIO[A]
}

class FiberImpl[A](zio: ZIO[A]) extends Fiber[A] {
  var maybeResult: Option[A] = None
  var callbacks: List[A => Any] = List.empty[A => Any]

  override def start(): Unit =
    ExecutionContext.global.execute { () =>
      zio.run { a =>
        maybeResult = Some(a)
        callbacks.foreach { callback =>
          callback(a)
        }
      }
    }

  override def join: ZIO[A] =
    maybeResult match {
      case Some(value) =>
        ZIO.succeedNow(value)
      case None =>
        ZIO.async { complete =>
          callbacks = complete :: callbacks
        }
    }
}

sealed trait ZIO[+A] { self =>

  def run(callback: A => Unit): Unit

  def zip[B](that: ZIO[B]): ZIO[(A, B)] =
    for {
      a <- self
      b <- that
    } yield (a, b)

  def map[B](f: A => B): ZIO[B] =
    flatMap(f andThen ZIO.succeedNow)

  def flatMap[B](f: A => ZIO[B]): ZIO[B] =
    ZIO.FlatMap(self, f)

  def as[B](value: B): ZIO[B] =
    self.map(_ => value)

  def fork: ZIO[Fiber[A]] =
    ZIO.Fork(self)

  def zipPar[B](that: ZIO[B]): ZIO[(A, B)] =
    for {
      aF <- self.fork
      bF <- that.fork
      a <- aF.join
      b <- bF.join
    } yield (a, b)
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

  case class Fork[A](zio: ZIO[A]) extends ZIO[Fiber[A]] {
    override def run(callback: Fiber[A] => Unit): Unit = {
      val fiber: Fiber[A] = new FiberImpl(zio)
      fiber.start()
      callback(fiber)
    }
  }
}
