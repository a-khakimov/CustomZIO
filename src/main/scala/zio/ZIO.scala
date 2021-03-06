package zio

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext

// Declarative encoding
// async
// ZIO[R, E, A]
// Stack safety
// Execution context
// Concurrency safety
// Type safety
// Error handling
// Environment
// Interruption

trait Fiber[+A] {
  def start(): Unit
  // 1. Early Completion
  // 2. Late Completion
  def join: ZIO[A]
}

class FiberImpl[A](zio: ZIO[A]) extends Fiber[A] {

  sealed trait FiberState

  case class Running(callbacks: List[A => Any]) extends FiberState
  case class Done(result: A) extends FiberState

  val state: AtomicReference[FiberState] =
    new AtomicReference(Running(List.empty))

  def complete(result: A): Unit = {
    var loop = true
    //var toComplete: List[A => Any] = List.empty
    while (loop) {
      val oldState = state.get()
      oldState match {
        case Running(callbacks) =>
          if (state.compareAndSet(oldState, Done(result))) {
            callbacks.foreach(callback => callback(result))
            loop = false
          }
        case Done(result) =>
          throw new Exception(s"Internal defect: Fiber being completed multiple times ($result)")
      }
    }
  }
  // var i = 0
  // for {
  //   _ <- ZIO.succeed(i += 1).fork.repeat(10000)
  //   result <- ZIO.succeed(i)
  // } yield result

  var maybeResult: Option[A] = None
  var callbacks: List[A => Any] = List.empty[A => Any]

  override def start(): Unit =
    ExecutionContext.global.execute {
      () => zio.run(complete)
    }

  override def join: ZIO[A] =
    ZIO.async { callback =>
      callbacks = callback :: callbacks
    }
    //maybeResult match {
    //  case Some(value) => ZIO.succeedNow(value)
    //  case None        => ZIO.async { complete =>
    //      callbacks = complete :: callbacks
    //    }
    //}

  def await(callback: A => Any): Unit = {
    var loop = true
    while (loop) {
      val oldState = state.get()
      oldState match {
        case Running(callbacks) =>
          val newState = Running(callback :: callbacks)
          loop = state.compareAndSet(oldState, newState)
        case Done(result) =>
          callback(result)
          loop = false
      }
    }
  }
}

sealed trait ZIO[+A] { self =>

  def map[B](f: A => B): ZIO[B] =
    ZIO.Map(self, f)

  def flatMap[B](f: A => ZIO[B]): ZIO[B] =
    ZIO.FlatMap(self, f)

  def as[B](value: B): ZIO[B] =
    self.map(_ => value)

  def fork: ZIO[Fiber[A]] =
    ZIO.Fork(self)

  def zip[B](that: ZIO[B]): ZIO[(A, B)] =
    for {
      a <- self
      b <- that
    } yield (a, b)

  def *>[B](that: => ZIO[B]): ZIO[B] =
    self zipRight that

  def zipRight[B](that: => ZIO[B]): ZIO[B] =
    zipWith(that)((_, b) => b)

  def zipWith[B, C](that: => ZIO[B])(f: (A, B) => C): ZIO[C] =
    for {
      a <- self
      b <- that
    } yield f(a, b)

  def zipPar[B](that: ZIO[B]): ZIO[(A, B)] =
    for {
      aF <- self.fork
      bF <- that.fork
      a <- aF.join
      b <- bF.join
    } yield (a, b)

  def repeat(n: Int): ZIO[Unit] =
    if (n <= 0) ZIO.succeedNow()
    else self *> repeat(n - 1)

  final def run(callback: A => Unit): Unit = {

    type Erased = ZIO[Any]
    type ErasedCallback = Any => Unit
    type Cont   = Any => Erased

    def erase[A](zio: ZIO[A]): Erased = zio

    def eraseCallback[A](cb: A => Unit): ErasedCallback =
      cb.asInstanceOf[ErasedCallback]

    val stack = scala.collection.mutable.Stack[Cont]()

    var currentZIO = erase(self)

    var loop = true

    def resume(): Unit = {
      loop = true
      run()
    }

    def complete(value: Any): Unit =
      if (stack.isEmpty) {
        loop = false
        callback(value.asInstanceOf[A])
      } else {
        val cont = stack.pop()
        currentZIO = cont(value)
      }

    def run(): Unit = {

      while (loop) {

        //println(currentZIO)

        currentZIO match {

          case ZIO.Succeed(value) =>
            complete(value)

          case ZIO.Effect(thunk) =>
            complete(thunk())

          case ZIO.Map(zio, cont) =>
            stack.push(cont andThen ZIO.succeedNow)
            currentZIO = zio

          case ZIO.FlatMap(zio, cont) =>
            stack.push(cont)
            currentZIO = zio

          case ZIO.Async(register) =>
            if (stack.isEmpty) {
              loop = false
              register(eraseCallback(callback))
            } else {
              loop = false
              register { a =>
                currentZIO = ZIO.succeedNow(a)
                resume()
              }
            }

          case ZIO.Fork(zio) =>
            val fiber = new FiberImpl(zio)
            fiber.start()
            complete(fiber)
        }
      }
    }

    run()
  }
}

object ZIO {

  // call by name
  def succeed[A](value: => A): ZIO[A] =
    ZIO.Effect(() => value)

  def succeedNow[A](value: A): ZIO[A] =
    ZIO.Succeed(value)

  def async[A](register: (A => Any) => Any): ZIO[A] =
    ZIO.Async(register)

  case class Succeed[A](value: A) extends ZIO[A]

  case class Effect[A](f: () => A) extends ZIO[A]

  case class Map[A, B](zio: ZIO[A], f: A => B) extends ZIO[B]

  case class FlatMap[A, B](zio: ZIO[A], f: A => ZIO[B]) extends ZIO[B]

  case class Async[A](register: (A => Any) => Any) extends ZIO[A]

  case class Fork[A](zio: ZIO[A]) extends ZIO[Fiber[A]]
}
