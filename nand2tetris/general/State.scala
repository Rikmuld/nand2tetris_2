package nand2tetris.general

import State._

case class State[S, +A](run: S => (Option[A], S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    flatFold(None)(f)

  def fold[B](b:Option[B])(f: A => B): State[S, B] =
    flatFold(b)(a => unit(f(a)))

  def flatFold[B](b:Option[B])(f: A => State[S, B]): State[S, B] = State(s => {
    val (oa, s1) = run(s)
    oa.fold((b, s))(a => f(a).run(s1))
  })
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (Some(a), s))

  def none[S, A]: State[S, A] =
    State(s => (None, s))

  def sequence[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] =
    State(s => (Some(s), s))

  def set[S](s: S): State[S, Unit] =
    State(_ => (Some(), s))

  def rewind[S, A](s: State[S, A]): State[S, A] = State(tokens => {
    val (result, newTokens) = s.run(tokens)
    result.fold[(Option[A], S)]((None, tokens))(a => (result, newTokens))
  })

  def optional[S, A](s: State[S, A]): State[S, Option[A]] = State(tokens => {
    val (result, newTokens) = s.run(tokens)
    result.fold[(Option[Option[A]], S)]((Some(None), tokens))(a => (Some(result), newTokens))
  })

  def optionalElse[S, A](z: A)(s: State[S, A]): State[S, A] =
    optional(s).map(a => a.fold(z)(b => b))
}