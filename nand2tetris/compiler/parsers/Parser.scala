package nand2tetris.compiler.parsers

import nand2tetris.compiler.Tokenizer.Tokens
import nand2tetris.compiler.grammar.LexicalElements._
import nand2tetris.general.State
import State._

/**
  * Created by rikmu on 17-12-2017.
  */
object Parser {
  type Parser[A] = State[Tokens, A]

  def matchIdentifier: Parser[Identifier] = State(_ match {
    case (a:Identifier) :: tokens => (Some(a), tokens)
    case tokens => (None, tokens)
  })

  def matchInteger: Parser[ConstantInteger] = State(_ match {
    case (a:ConstantInteger) :: tokens => (Some(a), tokens)
    case tokens => (None, tokens)
  })

  def matchString: Parser[ConstantString] = State(_ match {
    case (a:ConstantString) :: tokens => (Some(a), tokens)
    case tokens => (None, tokens)
  })

  def matchKeyword: Parser[Keyword] = State(_ match {
    case (a:Keyword) :: tokens => (Some(a), tokens)
    case tokens => (None, tokens)
  })

  def matchSymbol: Parser[Symbol] = State(_ match {
    case (a:Symbol) :: tokens => (Some(a), tokens)
    case tokens => (None, tokens)
  })

  def matchIf[A <: LexicalElement](f:A => Boolean): Parser[A] = State(_ match {
    case (a:A) :: tokens if f(a) => (Some(a), tokens)
    case tokens => (None, tokens)
  })

  def matchA[A <: LexicalElement](check:A): Parser[A] =
    matchIf(a => a == check)

  def matchAny[A <: LexicalElement](check:A*): Parser[A] =
    matchIf(a => check.contains(a))

  def tryAll[A](check:Parser[A]*): Parser[A] = State(tokens => {
    check.map(rewind(_)).foldLeft((None:Option[A], tokens))((acc, parser) => {
      acc._1.fold(parser.run(tokens))(res => acc)
    })
  })

  def matchEither[A <: LexicalElement, B <: LexicalElement](a: Parser[A], b: Parser[B]): Parser[LexicalElement] = State(tokens => {
    val (aRes, aTokens) = a.run(tokens)
    val bRun = b.run(tokens)

    aRes.fold[(Option[LexicalElement], Tokens)](bRun)(a => (Some(a), aTokens))
  })

  def prefix[A](prefix: LexicalElement)(parser: Parser[A]): Parser[A] = State.rewind(for {
    _  <- matchA(prefix)
    a <- parser
  } yield a)

  def suffix[A](suffix: LexicalElement)(parser: Parser[A]): Parser[A] = State.rewind(for {
    a <- parser
    _ <- matchA(suffix)
  } yield a)

  def between[A](prefix: LexicalElement, suffix: LexicalElement)(parser: => Parser[A]): Parser[A] = State.rewind(for {
    _ <- matchA(prefix)
    a <- parser
    _ <- matchA(suffix)
  } yield a)

  def parseAll[A](a: Parser[A]): Parser[Seq[A]] = {
    def cycle(sa: Parser[Seq[A]]): Parser[Seq[A]] = State(t => {
      val stepper = for {
        curr <- sa
        step <- a
      } yield curr :+ step

      val (res, tokens) = stepper.run(t)
      res.fold(sa.run(t))(la => cycle(State.unit(la)).run(tokens))
    })

    cycle(a.map(a => Seq(a))).fold[Seq[A]](Some(Seq()))((la: Seq[A]) => la)
  }

  def parseAllSpaced[A](splitter: Symbol)(pa:Parser[A]): Parser[Seq[A]] = optionalElse(Seq[A]())(for {
    head <- pa
    tail <- parseAll(prefix(splitter)(pa))
  } yield head +: tail)
}