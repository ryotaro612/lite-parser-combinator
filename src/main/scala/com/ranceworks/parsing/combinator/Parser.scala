package com.ranceworks.parsing.combinator

/**
  * Hutton, Graham, and Erik Meijer. "Monadic parser combinators." (1996).
  * http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf
  */
trait Parsers {
  def Parser[A](f: String => List[(A, String)]): Parser[A] = new Parser[A]{
    def apply(input: String) = f(input)
  }

  def success[A](v: A): Parser[A] = Parser{ input => List((v, input)) }

  def failure[A]: Parser[A] = Parser { input => List() }

  trait Parser[A] extends (String => List[(A, String)]) {
    def apply(input: String): List[(A, String)]

    def flatMap[B](f: A => Parser[B]) = Parser{ (input: String) =>
      for {
        (a, input2) <- this(input)
        b <- f(a)(input2)
      } yield b
    }

    def withFilter(pred: A => Boolean): Parser[A] = Parser { input: String =>
      for {
        elm <- this(input) if pred(elm._1)
      } yield elm
    }

    def map[B](f: A => B) = Parser {input =>
      for {
        a <- this(input)
      } yield(f(a._1), a._2)
    }
  }
}
