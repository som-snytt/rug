package com.atomist.tree.content.text.microgrammar.dsl

import com.atomist.rug.BadRugException
import com.atomist.source.StringFileArtifact
import com.atomist.tree.content.text.microgrammar._
import com.atomist.tree.content.text.microgrammar.matchers.Break
import com.atomist.util.scalaparsing.CommonTypesParser

import scala.util.matching.{Regex => ScalaRegex}

/**
  * Parse our matcher DSL using a Scala parser combinator.
  */
class MatcherDefinitionParser extends CommonTypesParser {

  import MatcherDefinitionParser._

  // This parser does NOT skip whitespace, unlike most parser combinators.
  // So we need to override this value.
  override protected val whiteSpace: ScalaRegex = "".r

  private def whitespaceSep: Parser[String] = """\s*""".r

  private def delimitedLiteral: Parser[Literal] =
    (StrictLiteralOpen ~ s"[^$StrictLiteralClose]*".r ~ StrictLiteralClose) ^^ {
      case _ ~ l ~ _ => Literal(l)
    }

  private def singleWordLiteral: Parser[Literal] =
    AnythingButReservedCharacters ^^ (l => Literal(l))

  private def literal: Parser[Literal] = delimitedLiteral | singleWordLiteral

  private def rex(implicit matcherName: String): Parser[Regex] =
    RegexpOpenToken ~> anythingBut(Set(escape(RegexpCloseToken), escape(BreakOpenToken))) <~ RegexpCloseToken ^^ (r => Regex(r, Some(matcherName)))

  /**
    * Skip till this clause
    *
    * @return
    */
  private def break(implicit matcherName: String): Parser[Break] =
    BreakOpenToken ~> matcherExpression <~ BreakCloseToken ^^ (m => Break(m))

  private def predicateValue: Parser[String] = "true" | "false" | "\\d+".r

  // Applies to a boxed clause
  // [curlyDepth=1]
  private def predicate(implicit matcherName: String): Parser[StatePredicateTest] =
  PredicateOpenToken ~> ident ~ "=" ~ predicateValue <~ PredicateCloseToken ^^ {
    case predicateName ~ "=" ~ predicateVal => StatePredicateTest(predicateName, predicateVal)
  }


  private def matcherTerm(implicit matcherName: String): Parser[Matcher] =
    rex |
      break |
      literal |
      inlineReference()

  private def concatenation(implicit matcherName: String): Parser[Matcher] =
    matcherTerm ~ opt(whitespaceSep) ~ matcherExpression ^^ {
      case left ~ _ ~ right =>
        //left ~? right
        Concat(Concat(left, Whitespace.?()), right, matcherName)
    }

  // $name:[.*]
  private def inlineReference()(implicit matcherName: String): Parser[Matcher] =
    VariableDeclarationToken ~> ident ~ ":" ~ rex ^^ {
      case newName ~ _ ~ regex => regex.copy(givenName = Some(newName))
    }

  private def matcherExpression(implicit matcherName: String): Parser[Matcher] =
      concatenation |
      matcherTerm

  /**
    * Parse the given microgrammar definition given a registry of known matchers.
    * Caller is responsible for updating the registry is they wish
    *
    * @param name       for the matcher
    * @param matcherDef definition
    * @return matcher definition
    */
  @throws[BadRugException]
  def parseMatcher(name: String, matcherDef: String): Matcher = matcherDef match {
    case null =>
      throw new BadRugException(s"The null string is not a valid microgrammar") {}
    case _ =>
      implicit val matcherName: String = name
      val m = parseTo(StringFileArtifact("<input>", matcherDef), phrase(matcherExpression))
      m
  }

}

object MatcherDefinitionParser {

  val BreakOpenToken = "¡"
  val BreakCloseToken = "¡"
  val RegexpOpenToken = "§"
  val RegexpCloseToken = "§"
  val DescendToken = "▶"
  val PredicateOpenToken = "["
  val PredicateCloseToken = "]"
  val VariableDeclarationToken = "$"
  val StrictLiteralOpen = "⟦"
  val StrictLiteralClose = "⟧"

  private def escape(token: String) = """\""" + token

  def anythingBut(tokens: Set[String]): ScalaRegex =
    ("""[^""" + // NOT any of the following characters
      tokens.mkString("") +
      """]+""").r // at least one of any other character

  val AnythingButReservedCharacters: ScalaRegex =
    anythingBut(Set(
      DescendToken,
      """\s""", // whitespace
      escape(PredicateOpenToken),
      escape(PredicateCloseToken),
      escape(BreakOpenToken), // didn't include BreakCloseToken because they're currently identical
      escape(RegexpOpenToken),  // didn't include RegexpCloseToken because they're currently identical
      VariableDeclarationToken
    ))
}

private case class StatePredicateTest(name: String, value: String)