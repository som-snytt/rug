package com.atomist.tree.content.text.microgrammar

import com.atomist.tree.content.text.microgrammar.dsl.MatcherDefinitionParser


object MatcherMicrogrammarConstruction {

  private val matcherParser = new MatcherDefinitionParser()

  // I feel like this should return an Either. will wait for a use case
  def matcherMicrogrammar(name: String, grammar: String, submatchers: Map[String, Any] = Map()): MatcherMicrogrammar = {

    val matcherRegistry = EmptyMatcherRegistry //I don't think this is going to be a thing. I have other plans

    val parsedMatcher = matcherParser.parseMatcher(name, grammar, matcherRegistry)

    new MatcherMicrogrammar(parsedMatcher, name)
  }

}
