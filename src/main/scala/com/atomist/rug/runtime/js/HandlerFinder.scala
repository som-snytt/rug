package com.atomist.rug.runtime.js

import com.atomist.param.{Parameter, Tag}
import com.atomist.rug.runtime.Handler
import com.atomist.rug.runtime.js.interop.JavaScriptHandlerContext
import com.atomist.source.ArtifactSource
import jdk.nashorn.api.scripting.ScriptObjectMirror

/**
  * Some common things used to extract handlers from nashorn
  */
trait HandlerFinder[T <: Handler] extends JavaScriptUtils{

  /**
    * The value of __kind in JS
    * @return
    */
  def kind: String
  /**
    * Is the supplied thing valid at all?
    * @param obj
    * @return
    */
  protected def isValidHandler(obj: ScriptObjectMirror): Boolean = {
    obj.hasMember("__kind") && obj.hasMember("__name") && obj.hasMember("__description") && obj.hasMember("handle")
  }

  def extractHandlers(rugAs: ArtifactSource, ctx: JavaScriptHandlerContext): Seq[T] = {
    val jsc = new JavaScriptContext(rugAs)
    jsc.vars.collect {
      case Var(_, handler) if isValidHandler(handler) && handler.getMember("__kind") == kind => extractHandler(jsc, handler, rugAs, ctx)
    }.flatten
  }

  protected def extractHandler(jsc: JavaScriptContext, handler: ScriptObjectMirror, rugAs: ArtifactSource, ctx: JavaScriptHandlerContext): Option[T]

  protected def name(obj: ScriptObjectMirror): String = obj.getMember("__name").asInstanceOf[String]
  protected def description(obj: ScriptObjectMirror): String = obj.getMember("__description").asInstanceOf[String]
  protected def kind(obj: ScriptObjectMirror): String = obj.getMember("__kind").asInstanceOf[String]

  protected def tags(someVar: ScriptObjectMirror): Seq[Tag] = {
    tags(someVar, Seq("__tags"))
  }
  /**
    * Either read the parameters field or look for annotated parameters
    * @return
    */
  protected def parameters(someVar: ScriptObjectMirror) : Seq[Parameter] = {
    parameters(someVar, Seq("__parameters"))
  }
}

