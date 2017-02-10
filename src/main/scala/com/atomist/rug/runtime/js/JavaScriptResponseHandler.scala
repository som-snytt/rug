package com.atomist.rug.runtime.js

import com.atomist.param.{Parameter, ParameterValues, Tag}
import com.atomist.rug.InvalidHandlerResultException
import com.atomist.rug.runtime.js.interop.JavaScriptHandlerContext
import com.atomist.rug.runtime.{InstructionResponse, ParameterizedHandler, ResponseHandler}
import com.atomist.rug.spi.Handlers.Plan
import com.atomist.source.ArtifactSource
import jdk.nashorn.api.scripting.ScriptObjectMirror

object JavaScriptResponseHandler extends HandlerFinder[JavaScriptResponseHandler] {
  /**
    *
    * The value of __kind in JS
    * @return
    */
  override val kind: String = "response-handler"

  override protected def extractHandler(jsc: JavaScriptContext, handler: ScriptObjectMirror, as: ArtifactSource, ctx: JavaScriptHandlerContext): Option[JavaScriptResponseHandler] = {
    Some(new JavaScriptResponseHandler(jsc, handler, as, name(handler), description(handler), parameters(handler), tags(handler)))
  }
}

class JavaScriptResponseHandler (jsc: JavaScriptContext,
                                 handler: ScriptObjectMirror,
                                 as: ArtifactSource,
                                 override val name: String,
                                 override val description: String,
                                 parameters: Seq[Parameter],
                                 override val tags: Seq[Tag])
  extends ParameterizedHandler
    with ResponseHandler
    with JavaScriptUtils {

  addParameters(parameters)

  override def handle(response: InstructionResponse, params: ParameterValues): Option[Plan] = {
    //TODO this handle method is almost identical to the command handler - extract it
    val validated = addDefaultParameterValues(params)
    validateParameters(validated)
    invokeMemberFunction(jsc, handler, "handle", response, validated) match {
      case plan: ScriptObjectMirror => ConstructPlan(plan)
      case other => throw new InvalidHandlerResultException(s"$name ResponseHandler did not return a recognized response ($other) when invoked with ${params.toString()}")
    }
  }
}
