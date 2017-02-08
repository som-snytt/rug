package com.atomist.project.archive

import com.atomist.rug.runtime.js.{JavaScriptCommandHandler, JavaScriptEventHandler, JavaScriptResponseHandler}
import com.atomist.rug.runtime.js.interop.JavaScriptHandlerContext
import com.atomist.rug.runtime.{CommandHandler, ResponseHandler, SystemEventHandler}
import com.atomist.source.ArtifactSource

/**
  * Gather all the handlers in an archive
  */
object HandlerArchiveReader {

  def extractHandlers(as: ArtifactSource, ctx: JavaScriptHandlerContext): Handlers = {
    Handlers(
      JavaScriptCommandHandler.extractHandlers(as, ctx),
      JavaScriptResponseHandler.extractHandlers(as,ctx),
      JavaScriptEventHandler.extractHandlers(as,ctx));
  }
}

case class Handlers(command: Seq[CommandHandler], response: Seq[ResponseHandler], event: Seq[SystemEventHandler])
