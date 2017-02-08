package com.atomist.rug.runtime.js

import com.atomist.param.{ParameterValue, SimpleParameterValue, SimpleParameterValues}
import com.atomist.plan.TreeMaterializer
import com.atomist.project.archive.{AtomistConfig, DefaultAtomistConfig}
import com.atomist.rug.runtime.CommandContext
import com.atomist.rug.runtime.js.interop.{JavaScriptHandlerContext, jsPathExpressionEngine}
import com.atomist.rug.ts.TypeScriptBuilder
import com.atomist.source.{SimpleFileBasedArtifactSource, StringFileArtifact}
import org.scalatest.{FlatSpec, Matchers}


class JavaScriptCommandHandlerTest extends FlatSpec with Matchers{
  val atomistConfig: AtomistConfig = DefaultAtomistConfig
  val treeMaterializer: TreeMaterializer = TestTreeMaterializer

  val kitties = "ShowMeTheKitties"
  val kittyDesc = "Search Youtube for kitty videos and post results to slack"
  val simpleCommandHandler =  StringFileArtifact(atomistConfig.handlersRoot + "/Handler.ts",
    s"""
       |import {HandleCommand, Instruction, Response, CommandContext, Plan, Message} from '@atomist/rug/operations/Handlers'
       |import {CommandHandler, Parameter, Tags, Intent} from '@atomist/rug/operations/Decorators'
       |
       |@CommandHandler("ShowMeTheKitties","Search Youtube for kitty videos and post results to slack")
       |@Tags("kitty", "youtube", "slack")
       |@Intent("show me kitties","cats please")
       |class KittieFetcher implements HandleCommand{
       |
       |  @Parameter({description: "his dudeness", pattern: "^.*$$"})
       |  name: string = "dude"
       |
       |  handle(ctx: CommandContext) : Plan {
       |    let pxe = ctx.pathExpressionEngine()
       |
       |    if(this.name != "el duderino") {
       |      throw new Error("This will not stand");
       |    }
       |    let result = new Plan()
       |    result.add({kind: "execution",
       |                name: "HTTP",
       |                parameters: {method: "GET", url: "http://youtube.com?search=kitty&safe=true", as: "JSON"},
       |                onSuccess: {kind: "respond", name: "Kitties"},
       |                onError: {text: "No kitties for you today!"}})
       |    return result;
       |  }
       |}
       |
       |export let command = new KittieFetcher();
       |
    """.stripMargin)

  it should "extract and run a response handler" in {
    val rugArchive = TypeScriptBuilder.compileWithModel(SimpleFileBasedArtifactSource(simpleCommandHandler))
    val handlers = JavaScriptCommandHandler.extractHandlers(rugArchive, new JavaScriptHandlerContext("XX", treeMaterializer))
    handlers.size should be(1)
    val handler = handlers.head
    handler.name should be(kitties)
    handler.description should be (kittyDesc)
    handler.tags.size should be(3)
    handler.intent.size should be(2)
    val plan = handler.handle(SimpleContext, SimpleParameterValues(SimpleParameterValue("name","el duderino")))
    //TODO = validate the plan
  }
}

object SimpleContext extends CommandContext{
  /**
    * Id of the team we're working on behalf of
    */
  override def teamId: String = "blah"

  override def pathExpressionEngine: jsPathExpressionEngine = new jsPathExpressionEngine(this)
}
