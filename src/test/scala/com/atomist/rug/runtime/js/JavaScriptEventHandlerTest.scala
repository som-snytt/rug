package com.atomist.rug.runtime.js

import com.atomist.param.SimpleParameterValue
import com.atomist.plan.TreeMaterializer
import com.atomist.project.archive.{AtomistConfig, DefaultAtomistConfig}
import com.atomist.rug.runtime.SystemEvent
import com.atomist.rug.runtime.js.interop.JavaScriptHandlerContext
import com.atomist.rug.spi.{InstructionKind, MavenCoordinate, MessageText}
import com.atomist.rug.spi.Handlers.{Instruction, Message, Plan}
import com.atomist.rug.ts.TypeScriptBuilder
import com.atomist.source.{SimpleFileBasedArtifactSource, StringFileArtifact}
import com.atomist.tree.pathexpression.PathExpression
import com.atomist.tree.{TerminalTreeNode, TreeNode}
import org.scalatest.{DiagrammedAssertions, FlatSpec, Matchers}


object JavaScriptEventHandlerTest {
  val atomistConfig: AtomistConfig = DefaultAtomistConfig
  val treeMaterializer: TreeMaterializer = TestTreeMaterializer

  val reOpenIssueHandlerName = "ClosedIssueReopener"

  val reOpenIssueHandlerDesc = "Reopens closed issues"

  val reOpenCloseIssueProgram =  StringFileArtifact(atomistConfig.handlersRoot + "/Handler.ts",
    s"""
       |import {HandleEvent, Plan, Message} from '@atomist/rug/operations/Handlers'
       |import {TreeNode, Match, PathExpression} from '@atomist/rug/tree/PathExpression'
       |import {EventHandler, Tags} from '@atomist/rug/operations/Decorators'
       |
       |@EventHandler("$reOpenIssueHandlerName", "$reOpenIssueHandlerDesc", new PathExpression<TreeNode,TreeNode>("/issue"))
       |@Tags("github", "issues")
       |class SimpleHandler implements HandleEvent<TreeNode,TreeNode> {
       |  handle(event: Match<TreeNode, TreeNode>){
       |    let issue = event.root
       |    let plan = new Plan()
       |
       |    let message = new Message("message1")
       |
       |    const treeNode = ((): TreeNode => {
       |      const x: any = function() {}
       |      x.nodeName = function(){ "nodeName1" }
       |      x.nodeTags = function(){ ["nodeTags1"] }
       |      x.children = function(){ [] }
       |      return x;
       |    });
       |    message.regarding(treeNode());
       |    message.addAction({
       |                kind: "command",
       |                name: {name: "n", group: "g", artifact: "a", version: "v"}
       |    })
       |    plan.add(message);
       |
       |    plan.add({kind: "execute",
       |                name: "HTTP",
       |                parameters: {method: "GET", url: "http://youtube.com?search=kitty&safe=true", as: "JSON"},
       |                onSuccess: {kind: "respond", name: "Kitties"},
       |                onError: {text: "No kitties for you today!"}});
       |    return plan;
       |  }
       |}
       |export let handler = new SimpleHandler();
      """.stripMargin)
}

class JavaScriptEventHandlerTest extends FlatSpec with Matchers with DiagrammedAssertions {

  import JavaScriptEventHandlerTest._

  it should "extract and run an event handler" in {
    val rugArchive = TypeScriptBuilder.compileWithModel(SimpleFileBasedArtifactSource(JavaScriptEventHandlerTest.reOpenCloseIssueProgram))
    val handlers = JavaScriptEventHandler.extractHandlers(rugArchive, new JavaScriptHandlerContext("XX", treeMaterializer))
    handlers.size should be(1)
    val handler = handlers.head
    handler.rootNodeName should be("issue")
    handler.tags.size should be (2)
    handler.name should be (reOpenIssueHandlerName)
    handler.description should be (reOpenIssueHandlerDesc)
    handler.pathExpression should not be null

    val actualPlan = handler.handle(SysEvent)
    val expectedPlan = Some(Plan(
      Seq(
        Message(MessageText("message1"),
          Seq(
            Instruction(
            None,
            "n",
            InstructionKind.Command,
            None,
            Some(MavenCoordinate("g", "a", "v")),
            None,
            None)
          ),
          None,
          None)
      ),
      Seq(
        Instruction(
          None,
          "HTTP",
          InstructionKind.Execute,
          Some(Seq(
            SimpleParameterValue("method", "GET"),
            SimpleParameterValue("url", "http://youtube.com?search=kitty&safe=true"),
            SimpleParameterValue("as", "JSON")
          )),
          None,
          None,
          None)
      )
    ))
    assert(actualPlan == expectedPlan)
  }
}

object SysEvent extends SystemEvent ("blah", "issue", 0l)

class IssueTreeNode extends TerminalTreeNode {

  override def nodeName: String = "issue"

  override def value: String = "blah"

  def state(): String = "closed"

  val number: Int = 10

  val repo: String = "rug"

  val owner: String = "atomist"

}

object TestTreeMaterializer extends TreeMaterializer {

  override def rootNodeFor(e: SystemEvent, pe: PathExpression): TreeNode = new IssueTreeNode()

  override def hydrate(teamId: String, rawRootNode: TreeNode, pe: PathExpression): TreeNode = rawRootNode
}
