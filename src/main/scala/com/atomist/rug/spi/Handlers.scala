package com.atomist.rug.spi

import com.atomist.param.ParameterValue
import com.atomist.rug.kind.core.ProjectMutableView
import com.atomist.tree.TreeNode

/**
  * Beans that map to the @atomist/rug/operations/operations/Handlers
  */

object Handlers {


  case class Plan(messages: Seq[Message], instructions: Seq[Instruction]) extends Callback

  object Plan {
    def build(fromNashon: Any): Option[Plan] = {
      Some(Plan(Nil, Nil))
    }
  }

  case class Message(body: MessageBody,
                     instructions: Seq[Instruction],
                     node: Option[TreeNode],
                     channelId: Option[String]) extends Callback

  case class Instruction(project: Option[ProjectMutableView],
                         name: String,
                         kind: InstructionKind.Value,
                         parameters: Option[Seq[ParameterValue]],
                         coordinates: Option[MavenCoordinate],
                         success: Option[Callback],
                         failure: Option[Callback]) extends Callback

  sealed trait Callback
}



object InstructionKind extends Enumeration {
  val Generate = Value("generate")
  val Edit = Value("edit")
  val Review = Value("review")
  val Execute = Value("execute")
  val Respond = Value("respond")
  val Command = Value("command")
}

case class MavenCoordinate(
  group: String,
  artifact: String,
  version: String
)


sealed trait MessageBody
case class JsonBody(json: String) extends MessageBody
case class MessageText(text: String) extends MessageBody
