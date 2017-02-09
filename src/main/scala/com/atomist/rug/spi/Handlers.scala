package com.atomist.rug.spi

import com.atomist.param.ParameterValue
import com.atomist.rug.kind.core.ProjectMutableView
import com.atomist.rug.spi.Handlers.Instruction.Detail
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
                     channelId: Option[String]) extends Callback

  sealed trait Callback

  sealed trait Instruction {
    def detail: Detail
  }
  object Instruction {
    case class Detail(name: String,
                      coordinates: Option[MavenCoordinate],
                      parameters: Seq[ParameterValue],
                      success: Option[Callback],
                      failure: Option[Callback])
    def from(name: String, detail: Detail): Instruction = {
      name match {
        case "generate" => Generate(detail)
        case "edit" => Edit(detail)
        case "review" => Review(detail)
        case "execute" => Execute(detail)
        case "command" => Command(detail)
        case "respond" => Respond(detail)
        case _ => throw new IllegalArgumentException(s"Cannot derive Instruction from '$name'.")
      }
    }
    case class Generate(detail: Detail) extends Instruction
    case class Edit(detail: Detail) extends Instruction
    case class Review(detail: Detail) extends Instruction
    case class Execute(detail: Detail) extends Instruction
    case class Command(detail: Detail) extends Instruction
    case class Respond(detail: Detail) extends Instruction with Callback
  }

  case class MavenCoordinate(group: String,
                             artifact: String,
                             version: String)

  sealed trait MessageBody
  case class JsonBody(json: String) extends MessageBody
  case class MessageText(text: String) extends MessageBody
}
