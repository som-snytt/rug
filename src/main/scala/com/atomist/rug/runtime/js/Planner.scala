package com.atomist.rug.runtime.js

import com.atomist.param.{ParameterValue, SimpleParameterValue}
import com.atomist.rug.spi.Handlers.Instruction.Respond
import com.atomist.rug.spi.Handlers._
import jdk.nashorn.api.scripting.ScriptObjectMirror
import jdk.nashorn.internal.runtime.Undefined

/**
  * That's a great plan, Walter. That's f***ing ingenious, if I understand it correctly. That's a Swiss f***ing watch.
  */
class Planner {

  def constructPlan(jsPlan: ScriptObjectMirror): Plan = {
    val jsMessages = jsPlan.getMember("messages").asInstanceOf[ScriptObjectMirror].values()
    val messages: Seq[Message] = jsMessages.toArray.toList.map { message =>
      val m = message.asInstanceOf[ScriptObjectMirror]
      constructMessage(m)
    }
    val instructions = constructInstructions(jsPlan)
    Plan(messages, instructions)
  }

  def constructMessage(jsMessage: ScriptObjectMirror): Message = {
    val instructions = constructInstructions(jsMessage)
    val messageBody = (jsMessage.getMember("body"), jsMessage.getMember("text")) match {
      case (json: ScriptObjectMirror, _) =>
        JsonBody(json.entrySet().toString)
      case (_, text: String) =>
        MessageText(text)
      case _ =>
        throw new IllegalStateException(s"Cannot determine message content from body: ${jsMessage.getMember("body")} or text: ${jsMessage.getMember("text")}")
    }
    Message(
      messageBody,
      instructions,
      None
    )
  }

  def constructInstructions(jsInstructions: ScriptObjectMirror): Seq[Instruction] = {
    jsInstructions.getMember("instructions") match {
      case u: Undefined => Nil
      case jsInstructions: ScriptObjectMirror =>
        jsInstructions.values().toArray.toList.map { instruction =>
          val jsInstruction = instruction.asInstanceOf[ScriptObjectMirror]
          val jsInstructionKind = jsInstruction.getMember("kind").asInstanceOf[String]
          val detail = constructInstructionDetail(jsInstruction)
          Instruction.from(jsInstructionKind, detail)
        }
    }
  }

  def constructInstructionDetail(jsInstruction: ScriptObjectMirror): Instruction.Detail = {
      val parameters: Seq[ParameterValue] = jsInstruction.getMember("parameters") match {
        case u: Undefined => Nil
        case o: ScriptObjectMirror =>
          val params = o.keySet().toArray.toList.map { key =>
            val name = key.asInstanceOf[String]
            val value = o.getMember(name)
            SimpleParameterValue(name, value)
          }
          params
      }
      val (name, coordinates) = jsInstruction.getMember("name") match {
        case name: String =>
          (name, None)
        case o: ScriptObjectMirror =>
          val name = o.getMember("name").asInstanceOf[String]
          val coordinates = MavenCoordinate(
            o.getMember("group").asInstanceOf[String],
            o.getMember("artifact").asInstanceOf[String],
            o.getMember("version").asInstanceOf[String]
          )
          (name, Some(coordinates))
      }
      val onSuccess: Option[Callback] = constructCallback(jsInstruction.getMember("onSuccess"))
      val onError: Option[Callback] = constructCallback(jsInstruction.getMember("onError"))
      Instruction.Detail(
        name,
        coordinates,
        parameters,
        onSuccess,
        onError
      )
  }

  def constructCallback(callback: Object): Option[Callback] = {
    callback match {
      case u: Undefined => None
      case jsOnSuccess: ScriptObjectMirror =>
        (jsOnSuccess.hasMember("text"), jsOnSuccess.hasMember("body"), jsOnSuccess.hasMember("kind"), jsOnSuccess.hasMember("messages")) match {
          case (true, _, _, _) => Some(constructMessage(jsOnSuccess))
          case (false, true, _, _) => Some(constructMessage(jsOnSuccess))
          case (false, false, true, _) => Some(Respond(constructInstructionDetail(jsOnSuccess)))
          case (false, false, false, true) => Some(constructPlan(jsOnSuccess))
          case _ => throw new IllegalStateException(s"Cannot derive into CallBack: $jsOnSuccess")
        }
    }
  }

}
