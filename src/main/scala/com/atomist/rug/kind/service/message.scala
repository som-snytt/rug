package com.atomist.rug.kind.service

import com.atomist.tree.TreeNode

class MessageBuilder(sender: Message => Unit) {

  def regarding(n: TreeNode, teamId: String): Message =
    Message(send, teamId, node = n)

  def say(msg: String, teamId: String): Message =
    Message(send, teamId, message = msg)

  def send(message: Message): Unit = sender(message)

}


// We use null for interop and JSON
case class Message(sender: Message => Unit,
                   teamId: String,
                   node: TreeNode = null,
                   message: String = null,
                   action: String = null) {

  def withAction(s: String): Message = copy(action = s)

  def send(): Unit = sender(this)

}

object ConsoleMessageBuilder extends MessageBuilder(
  m => println(m)
)