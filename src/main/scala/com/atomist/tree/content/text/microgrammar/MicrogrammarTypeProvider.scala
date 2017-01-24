package com.atomist.tree.content.text.microgrammar

import com.atomist.rug.kind.core.{FileArtifactBackedMutableView, FileType}
import com.atomist.rug.kind.dynamic.{ChildResolver, MutableContainerMutableView, MutableTreeNodeUpdater}
import com.atomist.rug.spi.{MutableView, TypeProvider, Typed}
import com.atomist.tree.content.text.MutableContainerTreeNode
import com.atomist.tree.content.text.grammar.MatchListener

/**
  * Type information for the results of evaluating a microgrammar.
  * It will have the name of the microgrammar, and can be
  * evaluated against any file
  *
  * @param microgrammar microgrammar to evaluate
  */
class MicrogrammarTypeProvider(microgrammar: Microgrammar)
  extends TypeProvider(classOf[MutableContainerMutableView])
    with ChildResolver {

  override val name: String = microgrammar.name

  override def description: String = s"Microgrammar type for [$name]"

  /**
    * Microgrammars can only be resolved from under files
    * @return set of node types this can resolve from. File type only
    */
  override def resolvesFromNodeTypes: Set[String] = Set(Typed.typeClassToTypeName(classOf[FileType]))

  override def findAllIn(context: MutableView[_]): Option[Seq[MutableView[_]]] = context match {
    case f: FileArtifactBackedMutableView =>
      val l: Option[MatchListener] = None
      val yay = microgrammar.findMatches(f.content, l)
      val views = yay collect {
        case moo: MutableContainerTreeNode =>
          f.registerUpdater(new MutableTreeNodeUpdater(moo))
          new MutableContainerMutableView(moo, f)
      }

      Some(views)
    case _ => None
  }
}
