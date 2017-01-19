package com.atomist.rug.runtime.rugdsl

import com.atomist.project.SimpleProjectOperationArguments
import com.atomist.project.edit.SuccessfulModification
import com.atomist.rug.DefaultRugPipeline
import com.atomist.rug.kind.DefaultTypeRegistry
import com.atomist.source.{SimpleFileBasedArtifactSource, StringFileArtifact}
import org.scalatest.{FlatSpec, Matchers}

object RugGeneratorTest {

  val SimpleGeneratorWithProjectNameParameter =
    """
      |@generator
      |editor SimpleEditor
      |
      |let project_name = $(/).name
      |
      |with Project
      |   do addFile "README" project_name
    """.stripMargin
}

class RugGeneratorTest extends FlatSpec with Matchers {

  import RugGeneratorTest._

  it should "run simple generator with default project_name parameter populated" in {

    val generator = StringFileArtifact(s".atomist/editors/SimpleEditor.rug", SimpleGeneratorWithProjectNameParameter)

    val as = SimpleFileBasedArtifactSource(generator)

    val ops = new DefaultRugPipeline(DefaultTypeRegistry).create(as, None)

    val red = ops.head.asInstanceOf[RugDrivenProjectEditor]

    red.name should be("SimpleEditor")

    val target = SimpleFileBasedArtifactSource()

    val p = SimpleProjectOperationArguments("", Map[String, Object]())

    red.modify(target, p) match {
      case sm: SuccessfulModification =>
        sm.result.totalFileCount should be(1)
        sm.result.findFile("README").get.content.contains("") should be(true)
      case _ => fail("generated project and no modifications")
    }
  }
}
