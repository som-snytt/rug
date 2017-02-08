package com.atomist.project.archive

import com.atomist.plan.TreeMaterializer
import com.atomist.rug.runtime.js.TestTreeMaterializer
import com.atomist.rug.runtime.js.interop.JavaScriptHandlerContext
import com.atomist.rug.ts.TypeScriptBuilder
import com.atomist.source.file.ClassPathArtifactSource
import org.scalatest.{FlatSpec, Matchers}

class HandlerArchiveReaderTest extends FlatSpec with Matchers {

  val atomistConfig: AtomistConfig = DefaultAtomistConfig
  val treeMaterializer: TreeMaterializer = TestTreeMaterializer

  it should "load handlers of different kinds from an archive" in {
    //val as = TestUtils.editorInSideFile(this, "MyHandlers.ts")
    val ts = ClassPathArtifactSource.toArtifactSource("com/atomist/project/archive/MyHandlers.ts")
    val moved = ts.withPathAbove(".atomist/handlers")
    val as = TypeScriptBuilder.compileWithModel(moved)
    val handlers = HandlerArchiveReader.extractHandlers(as, new JavaScriptHandlerContext("XX", treeMaterializer))
    handlers.command.size should be(2)
    handlers.response.size should be(2)
    handlers.event.size should be(1)
  }
}
