package com.atomist.project.archive

import com.atomist.project.SimpleProjectOperationArguments
import com.atomist.rug.Import
import com.atomist.rug.runtime.js.TypeScriptRugEditorTest
import com.atomist.rug.runtime.js.interop.{JavaScriptEventHandlerTest, jsPathExpressionEngine}
import com.atomist.rug.runtime.lang.js.NashornConstructorTest
import com.atomist.rug.ts.TypeScriptBuilder
import com.atomist.source.{ArtifactSource, SimpleFileBasedArtifactSource, StringFileArtifact}
import org.scalatest.{FlatSpec, Matchers}

class ProjectOperationArchiveReaderTest extends FlatSpec with Matchers {

  val atomistConfig: AtomistConfig = DefaultAtomistConfig

  val FirstEditor = StringFileArtifact(atomistConfig.editorsRoot + "/First.rug",
    """
      |editor First
      |
      |with File f do setPath ""
    """.stripMargin
  )

  val SecondOp = StringFileArtifact(atomistConfig.reviewersRoot + "/Second.rug",
    """
      |reviewer Second
      |
      |with File f do setPath ""
    """.stripMargin
  )

  val Generator = StringFileArtifact(atomistConfig.editorsRoot + "/Published.rug",
    """
      |generator Published
      |
      |First
    """.stripMargin
  )


  val EditorWithImports = StringFileArtifact(atomistConfig.editorsRoot + "/EditorWithImports.rug",
    """
      |editor EditorWithImports
      |
      |uses atomist.common-editors.UpdateReadme
      |uses atomist.common-editors.PomParameterizer
      |
      |with File f do setPath ""
    """.stripMargin
  )

  val EditorWithProjectName = StringFileArtifact(atomistConfig.editorsRoot + "/Stuff.rug",
    """
      |editor Stuff
      |
      |param project_name: ^.*$
      |
      |with File f do setPath ""
    """.stripMargin
  )

  val AnotherEditorWithProjectName = StringFileArtifact(atomistConfig.editorsRoot + "/MoreStuff.rug",
    """
      |editor MoreStuff
      |
      |param project_name: ^.*$
      |
      |with File f do setPath ""
    """.stripMargin
  )

  val GeneratorWithoutProjectName = StringFileArtifact(atomistConfig.editorsRoot + "/Published.rug",
    """
      |generator Published
      |
      |uses Stuff
      |uses MoreStuff
      |
      |Stuff
      |MoreStuff
    """.stripMargin
  )

  //https://github.com/atomist/rug/issues/258
  it should "only describe a single project_name parameter if it's declared" in {
    val apc = new ProjectOperationArchiveReader(atomistConfig)
    val ops = apc.findOperations(new SimpleFileBasedArtifactSource("", Seq(GeneratorWithoutProjectName, EditorWithProjectName, AnotherEditorWithProjectName)), None, Nil)
    assert(ops.generators.size === 1)
    assert(ops.generators.head.parameters.size === 1)
  }

  it should "parse single editor" in {
    val apc = new ProjectOperationArchiveReader(atomistConfig)
    val ops = apc.findOperations(new SimpleFileBasedArtifactSource("", FirstEditor), None, Nil)
    assert(ops.editors.size === 1)
    assert(ops.editorNames === Seq("First"))
    assert(ops.reviewers === Nil)
    assert(ops.reviewerNames === Nil)
    assert(ops.generators === Nil)
    assert(ops.generatorNames === Nil)
  }

  it should "parse editor and reviewer" in {
    val apc = new ProjectOperationArchiveReader(atomistConfig)
    val ops = apc.findOperations(new SimpleFileBasedArtifactSource("", Seq(FirstEditor, SecondOp)), None, Nil)
    assert(ops.editors.size === 1)
    assert(ops.editorNames === Seq("First"))
    assert(ops.reviewers.size === 1)
    assert(ops.reviewerNames === Seq("Second"))
    assert(ops.generators === Nil)
    assert(ops.generatorNames === Nil)
  }

  it should "parse editor and reviewer and generator" in {
    val apc = new ProjectOperationArchiveReader(atomistConfig)
    val ops = apc.findOperations(new SimpleFileBasedArtifactSource("", Seq(FirstEditor, SecondOp, Generator)), None, Nil)
    assert(ops.editors.size === 1)
    assert(ops.editorNames.toSet === Set("First"))
    assert(ops.reviewers.size === 1)
    assert(ops.reviewerNames === Seq("Second"))
    assert(ops.generators.size === 1)
    assert(ops.generatorNames === Seq("Published"))
  }

  it should "find imports in a project that uses them" in {
    val apc = new ProjectOperationArchiveReader(atomistConfig)
    val imports = apc.findImports(new SimpleFileBasedArtifactSource("", Seq(FirstEditor, SecondOp, Generator, EditorWithImports)))
    imports should equal(Seq(
      Import("atomist.common-editors.UpdateReadme"),
      Import("atomist.common-editors.PomParameterizer")
    ))
  }

  it should "find no imports in a project that uses none" in {
    val apc = new ProjectOperationArchiveReader(atomistConfig)
    val imports = apc.findImports(new SimpleFileBasedArtifactSource("", Seq(FirstEditor, SecondOp, Generator)))
    imports should equal(Nil)
  }

  it should "find typescript editor" in {
    val apc = new ProjectOperationArchiveReader(atomistConfig)
    val as = TypeScriptBuilder.compileWithModel(SimpleFileBasedArtifactSource(
      StringFileArtifact(".atomist/editors/SimpleEditor.ts", TypeScriptRugEditorTest.SimpleEditorTaggedAndMeta)
    ))
    val ops = apc.findOperations(as, None, Nil)
    assert(ops.editors.size === 1)
    assert(ops.editors.head.parameters.size === 2)
  }

  it should "allow invocation of other operation from TypeScript editor" in pending

  it should "find and invoke plain javascript generators" in {
    val apc = new ProjectOperationArchiveReader(atomistConfig)
    val f1 = StringFileArtifact("package.json", "{}")
    val f2 = StringFileArtifact("app/Thing.js", "var Thing = {};")

    val rugAs = SimpleFileBasedArtifactSource(
      StringFileArtifact(".atomist/editors/SimpleEditor.js",
        NashornConstructorTest.SimpleJavascriptEditor),
      f1,
      f2
    ) + TypeScriptBuilder.userModel

    val ops = apc.findOperations(rugAs, None, Nil)
    assert(ops.editors.size === 1)
    assert(ops.editors.head.parameters.size === 1)
  }

  it should "ignore unbound handler in the new style" in {
    val apc = new ProjectOperationArchiveReader(atomistConfig)
    val f1 = StringFileArtifact("package.json", "{}")
    val f2 = StringFileArtifact("app/Thing.ts", "class Thing {}")

    val rugAs = TypeScriptBuilder.compileWithModel(SimpleFileBasedArtifactSource(
      StringFileArtifact(".atomist/editors/SimpleGenerator.ts",
        TypeScriptRugEditorTest.SimpleGenerator),
      f1,
      f2,
      JavaScriptEventHandlerTest.reOpenCloseIssueProgram
    ))
    val ops = apc.findOperations(rugAs, None, Nil)
    assert(ops.generators.size === 1)
    assert(ops.generators.head.parameters.size === 0)
    val result = ops.generators.head.generate("woot", SimpleProjectOperationArguments("", Map("content" -> "woot")))
    // Should preserve content from the backing archive
    result.findFile(f1.path).get.content.equals(f1.content) should be(true)
    result.findFile(f2.path).get.content.equals(f2.content) should be(true)

    // Should contain new contain
    result.findFile("src/from/typescript").get.content.contains("Anders") should be(true)
  }
}