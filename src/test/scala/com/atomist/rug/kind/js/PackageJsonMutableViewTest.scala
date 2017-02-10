package com.atomist.rug.kind.js

import com.atomist.param.SimpleParameterValues
import com.atomist.rug.kind.core.ProjectMutableView
import com.atomist.rug.runtime.rugdsl.SimpleFunctionInvocationContext
import com.atomist.source.{EmptyArtifactSource, SimpleFileBasedArtifactSource, StringFileArtifact}
import org.scalatest.{FlatSpec, Matchers}

class PackageJsonMutableViewTest extends FlatSpec with Matchers {

  import PackageJsonTest._

  behavior of "PackageJsonMutableViewTest"

  val packageFile = StringFileArtifact("package.json", packageJson)

  val as = new SimpleFileBasedArtifactSource("name",
    Seq(
      packageFile
    )
  )

  it should "extract packageName" in {
    val v = new PackageJsonMutableView(packageFile,
      new ProjectMutableView(EmptyArtifactSource(""), as))
    val ic = SimpleFunctionInvocationContext("p", null, v, as, null, Map(),
      SimpleParameterValues.Empty, Nil)
    v.packageName(ic) should be ("module-name")
  }
}
