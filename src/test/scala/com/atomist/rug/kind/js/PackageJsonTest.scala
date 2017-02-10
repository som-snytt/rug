package com.atomist.rug.kind.js

import com.atomist.param.SimpleParameterValues
import com.atomist.rug.DefaultRugPipeline
import com.atomist.rug.InterpreterRugPipeline.DefaultRugArchive
import com.atomist.source.{EmptyArtifactSource, SimpleFileBasedArtifactSource, StringFileArtifact}
import org.scalatest.{FlatSpec, Matchers}

object PackageJsonTest {

  val packageJson =
    """
      |{
      |  "name": "module-name",
      |  "version": "10.3.1",
      |  "description": "An example module to illustrate the usage of a package.json",
      |  "author": "Your Name <you.name@example.org>",
      |  "contributors": [{
      |  "name": "Foo Bar",
      |  "email": "foo.bar@example.com"
      |}],
      |  "bin": {
      |  "module-name": "./bin/module-name"
      |},
      |  "scripts": {
      |    "test": "vows --spec --isolate",
      |    "start": "node index.js",
      |    "predeploy": "echo im about to deploy",
      |    "postdeploy": "echo ive deployed",
      |    "prepublish": "coffee --bare --compile --output lib/foo src/foo/*.coffee"
      |  },
      |  "main": "lib/foo.js",
      |  "repository": {
      |  "type": "git",
      |  "url": "https://github.com/nodejitsu/browsenpm.org"
      |},
      |  "bugs": {
      |  "url": "https://github.com/nodejitsu/browsenpm.org/issues"
      |},
      |  "keywords": [
      |  "nodejitsu",
      |  "example",
      |  "browsenpm"
      |],
      |  "dependencies": {
      |    "primus": "*",
      |    "async": "~0.8.0",
      |    "express": "4.2.x",
      |    "winston": "git://github.com/flatiron/winston#master",
      |    "bigpipe": "bigpipe/pagelet",
      |    "plates": "https://github.com/flatiron/plates/tarball/master"
      |  },
      |  "devDependencies": {
      |    "vows": "^0.7.0",
      |    "assume": "<1.0.0 || >=2.3.1 <2.4.5 || >=2.5.2 <3.0.0",
      |    "pre-commit": "*"
      |  },
      |  "preferGlobal": true,
      |  "private": true,
      |  "publishConfig": {
      |  "registry": "https://your-private-hosted-npm.registry.nodejitsu.com"
      |},
      |  "subdomain": "foobar",
      |  "analyze": true,
      |  "license": "MIT"
      |}
    """.stripMargin

}

class PackageJsonTest extends FlatSpec with Matchers {

  import PackageJsonTest._
  import com.atomist.rug.TestUtils._

  it should "update package name with native Rug function" in pendingUntilFixed {
    val prog =
      """
        |@description "Update package JSON"
        |editor Rename
        |
        |param new_name: [\w.\-_]+
        |
        |with packageJSON f
        |do
        |  setPackageName new_name
      """.stripMargin
    updateWith(prog)
  }

  it should "update package name with JavaScript JSON" in {
    val prog =
      """
        |@description "Update package JSON"
        |editor Rename
        |
        |param new_name: ^[\w.\-_]+$
        |
        |with PackageJson f
        |do
        |  setContent {
        |    //print(f.content());
        |    var pkg = JSON.parse(f.content());
        |    pkg.name = "Foo";
        |    return JSON.stringify(pkg, null, 4);
        |  }
      """.stripMargin
    updateWith(prog)
  }

  // Return new content
  private def updateWith(prog: String): String = {
    val filename = "package.json"
    val as = new SimpleFileBasedArtifactSource("name",
      Seq(
        StringFileArtifact(filename, packageJson)
      )
    )
    val newName = "Foo"
    val pas = new SimpleFileBasedArtifactSource(DefaultRugArchive, StringFileArtifact(new DefaultRugPipeline().defaultFilenameFor(prog), prog))

    val r = doModification(pas, as, EmptyArtifactSource(""), SimpleParameterValues(Map(
      "new_name" -> newName
    )))

    val f = r.findFile(filename).get
    f.content.contains(s"$newName") should be(true)
    f.content
  }
}
