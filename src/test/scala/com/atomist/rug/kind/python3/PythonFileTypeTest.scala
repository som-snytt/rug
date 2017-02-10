package com.atomist.rug.kind.python3

import com.atomist.project.archive.DefaultAtomistConfig
import com.atomist.rug.kind.DefaultTypeRegistry
import com.atomist.rug.kind.core.ProjectMutableView
import com.atomist.source.{EmptyArtifactSource, SimpleFileBasedArtifactSource, StringFileArtifact}
import com.atomist.tree.TreeNode
import com.atomist.tree.content.text.MutableContainerTreeNode
import com.atomist.tree.pathexpression.{PathExpressionEngine, PathExpressionParser}
import org.scalatest.{FlatSpec, Matchers}
import com.atomist.tree.utils.TreeNodeUtils

class PythonFileTypeTest extends FlatSpec with Matchers {

  import Python3ParserTest._

  private  val pex = new PathExpressionEngine


  it should "find Python file type using path expression" in {
    val proj = SimpleFileBasedArtifactSource(StringFileArtifact("src/setup.py", setupDotPy))
    val pmv = new ProjectMutableView(EmptyArtifactSource(""), proj, DefaultAtomistConfig)
    val expr = "/src/File()/PythonFile()"
    val rtn = pex.evaluate(pmv, PathExpressionParser.parseString(expr), DefaultTypeRegistry)
    assert(rtn.right.get.size === 1)
    //    rtn.right.get.foreach {
    //      case p: PythonFileMutableView =>
    //    }
  }

  it should "drill down to Python import statement using path expression" in {
    val proj = SimpleFileBasedArtifactSource(StringFileArtifact("src/setup.py", setupDotPy))
    val pmv = new ProjectMutableView(EmptyArtifactSource(""), proj, DefaultAtomistConfig)
    val expr = "/src/File()/PythonFile()//import_stmt()"
    val rtn = pex.evaluate(pmv, PathExpressionParser.parseString(expr), DefaultTypeRegistry)
    rtn.right.get.size should be>(2)
    rtn.right.get.foreach {
      case n: TreeNode if n.value.nonEmpty =>
      
      case x => //println(s"Was empty: $x")
     //println(s"Was empty: $x")
    }
  }

  it should "drill down to Python import from statement using path expression" in {
    val proj = SimpleFileBasedArtifactSource(StringFileArtifact("src/setup.py", setupDotPy))
    val pmv = new ProjectMutableView(EmptyArtifactSource(""), proj, DefaultAtomistConfig)
    val expr = "/src/File()/PythonFile()//import_from()"
    val rtn = pex.evaluate(pmv, PathExpressionParser.parseString(expr), DefaultTypeRegistry)
    rtn.right.get.size should be>(2)
    rtn.right.get.foreach {
      case n: TreeNode if n.value.nonEmpty =>
      
      case x => //println(s"Was empty: $x")
     //println(s"Was empty: $x")
    }
  }

  it should "map classdef to Class" in {
    val proj = SimpleFileBasedArtifactSource(StringFileArtifact("src/mymod.py", pythonClasses))
    val pmv = new ProjectMutableView(EmptyArtifactSource(""), proj, DefaultAtomistConfig)
    val expr1 = "/src/File()/PythonFile()//Class()"
    val rtn1 = pex.evaluate(pmv, PathExpressionParser.parseString(expr1), DefaultTypeRegistry)
    assert(rtn1.right.get.size === 3)

    val expr2 = "/src/File()/PythonFile()//classdef()"
    val rtn2 = pex.evaluate(pmv, PathExpressionParser.parseString(expr2), DefaultTypeRegistry)
    assert(rtn2.right.get.size === 3)

    assert(rtn1.right.get.size === rtn2.right.get.size)

  }

  it should "map funcdef to Func" in {
    val proj = SimpleFileBasedArtifactSource(StringFileArtifact("src/mymod.py", pythonFunctions))
    val pmv = new ProjectMutableView(EmptyArtifactSource(""), proj, DefaultAtomistConfig)
    val expr1 = "/src/File()/PythonFile()//Func()"
    val rtn1 = pex.evaluate(pmv, PathExpressionParser.parseString(expr1), DefaultTypeRegistry)
    assert(rtn1.right.get.size === 4)

    val expr2 = "/src/File()/PythonFile()//funcdef()"
    val rtn2 = pex.evaluate(pmv, PathExpressionParser.parseString(expr2), DefaultTypeRegistry)
    assert(rtn2.right.get.size === 4)

    assert(rtn1.right.get.size === rtn2.right.get.size)
    rtn1.right.get.foreach {
      case n: TreeNode if n.value.nonEmpty => println(TreeNodeUtils.toShorterString(n))
      case _ => throw new AssertionError("Treenode missing")
    }
  }

  it should "map tfpdef to Args" in {
    val proj = SimpleFileBasedArtifactSource(StringFileArtifact("src/mymod.py", pythonFunctions))
    val pmv = new ProjectMutableView(EmptyArtifactSource(""), proj, DefaultAtomistConfig)
    val expr1 = "/src/File()/PythonFile()//Func()[/NAME[@value='say_colour']]//Args()"
    val rtn1 = pex.evaluate(pmv, PathExpressionParser.parseString(expr1), DefaultTypeRegistry)
    assert(rtn1.right.get.size === 2)

    val expr2 = "/src/File()/PythonFile()//funcdef()[/NAME[@value='say_colour']]//Args()"
    val rtn2 = pex.evaluate(pmv, PathExpressionParser.parseString(expr2), DefaultTypeRegistry)
    assert(rtn2.right.get.size === 2)

    val expr3 = "/src/File()/PythonFile()//funcdef()[/NAME[@value='say_colour']]//tfpdef()"
    val rtn3 = pex.evaluate(pmv, PathExpressionParser.parseString(expr3), DefaultTypeRegistry)
    assert(rtn3.right.get.size === 2)
  }

  it should "map lambdadef to Lambda" in {
    val proj = SimpleFileBasedArtifactSource(StringFileArtifact("src/mymod.py", pythonFunctions))
    val pmv = new ProjectMutableView(EmptyArtifactSource(""), proj, DefaultAtomistConfig)
    val expr1 = "/src/File()/PythonFile()//Lambda()"
    val rtn1 = pex.evaluate(pmv, PathExpressionParser.parseString(expr1), DefaultTypeRegistry)
    assert(rtn1.right.get.size === 1)

    val expr2 = "/src/File()/PythonFile()//lambdef()"
    val rtn2 = pex.evaluate(pmv, PathExpressionParser.parseString(expr2), DefaultTypeRegistry)
    assert(rtn2.right.get.size === 1)

    assert(rtn1.right.get.size === rtn2.right.get.size)
    rtn1.right.get.foreach {
      case n: TreeNode if n.value.nonEmpty => println(TreeNodeUtils.toShorterString(n))
      case _ => throw new AssertionError("Treenode missing")
    }
  }

  it should "map classdef and funcdef to Class and Func" in {
    val proj = SimpleFileBasedArtifactSource(StringFileArtifact("src/mymod.py", pythonFunctions))
    val pmv = new ProjectMutableView(EmptyArtifactSource(""), proj, DefaultAtomistConfig)
    val expr1 = "/src/File()/PythonFile()//Class()//Func()"
    val rtn1 = pex.evaluate(pmv, PathExpressionParser.parseString(expr1), DefaultTypeRegistry)
    assert(rtn1.right.get.size === 2)

    val expr2 = "/src/File()/PythonFile()//Class()//funcdef()"
    val rtn2 = pex.evaluate(pmv, PathExpressionParser.parseString(expr2), DefaultTypeRegistry)
    assert(rtn2.right.get.size === 2)

    assert(rtn1.right.get.size === rtn2.right.get.size)
  }

  it should "map import_stmt to Import" in {
    val proj = SimpleFileBasedArtifactSource(StringFileArtifact("src/mymod.py", importStmts))
    val pmv = new ProjectMutableView(EmptyArtifactSource(""), proj, DefaultAtomistConfig)
    val expr1 = "/src/File()/PythonFile()//Import()"
    val rtn1 = pex.evaluate(pmv, PathExpressionParser.parseString(expr1), DefaultTypeRegistry)
    assert(rtn1.right.get.size === 4)

    val expr2 = "/src/File()/PythonFile()//import_stmt()"
    val rtn2 = pex.evaluate(pmv, PathExpressionParser.parseString(expr2), DefaultTypeRegistry)
    assert(rtn2.right.get.size === 4)

    assert(rtn1.right.get.size === rtn2.right.get.size)
  }
}
