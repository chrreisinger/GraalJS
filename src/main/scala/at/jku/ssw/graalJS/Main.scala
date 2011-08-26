package at.jku.ssw.graalJS

import java.io.{Reader, FileReader}
import org.mozilla.javascript._
import ast._
import tools.ToolErrorReporter

object Main {
  def mockMethod = 41

  def parse(source: Either[String, Reader], sourceName: String = "unnamed script", lineno: Int = 1, compilationErrorReporter: ErrorReporter = new ToolErrorReporter(true)): AstRoot = {
    val parser = new Parser(new CompilerEnvirons, compilationErrorReporter)
    source match {
      case Left(sourceString) => parser.parse(sourceString, sourceName, lineno)
      case Right(sourceReader) => parser.parse(sourceReader, sourceName, lineno)
    }
  }

  def main(arguments: Array[String]) {
    val ast = parse(Right(new FileReader("test.js")), lineno = 1001)
    val prePass = new PrePass
    ast.visit(prePass)
    val interpreter = new Interpreter(prePass.linerAST, new Array[AnyRef](prePass.maxLocals), new Array[AnyRef](prePass.maxOperandStackSize))
    interpreter.interpret()
    val graphBuilder = new GraphBuilder(Class.forName("at.jku.ssw.graalJS.Main").getMethod("mockMethod"), prePass.maxLocals, prePass.maxOperandStackSize)
    ast.visit(graphBuilder)
    graphBuilder.visualize()
    graphBuilder.run
  }
}