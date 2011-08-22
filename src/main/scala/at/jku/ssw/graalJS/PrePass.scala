package at.jku.ssw.graalJS

import org.mozilla.javascript.ast._
import scala.collection.JavaConverters._

final class PrePass extends NodeVisitor {
  var maxLocals = 0
  var maxOperandStackSize = 0
  private val linearASTBuffer = new collection.immutable.VectorBuilder[AstNode]
  lazy val linerAST = linearASTBuffer.result()

  private val variables = new scala.collection.mutable.HashMap[String, Int]

  private def calcMaxOperandStackSize(expression: AstNode) {
    maxOperandStackSize = math.max(maxOperandStackSize, calcOperandStackSize(expression))

    def calcOperandStackSize(expression: AstNode): Int =
      expression match {
        case name: Name =>
          name.setLineno(variables.getOrElse(name.getIdentifier, sys.error("variable " + name.getIdentifier + " not found line:" + name.getLineno)))
          1
        case _: NumberLiteral | _: StringLiteral => 1
        case functionCall: FunctionCall => functionCall.getArguments.asScala.map(calcOperandStackSize).max
        case assignment: Assignment => math.max(calcOperandStackSize(assignment.getLeft), calcOperandStackSize(assignment.getRight))
        case infixExpression: InfixExpression =>
          val left = calcOperandStackSize(infixExpression.getLeft)
          val right = calcOperandStackSize(infixExpression.getRight)
          infixExpression.getLeft match {
            case _: Name | _: NumberLiteral | _: StringLiteral => left + right
            case _ => math.max(left, right)
          }
        case _ => sys.error("unknown node " + expression.getClass)
      }
  }

  def visit(node: AstNode) =
    node match {
      case _: AstRoot => true
      case variableDeclaration: VariableDeclaration => true
      case variableInitializer: VariableInitializer =>
        val target = variableInitializer.getTarget.asInstanceOf[Name].getIdentifier
        variables(target) = maxLocals
        if (variableInitializer.getInitializer != null) calcMaxOperandStackSize(variableInitializer.getInitializer)
        calcMaxOperandStackSize(variableInitializer.getTarget)
        maxLocals += 1
        linearASTBuffer += node
        false
      case _: WhileLoop | _: DoLoop | _: ForLoop | _: IfStatement =>
        linearASTBuffer += node
        true
      case expressionStatement: ExpressionStatement =>
        calcMaxOperandStackSize(expressionStatement.getExpression)
        linearASTBuffer += node
        false
      case infixExpression: InfixExpression =>
        calcMaxOperandStackSize(infixExpression)
        linearASTBuffer += node
        false
      case emptyExpression: EmptyExpression =>
        linearASTBuffer += node
        false
      case scope: Scope => true
      case _ => sys.error("unknown node " + node.getClass)
    }
}