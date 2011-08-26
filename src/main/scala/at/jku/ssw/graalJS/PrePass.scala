package at.jku.ssw.graalJS

import org.mozilla.javascript.ast._
import scala.collection.JavaConverters._

final case class GotoNode(var offset: Int) extends AstNode {
  override def toString = "" + offset

  def toSource(depth: Int) = sys.error("impossible")

  def visit(visitor: NodeVisitor) {
    sys.error("impossible")
  }
}

final case class ConditionalJumpNode(var offset: Int, trueJump: Boolean = false) extends AstNode {
  override def toString = "" + offset + " " + trueJump

  def toSource(depth: Int) = sys.error("impossible")

  def visit(visitor: NodeVisitor) {
    sys.error("impossible")
  }
}

final class PrePass extends NodeVisitor {
  var maxLocals = 0
  var maxOperandStackSize = 0

  private val linearASTBuffer = new collection.mutable.ArrayBuffer[AstNode]
  lazy val linerAST = linearASTBuffer.result()

  private val variables = new scala.collection.mutable.HashMap[String, Int]

  private def calcMaxOperandStackSize(expression: AstNode) {
    maxOperandStackSize = math.max(maxOperandStackSize, calcOperandStackSize(expression))

    def calcOperandStackSize(expression: AstNode): Int =
      expression match {
        case _: NumberLiteral | _: StringLiteral | _: Name => 1
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

  private def setVarIndex(name: Name) {
    name.varIndex = variables.getOrElse(name.getIdentifier, sys.error("variable " + name.getIdentifier + " not found line:" + name.getLineno))
  }

  def visit(node: AstNode) =
    node match {
      case _: AstRoot | _: VariableDeclaration => true
      case variableInitializer: VariableInitializer =>
        val name = variableInitializer.getTarget.asInstanceOf[Name]
        variables(name.getIdentifier) = maxLocals
        setVarIndex(name)
        if (variableInitializer.getInitializer != null) {
          calcMaxOperandStackSize(variableInitializer.getInitializer)
          variableInitializer.getInitializer.visit(this)
        } else {
          linearASTBuffer += null
        }
        calcMaxOperandStackSize(variableInitializer.getTarget)
        maxLocals += 1
        linearASTBuffer += node
        false
      case ifStatement: IfStatement =>
        ifStatement.getCondition.visit(this)
        val jump = new ConditionalJumpNode(-1, false)
        linearASTBuffer += jump
        val size = linearASTBuffer.size
        ifStatement.getThenPart.visit(this)
        jump.offset = linearASTBuffer.size - size + 1
        if (ifStatement.getElsePart != null) {
          val gotoNode = new GotoNode(-1)
          val pos = linearASTBuffer.size
          linearASTBuffer += gotoNode
          ifStatement.getElsePart.visit(this)
          gotoNode.offset = linearASTBuffer.size - pos - 1
        }
        false
      case whileLoop: WhileLoop =>
        val start = linearASTBuffer.size
        whileLoop.getCondition.visit(this)
        val jump = new ConditionalJumpNode(-1, false)
        linearASTBuffer += jump
        val size = linearASTBuffer.size
        whileLoop.getBody.visit(this)
        jump.offset = linearASTBuffer.size - size + 1
        linearASTBuffer += new GotoNode(start - linearASTBuffer.size - 1)
        false
      case returnStatement: ReturnStatement =>
        if (returnStatement.getReturnValue == null) linearASTBuffer += null
        else returnStatement.getReturnValue.visit(this)
        linearASTBuffer += returnStatement
        false
      case expressionStatement: ExpressionStatement =>
        calcMaxOperandStackSize(expressionStatement.getExpression)
        expressionStatement.getExpression.visit(this)
        linearASTBuffer += node
        false
      case assignment: Assignment =>
        setVarIndex(assignment.getLeft.asInstanceOf[Name])
        assignment.getRight.visit(this)
        linearASTBuffer += node
        false
      case functionCall: FunctionCall =>
        calcMaxOperandStackSize(functionCall)
        functionCall.getArguments.asScala.foreach(_.visit(this))
        linearASTBuffer += node
        false
      case infixExpression: InfixExpression =>
        calcMaxOperandStackSize(infixExpression)
        infixExpression.getLeft.visit(this)
        infixExpression.getRight.visit(this)
        linearASTBuffer += node
        false
      case name: Name =>
        setVarIndex(name)
        linearASTBuffer += node
        false
      case _: NumberLiteral | _: StringLiteral | _: EmptyExpression =>
        linearASTBuffer += node
        false
      case scope: Scope => true
      case _ => sys.error("unknown node " + node.getClass)
    }
}