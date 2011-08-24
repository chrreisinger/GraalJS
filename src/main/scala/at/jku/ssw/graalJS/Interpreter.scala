package at.jku.ssw.graalJS

import org.mozilla.javascript.Token
import org.mozilla.javascript.ast._
import annotation.tailrec

final class Interpreter(nodes: collection.mutable.ArrayBuffer[AstNode], localVariables: Array[AnyRef], operandStack: Array[AnyRef]) {
  private var top = -1

  println("localVariables size:" + localVariables.length)
  println("operandStack size:" + operandStack.length)

  private def peek = operandStack(top)

  private def clearStack() {
    for (i <- 0 to top) operandStack(i) = null
    top = -1
  }

  private def push(value: AnyRef): AnyRef = {
    top += 1
    operandStack(top) = value
    require(top <= operandStack.length)
    value
  }

  private def pop(): AnyRef = {
    val element = operandStack(top)
    operandStack(top) = null
    top -= 1
    require(top >= -1)
    element
  }

  private def toNumber(value: Double): java.lang.Number =
    if (value.asInstanceOf[Int].asInstanceOf[Double] == value) new JSInteger(value.asInstanceOf[Int])
    else new JSDouble(value)

  private def interpretExpression(expression: AstNode): AnyRef = {
    val value = expression match {
      case functionCall: FunctionCall =>
        require(functionCall.getTarget.asInstanceOf[Name].getIdentifier == "println")
        require(functionCall.getArguments.size == 1)
        for (i <- 0 to functionCall.getArguments.size - 1) interpretExpression(functionCall.getArguments.get(i))
        println(pop())
        UndefinedValue
      case _: EmptyExpression => peek
      case assignment: Assignment =>
        val name = assignment.getLeft.asInstanceOf[Name]
        require(assignment.getOperator == Token.ASSIGN)
        require(name.getLineno < localVariables.length)
        localVariables(name.getLineno) = pop()
        UndefinedValue
      case infixExpression: InfixExpression =>
        val operator = infixExpression.getOperator
        if (operator == Token.OR || operator == Token.AND) {
          interpretExpression(infixExpression.getLeft)
          val left = peek.asInstanceOf[JSBoolean].booleanValue
          if ((!left && operator == Token.OR) || (left && operator == Token.AND)) {
            pop()
            interpretExpression(infixExpression.getRight)
            val right = pop().asInstanceOf[JSBoolean].booleanValue
            if (operator == Token.OR) push(new JSBoolean(left || right))
            else push(new JSBoolean(left && right))
          }
          peek
        } else {
          val right = pop()
          val left = pop()
          val result = (left, right) match {
            //TODO
            case (UndefinedValue, rightValue) => operator match {
              case Token.ADD | Token.SUB | Token.MUL | Token.DIV => rightValue
              case Token.EQ | Token.NE | Token.LT | Token.LE | Token.GT | Token.GE => new JSBoolean(true)
              case _ => sys.error("unknown operator " + Token.typeToName(operator))
            }
            //TODO
            case (leftValue, UndefinedValue) => operator match {
              case Token.ADD | Token.SUB | Token.MUL | Token.DIV => leftValue
              case Token.EQ | Token.NE | Token.LT | Token.LE | Token.GT | Token.GE => new JSBoolean(true)
              case _ => sys.error("unknown operator " + Token.typeToName(operator))
            }
            case (leftString: String, rightString: String) =>
              require(operator == Token.ADD)
              leftString + rightString
            case (leftInt: JSInteger, rightInt: JSInteger) => operator match {
              case Token.ADD => toNumber(leftInt.doubleValue + rightInt.doubleValue)
              case Token.SUB => toNumber(leftInt.doubleValue - rightInt.doubleValue)
              case Token.MUL => toNumber(leftInt.doubleValue * rightInt.doubleValue)
              case Token.DIV => toNumber(leftInt.doubleValue / rightInt.doubleValue)
              case Token.EQ => new JSBoolean(leftInt.intValue == rightInt.intValue)
              case Token.NE => new JSBoolean(leftInt.intValue != rightInt.intValue)
              case Token.LT => new JSBoolean(leftInt.intValue < rightInt.intValue)
              case Token.LE => new JSBoolean(leftInt.intValue <= rightInt.intValue)
              case Token.GT => new JSBoolean(leftInt.intValue > rightInt.intValue)
              case Token.GE => new JSBoolean(leftInt.intValue >= rightInt.intValue)
              case _ => sys.error("unknown operator " + Token.typeToName(operator))
            }
            case (leftNumber: java.lang.Number, rightNumber: java.lang.Number) => operator match {
              case Token.ADD => toNumber(leftNumber.doubleValue + rightNumber.doubleValue)
              case Token.SUB => toNumber(leftNumber.doubleValue - rightNumber.doubleValue)
              case Token.MUL => toNumber(leftNumber.doubleValue * rightNumber.doubleValue)
              case Token.DIV => toNumber(leftNumber.doubleValue / rightNumber.doubleValue)
              case Token.EQ => new JSBoolean(leftNumber.doubleValue == rightNumber.doubleValue)
              case Token.NE => new JSBoolean(leftNumber.doubleValue != rightNumber.doubleValue)
              case Token.LT => new JSBoolean(leftNumber.doubleValue < rightNumber.doubleValue)
              case Token.LE => new JSBoolean(leftNumber.doubleValue <= rightNumber.doubleValue)
              case Token.GT => new JSBoolean(leftNumber.doubleValue > rightNumber.doubleValue)
              case Token.GE => new JSBoolean(leftNumber.doubleValue >= rightNumber.doubleValue)
              case _ => sys.error("unknown operator " + Token.typeToName(operator))
            }
          }
          push(result)
        }
      case _ => sys.error("unknown node " + expression.getClass)
    }
    expression.setLength(calcNewType(typeToExpressionType(expression.getLength), value))
    value
  }

  @tailrec
  def interpret(index: Int = 0) {
    var next = index + 1
    nodes(index) match {
      case null => push(UndefinedValue)
      case name: Name => push(localVariables(name.getLineno))
      case numberLiteral: NumberLiteral => push(toNumber(numberLiteral.getNumber))
      case stringLiteral: StringLiteral => push(stringLiteral.getValue)
      case variableInitializer: VariableInitializer =>
        val name = variableInitializer.getTarget.asInstanceOf[Name]
        require(name.getLineno < localVariables.length)
        localVariables(name.getLineno) = pop()
      /*
    case ifStatement: IfStatement =>
      if (interpretExpression(ifStatement.getCondition).asInstanceOf[JSBoolean].booleanValue) {
        pop()
        ifStatement.getThenPart.visit(this)
      }
      else if (ifStatement.getElsePart != null) {
        pop()
        ifStatement.getElsePart.visit(this)
      }
      else pop()
      false
      case whileLoop: WhileLoop =>
        while (interpretExpression(whileLoop.getCondition).asInstanceOf[JSBoolean].booleanValue) {
          pop()
          whileLoop.getBody.visit(this)
        }
        pop()
        false
    case doLoop: DoLoop =>
      doLoop.getBody.visit(this)
      while (interpretExpression(doLoop.getCondition).asInstanceOf[JSBoolean].booleanValue) {
        pop()
        doLoop.getBody.visit(this)
      }
      pop()
      false
    case forLoop: ForLoop =>
      forLoop.getInitializer.visit(this)
      while (interpretExpression(forLoop.getCondition).asInstanceOf[JSBoolean].booleanValue) {
        pop()
        forLoop.getBody.visit(this)
        interpretExpression(forLoop.getIncrement)
      }
      pop()
      false
      */
      case GotoNode(offset) => next += offset
      case ConditionalJumpNode(offset, trueJump) =>
        val result = pop().asInstanceOf[JSBoolean].booleanValue
        if ((trueJump && result) || (!trueJump && !result)) next += offset
      case infixExpression: InfixExpression => interpretExpression(infixExpression)
      case emptyExpression: EmptyExpression => clearStack()
      case expressionStatement: ExpressionStatement => clearStack()
      case scope: Scope => true
      case node => sys.error("unknown node " + node.getClass)
    }
    if (next < nodes.size) interpret(next)
  }

}
