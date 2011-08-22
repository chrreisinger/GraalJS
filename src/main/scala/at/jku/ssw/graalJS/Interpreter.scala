package at.jku.ssw.graalJS

import org.mozilla.javascript.Token
import org.mozilla.javascript.ast._

final class Interpreter(localVariables: Array[AnyRef], operandStack: Array[AnyRef]) extends NodeVisitor {
  type JSDouble = java.lang.Double
  type JSBoolean = java.lang.Boolean
  type JSInteger = java.lang.Integer

  private var top = -1

  println("localVariables size:" + localVariables.length)
  println("operandStack size:" + operandStack.length)

  private object ExpressionType extends Enumeration {
    val Undefined = Value(0)
    val AllwaysInt = Value(-1)
    val AllwaysBoolean = Value(-2)
    val AllwaysDouble = Value(-3)
    val AllwaysString = Value(-4)
    val Mixed = Value(-5)
  }

  import ExpressionType._

  private case object UndefinedValue

  private def typeToExpressionType(_type: Int): ExpressionType.Value =
    if (_type >= 0) Undefined
    else _type match {
      case -1 => AllwaysInt
      case -2 => AllwaysBoolean
      case -3 => AllwaysDouble
      case -4 => AllwaysString
      case -5 => Mixed
    }

  private def calcNewType(oldType: ExpressionType.Value, newValue: AnyRef): Int =
    ((oldType, newValue) match {
      case (Undefined, _) => newValue match {
        case _: JSInteger => AllwaysInt
        case _: JSBoolean => AllwaysBoolean
        case _: JSDouble => AllwaysDouble
        case _: String => AllwaysString
        case UndefinedValue => Undefined
      }
      case (AllwaysInt, _: JSDouble) => AllwaysDouble
      case (AllwaysDouble, _: JSInteger) => AllwaysDouble
      case (AllwaysInt, _: JSInteger) => AllwaysInt
      case (AllwaysBoolean, _: JSBoolean) => AllwaysBoolean
      case (AllwaysDouble, _: JSDouble) => AllwaysDouble
      case (AllwaysString, _: String) => AllwaysString
      case _ => Mixed
    }).id

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

  private def interpretExpression(expression: AstNode): AnyRef = {
    def toNumber(value: Double): java.lang.Number =
      if (value.asInstanceOf[Int].asInstanceOf[Double] == value) new JSInteger(value.asInstanceOf[Int])
      else new JSDouble(value)

    val value = expression match {
      case name: Name => push(localVariables(name.getLineno))
      case numberLiteral: NumberLiteral => push(toNumber(numberLiteral.getNumber))
      case stringLiteral: StringLiteral => push(stringLiteral.getValue)
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
        interpretExpression(assignment.getRight)
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
          interpretExpression(infixExpression.getLeft)
          interpretExpression(infixExpression.getRight)
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

  def visit(node: AstNode) =
    node match {
      case _: AstRoot => true
      case variableDeclaration: VariableDeclaration => true
      case variableInitializer: VariableInitializer =>
        val name = variableInitializer.getTarget.asInstanceOf[Name]
        require(name.getLineno < localVariables.length)

        if (variableInitializer.getInitializer == null) push(UndefinedValue)
        else interpretExpression(variableInitializer.getInitializer)
        localVariables(name.getLineno) = pop()
        false
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
      case infixExpression: InfixExpression =>
        interpretExpression(infixExpression)
        false
      case emptyExpression: EmptyExpression =>
        clearStack()
        false
      case expressionStatement: ExpressionStatement =>
        interpretExpression(expressionStatement.getExpression)
        clearStack()
        false
      case scope: Scope => true
      case _ => sys.error("unknown node " + node.getClass)
    }
}
