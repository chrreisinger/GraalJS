package at.jku.ssw.graalJS

import org.mozilla.javascript.Token
import org.mozilla.javascript.ast._
import annotation.tailrec

final class Interpreter(nodes: collection.mutable.ArrayBuffer[AstNode], localVariables: Array[AnyRef], operandStack: Array[AnyRef]) {

  type JSDouble = java.lang.Double
  type JSBoolean = java.lang.Boolean
  type JSInteger = java.lang.Long

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

  private def interpretExpression(expression: AstNode) {
    def toNumber(value: Double): java.lang.Number =
      if (value.asInstanceOf[Long].asInstanceOf[Double] == value) new JSInteger(value.asInstanceOf[Long])
      else new JSDouble(value)

    val value = expression match {
      case name: Name => push(localVariables(name.varIndex))
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
        require(name.varIndex < localVariables.length)
        localVariables(name.varIndex) = pop()
        UndefinedValue
      case infixExpression: InfixExpression =>
        val operator = infixExpression.getOperator
        if (operator == Token.OR || operator == Token.AND) {
          /*
          TODO
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
          */
          sys.error("not implemented")
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
              case Token.EQ => new JSBoolean(leftInt.longValue == rightInt.longValue)
              case Token.NE => new JSBoolean(leftInt.longValue != rightInt.longValue)
              case Token.LT => new JSBoolean(leftInt.longValue < rightInt.longValue)
              case Token.LE => new JSBoolean(leftInt.longValue <= rightInt.longValue)
              case Token.GT => new JSBoolean(leftInt.longValue > rightInt.longValue)
              case Token.GE => new JSBoolean(leftInt.longValue >= rightInt.longValue)
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
    import DataType._
    expression.dataType = (expression.dataType, value) match {
      case (Undefined, _) => value match {
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
    }
  }

  @tailrec
  def interpret(index: Int = 0): Long = {
    var next = index + 1
    nodes(index) match {
      case null => push(UndefinedValue)
      case variableInitializer: VariableInitializer =>
        val name = variableInitializer.getTarget.asInstanceOf[Name]
        require(name.varIndex < localVariables.length)
        localVariables(name.varIndex) = pop()
      case GotoNode(offset) => next += offset
      case ConditionalJumpNode(offset, trueJump) =>
        val result = pop().asInstanceOf[JSBoolean].booleanValue
        if ((trueJump && result) || (!trueJump && !result)) next += offset
      case node@(_: InfixExpression | _: NumberLiteral | _: Name | _: StringLiteral | _: FunctionCall) => interpretExpression(node)
      case emptyExpression: EmptyExpression => //clearStack()
      case expressionStatement: ExpressionStatement => //clearStack()
      case returnStatement: ReturnStatement => println("return value:" + pop())
      case scope: Scope => true
      case node => sys.error("unknown node " + node.getClass)
    }
    if (next >= nodes.size) pop().asInstanceOf[JSInteger].longValue() //println("interpreter ende")
    else interpret(next)
  }

}
