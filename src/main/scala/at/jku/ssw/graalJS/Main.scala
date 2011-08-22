/*
 *     OpenVC, an open source VHDL compiler/simulator
 *     Copyright (C) 2010  Christian Reisinger
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package at.jku.ssw.graalJS

import java.io.{Reader, FileReader}
import org.mozilla.javascript._
import ast._
import tools.ToolErrorReporter
import scala.collection.JavaConverters._

/*
type in expression speichern undefined, allways A | B, mixed (done)
variablen mit index zugreifen (done)
expression auf int spezialisieren, z.B vergleiche (done)

variables array mit index (done)
variablen auflösen in einem pass (done)
maxLocals (done)
maxExpressionStackSize (done)
ast lineraize (done)
*/
object Main {


  class PrePass extends NodeVisitor {
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

  class Interpreter(localVariables: Array[AnyRef], operandStack: Array[AnyRef]) extends NodeVisitor {
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
              case (leftDouble: JSDouble, rightDouble: JSDouble) => operator match {
                case Token.ADD => new JSDouble(leftDouble.doubleValue + rightDouble.doubleValue)
                case Token.SUB => new JSDouble(leftDouble.doubleValue - rightDouble.doubleValue)
                case Token.MUL => new JSDouble(leftDouble.doubleValue * rightDouble.doubleValue)
                case Token.DIV => new JSDouble(leftDouble.doubleValue / rightDouble.doubleValue)
                case Token.EQ => new JSBoolean(leftDouble.doubleValue == rightDouble.doubleValue)
                case Token.NE => new JSBoolean(leftDouble.doubleValue != rightDouble.doubleValue)
                case Token.LT => new JSBoolean(leftDouble.doubleValue < rightDouble.doubleValue)
                case Token.LE => new JSBoolean(leftDouble.doubleValue <= rightDouble.doubleValue)
                case Token.GT => new JSBoolean(leftDouble.doubleValue > rightDouble.doubleValue)
                case Token.GE => new JSBoolean(leftDouble.doubleValue >= rightDouble.doubleValue)
                case _ => sys.error("unknown operator " + Token.typeToName(operator))
              }
              case (leftInt: JSInteger, rightInt: JSInteger) => operator match {
                case Token.ADD => new JSInteger(leftInt.intValue + rightInt.intValue)
                case Token.SUB => new JSInteger(leftInt.intValue - rightInt.intValue)
                case Token.MUL => new JSInteger(leftInt.intValue * rightInt.intValue)
                case Token.DIV => new JSInteger(leftInt.intValue / rightInt.intValue)
                case Token.EQ => new JSBoolean(leftInt.intValue == rightInt.intValue)
                case Token.NE => new JSBoolean(leftInt.intValue != rightInt.intValue)
                case Token.LT => new JSBoolean(leftInt.intValue < rightInt.intValue)
                case Token.LE => new JSBoolean(leftInt.intValue <= rightInt.intValue)
                case Token.GT => new JSBoolean(leftInt.intValue > rightInt.intValue)
                case Token.GE => new JSBoolean(leftInt.intValue >= rightInt.intValue)
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
    ast.visit(new Interpreter(new Array[AnyRef](prePass.maxLocals), new Array[AnyRef](prePass.maxOperandStackSize)))
  }
}

/*
         ____________                           ___
        /  _________/\                         /  /\
       /  /\________\/_________   _________   /  / /_________
      /  /_/______   /  ______/\ /_____   /\ /  / //_____   /\
     /________   /\ /  /\_____\/_\____/  / //  / /_\____/  / /
     \______ /  / //  / /      /  ___   / //  / //  ___   / /
   _________/  / //  /_/___   /  /__/  / //  / //  /__/  / /
  /___________/ //________/\ /________/ //__/ //________/ /
  \___________\/ \________\/ \________\/ \__\/ \________\/

            ______________________________________________________________
         | / ____________    ____________________   ___    __________  /\
        _|/ /  _________/\  /_/_/_/_/_/_/_/_/_/_/  /  /\  /_/_/_/_/_/ / /
       | / /  /\________\/_________   _________   /  / /_________    / /
      _|/ /  /_/______   /  ______/\ /_____   /\ /  / //_____   /\  / /
     | / /________   /\ /  /\_____\/_\____/  / //  / /_\____/  / / / /
    _|/  \______ /  / //  / /      /  ___   / //  / //  ___   / / / /
   | / _________/  / //  /_/___   /  /__/  / //  / //  /__/  / / / /
  _|/ /___________/ //________/\ /________/ //__/ //________/ / / /
 | /  \___________\/ \________\/ \________\/ \__\/ \________\/ / /
 |/___________________________________________________________/ /
  \___________________________________________________________\/

                                                    ___
                                                  /  /\
MyClass      _________   _________   _________   /  / /_________
            /  ______/\ /  ______/\ /_____   /\ /  / //_____   /\
           /  /_____ \//  /\_____\/_\____/  / //  / /_\____/  / /
          /_____   /\ /  / /      /  ___   / //  / //  ___   / /
   ___   ______/  / //  /_/___   /  /__/  / //  / //  /__/  / /
  /__/\ /________/ //________/\ /________/ //__/ //________/ /
  \__\/ \________\/ \________\/ \________\/ \__\/ \________\/


         ________  __________________________________________________
      / ____  /\  __________________________________________________
     /_______/  \  __________________________________________________
     \_______\ \ \  __________________________________________________
     / ____  / /\ \  ____________   _____________________   ___   _____
    /_______/ /\/ / /  _________/\   ___________________   /  /\   _____
    \_______\ \/ / /  /\________\/_________   _________   /  / /_________
    / ____  / / / /  /_/______   /  ______/\ /_____   /\ /  / //_____   /\
   /_______/ / / /________   /\ /  /\_____\/_\____/  / //  / /_\____/  / /
   \_______\  /  \______ /  / //  / /      /  ___   / //  / //  ___   / /
   / ____  / / _________/  / //  /_/___   /  /__/  / //  / //  /__/  / /
  /_______/ / /___________/ //________/\ /________/ //__/ //________/ /
  \_______\/  \___________\/ \________\/ \________\/ \__\/ \________\/
    ________________________________________________________________
     ______________________________________________________________
      ____________________________________________________________
      */