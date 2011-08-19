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

object Main {

  val variables = new scala.collection.mutable.HashMap[String, AnyRef]

  class Interpreter extends NodeVisitor {
    type JSDouble = java.lang.Double
    type JSBoolean = java.lang.Boolean

    def interpretExpression(expression: AstNode): AnyRef = expression match {
      case name: Name => variables.getOrElse(name.getIdentifier, sys.error("variable " + name.getIdentifier + " not found line:" + name.getLineno))
      case numberLiteral: NumberLiteral => new JSDouble(numberLiteral.getNumber)
      case stringLiteral: StringLiteral => stringLiteral.getValue
      case functionCall: FunctionCall =>
        require(functionCall.getTarget.asInstanceOf[Name].getIdentifier == "println")
        require(functionCall.getArguments.size == 1)
        println(interpretExpression(functionCall.getArguments.get(0)))
        null
      case infixExpression: InfixExpression =>
        if (infixExpression.getOperator == Token.OR || infixExpression.getOperator == Token.AND) {
          if (infixExpression.getOperator == Token.OR) new JSBoolean(interpretExpression(infixExpression.getLeft).asInstanceOf[JSBoolean].booleanValue || interpretExpression(infixExpression.getRight).asInstanceOf[JSBoolean].booleanValue)
          else new JSBoolean(interpretExpression(infixExpression.getLeft).asInstanceOf[JSBoolean].booleanValue && interpretExpression(infixExpression.getRight).asInstanceOf[JSBoolean].booleanValue)
        }
        else if (infixExpression.getOperator == Token.ASSIGN) {
          val identifier = infixExpression.getLeft.asInstanceOf[Name].getIdentifier
          if (!variables.contains(identifier)) sys.error("variable " + identifier + " not found line:" + infixExpression.getLeft.getLineno)
          variables(identifier) = interpretExpression(infixExpression.getRight)
          null
        }
        else {
          val left = interpretExpression(infixExpression.getLeft)
          val right = interpretExpression(infixExpression.getRight)
          infixExpression.getOperator match {
            case Token.ADD =>
              if (left.isInstanceOf[JSDouble]) new JSDouble(left.asInstanceOf[JSDouble].doubleValue + right.asInstanceOf[JSDouble].doubleValue)
              else left.asInstanceOf[String] + right.asInstanceOf[String]
            case Token.SUB => new JSDouble(left.asInstanceOf[JSDouble].doubleValue - right.asInstanceOf[JSDouble].doubleValue)
            case Token.MUL => new JSDouble(left.asInstanceOf[JSDouble].doubleValue * right.asInstanceOf[JSDouble].doubleValue)
            case Token.DIV => new JSDouble(left.asInstanceOf[JSDouble].doubleValue / right.asInstanceOf[JSDouble].doubleValue)
            case Token.EQ => new JSBoolean(left.asInstanceOf[JSDouble].doubleValue == right.asInstanceOf[JSDouble].doubleValue)
            case Token.NE => new JSBoolean(left.asInstanceOf[JSDouble].doubleValue != right.asInstanceOf[JSDouble].doubleValue)
            case Token.LT => new JSBoolean(left.asInstanceOf[JSDouble].doubleValue < right.asInstanceOf[JSDouble].doubleValue)
            case Token.LE => new JSBoolean(left.asInstanceOf[JSDouble].doubleValue <= right.asInstanceOf[JSDouble].doubleValue)
            case Token.GT => new JSBoolean(left.asInstanceOf[JSDouble].doubleValue > right.asInstanceOf[JSDouble].doubleValue)
            case Token.GE => new JSBoolean(left.asInstanceOf[JSDouble].doubleValue >= right.asInstanceOf[JSDouble].doubleValue)
            case operator => sys.error("unknown operator " + Token.typeToName(operator))
          }
        }
      case _ => sys.error("unknown node " + expression.getClass)
    }

    def visit(node: AstNode) =
      node match {
        case _: AstRoot => true
        case variableDeclaration: VariableDeclaration => true
        case variableInitializer: VariableInitializer =>
          val target = variableInitializer.getTarget.asInstanceOf[Name].getIdentifier
          val value =
            if (variableInitializer.getInitializer == null) new JSDouble(0.0)
            else interpretExpression(variableInitializer.getInitializer)
          variables(target) = value
          false
        case whileLoop: WhileLoop =>
          while (interpretExpression(whileLoop.getCondition).asInstanceOf[JSBoolean]) whileLoop.getBody.visit(this)
          false
        case expressionStatement: ExpressionStatement =>
          interpretExpression(expressionStatement.getExpression)
          false
        case scope: Scope => true
        case _ => sys.error("unknown node " + node.getClass)
      }

  }

  def parse(source: Either[String, Reader], sourceName: String = "unnamed script", lineno: Int = 1, compilationErrorReporter: ErrorReporter = new ToolErrorReporter(true)): AstRoot = {
    val p = new Parser(new CompilerEnvirons, compilationErrorReporter)
    source match {
      case Left(sourceString) => p.parse(sourceString, sourceName, lineno)
      case Right(sourceReader) => p.parse(sourceReader, sourceName, lineno)
    }

  }

  def main(arguments: Array[String]) {
    parse(Right(new FileReader("test.js"))).visit(new Interpreter)
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