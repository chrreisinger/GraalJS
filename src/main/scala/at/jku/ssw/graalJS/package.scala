package at.jku.ssw

import org.mozilla.javascript.ast.AstNode

package object graalJS {

  object DataType extends Enumeration {
    val Undefined = Value(0)
    val AllwaysInt = Value(-1)
    val AllwaysBoolean = Value(-2)
    val AllwaysDouble = Value(-3)
    val AllwaysString = Value(-4)
    val Mixed = Value(-5)
  }

  import DataType._

  case object UndefinedValue

  def toDataType(_type: Int): DataType.Value =
    if (_type >= 0) Undefined
    else _type match {
      case -1 => AllwaysInt
      case -2 => AllwaysBoolean
      case -3 => AllwaysDouble
      case -4 => AllwaysString
      case -5 => Mixed
    }

  implicit def toRichNode(node: AstNode) = new {
    def dataType = toDataType(node.getLength)

    def dataType_=(_dataType: DataType.Value) {
      node.setLength(_dataType.id)
    }

    def varIndex_=(varIndex: Int) {
      node.setLineno(varIndex)
    }

    def varIndex = node.getLineno

    def astIndex_=(astIndex: Int) {
      node.setPosition(astIndex)
    }

    def astIndex = node.getPosition
  }
}