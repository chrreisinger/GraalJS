package at.jku.ssw

package object graalJS {
  type JSDouble = java.lang.Double
  type JSBoolean = java.lang.Boolean
  type JSInteger = java.lang.Integer

  object ExpressionType extends Enumeration {
    val Undefined = Value(0)
    val AllwaysInt = Value(-1)
    val AllwaysBoolean = Value(-2)
    val AllwaysDouble = Value(-3)
    val AllwaysString = Value(-4)
    val Mixed = Value(-5)
  }

  import ExpressionType._

  case object UndefinedValue

  def typeToExpressionType(_type: Int): ExpressionType.Value =
    if (_type >= 0) Undefined
    else _type match {
      case -1 => AllwaysInt
      case -2 => AllwaysBoolean
      case -3 => AllwaysDouble
      case -4 => AllwaysString
      case -5 => Mixed
    }

  def calcNewType(oldType: ExpressionType.Value, newValue: AnyRef): Int =
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
}