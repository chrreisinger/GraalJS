package at.jku.ssw.graalJS

import DataType._
import java.lang.reflect.Method
import org.mozilla.javascript.Token
import org.mozilla.javascript.ast._
import com.sun.cri.ci.{CiConstant, CiKind}
import com.sun.cri.bytecode.Bytecodes._
import com.oracle.max.graal.compiler.value.FrameStateBuilder
import com.oracle.max.graal.nodes._
import com.oracle.max.graal.nodes.calc._
import com.oracle.max.graal.compiler.debug.IdealGraphPrinterObserver
import com.oracle.max.graal.runtime.{HotSpotMethodResolved, HotSpotTargetMethod, CompilerImpl}
import com.oracle.max.graal.compiler.GraalOptions

final class GraphBuilder(method: Method, maxLocals: Int, maxStackSize: Int) extends NodeVisitor {

  // Obtain RiMethod and RiRuntime instances.
  private val compilerInstance = CompilerImpl.getInstance()
  private val vmEntries = compilerInstance.getVMEntries
  private val riMethod = vmEntries.getRiMethod(method)
  private val riRuntime = compilerInstance.getRuntime

  // Create the compiler graph for the method.
  private val graph = new CompilerGraph(riRuntime)
  private val frameState = new FrameStateBuilder(riMethod, maxLocals, maxStackSize, graph)

  def run() {
    // Compile and print disassembly.
    val result = compilerInstance.getCompiler.compileMethod(riMethod, graph)
    //println(riRuntime.disassemble(result.targetMethod()))

    // Install method!
    HotSpotTargetMethod.installMethod(compilerInstance, riMethod.asInstanceOf[HotSpotMethodResolved], result.targetMethod())
    println(method.invoke(null))
  }

  def visualize() {
    GraalOptions.Plot = true
    val printer = new IdealGraphPrinterObserver("localhost", 4444)
    printer.printSingleGraph(method.getName, graph)
  }


  private implicit def toRichNode(node: AstNode) = new {
    def dataType = toDataType(node.getLength)

    def kind = toDataType(node.getLength) match {
      case AllwaysInt => CiKind.Int
      case AllwaysBoolean => CiKind.Boolean
      case AllwaysDouble => CiKind.Double
      case AllwaysString => CiKind.Object
    }

    def varIndex = node.getLineno
  }

  private def appendConstant(constant: CiConstant): ConstantNode = graph.unique(new ConstantNode(constant))

  private def append(v: ValueNode) = v

  def visit(node: AstNode): Boolean =
    node match {
      case _: AstRoot => true
      case name: Name =>
        frameState.push(name.kind, frameState.loadLocal(name.varIndex))
        false
      case numberLiteral: NumberLiteral =>
        val constant = numberLiteral.dataType match {
          case AllwaysInt => CiConstant.forInt(numberLiteral.getNumber.toInt)
          case AllwaysDouble => CiConstant.forDouble(numberLiteral.getNumber)
        }
        frameState.push(constant.kind.stackKind(), appendConstant(constant))
        false
      case stringLiteral: StringLiteral =>
        val constant = CiConstant.forObject(stringLiteral.getValue)
        frameState.push(constant.kind.stackKind(), appendConstant(constant))
        false
      case assignment: Assignment =>
        val name = assignment.getLeft.asInstanceOf[Name]
        assignment.getRight.visit(this)
        frameState.storeLocal(name.varIndex, frameState.pop(name.kind))
        false
      case functionCall: FunctionCall => sys.error("not implemented")
      case _: EmptyExpression => false //nothing
      case infixExpression: InfixExpression =>
        def genConvert(opcode: Int, from: CiKind, to: CiKind) {
          val tt = to.stackKind()
          frameState.push(tt, append(graph.unique(new ConvertNode(opcode, frameState.pop(from.stackKind()), tt))))
        }
        def genArithmeticOp(result: CiKind, opcode: Int) {
          val y = frameState.pop(result)
          val x = frameState.pop(result)
          val isStrictFP = false
          val v = opcode match {
            case IADD | LADD => new IntegerAddNode(result, x, y)
            case FADD | DADD => new FloatAddNode(result, x, y, isStrictFP)
            case ISUB | LSUB => new IntegerSubNode(result, x, y)
            case FSUB | DSUB => new FloatSubNode(result, x, y, isStrictFP)
            case IMUL | LMUL => new IntegerMulNode(result, x, y)
            case FMUL | DMUL => new FloatMulNode(result, x, y, isStrictFP)
            case IDIV | LDIV => new IntegerDivNode(result, x, y)
            case FDIV | DDIV => new FloatDivNode(result, x, y, isStrictFP)
            case IREM | LREM => new IntegerRemNode(result, x, y)
            case FREM | DREM => new FloatRemNode(result, x, y, isStrictFP)
            case _ => sys.error("unknown opcode " + opcode)
          }
          val result1 = append(graph.unique(v))
          frameState.push(result, result1)
        }
        def genIfSame(kind: CiKind, condition: Condition) {
          val y = frameState.pop(kind)
          val x = frameState.pop(kind)
          require(!x.isDeleted && !y.isDeleted)
          frameState.ipush(append(MaterializeNode.create(graph.unique(new CompareNode(x, condition, y)), graph)))
        }
        (infixExpression.getLeft.dataType, infixExpression.getRight.dataType) match {
          case (AllwaysInt, AllwaysDouble) =>
            infixExpression.getLeft.visit(this)
            genConvert(I2D, CiKind.Int, CiKind.Double)
            infixExpression.getRight.visit(this)
          case (AllwaysDouble, AllwaysInt) =>
            infixExpression.getLeft.visit(this)
            infixExpression.getRight.visit(this)
            genConvert(I2D, CiKind.Int, CiKind.Double)
          case (AllwaysInt, AllwaysInt) | (AllwaysDouble, AllwaysDouble) =>
            infixExpression.getLeft.visit(this)
            infixExpression.getRight.visit(this)
        }
        val kind = infixExpression.kind
        infixExpression.getOperator match {
          case Token.OR | Token.AND => sys.error("not implemented")
          case Token.EQ => genIfSame(kind, Condition.EQ)
          case Token.NE => genIfSame(kind, Condition.NE)
          case Token.LT => genIfSame(kind, Condition.LT)
          case Token.LE => genIfSame(kind, Condition.LE)
          case Token.GT => genIfSame(kind, Condition.GT)
          case Token.GE => genIfSame(kind, Condition.GE)
          case Token.ADD =>
            if (kind == CiKind.Int) genArithmeticOp(kind, IADD)
            else genArithmeticOp(kind, DADD)
          case Token.SUB =>
            if (kind == CiKind.Int) genArithmeticOp(kind, ISUB)
            else genArithmeticOp(kind, DSUB)
          case Token.MUL =>
            if (kind == CiKind.Int) genArithmeticOp(kind, IMUL)
            else genArithmeticOp(kind, DMUL)
          case Token.DIV =>
            if (kind == CiKind.Int) genArithmeticOp(kind, IDIV)
            else genArithmeticOp(kind, DDIV)
          case _ => sys.error("unknown operator " + Token.typeToName(infixExpression.getOperator))
        }
        false
      case variableDeclaration: VariableDeclaration => true
      case variableInitializer: VariableInitializer =>
        val name = variableInitializer.getTarget.asInstanceOf[Name]
        variableInitializer.getInitializer.visit(this)
        frameState.storeLocal(name.varIndex, frameState.pop(variableInitializer.getInitializer.kind))
        false
      case whileLoop: WhileLoop => sys.error("not implemented")
      case expressionStatement: ExpressionStatement =>
        expressionStatement.getExpression.visit(this)
        if (expressionStatement.getNext == null) {
          //last expression is return value
          val returnNode = graph.add(new ReturnNode(frameState.pop(expressionStatement.getExpression.kind)))
          graph.setReturn(returnNode)
          graph.start().setNext(returnNode)
        } else {
          frameState.pop(expressionStatement.getExpression.kind)
        }
        false
      case returnStatement: ReturnStatement =>
        val returnNode =
          if (returnStatement.getReturnValue == null) {
            returnStatement.getReturnValue.visit(this)
            graph.add(new ReturnNode(frameState.pop(returnStatement.getReturnValue.kind)))
          } else graph.add(new ReturnNode(null))
        graph.setReturn(returnNode)
        graph.start().setNext(returnNode)
        false
    }
}