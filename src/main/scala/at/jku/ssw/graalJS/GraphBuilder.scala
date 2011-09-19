package at.jku.ssw.graalJS

import DataType._
import java.lang.reflect.Method
import org.mozilla.javascript.Token
import org.mozilla.javascript.ast._
import com.sun.cri.ci.{CiConstant, CiKind}
import com.sun.cri.bytecode.Bytecodes._
import com.oracle.max.graal.compiler.value.FrameStateBuilder
import com.oracle.max.graal.graph.Node
import com.oracle.max.graal.nodes._
import com.oracle.max.graal.nodes.java.ExceptionObjectNode
import com.oracle.max.graal.nodes.calc._
import com.oracle.max.graal.compiler.debug.IdealGraphPrinterObserver
import com.oracle.max.graal.runtime.{HotSpotMethodResolved, HotSpotTargetMethod, CompilerImpl}
import com.oracle.max.graal.compiler.GraalOptions

final class GraphBuilder(nodes: collection.mutable.ArrayBuffer[AstNode], method: Method, maxLocals: Int, maxStackSize: Int) extends NodeVisitor {

  // Obtain RiMethod and RiRuntime instances.
  private val compilerInstance = CompilerImpl.getInstance()
  private val vmEntries = compilerInstance.getVMEntries
  private val riMethod = vmEntries.getRiMethod(method)
  private val riRuntime = compilerInstance.getRuntime

  // Create the compiler graph for the method.
  private val graph = new CompilerGraph(riRuntime)
  private val frameState = new FrameStateBuilder(riMethod, maxLocals, maxStackSize, graph)

  def run() {
    //GraalOptions.Extend = true //endlos schleifen
    riMethod.compilerStorage().put("AST", nodes) //save the linear ast, so that the interpreter san reuse it
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

    def astIndex: Int = {
      require(node.getPosition <= nodes.size)
      node.getPosition
    }
  }

  private def appendConstant(constant: CiConstant): ConstantNode = graph.unique(new ConstantNode(constant))

  var lastNode: FixedWithNextNode = _

  private def branchTaken(astNode: AstNode): Boolean = {
    var taken = true
    astNode.visit(new NodeVisitor {
      def visit(node: AstNode): Boolean =
        node match {
          case assignment: Assignment =>
            assignment.getRight.visit(this)
            false
          case _: EmptyExpression | _: ExpressionStatement | _: ReturnStatement | _: Scope | _: VariableInitializer =>
            true
          case node@(_: InfixExpression | _: NumberLiteral | _: Name | _: StringLiteral | _: FunctionCall) =>
            taken = !taken || node.dataType != Undefined
            taken
        }
    })
    taken
  }

  private def append(v: FixedNode): ValueNode = {
    if (v.isInstanceOf[DeoptimizeNode] && lastNode.predecessor() != null) {
      var cur: Node = lastNode
      var prev = cur
      val mybreaks = new scala.util.control.Breaks
      import mybreaks.{break, breakable}
      breakable {
        while (cur != cur.graph().start() && !(cur.isInstanceOf[ControlSplitNode])) {
          require(cur.predecessor() != null)
          prev = cur
          cur = cur.predecessor()
          if (cur.predecessor() == null || cur.isInstanceOf[ExceptionObjectNode]) {
            break()
          }
        }
      }

      if (cur.isInstanceOf[IfNode]) {
        val ifNode = cur.asInstanceOf[IfNode]
        if (ifNode.falseSuccessor() == prev) {
          val successor = ifNode.trueSuccessor()
          ifNode.setTrueSuccessor(null)
          val condition = ifNode.compare()
          val fixedGuard = graph.add(new FixedGuardNode(condition))
          fixedGuard.setNext(successor)
          ifNode.replaceAndDelete(fixedGuard)
          lastNode = null
          return v
        }
      } else if (prev ne cur) {
        prev.replaceAtPredecessors(v)
        lastNode = null
        return v
      }
    }
    lastNode.setNext(v)
    lastNode = null
    v
  }

  private def append(v: FixedWithNextNode) {
    lastNode.setNext(v)
    lastNode = v
  }

  private def append(v: ValueNode) = v

  def visit(node: AstNode): Boolean =
    node match {
      case _: AstRoot =>
        lastNode = graph.add(new AnchorNode)
        graph.start().setNext(lastNode)
        true
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
        frameState.storeLocal(name.varIndex, frameState.pop(assignment.getRight.kind))
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
            case IADD | LADD => new SafeAddNode(x, y)
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
          frameState.push(result, append(graph.unique(v)))
        }
        def genIfSame(kind: CiKind, condition: Condition) {
          val y = frameState.pop(kind)
          val x = frameState.pop(kind)
          require(!x.isDeleted && !y.isDeleted)
          frameState.ipush(append(MaterializeNode.create(graph.unique(new CompareNode(x, condition, y)), graph)))
        }
        val kind = (infixExpression.getLeft.dataType, infixExpression.getRight.dataType) match {
          case (AllwaysInt, AllwaysDouble) =>
            infixExpression.getLeft.visit(this)
            genConvert(I2D, CiKind.Int, CiKind.Double)
            infixExpression.getRight.visit(this)
            infixExpression.getRight.kind
          case (AllwaysDouble, AllwaysInt) =>
            infixExpression.getLeft.visit(this)
            infixExpression.getRight.visit(this)
            genConvert(I2D, CiKind.Int, CiKind.Double)
            infixExpression.getLeft.kind
          case (AllwaysInt, AllwaysInt) | (AllwaysDouble, AllwaysDouble) =>
            infixExpression.getLeft.visit(this)
            infixExpression.getRight.visit(this)
            infixExpression.getLeft.kind
        }
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
      case ifStmt: IfStatement =>
        ifStmt.getCondition.visit(this)
        val probability = 0.5
        val y = appendConstant(CiConstant.INT_1)
        val x = frameState.ipop()
        val ifNode = graph.add(new IfNode(graph.unique(new CompareNode(x, Condition.EQ, y)), probability))
        append(ifNode)
        val ifFrameState = frameState.duplicate(if (ifStmt.getElsePart != null) ifStmt.getElsePart.astIndex else ifStmt.getNext.asInstanceOf[AstNode].astIndex) //erste instruktion nach dem then zweig
        val trueAnchor = graph.add(new AnchorNode)
        ifNode.setTrueSuccessor(trueAnchor)
        val falseAnchor = graph.add(new AnchorNode)
        ifNode.setFalseSuccessor(falseAnchor)
        lastNode = trueAnchor
        ifStmt.getThenPart.visit(this)
        val trueEnd = graph.add(new EndNode)
        val trueEndState = frameState.create(ifStmt.getNext.asInstanceOf[AstNode].astIndex) //nächstes statement nach if statement
        append(trueEnd)
        lastNode = falseAnchor
        frameState.initializeFrom(ifFrameState)
        if (ifStmt.getElsePart != null) {
          if (branchTaken(ifStmt.getElsePart)) ifStmt.getElsePart.visit(this)
          else append(graph.add(new DeoptimizeNode(DeoptimizeNode.DeoptAction.None)))
        }
        val falseEnd = graph.add(new EndNode)
        append(falseEnd)
        val mergeNode = graph.add(new MergeNode)
        mergeNode.addEnd(trueEnd)
        trueEndState.merge(mergeNode, frameState)
        mergeNode.addEnd(falseEnd)
        mergeNode.setStateAfter(trueEndState)
        frameState.initializeFrom(trueEndState)
        lastNode = mergeNode
        false
      case whileLoop: WhileLoop =>
        val loopBegin = graph.add(new LoopBeginNode)
        val endNode = graph.add(new EndNode)
        append(endNode)
        loopBegin.addEnd(endNode)
        val foo = frameState.create(whileLoop.getCondition.astIndex) //vor der schleife, anfang der condition
        foo.insertLoopPhis(loopBegin)
        frameState.initializeFrom(foo)
        lastNode = loopBegin
        loopBegin.setStateAfter(foo)
        whileLoop.getCondition.visit(this)
        val probability = 0.5
        val y = appendConstant(CiConstant.INT_1)
        val x = frameState.ipop()
        val ifNode = graph.add(new IfNode(graph.unique(new CompareNode(x, Condition.EQ, y)), probability))
        append(ifNode)
        val trueAnchor = graph.add(new AnchorNode)
        ifNode.setTrueSuccessor(trueAnchor)
        val falseAnchor = graph.add(new AnchorNode)
        ifNode.setFalseSuccessor(falseAnchor)
        val loopExitState = frameState.duplicate(whileLoop.getNext.asInstanceOf[AstNode].astIndex) //nach der schleifen
        lastNode = trueAnchor
        if (!branchTaken(whileLoop.getBody)) whileLoop.getBody.visit(this)
        else append(graph.add(new DeoptimizeNode(DeoptimizeNode.DeoptAction.None)))
        val loopEndNode = graph.add(new LoopEndNode)
        foo.merge(loopBegin, frameState)
        append(loopEndNode)
        loopEndNode.setLoopBegin(loopBegin)
        lastNode = falseAnchor
        frameState.initializeFrom(loopExitState)
        false
      case expressionStatement: ExpressionStatement =>
        expressionStatement.getExpression.visit(this)
        if (!expressionStatement.getExpression.isInstanceOf[Assignment])
          if (expressionStatement.getNext == null) {
            //last expression is return value
            val returnNode = graph.add(new ReturnNode(frameState.pop(expressionStatement.getExpression.kind)))
            graph.setReturn(returnNode)
            append(returnNode)
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
        append(returnNode)
        false
      case scope: Scope => true
    }
}