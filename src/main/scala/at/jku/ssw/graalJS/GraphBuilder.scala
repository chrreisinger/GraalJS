package at.jku.ssw.graalJS

import _root_.java.lang.reflect.Method
import org.mozilla.javascript.Token
import org.mozilla.javascript.ast._
import com.sun.cri.ci.{CiConstant, CiKind}
import com.sun.cri.bytecode.Bytecodes._
import com.oracle.max.graal.compiler.value.FrameStateBuilder
import com.oracle.max.graal.nodes._
import com.oracle.max.graal.nodes.calc._
import ExpressionType._
import com.oracle.max.graal.compiler.debug.IdealGraphPrinterObserver
import com.oracle.max.graal.runtime.{HotSpotMethodResolved, HotSpotTargetMethod, CompilerImpl}
import com.sun.cri.ri.RiMethod
import com.oracle.max.graal.compiler.GraalOptions

final class GraphBuilder(m: Method, maxLocals: Int, maxStackSize: Int) extends NodeVisitor {

  // Obtain RiMethod and RiRuntime instances.
  val compilerInstance = CompilerImpl.getInstance()
  val vmEntries = compilerInstance.getVMEntries
  val riMethod = vmEntries.getRiMethod(m)
  val riRuntime = compilerInstance.getRuntime

  // Create the compiler graph for the method.
  val graph = new CompilerGraph(riRuntime)
  val frameState = new FrameStateBuilder(riMethod, maxLocals, maxStackSize, graph)

  def run() {
    // Compile and print disassembly.
    val result = compilerInstance.getCompiler.compileMethod(riMethod, graph);
    //println(riRuntime.disassemble(result.targetMethod()));

    // Install method!
    HotSpotTargetMethod.installMethod(compilerInstance, riMethod.asInstanceOf[HotSpotMethodResolved], result.targetMethod())
    println(m.invoke(null))
  }

  implicit def toRichNode(node: AstNode) = new {
    def dataType = typeToExpressionType(node.getLength)

    def kind = typeToExpressionType(node.getLength) match {
      case AllwaysInt => CiKind.Int
      case AllwaysBoolean => CiKind.Boolean
      case AllwaysDouble => CiKind.Double
      case AllwaysString => CiKind.Object
    }

    def varIndex = node.getLineno
  }

  /*
     private void insertLoopPhis(LoopBeginNode loopBegin, FrameState newState) {
         int stackSize = newState.stackSize();
         for (int i = 0; i < stackSize; i++) {
             // always insert phis for the stack
             ValueNode x = newState.stackAt(i);
             if (x != null) {
                 newState.setupPhiForStack(loopBegin, i).addInput(x);
             }
         }
         int localsSize = newState.localsSize();
         for (int i = 0; i < localsSize; i++) {
             ValueNode x = newState.localAt(i);
             if (x != null) {
                 newState.setupPhiForLocal(loopBegin, i).addInput(x);
             }
         }
     }


    private void ifNode(ValueNode x, Condition cond, ValueNode y) {
        assert !x.isDeleted() && !y.isDeleted();
        double probability = method.branchProbability(bci());
        if (probability < 0) {
            if (GraalOptions.TraceProbability) {
                TTY.println("missing probability in " + method + " at bci " + bci());
            }
            probability = 0.5;
        }

        IfNode ifNode = graph.add(new IfNode(graph.unique(new CompareNode(x, cond, y)), probability));
        append(ifNode);
        BlockMap.BranchOverride override = branchOverride.get(bci());
        FixedNode tsucc;
        if (override == null || override.taken == false) {
            tsucc = createTargetAt(stream().readBranchDest(), frameState);
        } else {
            tsucc = createTarget(override.block, frameState);
        }
        ifNode.setBlockSuccessor(0, tsucc);
        FixedNode fsucc;
        if (override == null || override.taken == true) {
            fsucc = createTargetAt(stream().nextBCI(), frameState);
        } else {
            fsucc = createTarget(override.block, frameState);
        }
        ifNode.setBlockSuccessor(1, fsucc);
    }
        private static LoopBeginNode loopBegin(Block block) {
        EndNode endNode = (EndNode) block.firstInstruction.next();
        LoopBeginNode loopBegin = (LoopBeginNode) endNode.merge();
        return loopBegin;
    }
  */

  private def appendConstant(constant: CiConstant): ConstantNode = graph.unique(new ConstantNode(constant))

  private def append(v: ValueNode) = v

  private def storeLocal(kind: CiKind, index: Int) {
    frameState.storeLocal(index, frameState.pop(kind))
  }

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
        storeLocal(name.kind, name.varIndex)
        false
      case functionCall: FunctionCall => sys.error("not implemented")
      case _: EmptyExpression => false //nothing
      case infixExpression: InfixExpression =>
        infixExpression.getLeft.visit(this)
        infixExpression.getRight.visit(this)
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
            case _ => sys.error("wow")
          }
          val result1 = append(graph.unique(v))
          frameState.push(result, result1)
        }
        def genIfSame(kind: CiKind, cond: Condition) {
          val y = frameState.pop(kind)
          val x = frameState.pop(kind)
          require(!x.isDeleted && !y.isDeleted)
          frameState.ipush(append(MaterializeNode.create(graph.unique(new CompareNode(x, cond, y)), graph)))
        }
        val kind = (infixExpression.getLeft.dataType, infixExpression.getRight.dataType) match {
          case (AllwaysInt, AllwaysDouble) =>
            genConvert(I2D, CiKind.Int, CiKind.Double)
            CiKind.Double
          case (AllwaysDouble, AllwaysInt) =>
            genConvert(I2D, CiKind.Int, CiKind.Double)
            CiKind.Double
          case (AllwaysInt, AllwaysInt) => CiKind.Int
        }
        infixExpression.getOperator match {
          case Token.OR | Token.AND =>
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
        storeLocal(variableInitializer.getInitializer.kind, name.varIndex)
        false
      case whileLoop: WhileLoop => sys.error("not implemented")
      case expressionStatement: ExpressionStatement =>
        expressionStatement.getExpression.visit(this)
        if (expressionStatement.getNext == null) {
          //last expression ist return value
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

  def visualize() {
    GraalOptions.Plot = true
    val printer = new IdealGraphPrinterObserver("localhost", 4444)
    printer.printSingleGraph("test", graph)
  }
}