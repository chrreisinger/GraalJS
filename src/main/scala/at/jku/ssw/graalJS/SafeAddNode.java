package at.jku.ssw.graalJS;

import com.oracle.max.graal.nodes.*;
import com.oracle.max.graal.nodes.calc.*;
import com.oracle.max.graal.nodes.spi.*;
import com.sun.cri.bytecode.*;
import com.sun.cri.ci.*;

public final class SafeAddNode extends IntegerArithmeticNode implements LIRLowerable {

    public SafeAddNode(ValueNode x, ValueNode y) {
        super(CiKind.Int, Bytecodes.IADD, x, y);
    }

    @Override
    public void generate(LIRGeneratorTool generator) {
        generator.integerAdd(this, x(), y());
        generator.deoptimizeOn(Condition.OF);
    }
}
