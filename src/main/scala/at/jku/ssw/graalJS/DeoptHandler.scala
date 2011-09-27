/*
 * Copyright (c) 2011, 2011, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package at.jku.ssw.graalJS

import java.lang.reflect.Modifier

import com.sun.cri.ri._
import org.mozilla.javascript.ast.AstNode


class DeoptHandler {

  /**
    * Deoptimization handler method for methods with a void return parameter.
    */
  def handle_void(method: RiMethod, bci: Int, values: Array[AnyRef], numLocals: Int, numStack: Int, numLocks: Int) {
    handle(method, bci, values, numLocals, numStack, numLocks)
  }

  /**
    * Deoptimization handler method for methods with an int return parameter.
    */
  def handle_int(method: RiMethod, bci: Int, values: Array[AnyRef], numLocals: Int, numStack: Int, numLocks: Int): Int = {
    handle(method, bci, values, numLocals, numStack, numLocks).asInstanceOf[Int]
  }

  /**
    * Deoptimization handler method for methods with an long return parameter.
    */
  def handle_long(method: RiMethod, bci: Int, values: Array[AnyRef], numLocals: Int, numStack: Int, numLocks: Int): Long = {
    handle(method, bci, values, numLocals, numStack, numLocks)
  }

  /**
    * Deoptimization handler method for methods with an object return parameter.
    */
  def handle_object(method: RiMethod, bci: Int, values: Array[AnyRef], numLocals: Int, numStack: Int, numLocks: Int): AnyRef = {
    handle(method, bci, values, numLocals, numStack, numLocks)
    null
  }

  /**
    * Deoptimization handler method: prints the current state of the method execution.
    */
  def handle(method: RiMethod, bci: Int, values: Array[AnyRef], numLocals: Int, numStack: Int, numLocks: Int): Long = {
    //println("Deoptimization: " + method.name() + "@" + bci)
    val argCount = method.signature().argumentCount(!Modifier.isStatic(method.accessFlags()))
    //println(numLocals + " " + numStack + " " + values.length)
    //println(values.mkString(","))
    val localVariables = new Array[AnyRef](method.compilerStorage().get("MAX_LOCALS").asInstanceOf[Int])
    val operandStack = new Array[AnyRef](method.compilerStorage().get("MAX_STACK").asInstanceOf[Int])
    System.arraycopy(values, argCount, localVariables, 0, numLocals)
    System.arraycopy(values, argCount + numLocals, operandStack, 0, numStack)
    val interpreter = new Interpreter(method.compilerStorage().get("AST").asInstanceOf[collection.mutable.ArrayBuffer[AstNode]], localVariables, operandStack)
    interpreter.interpret(bci)
  }

}
