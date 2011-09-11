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


class DeoptHandler {

  /**
    * Deoptimization handler method for methods with a void return parameter.
    */
  def handle_void(method: RiMethod, bci: Int, values: Array[AnyRef], numLocals: Int, numStack: Int, numLocks: Int) {
    handle(method, bci, values, numLocals, numStack, numLocks)
  }

  /**
    * Deoptimization handler method for methods with an int return parameter.gd
    */
  def handle_int(method: RiMethod, bci: Int, values: Array[AnyRef], numLocals: Int, numStack: Int, numLocks: Int): Int = {
    handle(method, bci, values, numLocals, numStack, numLocks)
    42 //the answer to all questions
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
  def handle(method: RiMethod, bci: Int, values: Array[AnyRef], numLocals: Int, numStack: Int, numLocks: Int): Int = {
    System.out.print("Deoptimization: " + method.name() + "@" + bci)
    var p = 0
    System.out.print("\nArguments: ")
    val argCount = method.signature().argumentCount(!Modifier.isStatic(method.accessFlags()))
    for (i <- 0 to argCount) {
      System.out.printf("%s ", values(p))
      p += 1
    }
    System.out.print("\nLocals: ")
    for (i <- argCount to numLocals) {
      System.out.printf("%s ", values(p))
      p += 1
    }
    System.out.print("\nExpression stack: ")
    for (i <- 0 to numStack) {
      System.out.printf("%s ", values(p))
      p += 1
    }
    System.out.print("\nLocks: ")
    for (i <- 0 to numLocks) {
      System.out.printf("%s ", values(p))
      p += 1
    }
    System.out.println()
    42
  }

}
