/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.exceptions

trait ThinThrowable { self: Throwable =>
  // Sometimes we want to use Exceptions/Throwables in try/catch as a form of
  // flow control (yeah, usually not that great of an idea), for example with
  // ProcessingError and withParseErrorThrowing. Unfortunately, the problem
  // with this is that Exceptions are pretty heavy to create, mostly do to the
  // building of stack traces. But, in cases where we use Exceptions for flow
  // control, we don't need all this extra baggage of the stack traces. So
  // override the fillInStackTrace method so that stack traces aren't
  // generated, resulting in a big performance gain when used.
  override def fillInStackTrace(): Throwable = null
}

trait ThinThrowableWithCause extends ThinThrowable { self: Throwable =>
  // The Throwable(cause: Throwable) constructor calls cause.toString and saves
  // it in the private detailMessage variable. In our case, often times this
  // detailedMessage will never even be accessed because of backtracking or
  // suspensions. In these cases, we really only want to build the string when
  // calling getMessage() to save memory and computation time.
  //
  // Note that this only applies when extending a Throwable(cause: Throwable)
  // or the equivalent Exception. If extending an Exception accepts a a
  // (message: String) or does not accept a (cause: Throwable), then using this
  // is unnecessary.

  // The cause for this exception. Override this rather than passing the cause
  // as an argument to the Throwable constructor.
  def throwableCause: Throwable

  private lazy val message_ = throwableCause.getMessage()

  override def getMessage(): String = message_

  private lazy val cause_ = throwableCause

  override def getCause(): Throwable = cause_
}

abstract class UnsuppressableException(m: String) extends Exception(m) {
  def this() = this("") // no arg constructor also.
}
class UsageException(m: String) extends UnsuppressableException(m)
class NotYetImplementedException(m: String) extends UnsuppressableException("Not yet implemented: " + m)
class Abort(m: String) extends UnsuppressableException(m) {
  def this(th: Throwable) = this(th.getMessage())
}

class Assert {
  def shortBacktrace = {
    val frames = Thread.currentThread().getStackTrace().toList.take(6).tail.tail
    frames.map { _.toString }.mkString("\n")
  }

  def toss(x: Throwable) = {
    throw x
  }
}

object Assert extends Assert {

  /*
   * Note that in macro definitions, the argument names here must match the argument names
   * in the macro implementations.
   */

  /**
   * Verbose name helps you get the sense of the predicate right.
   */
  def usageErrorUnless(testAbortsIfFalse: Boolean, message: String): Unit = macro AssertMacros.usageMacro2
  def usageErrorUnless(testAbortsIfFalse: Boolean): Unit = macro AssertMacros.usageMacro1
  /**
   * Brief form
   */
  def usage(testAbortsIfFalse: Boolean, message: String): Unit = macro AssertMacros.usageMacro2
  def usage(testAbortsIfFalse: Boolean): Unit = macro AssertMacros.usageMacro1

  /**
   * test for something that the program is supposed to be insuring.
   *
   * This is for more complex invariants than the simple 'impossible' case.
   */
  def invariant(testAbortsIfFalse: Boolean): Unit = macro AssertMacros.invariantMacro1

  /**
   * Conditional behavior for NYIs
   */
  def notYetImplemented(): Nothing = macro AssertMacros.notYetImplementedMacro0
  def notYetImplemented(testThatWillThrowIfTrue: Boolean): Unit = macro AssertMacros.notYetImplementedMacro1
  def notYetImplemented(testThatWillThrowIfTrue: Boolean, msg: String): Unit = macro AssertMacros.notYetImplementedMacro2
  //
  // Throughout this file, specifying return type Nothing
  // gets rid of many spurious (scala compiler bug) dead code
  // warnings. It doesn't hurt to have them, so they're in now
  // which allows the scala compiler checking for dead-code pass
  // to be enabled.
  //

  def usageError(message: String = "Usage error."): Nothing = {
    abort(message)
  }

  def nyi(info: String): Nothing = {
    toss(new NotYetImplementedException(info + "\n" + shortBacktrace))
  }

  def nyi(): Nothing = {
    toss(new NotYetImplementedException(shortBacktrace))
  }

  def abort(message: String = ""): Nothing = {
    toss(new Abort(message + "\n" + shortBacktrace))
  }

  /**
   * Like abort, but takes 2nd argument that is expected to be the text
   * of the test expression (as captured by macro.
   */
  def abort2(message: String, testAsString: String): Nothing = {
    abort(message + " (" + testAsString + ")")
  }

  def abort(th: Throwable): Nothing = {
    toss(new Abort(th))
  }

  def impossible(message: String = "impossible! this code path is supposed to be unreachable."): Nothing = {
    abort(message)
  }

  /**
   * use when a match/case has exhausted all possibles.
   *
   * Sometimes, if you are just dispatching on an enum, scala can prove you've exhausted all
   * possibles. In other cases, use this. Eg., exhaustive case analysis on unsealed case classes (so
   * the compiler can't assure you, but you still believe you are being exhaustive). Under program
   * maintenance people break these things. Hence, use this to catch those kinds of fall throughs.
   */
  def impossibleCase(x: Any = null) = {
    val extra =
      if (x == null) ""
      else " Value was: " + x + "."
    impossible("Should be no fall through to this case." + extra)
  }

  def impossibleCase: Nothing = impossibleCase(null)

  /**
   * Use when a case or if/then analysis has fallen through to a situation that
   * a program invariant should be assuring doesn't happen. That is, where
   * the case analysis has exhaused all the situations that are consistent with
   * the invariant.
   *
   * This is different from an impossible - those are for situations which
   * are simpler to show are impossible.
   */
  def invariantFailed(msg: String = "") = {
    abort("Invariant broken. " + msg)
  }

}
