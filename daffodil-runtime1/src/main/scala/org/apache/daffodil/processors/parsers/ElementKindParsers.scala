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

package org.apache.daffodil.processors.parsers

import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.infoset.DIComplex
import org.apache.daffodil.infoset.DIElement
import org.apache.daffodil.infoset.DINode
import org.apache.daffodil.processors.ChoiceDispatchKeyEv
import org.apache.daffodil.processors.DelimiterParseEv
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.EscapeSchemeParseEv
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.NamedQName
import org.apache.daffodil.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.dsom.SchemaDefinitionDiagnosticBase
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.equality.ViewEqual

class ComplexTypeParser(rd: RuntimeData, bodyParser: Parser)
  extends CombinatorParser(rd) {
  override def nom = "ComplexType"

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {
    start.mpstate.childIndexStack.push(1L) // one-based indexing
    bodyParser.parse1(start)
    start.mpstate.childIndexStack.pop()
    ()
  }
}

class UnorderedSequenceCombinatorParser(rd: RuntimeData, bodyParser: Parser,
                                        sepParser: Parser,
                                        sepPosition: SeparatorPosition,
                                        sortOrder: Seq[(NamedQName, NS)],
                                        scalarMembers: Seq[(NamedQName, NS)])
  extends CombinatorParser(rd) {
  override def nom = "UnorderedSequence"

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(bodyParser, sepParser)

  def checkScalarsOccurExactlyOnce(children: ArrayBuffer[DINode], pstate: PState): Unit = {
    // Always occurs, does not depend on validation
    scalarMembers.foreach {
      case (nqname, path) => {
        val scalarChildren = children.filter(e => {
          e.namedQName == nqname
        })
        val numScalarChildren = scalarChildren.length
        if (numScalarChildren > 1 || numScalarChildren == 0)
          pstate.SDE("%s is scalar but occurred %s times.\nPath: %s\nOffending children: %s",
            nqname, scalarChildren.length, path, scalarChildren.mkString(", "))
      }
    }
  }

  // TODO: Defaulting is not working for dropDefaulted in unordered seq
  //
  //  def dropDefaulted(elt: DIElement, pstate: PState): Unit = {
  //    // Always occurs, does not depend on validation
  //
  //    // RequiredElement, index in its array <= minOccurs
  //    // If empty rep, a required element that has a default value will be defaulted.
  //
  //    // Drop anything after the minOccurs that was defaulted.
  //
  //    // So we need the original minOccurs for each element.
  ////    val childrenDetached = elt.removeContent().toList.asInstanceOf[List[org.jdom2.Element]]
  ////    val erds = childrenDetached.map(c => {
  ////      val infoSetElem = Infoset.newElement(c)
  ////      val rd = infoSetElem.schemaComponent
  ////      rd
  ////    }).distinct
  //    
  //    //val childrenDetached = elt.contents
  //    val erds = elt.erd.childERDs.distinct
  //    val newChildren = scala.collection.mutable.Seq.empty[DINode]
  //
  //    erds.foreach(erd => {
  //      val minOccurs = erd.minOccurs.get
  //      val ns = erd.targetNamespace
  //      val name = erd.namedQName//erd.name
  //
  //      val children = elt.contents.filter(c => {
  //          name == c.namedQName
  //      })
  //
  //      val numChildren = children.length
  //
  //      if (numChildren >= minOccurs) {
  //        val firstChild = children.head
  //        val restOfChildren = children.tail
  //        val restOfNonDefaultedChildren = restOfChildren.filterNot(c => c.getAttribute("defaulted", XMLUtils.INT_NS_OBJECT).getBooleanValue())
  //        val restOfNonDefaultedChildren = restOfChildren.view.filterNot(c => { c.asInstanceOf[DIElement] match {
  //          case s: DISimple if s.i=> 
  //          case c: DIComplex =>
  //        } })
  //        
  //        val newContent = firstChild :: restOfNonDefaultedChildren
  //        if (newContent.length > 0)
  //          elt.addContent(newContent)
  //      } else {
  //        // Nothing to do here, reattach children.
  //        elt.addContent(childrenDetached)
  //      }
  //    })
  //  }

  /**
   * We want to check that we met he expected occurrence values
   * but we don't want to SDE, instead we issue a PE (ProcessingError).
   */
  def checkOccurrence(elt: DIElement, childrenList: ArrayBuffer[DINode], state: PState): Unit = {

    val erds = childrenList.map(c => c.erd)

    erds.foreach(erd => {
      val minOccurs = erd.minOccurs
      val maxOccurs = erd.maxOccurs
      val name = erd.namedQName

      val children = childrenList.filter(c => name == c.namedQName)

      val numChildren = children.length

      minOccurs.foreach { minOccurs =>
        if (numChildren < minOccurs) {
          PE(state, "UnorderedSequence.checkOccurrence - %s failed minOccurs check. Expected at least %s but found %s.",
            elt.erd, minOccurs, numChildren)
        }
      }
      maxOccurs.foreach { maxOccurs =>
        if (numChildren > maxOccurs) {
          PE(state, "UnorderedSequence.checkOccurrence - %s failed maxOccurs check. Expected at most %s but found %s.",
            elt.erd, maxOccurs, numChildren)
        }
      }
    })
  }

  def sort(complex: DIComplex, childrenDetached: ArrayBuffer[DINode], pstate: PState): Unit = { //def sort(elt: DIElement, pstate: PState): Unit = {

    //    println("Children Detached: " + childrenDetached)
    //    println("Sort Order: " + sortOrder)
    //    
    val sorted = ArrayBuffer.empty[DINode]

    sortOrder.foreach {
      case (namedQName, ns) => {
        val newContent = childrenDetached.filter(child => child.namedQName == namedQName)
        sorted.appendAll(newContent)
      }
    }
    complex.childNodes.clear()
    complex.childNodes.appendAll(sorted)
  }

  def processResult(start: PState): Unit = {
    val elt = start.thisElement
    val complex = elt.asComplex
    val childrenDetached = complex.childNodes

    checkScalarsOccurExactlyOnce(childrenDetached, start)
    // TODO: Defaulting isn't working yet
    //    dropDefaulted(currentElemAfter, end)
    checkOccurrence(elt, childrenDetached, start)

    // Sort so that the contents are in the expected order.
    sort(complex, childrenDetached, start)
  }

  def checkN(pstate: PState, n: Long): Boolean = {
    true // Unbounded
  }

  def parseAllRepeats(initialState: PState): Unit = {
    Assert.invariant(initialState.processorStatus eq Success)
    val startState = initialState.mark("RepUnboundedParser1")
    val pstate = initialState
    var priorState = initialState.mark("RepUnboundedParser2")
    var returnFlag = false
    var isFirst = true
    while (!returnFlag && (pstate.processorStatus eq Success)) {

      // TODO: Defaulting isn't working yet
      //      erd.maxOccurs.foreach { maxOccurs =>
      //        if ((occursCountKind == OccursCountKind.Implicit) &&
      //          (maxOccurs == -1)) {
      //          erd.minOccurs.foreach { minOccurs =>
      //            if (pstate.mpstate.arrayPos - 1 <= minOccurs) {
      //              // Is required element
      //              // Need to trigger default value creation
      //              // in right situations (like the element is defaultable)
      //              // This is relatively easy for simple types
      //              // for complex types, defaulting is trickier as one
      //              // must recursively traverse the type, and then determine one
      //              // has not advanced the data at all.
      //            }
      //          }
      //        }
      //      }

      //
      // Every parse is a new point of uncertainty.
      pstate.pushDiscriminator
      if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, this)

      try {
        sepPosition match {
          case SeparatorPosition.Prefix => {
            sepParser.parse1(pstate) // parse separator
            bodyParser.parse1(pstate)
          }
          case SeparatorPosition.Infix => {
            // Optional Sep (if not first)
            if (!isFirst) sepParser.parse1(pstate)
            bodyParser.parse1(pstate)
          }
          case SeparatorPosition.Postfix => {
            bodyParser.parse1(pstate)
            sepParser.parse1(pstate) // parse separator
          }
        }

      } catch {
        case sde: SchemaDefinitionDiagnosticBase => {
          pstate.discard(startState)
          throw sde
        }
      }

      if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, this)
      if (pstate.processorStatus ne Success) {
        //
        // Did not succeed
        //
        // Was a discriminator set?
        //
        if (pstate.discriminator == true) {
          // we fail the whole RepUnbounded, because there was a discriminator set
          // before the failure.
          pstate.reset(startState)
          // no need discard priorState, that is implicitly discarded by resetting the startState
        } else {
          //
          // no discriminator, so suppress the failure. Loop terminated with prior element.
          //

          log(LogLevel.Debug, "Failure suppressed. This is normal termination of a occursCountKind='parsed' array.")
          pstate.reset(priorState)
          pstate.discard(startState)
        }
        returnFlag = true
      } else {
        // Success
        // Need to check for forward progress
        if (pstate.bitPos =#= priorState.bitPos0b) {
          pstate.discard(priorState) // didn't move, but might have assigned variables, have to undo those.
          pstate.discard(startState)
          PE(pstate,
            "RepUnbounded - No forward progress at byte %s. Attempt to parse %s " +
              "succeeded but consumed no data.\nPlease re-examine your schema to correct this infinite loop.",
            pstate.bytePos, rd.diagnosticDebugName)
          returnFlag = true
        } else {
          pstate.discard(priorState)
          priorState = pstate.mark("RepUnboundedParser3")
          pstate.mpstate.moveOverOneArrayIndexOnly
          returnFlag = false
        }
      }
      pstate.popDiscriminator
      isFirst = false
    }
    Assert.invariant(returnFlag == true)
  }

  def parse(state: PState): Unit = {
    state.mpstate.groupIndexStack.push(1L) // one-based indexing

    if (!checkN(state, -1)) return

    parseAllRepeats(state)

    processResult(state)

    state.mpstate.groupIndexStack.pop()
    state.mpstate.moveOverOneGroupIndexOnly()
    ()
  }

}

/**
 * The purpose of this parser is to create/evaluate delimiter DFAs
 * and push them to the delimiter stack (bring them in scope) for
 * subsequent (internal/body) parse steps.  Then on the way out pop
 * the delimiter DFAs (bring them out of scope) after
 * the internal/body parser has completed.
 */
class DelimiterStackParser(delimiters: Array[DelimiterParseEv],
                           ctxt: RuntimeData, bodyParser: Parser)
  extends CombinatorParser(ctxt) {

  override lazy val childProcessors = List(bodyParser)

  override lazy val runtimeDependencies = delimiters.toSeq

  def parse(start: PState): Unit = {

    val newLocalIndex = start.mpstate.delimiters.length
    start.mpstate.delimitersLocalIndexStack.push(newLocalIndex)

    // evaluate and add delimiters to the stack
    var i: Int = 0
    while (i < delimiters.length) {
      start.mpstate.delimiters ++= delimiters(i).evaluate(start)
      i += 1
    }

    // set the index of the newly added delimiters
    val newDelimLen = start.mpstate.delimiters.length
    i = newLocalIndex
    while (i < newDelimLen) {
      start.mpstate.delimiters(i).indexInDelimiterStack = i
      i += 1
    }

    // parse
    bodyParser.parse1(start)

    // pop delimiters
    start.mpstate.delimiters.reduceToSize(start.mpstate.delimitersLocalIndexStack.pop)
  }
}

/**
 * *
 * This parser should only ever be called when a dynamic escape scheme exists
 * so the escape scheme is evaluated in the right scope. If a constant
 * escape scheme exists, the Evaluatable should store the constant and this
 * should never be called.
 *
 * Note that the escape scheme evaluatable (and its dependencies) are manually
 * cached, so upon exiting scope the cache must be invalidated.
 */
class DynamicEscapeSchemeParser(escapeScheme: EscapeSchemeParseEv,
                                ctxt: TermRuntimeData, bodyParser: Parser)
  extends CombinatorParser(ctxt) {

  override lazy val childProcessors = Seq(bodyParser)

  override lazy val runtimeDependencies = List(escapeScheme)

  def parse(start: PState): Unit = {
    // evaluate the dynamic escape scheme in the correct scope. the resulting
    // value is cached in the Evaluatable (since it is manually cached) and
    // future parsers that use this escape scheme will use that cached value.
    escapeScheme.newCache(start)
    escapeScheme.evaluate(start)

    // Parse
    bodyParser.parse1(start)

    // invalidate the escape scheme cache
    escapeScheme.invalidateCache(start)
  }
}

class SequenceCombinatorParser(rd: TermRuntimeData, bodyParser: Parser)
  extends CombinatorParser(rd) {
  override def nom = "Sequence"

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {
    start.mpstate.groupIndexStack.push(1L) // one-based indexing

    bodyParser.parse1(start)

    start.mpstate.groupIndexStack.pop()
    start.mpstate.moveOverOneGroupIndexOnly()
    ()
  }
}

/**
 * This is essentially just a wrapper around the bodyParser, which is an
 * AltCompParser. This is only here to maintain symmetry with the unparse side,
 * which has a more complicated unparser that differs from an AltCompUnparser.
 */
class ChoiceCombinatorParser(rd: TermRuntimeData, bodyParser: Parser)
  extends CombinatorParser(rd) {
  override def nom = "Choice"

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {
    bodyParser.parse1(start)
  }
}

class ChoiceDispatchCombinatorParser(rd: TermRuntimeData, dispatchKeyEv: ChoiceDispatchKeyEv, dispatchBranchKeyMap: Map[String, Parser])
  extends CombinatorParser(rd) {
  override def nom = "ChoiceDispatch"

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = dispatchBranchKeyMap.values.toSeq

  def parse(pstate: PState): Unit = {
    val key = dispatchKeyEv.evaluate(pstate)

    val parserOpt = dispatchBranchKeyMap.get(key)
    if (parserOpt.isEmpty) {
      val diag = new ChoiceDispatchNoMatch(context.schemaFileLocation, pstate, key)
      pstate.setFailed(diag)
    } else {
      val parser = parserOpt.get

      // Note that we are intentionally not pushing/popping a new
      // discriminator here, as is done in the ChoiceCombinatorParser and
      // AltCompParser. This has the effect that if a branch of this direct
      // dispatch choice specifies a discriminator, then it will discriminate a
      // point of uncertainty outside of the choice. If we pushed a new
      // discriminator here if would essentially ignore discriminators on a
      // choice branch.

      log(LogLevel.Debug, "Dispatching to choice alternative: %s", parser)
      parser.parse1(pstate)

      if (pstate.processorStatus eq Success) {
        log(LogLevel.Debug, "Choice dispatch success: %s", parser)
      } else {
        log(LogLevel.Debug, "Choice dispatch failed: %s", parser)
        val diag = new ChoiceDispatchFailed(context.schemaFileLocation, pstate, pstate.diagnostics)
        pstate.setFailed(diag)
      }
    }
  }
}

class ArrayCombinatorParser(erd: ElementRuntimeData, bodyParser: Parser)
  extends CombinatorParser(erd) {
  override def nom = "Array"
  override lazy val childProcessors = Seq(bodyParser)

  override lazy val runtimeDependencies = Nil

  def parse(start: PState): Unit = {

    start.mpstate.arrayIndexStack.push(1L) // one-based indexing
    start.mpstate.occursBoundsStack.push(start.tunable.maxOccursBounds)

    bodyParser.parse1(start)

    val actualOccurs = start.mpstate.arrayIndexStack.pop()
    start.mpstate.occursBoundsStack.pop()

    if (start.processorStatus ne Success) return

    val shouldValidate =
      start.dataProc.isDefined && start.dataProc.value.getValidationMode != ValidationMode.Off

    if (shouldValidate && erd.minOccurs.isDefined && erd.maxOccurs.isDefined) {
      val minO = erd.minOccurs.get
      val maxO = erd.maxOccurs.get
      val isUnbounded = maxO == -1
      val occurrence = actualOccurs - 1

      if (isUnbounded && occurrence < minO)
        start.validationError("%s occurred '%s' times when it was expected to be a " +
          "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.diagnosticDebugName,
          occurrence, minO)
      else if (!isUnbounded && (occurrence < minO || occurrence > maxO))
        start.validationError("%s occurred '%s' times when it was expected to be a " +
          "minimum of '%s' and a maximum of '%s' times.", erd.diagnosticDebugName,
          occurrence, minO, maxO)
      else {
        //ok
      }
    }
  }
}

// This follows the same behavior as Arrays for parsing
class OptionalCombinatorParser(erd: ElementRuntimeData, bodyParser: Parser) extends ArrayCombinatorParser(erd, bodyParser) {
  override def nom = "Optional"
}
