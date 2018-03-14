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

package org.apache.daffodil.grammar
import org.apache.daffodil.dsom.SequenceTermBase
import org.apache.daffodil.grammar.primitives.SequenceCombinator
import org.apache.daffodil.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.schema.annotation.props.gen.SequenceKind
import org.apache.daffodil.grammar.primitives.UnorderedSequenceCombinator

trait SequenceGrammarMixin extends GrammarMixin { self: SequenceTermBase =>

  final override lazy val groupContent = prod("groupContent") {
    self.sequenceKind match {
      case SequenceKind.Ordered => orderedSequenceContent
      case SequenceKind.Unordered => unorderedSequenceContent //subsetError("Unordered sequences are not supported.") // unorderedSequenceContent
    }
  }

  private lazy val orderedSequenceContent = prod("sequenceContent") {
    SequenceCombinator(this, orderedTerms)
  }

  private lazy val unorderedSequenceContent = prod("unorderedSequenceContent") {
    //    lazy val uoseq = self.unorderedSeq.get
    //    UnorderedSequenceCombinator(this, uoseq.terms)
    val choiceTerms = groupMembers.map { _.asTermInChoice }
    //Console.out.println(unorderedSep.parser)
    val terms = groupMembers.map { _.asTermInUnorderedSequence }
    //Console.out.println("terms: " + terms)
    UnorderedSequenceCombinator(this, terms, unorderedSep, self.separatorPosition)
  }

  protected lazy val orderedTerms = groupMembers.map { _.asTermInOrderedSequence }

  /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */
  final lazy val hasPrefixSep = sepExpr(SeparatorPosition.Prefix)

  final lazy val hasInfixSep = sepExpr(SeparatorPosition.Infix)

  final lazy val hasPostfixSep = sepExpr(SeparatorPosition.Postfix)
  
  final lazy val unorderedSep = getUnorderedSep(this, hasSeparator)

  // note use of pass by value. We don't want to even need the SeparatorPosition property unless there is a separator.
  private def sepExpr(pos: => SeparatorPosition): Boolean = {
    if (hasSeparator) if (separatorPosition eq pos) true else false
    else false
  }

  final lazy val hasSeparator = separatorParseEv.isKnownNonEmpty
}

