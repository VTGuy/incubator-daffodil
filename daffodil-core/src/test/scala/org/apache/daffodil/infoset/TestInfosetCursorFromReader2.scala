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

package org.apache.daffodil.infoset

import org.junit.Test
import org.junit.Assert._
import org.apache.daffodil.Implicits._
import org.apache.daffodil.equality._
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.xml.XMLUtils
import scala.collection.immutable.Stream.consWrapper

object INoWarnU1 { ImplicitsSuppressUnusedImportWarning() }

class TestInfosetInputterFromReader2 {

  def infosetUnlimitedSource(size: Int) = {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()
    val pf = compiler.compileNode(sch)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val rootERD = u.ssrd.elementRuntimeData

    def foos: Stream[String] = "<foo>Hello</foo>" #:: foos
    val ex = XMLUtils.EXAMPLE_NAMESPACE.toString
    def strings =
      (("<bar xmlns='" + ex + "' >") #:: foos.take(size))

    val rdr = new java.io.InputStreamReader(
      new StreamInputStream(strings))

    val inputter = new XMLTextInfosetInputter(rdr)
    inputter.initialize(rootERD, u.getTunables())
    val ic = Adapter(inputter)
    ic
  }

  class StreamInputStream(
    private var strings: Stream[String]) extends java.io.InputStream {

    private var bytes = {
      val ss = strings.flatMap { _.getBytes() } ++ "</bar>".getBytes().toStream
      strings = Nil.toStream
      ss
    }

    override def read(): Int = {
      if (bytes.isEmpty) -1
      else {
        val b = bytes.head
        bytes = bytes.tail
        b.toInt
      }
    }

    override def close() { bytes = Nil.toStream }
  }

  @Test def testStreamingBehavior1() {
    val count = 100
    val is = infosetUnlimitedSource(count)
    val Start(bar_s: DIComplex) = is.next
    val Start(foo_arr_s: DIArray) = is.next
    1 to count foreach { i =>
      val Start(foo_1_s: DISimple) = is.next
      val End(foo_1_e: DISimple) = is.next
      assertTrue(foo_1_s eq foo_1_e)
      assertTrue(foo_1_s.dataValue.isInstanceOf[String])
      assertEquals("Hello", foo_1_s.dataValueAsString)
    }
    val End(foo_arr_e: DIArray) = is.next
    val End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
  }

  // @Test // uncomment to watch storage on jvisualvm to convince self of non-leaking.
  def testStreamingBehavior2() {
    val count = 100000000
    val is = infosetUnlimitedSource(count)
    val Start(bar_s: DIComplex) = is.next
    val Start(foo_arr_s: DIArray) = is.next
    1 to count foreach { i =>
      val Start(foo_1_s: DISimple) = is.next
      val End(foo_1_e: DISimple) = is.next
      assertTrue(foo_1_s eq foo_1_e)
      assertTrue(foo_1_s.dataValue.isInstanceOf[String])
      assertTrue(foo_1_s.dataValueAsString =:= "Hello")
      val arr = bar_s.getChildArray(foo_1_s.runtimeData)
      if (arr.length % 100L =#= 0L) {
        // println("array length is " + arr.length)
        foo_arr_s.reduceToSize(0)
      }
      arr.asInstanceOf[DIArray].children
    }
    val End(foo_arr_e: DIArray) = is.next
    val End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
  }
}
