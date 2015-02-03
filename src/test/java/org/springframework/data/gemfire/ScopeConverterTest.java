/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.After;
import org.junit.Test;

import com.gemstone.gemfire.cache.Scope;

/**
 * The ScopeConverterTest class is a test suite of test cases testing the contract and functionality
 * of the ScopeConverter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.ScopeConverter
 * @since 1.6.0
 */
public class ScopeConverterTest {

	private final ScopeConverter converter = new ScopeConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(Scope.DISTRIBUTED_ACK, converter.convert("distributed-ACK"));
		assertEquals(Scope.DISTRIBUTED_NO_ACK, converter.convert(" Distributed_NO-aCK"));
		assertEquals(Scope.LOCAL, converter.convert("loCAL  "));
		assertEquals(Scope.GLOBAL, converter.convert(" GLOBal  "));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertIllegalValue() {
		try {
			converter.convert("illegal-value");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(illegal-value) is not a valid Scope!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		assertNull(converter.getValue());
		converter.setAsText("DisTributeD-nO_Ack");
		assertEquals(Scope.DISTRIBUTED_NO_ACK, converter.getValue());
		converter.setAsText("distributed-ack");
		assertEquals(Scope.DISTRIBUTED_ACK, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextWithIllegalValue() {
		try {
			assertNull(converter.getValue());
			converter.setAsText("d!5tr!but3d-n0_@ck");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(d!5tr!but3d-n0_@ck) is not a valid Scope!", expected.getMessage());
			throw expected;
		}
		finally {
			assertNull(converter.getValue());
		}
	}

}
