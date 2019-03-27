/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.wan;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.After;
import org.junit.Test;

/**
 * The StartupPolicyConverterTest class is a test suite of test cases testing the contract and functionality
 * of the StartupPolicyConverter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see StartupPolicyType
 * @see StartupPolicyConverter
 * @since 1.6.0
 */
public class StartupPolicyConverterTest {

	private StartupPolicyConverter converter = new StartupPolicyConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(StartupPolicyType.NONE, converter.convert("none"));
		assertEquals(StartupPolicyType.PRIMARY, converter.convert("Primary"));
		assertEquals(StartupPolicyType.SECONDARY, converter.convert("SecONdARY"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertIllegalValue() {
		try {
			converter.convert("tertiary");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(tertiary) is not a valid StartupPolicyType!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		converter.setAsText("priMARY");
		assertEquals(StartupPolicyType.PRIMARY, converter.getValue());
		converter.setAsText("SecondAry");
		assertEquals(StartupPolicyType.SECONDARY, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextWithIllegalValue() {
		try {
			assertNull(converter.getValue());
			converter.setAsText("invalid");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(invalid) is not a valid StartupPolicyType!", expected.getMessage());
			throw expected;
		}
		finally {
			assertNull(converter.getValue());
		}
	}

}
