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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.After;
import org.junit.Test;

import com.gemstone.gemfire.cache.ExpirationAction;

/**
 * The ExpirationActionTypeConverterTest class is a test suite of test cases testing the contract and functionality
 * of the ExpirationActionTypeConverter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see ExpirationActionConverter
 * @since 1.6.0
 */
public class ExpirationActionConverterTest {

	private final ExpirationActionConverter converter = new ExpirationActionConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(ExpirationAction.DESTROY, converter.convert("destroy"));
		assertEquals(ExpirationAction.INVALIDATE, converter.convert("inValidAte"));
		assertEquals(ExpirationAction.LOCAL_DESTROY, converter.convert("LOCAL_dEsTrOy"));
		assertEquals(ExpirationAction.LOCAL_INVALIDATE, converter.convert("Local_Invalidate"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertIllegalValue() {
		try {
			converter.convert("illegal_value");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(illegal_value) is not a valid ExpirationAction!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		converter.setAsText("InValidAte");
		assertEquals(ExpirationAction.INVALIDATE, converter.getValue());
		converter.setAsText("Local_Destroy");
		assertEquals(ExpirationAction.LOCAL_DESTROY, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextWithIllegalValue() {
		try {
			assertNull(converter.getValue());
			converter.setAsText("destruction");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(destruction) is not a valid ExpirationAction!", expected.getMessage());
			throw expected;
		}
		finally {
			assertNull(converter.getValue());
		}
	}

}
