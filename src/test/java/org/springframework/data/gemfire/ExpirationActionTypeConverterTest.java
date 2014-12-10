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

import org.junit.After;
import org.junit.Test;

/**
 * The ExpirationActionTypeConverterTest class is a test suite of test cases testing the contract and functionality
 * of the ExpirationActionTypeConverter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.ExpirationActionTypeConverter
 * @since 1.6.0
 */
public class ExpirationActionTypeConverterTest {

	private final ExpirationActionTypeConverter converter = new ExpirationActionTypeConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(ExpirationActionType.DESTROY, converter.convert("destroy"));
		assertEquals(ExpirationActionType.INVALIDATE, converter.convert("InvalidAte"));
		assertEquals(ExpirationActionType.LOCAL_DESTROY, converter.convert("LOCAL_dEsTrOy"));
		assertEquals(ExpirationActionType.LOCAL_INVALIDATE, converter.convert("Local_Invalidate"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertThrowsIllegalArgumentException() {
		try {
			converter.convert("blow_up");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Source (blow_up) is not a valid ExpirationActionType!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		converter.setAsText("InValidAte");
		assertEquals(ExpirationActionType.INVALIDATE, converter.getValue());
		converter.setAsText("Local_Destroy");
		assertEquals(ExpirationActionType.LOCAL_DESTROY, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextThrowsIllegalArgumentException() {
		try {
			converter.setAsText("destruction");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Source (destruction) is not a valid ExpirationActionType!", expected.getMessage());
			throw expected;
		}
	}

}
