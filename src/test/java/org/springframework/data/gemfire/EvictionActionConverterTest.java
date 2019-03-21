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

import com.gemstone.gemfire.cache.EvictionAction;

/**
 * The EvictionActionTypeConverterTest class is a test suite of test cases testing the contract and functionality
 * of the EvictionActionTypeConverter.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see EvictionActionConverter
 * @since 1.6.0
 */
public class EvictionActionConverterTest {

	private EvictionActionConverter converter = new EvictionActionConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(EvictionAction.LOCAL_DESTROY, converter.convert("local_destroy"));
		assertEquals(EvictionAction.NONE, converter.convert("None"));
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, converter.convert("OverFlow_TO_dIsk"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertIllegalValue() {
		try {
			converter.convert("invalid_value");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(invalid_value) is not a valid EvictionAction!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		assertNull(converter.getValue());
		converter.setAsText("Local_Destroy");
		assertEquals(EvictionAction.LOCAL_DESTROY, converter.getValue());
		converter.setAsText("overflow_to_disk");
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextWithIllegalValue() {
		try {
			assertNull(converter.getValue());
			converter.setAsText("destroy");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(destroy) is not a valid EvictionAction!", expected.getMessage());
			throw expected;
		}
		finally {
			assertNull(converter.getValue());
		}
	}

}
