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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Test;

import com.gemstone.gemfire.cache.ExpirationAction;

/**
 * The ExpirationActionTypeTest class is a test suite of test cases testing the contract and functionality
 * of the ExpirationActionType enum.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.ExpirationActionType
 * @since 1.6.0
 */
public class ExpirationActionTypeTest {

	@Test
	public void testDefault() {
		assertEquals(ExpirationActionType.INVALIDATE, ExpirationActionType.DEFAULT);
		assertEquals(ExpirationAction.INVALIDATE, ExpirationActionType.DEFAULT.getExpirationAction());
	}

	@Test
	public void testValueOf() {
		assertEquals(ExpirationActionType.DESTROY, ExpirationActionType.valueOf(ExpirationAction.DESTROY));
		assertEquals(ExpirationActionType.INVALIDATE, ExpirationActionType.valueOf(ExpirationAction.INVALIDATE));
		assertEquals(ExpirationActionType.LOCAL_DESTROY, ExpirationActionType.valueOf(ExpirationAction.LOCAL_DESTROY));
		assertEquals(ExpirationActionType.LOCAL_INVALIDATE, ExpirationActionType.valueOf(ExpirationAction.LOCAL_INVALIDATE));

	}

	@Test
	public void testValueOfExpirationActionOrdinalValues() {
		try {
			for (int ordinal = 0; ordinal < Integer.MAX_VALUE; ordinal++) {
				assertNotNull(ExpirationActionType.valueOf(ExpirationAction.fromOrdinal(ordinal)));
			}
		}
		catch (ArrayIndexOutOfBoundsException ignore) {
		}
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(ExpirationActionType.DESTROY, ExpirationActionType.valueOfIgnoreCase("destroy"));
		assertEquals(ExpirationActionType.INVALIDATE, ExpirationActionType.valueOfIgnoreCase("Invalidate"));
		assertEquals(ExpirationActionType.LOCAL_DESTROY, ExpirationActionType.valueOfIgnoreCase("LOCAL_DESTROY"));
		assertEquals(ExpirationActionType.LOCAL_INVALIDATE, ExpirationActionType.valueOfIgnoreCase("Local_Invalidate"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidStringValues() {
		assertNull(ExpirationActionType.valueOfIgnoreCase("Invalid"));
		assertNull(ExpirationActionType.valueOfIgnoreCase("local destroy"));
	}

}
