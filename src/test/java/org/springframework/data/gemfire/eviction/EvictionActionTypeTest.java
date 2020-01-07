/*
 * Copyright 2010-2020 the original author or authors.
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

package org.springframework.data.gemfire.eviction;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import org.apache.geode.cache.EvictionAction;

import org.junit.Test;

/**
 * The EvictionActionTypeTest class is a test suite of test cases testing the contract and functionality
 * of the EvictionActionType enum.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see EvictionActionType
 * @see org.apache.geode.cache.EvictionAction
 * @since 1.6.0
 */
public class EvictionActionTypeTest {

	@Test
	public void testStaticGetEvictionAction() {
		assertEquals(EvictionAction.LOCAL_DESTROY, EvictionActionType.getEvictionAction(
			EvictionActionType.LOCAL_DESTROY));
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, EvictionActionType.getEvictionAction(
			EvictionActionType.OVERFLOW_TO_DISK));
	}

	@Test
	public void testStaticGetEvictionActionWithNull() {
		assertNull(EvictionActionType.getEvictionAction(null));
	}

	@Test
	public void testGetEvictionAction() {
		assertEquals(EvictionAction.LOCAL_DESTROY, EvictionActionType.LOCAL_DESTROY.getEvictionAction());
		assertEquals(EvictionAction.NONE, EvictionActionType.NONE.getEvictionAction());
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, EvictionActionType.OVERFLOW_TO_DISK.getEvictionAction());
		assertEquals(EvictionAction.DEFAULT_EVICTION_ACTION, EvictionActionType.DEFAULT.getEvictionAction());
	}

	@Test
	public void testDefault() {
		assertEquals(EvictionAction.DEFAULT_EVICTION_ACTION, EvictionActionType.DEFAULT.getEvictionAction());
		assertSame(EvictionActionType.LOCAL_DESTROY, EvictionActionType.DEFAULT);
	}

	@Test
	public void testValueOf() {
		assertEquals(EvictionActionType.LOCAL_DESTROY, EvictionActionType.valueOf(EvictionAction.LOCAL_DESTROY));
		assertEquals(EvictionActionType.NONE, EvictionActionType.valueOf(EvictionAction.NONE));
		assertEquals(EvictionActionType.OVERFLOW_TO_DISK, EvictionActionType.valueOf(EvictionAction.OVERFLOW_TO_DISK));
	}

	@Test
	public void testValueOfWithNull() {
		assertNull(EvictionActionType.valueOf((EvictionAction) null));
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(EvictionActionType.LOCAL_DESTROY, EvictionActionType.valueOfIgnoreCase("Local_Destroy"));
		assertEquals(EvictionActionType.NONE, EvictionActionType.valueOfIgnoreCase("none"));
		assertEquals(EvictionActionType.NONE, EvictionActionType.valueOfIgnoreCase("NONE"));
		assertEquals(EvictionActionType.OVERFLOW_TO_DISK, EvictionActionType.valueOfIgnoreCase("OverFlow_TO_DiSk"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValues() {
		assertNull(EvictionActionType.valueOfIgnoreCase("REMOTE_DESTROY"));
		assertNull(EvictionActionType.valueOfIgnoreCase("All"));
		assertNull(EvictionActionType.valueOfIgnoreCase(" none  "));
		assertNull(EvictionActionType.valueOfIgnoreCase("underflow_from_disk"));
		assertNull(EvictionActionType.valueOfIgnoreCase("  "));
		assertNull(EvictionActionType.valueOfIgnoreCase(""));
		assertNull(EvictionActionType.valueOfIgnoreCase(null));
	}

}
