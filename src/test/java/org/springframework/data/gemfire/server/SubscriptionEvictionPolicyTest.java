/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.apache.geode.cache.server.ClientSubscriptionConfig;
import org.junit.Test;

/**
 * The SubscriptionEvictionPolicyTest class is a test suite of test cases testing the contract and functionality
 * of the SubscriptionEvictionPolicy enum.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.server.SubscriptionEvictionPolicy
 * @since 1.6.0
 */
public class SubscriptionEvictionPolicyTest {

	@Test
	public void testDefault() {
		assertEquals(ClientSubscriptionConfig.DEFAULT_EVICTION_POLICY.toLowerCase(),
			SubscriptionEvictionPolicy.DEFAULT.name().toLowerCase());
		assertSame(SubscriptionEvictionPolicy.NONE, SubscriptionEvictionPolicy.DEFAULT);
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(SubscriptionEvictionPolicy.ENTRY, SubscriptionEvictionPolicy.valueOfIgnoreCase("entry"));
		assertEquals(SubscriptionEvictionPolicy.MEM, SubscriptionEvictionPolicy.valueOfIgnoreCase("Mem"));
		assertEquals(SubscriptionEvictionPolicy.NONE, SubscriptionEvictionPolicy.valueOfIgnoreCase("NOne"));
		assertEquals(SubscriptionEvictionPolicy.NONE, SubscriptionEvictionPolicy.valueOfIgnoreCase("nONE"));
		assertEquals(SubscriptionEvictionPolicy.NONE, SubscriptionEvictionPolicy.valueOfIgnoreCase("NONE"));
	}

	@Test
	public void testValueOfIgnoreCaseWithIllegalValue() {
		assertNull(SubscriptionEvictionPolicy.valueOfIgnoreCase("KEYS"));
		assertNull(SubscriptionEvictionPolicy.valueOfIgnoreCase("Memory"));
		assertNull(SubscriptionEvictionPolicy.valueOfIgnoreCase("all"));
		assertNull(SubscriptionEvictionPolicy.valueOfIgnoreCase("no"));
		assertNull(SubscriptionEvictionPolicy.valueOfIgnoreCase("one"));
		assertNull(SubscriptionEvictionPolicy.valueOfIgnoreCase("  "));
		assertNull(SubscriptionEvictionPolicy.valueOfIgnoreCase(""));
		assertNull(SubscriptionEvictionPolicy.valueOfIgnoreCase(null));
	}

	@Test
	public void testSetEvictionPolicy() {
		ClientSubscriptionConfig mockConfig = mock(ClientSubscriptionConfig.class,
			"testSetEvictionPolicy.ClientSubscriptionConfig");

		ClientSubscriptionConfig returnedConfig = SubscriptionEvictionPolicy.MEM.setEvictionPolicy(mockConfig);

		assertSame(mockConfig, returnedConfig);

		verify(mockConfig, times(1)).setEvictionPolicy(eq("mem"));
	}

	@Test
	public void testSetEvictionPolicyWithNull() {
		assertNull(SubscriptionEvictionPolicy.ENTRY.setEvictionPolicy(null));
	}

}
