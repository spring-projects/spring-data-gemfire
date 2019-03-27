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
import static org.junit.Assert.assertSame;

import org.junit.Test;

import com.gemstone.gemfire.cache.util.GatewayHub;

/**
 * The StartupPolicyTypeTest class is a test suite of test cases testing the contract and functionality
 * of the StartupPolicyType enum.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see StartupPolicyType
 * @since 1.6.0
 */
@SuppressWarnings("deprecation")
public class StartupPolicyTypeTest {

	@Test
	public void testDefault() {
		assertEquals(GatewayHub.DEFAULT_STARTUP_POLICY, StartupPolicyType.DEFAULT.getName());
		assertSame(StartupPolicyType.NONE, StartupPolicyType.DEFAULT);
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(StartupPolicyType.NONE, StartupPolicyType.valueOfIgnoreCase("NONE"));
		assertEquals(StartupPolicyType.PRIMARY, StartupPolicyType.valueOfIgnoreCase("Primary"));
		assertEquals(StartupPolicyType.SECONDARY, StartupPolicyType.valueOfIgnoreCase("secondary"));
		assertEquals(StartupPolicyType.SECONDARY, StartupPolicyType.valueOfIgnoreCase("SECONDary"));
		assertEquals(StartupPolicyType.PRIMARY, StartupPolicyType.valueOfIgnoreCase("PriMarY"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValues() {
		assertNull(StartupPolicyType.valueOfIgnoreCase("NO"));
		assertNull(StartupPolicyType.valueOfIgnoreCase("Prime"));
		assertNull(StartupPolicyType.valueOfIgnoreCase("second"));
		assertNull(StartupPolicyType.valueOfIgnoreCase("all"));
		assertNull(StartupPolicyType.valueOfIgnoreCase("N0N3"));
	}

}
