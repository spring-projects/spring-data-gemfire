/*
 * Copyright 2010-2019 the original author or authors.
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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import org.apache.geode.cache.Scope;
import org.junit.Test;

/**
 * The ScopeTypeTest class is a test suite of test cases testing the contract and functionality of the ScopeType enum.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.ScopeType
 * @see org.apache.geode.cache.Scope
 * @since 1.6.0
 */
public class ScopeTypeTest {

	@Test
	public void testStaticGetScope() {
		assertEquals(Scope.GLOBAL, ScopeType.getScope(ScopeType.GLOBAL));
		assertEquals(Scope.LOCAL, ScopeType.getScope(ScopeType.LOCAL));
	}

	@Test
	public void testStaticGetScopeWithNull() {
		assertNull(ScopeType.getScope(null));
	}

	@Test
	public void testGetScope() {
		assertEquals(Scope.DISTRIBUTED_ACK, ScopeType.DISTRIBUTED_ACK.getScope());
		assertEquals(Scope.DISTRIBUTED_NO_ACK, ScopeType.DISTRIBUTED_NO_ACK.getScope());
		assertEquals(Scope.LOCAL, ScopeType.LOCAL.getScope());
		assertEquals(Scope.GLOBAL, ScopeType.GLOBAL.getScope());
	}

	@Test
	public void testValueOf() {
		try {
			for (int ordinal = 0; ordinal < Integer.MAX_VALUE; ordinal++) {
				Scope expectedScope = Scope.fromOrdinal(ordinal);
				ScopeType scopeType = ScopeType.valueOf(expectedScope);

				assertNotNull(scopeType);
				assertSame(expectedScope, scopeType.getScope());
			}
		}
		catch (ArrayIndexOutOfBoundsException ignore) {
		}
	}

	@Test
	public void testValueOfWithNull() {
		assertNull(ScopeType.valueOf((Scope) null));
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(ScopeType.DISTRIBUTED_ACK, ScopeType.valueOfIgnoreCase("distributed_ack"));
		assertEquals(ScopeType.DISTRIBUTED_NO_ACK, ScopeType.valueOfIgnoreCase("Distributed_No_Ack"));
		assertEquals(ScopeType.LOCAL, ScopeType.valueOfIgnoreCase("LOCal "));
		assertEquals(ScopeType.GLOBAL, ScopeType.valueOfIgnoreCase("GLOBAL"));
		assertEquals(ScopeType.GLOBAL, ScopeType.valueOfIgnoreCase(" global  "));
		assertEquals(ScopeType.DISTRIBUTED_ACK, ScopeType.valueOfIgnoreCase("DisTRIBUTEd-acK"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValues() {
		assertNull(ScopeType.valueOfIgnoreCase(" distributed ack "));
		assertNull(ScopeType.valueOfIgnoreCase("D!str!but3d_N0_@ck"));
		assertNull(ScopeType.valueOfIgnoreCase("DISTRIBUTED_CANT_ACK"));
		assertNull(ScopeType.valueOfIgnoreCase("Dist_ACK"));
		assertNull(ScopeType.valueOfIgnoreCase("NOT-Distributed-ACK"));
		assertNull(ScopeType.valueOfIgnoreCase("LOCALE"));
		assertNull(ScopeType.valueOfIgnoreCase("GLO-BAL"));
		assertNull(ScopeType.valueOfIgnoreCase("  "));
		assertNull(ScopeType.valueOfIgnoreCase(""));
		assertNull(ScopeType.valueOfIgnoreCase(null));
	}

}
