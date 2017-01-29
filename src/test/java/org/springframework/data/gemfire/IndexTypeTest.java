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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * The IndexTypeTest class is a test suite of test cases testing the contract and functionality
 * of the IndexType enum class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.IndexType
 * @since 1.5.2
 */
@SuppressWarnings("deprecation")
public class IndexTypeTest {

	@Test
	public void testGetGemfireIndexType() {
		assertEquals(org.apache.geode.cache.query.IndexType.FUNCTIONAL, IndexType.FUNCTIONAL.getGemfireIndexType());
		assertEquals(org.apache.geode.cache.query.IndexType.HASH, IndexType.HASH.getGemfireIndexType());
		assertEquals(org.apache.geode.cache.query.IndexType.PRIMARY_KEY, IndexType.KEY.getGemfireIndexType());
		assertEquals(org.apache.geode.cache.query.IndexType.PRIMARY_KEY, IndexType.PRIMARY_KEY.getGemfireIndexType());
	}

	@Test
	public void testValueOf() {
		assertEquals(IndexType.FUNCTIONAL, IndexType.valueOf(org.apache.geode.cache.query.IndexType.FUNCTIONAL));
		assertEquals(IndexType.HASH, IndexType.valueOf(org.apache.geode.cache.query.IndexType.HASH));
		assertEquals(IndexType.PRIMARY_KEY, IndexType.valueOf(org.apache.geode.cache.query.IndexType.PRIMARY_KEY));
	}

	@Test
	public void testValueOfWithNull() {
		assertNull(IndexType.valueOf((org.apache.geode.cache.query.IndexType) null));
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(IndexType.FUNCTIONAL, IndexType.valueOfIgnoreCase("functional"));
		assertEquals(IndexType.HASH, IndexType.valueOfIgnoreCase("HasH"));
		assertEquals(IndexType.KEY, IndexType.valueOfIgnoreCase("Key"));
		assertEquals(IndexType.PRIMARY_KEY, IndexType.valueOfIgnoreCase("PriMary_Key"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValues() {
		assertNull(IndexType.valueOfIgnoreCase("Prime_Index"));
		assertNull(IndexType.valueOfIgnoreCase("SECONDARY_INDEX"));
		assertNull(IndexType.valueOfIgnoreCase("unique_index"));
		assertNull(IndexType.valueOfIgnoreCase(null));
		assertNull(IndexType.valueOfIgnoreCase("  "));
		assertNull(IndexType.valueOfIgnoreCase(""));
	}

	@Test
	public void testIsFunctional() {
		assertTrue(IndexType.FUNCTIONAL.isFunctional());
		assertFalse(IndexType.HASH.isFunctional());
		assertFalse(IndexType.KEY.isFunctional());
		assertFalse(IndexType.PRIMARY_KEY.isFunctional());
	}

	@Test
	public void testIsNullSafeFunctional() {
		assertFalse(IndexType.isFunctional(null));
		assertTrue(IndexType.isFunctional(IndexType.FUNCTIONAL));
		assertFalse(IndexType.isFunctional(IndexType.HASH));
	}

	@Test
	public void testIsHash() {
		assertFalse(IndexType.FUNCTIONAL.isHash());
		assertTrue(IndexType.HASH.isHash());
		assertFalse(IndexType.KEY.isHash());
		assertFalse(IndexType.PRIMARY_KEY.isHash());
	}

	@Test
	public void testIsNullSafeHash() {
		assertFalse(IndexType.isHash(null));
		assertTrue(IndexType.isHash(IndexType.HASH));
		assertFalse(IndexType.isHash(IndexType.KEY));
	}

	@Test
	public void testIsKey() {
		assertFalse(IndexType.FUNCTIONAL.isKey());
		assertFalse(IndexType.HASH.isKey());
		assertTrue(IndexType.KEY.isKey());
		assertTrue(IndexType.PRIMARY_KEY.isKey());
	}

	@Test
	public void testIsNullSafeKey() {
		assertFalse(IndexType.isKey(null));
		assertFalse(IndexType.isKey(IndexType.FUNCTIONAL));
		assertTrue(IndexType.isKey(IndexType.KEY));
		assertTrue(IndexType.isKey(IndexType.PRIMARY_KEY));
	}

}
