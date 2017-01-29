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

import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for {@link DiskStoreFactoryBean}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.DiskStore
 * @see org.springframework.data.gemfire.DiskStoreFactoryBean
 * @since 1.3.4
 */
public class DiskStoreFactoryBeanTest {

	private final DiskStoreFactoryBean factoryBean = new DiskStoreFactoryBean();

	@Before
	public void setup() {
		factoryBean.setBeanName("testDiskStore");
	}

	@Test
	public void testValidateCompactionThresholdWhenNull() {
		factoryBean.validateCompactionThreshold(null);
	}

	@Test
	public void testValidateCompactionThresholdWhenValid() {
		factoryBean.validateCompactionThreshold(0);
		factoryBean.validateCompactionThreshold(50);
		factoryBean.validateCompactionThreshold(100);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testValidateCompactionThresholdWhenLessThan0() {
		try {
			factoryBean.validateCompactionThreshold(-1);
		}
		catch (IllegalArgumentException expected) {
			assertEquals(String.format("The DiskStore's (%1$s) compaction threshold (%2$d) must be an integer value between 0 and 100 inclusive.",
				factoryBean.resolveDiskStoreName(), -1), expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testValidateCompactionThresholdWhenGreaterThan100() {
		try {
			factoryBean.validateCompactionThreshold(101);
		}
		catch (IllegalArgumentException expected) {
			assertEquals(String.format("The DiskStore's (%1$s) compaction threshold (%2$d) must be an integer value between 0 and 100 inclusive.",
				factoryBean.resolveDiskStoreName(), 101), expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetCompactionThreshold() {
		factoryBean.setCompactionThreshold(75);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetCompactionThreadWithIllegalArgument() {
		try {
			factoryBean.setCompactionThreshold(200);
		}
		catch (IllegalArgumentException expected) {
			assertEquals(String.format(
				"The DiskStore's (%1$s) compaction threshold (%2$d) must be an integer value between 0 and 100 inclusive.",
				factoryBean.resolveDiskStoreName(), 200), expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalStateException.class)
	public void testAfterPropertiesSetWithNoCacheReference() throws Exception {
		try {
			factoryBean.afterPropertiesSet();
		}
		catch (IllegalStateException expected) {
			assertEquals("Cache is required to create DiskStore [testDiskStore]", expected.getMessage());
			throw expected;
		}
	}
}
