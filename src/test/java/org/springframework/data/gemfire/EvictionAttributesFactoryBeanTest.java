/*
 * Copyright 2010-2019 the original author or authors.
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
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAlgorithm;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.util.ObjectSizer;

/**
 * The EvictionAttributesFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the EvictionAttributesFactoryBean class used to create Region Eviction configuration settings.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.EvictionAttributesFactoryBean
 * @see com.gemstone.gemfire.cache.EvictionAttributes
 * @since 1.3.4
 */
public class EvictionAttributesFactoryBeanTest {

	private EvictionAttributesFactoryBean factoryBean;

	private ObjectSizer mockObjectSizer;

	@Before
	public void setup() {
		factoryBean = new EvictionAttributesFactoryBean();
		mockObjectSizer = mock(ObjectSizer.class, "MockObjectSizer");
	}

	@After
	public void tearDown() {
		factoryBean = null;
		mockObjectSizer = null;
	}

	@Test
	public void testIsSingleton() {
		assertTrue(new EvictionAttributesFactoryBean().isSingleton());
	}

	@Test
	public void testCreateEntryCountEvictionAttributesWithNullAction() {
		factoryBean.setAction(null);
		factoryBean.setObjectSizer(mockObjectSizer);
		factoryBean.setThreshold(1024);
		factoryBean.setType(EvictionPolicyType.ENTRY_COUNT);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.DEFAULT_EVICTION_ACTION, evictionAttributes.getAction());
		assertNull(evictionAttributes.getObjectSizer());
		assertEquals(1024, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, evictionAttributes.getAlgorithm());
	}

	@Test
	public void testCreateEntryCountEvictionAttributesWithLocalDestroy() {
		factoryBean.setAction(EvictionAction.LOCAL_DESTROY);
		factoryBean.setObjectSizer(mockObjectSizer);
		factoryBean.setThreshold(128);
		factoryBean.setType(EvictionPolicyType.ENTRY_COUNT);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.LOCAL_DESTROY, evictionAttributes.getAction());
		assertNull(evictionAttributes.getObjectSizer());
		assertEquals(128, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, evictionAttributes.getAlgorithm());
	}

	@Test
	public void testCreateEntryCountEvictionAttributesWithNone() {
		factoryBean.setAction(EvictionAction.NONE);
		factoryBean.setObjectSizer(mockObjectSizer);
		factoryBean.setThreshold(null);
		factoryBean.setType(EvictionPolicyType.ENTRY_COUNT);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.NONE, evictionAttributes.getAction());
		assertNull(evictionAttributes.getObjectSizer());
		assertEquals(EvictionAttributesFactoryBean.DEFAULT_LRU_MAXIMUM_ENTRIES, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, evictionAttributes.getAlgorithm());
	}

	@Test
	public void testCreateEntryCountEvictionAttributesWithOverflowToDisk() {
		factoryBean.setAction(EvictionAction.OVERFLOW_TO_DISK);
		factoryBean.setObjectSizer(mockObjectSizer);
		factoryBean.setThreshold(null);
		factoryBean.setType(EvictionPolicyType.ENTRY_COUNT);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, evictionAttributes.getAction());
		assertNull(evictionAttributes.getObjectSizer());
		assertEquals(EvictionAttributesFactoryBean.DEFAULT_LRU_MAXIMUM_ENTRIES, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, evictionAttributes.getAlgorithm());
	}

	@Test
	public void testCreateHeapPercentageEvictionAttributesWithNullAction() {
		factoryBean.setAction(null);
		factoryBean.setObjectSizer(mockObjectSizer);
		factoryBean.setType(EvictionPolicyType.HEAP_PERCENTAGE);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.DEFAULT_EVICTION_ACTION, evictionAttributes.getAction());
		assertSame(mockObjectSizer, evictionAttributes.getObjectSizer());
		assertEquals(EvictionAlgorithm.LRU_HEAP, evictionAttributes.getAlgorithm());
	}

	@Test
	public void testCreateHeapPercentageEvictionAttributesWithLocalDestroy() {
		factoryBean.setAction(EvictionAction.LOCAL_DESTROY);
		factoryBean.setObjectSizer(null);
		factoryBean.setThreshold(null);
		factoryBean.setType(EvictionPolicyType.HEAP_PERCENTAGE);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.LOCAL_DESTROY, evictionAttributes.getAction());
		assertNull(evictionAttributes.getObjectSizer());
		assertEquals(EvictionAlgorithm.LRU_HEAP, evictionAttributes.getAlgorithm());
	}

	@Test
	public void testCreateHeapPercentageEvictionAttributesWithNone() {
		factoryBean.setAction(EvictionAction.NONE);
		factoryBean.setObjectSizer(mockObjectSizer);
		factoryBean.setThreshold(null);
		factoryBean.setType(EvictionPolicyType.HEAP_PERCENTAGE);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.NONE, evictionAttributes.getAction());
		assertSame(mockObjectSizer, evictionAttributes.getObjectSizer());
		assertEquals(EvictionAlgorithm.LRU_HEAP, evictionAttributes.getAlgorithm());
	}

	@Test
	public void testCreateHeapPercentageEvictionAttributesWithOverflowToDisk() {
		factoryBean.setAction(EvictionAction.OVERFLOW_TO_DISK);
		factoryBean.setObjectSizer(mockObjectSizer);
		factoryBean.setThreshold(null);
		factoryBean.setType(EvictionPolicyType.HEAP_PERCENTAGE);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, evictionAttributes.getAction());
		assertSame(mockObjectSizer, evictionAttributes.getObjectSizer());
		assertEquals(EvictionAlgorithm.LRU_HEAP, evictionAttributes.getAlgorithm());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCreateHeapPercentageEvictionAttributesSettingThreshold() {
		EvictionAttributesFactoryBean factoryBean = new EvictionAttributesFactoryBean();

		try {
			factoryBean.setType(EvictionPolicyType.HEAP_PERCENTAGE);
			factoryBean.setThreshold(85);
			factoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("HEAP_PERCENTAGE (LRU_HEAP algorithm) does not support threshold (a.k.a. maximum)!",
				expected.getMessage());
			assertEquals(85, factoryBean.getThreshold().intValue());
			assertEquals(EvictionPolicyType.HEAP_PERCENTAGE, factoryBean.getType());
			throw expected;
		}
	}

	@Test
	public void testCreateMemorySizeEvictionAttributesWithNullAction() {
		factoryBean.setAction(null);
		factoryBean.setObjectSizer(mockObjectSizer);
		factoryBean.setThreshold(null);
		factoryBean.setType(EvictionPolicyType.MEMORY_SIZE);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.DEFAULT_EVICTION_ACTION, evictionAttributes.getAction());
		assertSame(mockObjectSizer, evictionAttributes.getObjectSizer());
		assertEquals(EvictionAttributesFactoryBean.DEFAULT_MEMORY_MAXIMUM_SIZE, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, evictionAttributes.getAlgorithm());
	}

	@Test
	public void testCreateMemorySizeEvictionAttributesWithLocalDestroy() {
		factoryBean.setAction(EvictionAction.LOCAL_DESTROY);
		factoryBean.setObjectSizer(mockObjectSizer);
		factoryBean.setThreshold(1024);
		factoryBean.setType(EvictionPolicyType.MEMORY_SIZE);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.LOCAL_DESTROY, evictionAttributes.getAction());
		assertSame(mockObjectSizer, evictionAttributes.getObjectSizer());
		assertEquals(1024, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, evictionAttributes.getAlgorithm());
	}

	@Test
	public void testCreateMemorySizeEvictionAttributesWithNone() {
		factoryBean.setAction(EvictionAction.NONE);
		factoryBean.setObjectSizer(null);
		factoryBean.setThreshold(256);
		factoryBean.setType(EvictionPolicyType.MEMORY_SIZE);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.NONE, evictionAttributes.getAction());
		assertNull(evictionAttributes.getObjectSizer());
		assertEquals(256, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, evictionAttributes.getAlgorithm());
	}

	@Test
	public void testCreateMemorySizeEvictionAttributesWithOverflowToDisk() {
		factoryBean.setAction(EvictionAction.OVERFLOW_TO_DISK);
		factoryBean.setObjectSizer(null);
		factoryBean.setThreshold(null);
		factoryBean.setType(EvictionPolicyType.MEMORY_SIZE);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, evictionAttributes.getAction());
		assertNull(evictionAttributes.getObjectSizer());
		assertEquals(EvictionAttributesFactoryBean.DEFAULT_MEMORY_MAXIMUM_SIZE, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, evictionAttributes.getAlgorithm());
	}

}
