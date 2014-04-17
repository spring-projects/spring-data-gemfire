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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;

import org.junit.Test;

import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAlgorithm;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.util.ObjectSizer;
import com.gemstone.gemfire.internal.cache.lru.LRUCapacityController;
import com.gemstone.gemfire.internal.cache.lru.MemLRUCapacityController;

/**
 * The EvictionAttributesFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the EvictionAttributesFactoryBean class used to create Region Eviction configuration settings.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.config.EvictionAttributesFactoryBean
 * @see com.gemstone.gemfire.cache.EvictionAttributes
 * @since 1.3.4
 */
public class EvictionAttributesFactoryBeanTest {

	@Test
	public void testCreateEntryCountHeapAttributes() {
		EvictionAttributesFactoryBean factoryBean = new EvictionAttributesFactoryBean();

		factoryBean.setAction(EvictionAction.NONE);
		factoryBean.setObjectSizer(null);
		factoryBean.setThreshold(8192);
		factoryBean.setType(EvictionType.ENTRY_COUNT);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.NONE, evictionAttributes.getAction());
		assertNull(evictionAttributes.getObjectSizer());
		assertEquals(8192, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, evictionAttributes.getAlgorithm());

		ObjectSizer mockObjectSizer = mock(ObjectSizer.class, "testCreateEntryCountHeapAttributes");

		factoryBean.setAction(null);
		factoryBean.setObjectSizer(mockObjectSizer); // ObjectSize is not used for ENTRY LRU!
		factoryBean.setThreshold(null);
		factoryBean.afterPropertiesSet();

		evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.DEFAULT_EVICTION_ACTION, evictionAttributes.getAction());
		assertNull(evictionAttributes.getObjectSizer());
		assertEquals(LRUCapacityController.DEFAULT_MAXIMUM_ENTRIES, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, evictionAttributes.getAlgorithm());
	}

	@Test
	public void testCreateHeapPercentageEvictionAttributes() {
		EvictionAttributesFactoryBean factoryBean = new EvictionAttributesFactoryBean();

		factoryBean.setAction(EvictionAction.LOCAL_DESTROY);
		factoryBean.setObjectSizer(null);
		//factoryBean.setThreshold(50); // Threshold is not used in HEAP LRU
		factoryBean.setType(EvictionType.HEAP_PERCENTAGE);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.LOCAL_DESTROY, evictionAttributes.getAction());
		assertNull(evictionAttributes.getObjectSizer());
		assertEquals(EvictionAlgorithm.LRU_HEAP, evictionAttributes.getAlgorithm());

		ObjectSizer mockObjectSizer = mock(ObjectSizer.class, "testCreateHeapPercentageEvictionAttributes");

		factoryBean.setAction(null);
		factoryBean.setObjectSizer(mockObjectSizer);
		factoryBean.setThreshold(null);
		factoryBean.afterPropertiesSet();

		evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.DEFAULT_EVICTION_ACTION, evictionAttributes.getAction());
		assertSame(mockObjectSizer, evictionAttributes.getObjectSizer());
		assertEquals(EvictionAlgorithm.LRU_HEAP, evictionAttributes.getAlgorithm());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCreateHeapPercentageEvictionAttributesSettingThreshold() {
		EvictionAttributesFactoryBean factoryBean = new EvictionAttributesFactoryBean();

		try {
			factoryBean.setType(EvictionType.HEAP_PERCENTAGE);
			factoryBean.setThreshold(85);
			factoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The HEAP_PERCENTAGE (LRU_HEAP algorithm) does not support threshold (a.k.a. maximum)!",
				expected.getMessage());
			assertEquals(85, factoryBean.getThreshold().intValue());
			assertEquals(EvictionType.HEAP_PERCENTAGE, factoryBean.getType());
			throw expected;
		}
	}

	@Test
	public void testCreateMemorySizeEvictionAttributes() {
		EvictionAttributesFactoryBean factoryBean = new EvictionAttributesFactoryBean();

		factoryBean.setAction(EvictionAction.OVERFLOW_TO_DISK);
		factoryBean.setObjectSizer(null);
		factoryBean.setThreshold(512);
		factoryBean.setType(EvictionType.MEMORY_SIZE);
		factoryBean.afterPropertiesSet();

		EvictionAttributes evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, evictionAttributes.getAction());
		assertNull(evictionAttributes.getObjectSizer());
		assertEquals(512, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, evictionAttributes.getAlgorithm());

		ObjectSizer mockObjectSizer = mock(ObjectSizer.class, "testCreateMemorySizeEvictionAttributes");

		factoryBean.setAction(null);
		factoryBean.setObjectSizer(mockObjectSizer);
		factoryBean.setThreshold(null);
		factoryBean.afterPropertiesSet();

		evictionAttributes = factoryBean.getObject();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.DEFAULT_EVICTION_ACTION, evictionAttributes.getAction());
		assertSame(mockObjectSizer, evictionAttributes.getObjectSizer());
		assertEquals(MemLRUCapacityController.DEFAULT_MAXIMUM_MEGABYTES, evictionAttributes.getMaximum());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, evictionAttributes.getAlgorithm());
	}

}
