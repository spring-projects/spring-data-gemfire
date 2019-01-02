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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import javax.annotation.Resource;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.EvictionAction;
import org.apache.geode.cache.EvictionAlgorithm;
import org.apache.geode.cache.Region;
import org.apache.geode.internal.cache.lru.LRUCapacityController;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The RegionEvictionAttributesNamespaceTest class is a test suite of test cases testing the use of
 * Eviction configuration settings (EvictionAttributes) in the SDG XML namespace.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.3.4
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class RegionEvictionAttributesNamespaceTest {

	@Resource(name = "One")
	private Region one;

	@Resource(name = "Two")
	private Region two;

	@Resource(name = "Three")
	private Region three;

	@Resource(name = "Four")
	private Region four;

	@Resource(name = "Five")
	private Region five;

	@Resource(name = "Six")
	private Region six;

	@Test
	public void testEntryCountRegionEvictionAttributes() {
		assertNotNull(one);
		assertNotNull(one.getAttributes());
		assertEquals(DataPolicy.REPLICATE, one.getAttributes().getDataPolicy());
		assertNotNull(one.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, one.getAttributes().getEvictionAttributes().getAction());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, one.getAttributes().getEvictionAttributes().getAlgorithm());
		assertEquals(4096, one.getAttributes().getEvictionAttributes().getMaximum());

		assertNotNull(two);
		assertNotNull(two.getAttributes());
		assertEquals(DataPolicy.PARTITION, two.getAttributes().getDataPolicy());
		assertNotNull(two.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.LOCAL_DESTROY, two.getAttributes().getEvictionAttributes().getAction());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, two.getAttributes().getEvictionAttributes().getAlgorithm());

		assertEquals(LRUCapacityController.DEFAULT_MAXIMUM_ENTRIES,
			two.getAttributes().getEvictionAttributes().getMaximum());
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testHeapPercentageRegionEvictionAttributes() {
		assertNotNull(three);
		assertNotNull(three.getAttributes());
		assertEquals(DataPolicy.REPLICATE, three.getAttributes().getDataPolicy());
		assertNotNull(three.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, three.getAttributes().getEvictionAttributes().getAction());
		assertEquals(EvictionAlgorithm.LRU_HEAP, three.getAttributes().getEvictionAttributes().getAlgorithm());

		assertNotNull(four);
		assertNotNull(four.getAttributes());
		assertEquals(DataPolicy.PARTITION, four.getAttributes().getDataPolicy());
		assertNotNull(four.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, four.getAttributes().getEvictionAttributes().getAction());
		assertEquals(EvictionAlgorithm.LRU_HEAP, three.getAttributes().getEvictionAttributes().getAlgorithm());

		try {
			four.getAttributes().getEvictionAttributes().getMaximum();
		}
		catch (UnsupportedOperationException expected) {
			assertEquals("LRUHeap does not support a maximum", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testMemorySizeRegionEvictionAttributes() {
		assertNotNull(five);
		assertNotNull(five.getAttributes());
		assertEquals(DataPolicy.REPLICATE, five.getAttributes().getDataPolicy());
		assertNotNull(five.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, five.getAttributes().getEvictionAttributes().getAction());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, five.getAttributes().getEvictionAttributes().getAlgorithm());
		assertEquals(128, five.getAttributes().getEvictionAttributes().getMaximum());

		assertNotNull(six);
		assertNotNull(six.getAttributes());
		assertEquals(DataPolicy.PARTITION, six.getAttributes().getDataPolicy());
		assertNotNull(six.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, six.getAttributes().getEvictionAttributes().getAction());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, six.getAttributes().getEvictionAttributes().getAlgorithm());

		int expectedMaximum = (Boolean.getBoolean("org.springframework.data.gemfire.test.GemfireTestRunner.nomock")
			? 512 : 256);

		assertEquals(expectedMaximum, six.getAttributes().getEvictionAttributes().getMaximum());
	}

}
