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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import javax.annotation.Resource;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.EvictionAction;
import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The RegionsWithDiskStoreAndPersistenceEvictionSettingsTest class is a test suite testing the functionality
 * of GemFire Cache Regions when persistent/non-persistent with and without Eviction settings when specifying a
 * Disk Store.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.test.context.ContextConfiguration
 * @since 1.4.0.RC1
 */
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@RunWith(SpringJUnit4ClassRunner.class)
public class RegionsWithDiskStoreAndPersistenceEvictionSettingsTest {

	@Resource(name = "NotPersistentNoOverflowRegion")
	private Region notPersistentNoOverflowRegion;

	@Resource(name = "NotPersistentOverflowRegion")
	private Region notPersistentOverflowRegion;

	@Resource(name = "PersistentNoOverflowRegion")
	private Region persistentNoOverflowRegion;

	@Resource(name = "PersistentOverflowRegion")
	private Region persistentOverflowRegion;

	@Test
	public void testNotPersistentNoOverflowRegion() {
		assertNotNull("The Not Persistent, No Overflow Region was not properly configured and initialized!",
			notPersistentNoOverflowRegion);

		assertNotNull(notPersistentNoOverflowRegion.getAttributes());
		assertEquals(DataPolicy.PARTITION, notPersistentNoOverflowRegion.getAttributes().getDataPolicy());
		assertNotNull(notPersistentNoOverflowRegion.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.NONE, notPersistentNoOverflowRegion.getAttributes()
			.getEvictionAttributes().getAction());
		assertNull(notPersistentNoOverflowRegion.getAttributes().getDiskStoreName());
	}

	@Test
	public void testNotPersistentOverflowRegion() {
		assertNotNull("The Not Persistent, Overflow Region was not properly configured and initialized!",
			notPersistentOverflowRegion);

		assertNotNull(notPersistentOverflowRegion.getAttributes());
		assertEquals(DataPolicy.PARTITION, notPersistentOverflowRegion.getAttributes().getDataPolicy());
		assertNotNull(notPersistentOverflowRegion.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, notPersistentOverflowRegion.getAttributes()
			.getEvictionAttributes().getAction());
		assertEquals("DiskStoreOne", notPersistentOverflowRegion.getAttributes().getDiskStoreName());
	}

	@Test
	public void testPersistentNoOverflowRegion() {
		assertNotNull("The Persistent, No Overflow Region was not properly configured and initialized!",
			persistentNoOverflowRegion);

		assertNotNull(persistentNoOverflowRegion.getAttributes());
		assertEquals(DataPolicy.PERSISTENT_PARTITION, persistentNoOverflowRegion.getAttributes().getDataPolicy());
		assertNotNull(persistentNoOverflowRegion.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.LOCAL_DESTROY, persistentNoOverflowRegion.getAttributes()
			.getEvictionAttributes().getAction());
		assertEquals("DiskStoreOne", persistentNoOverflowRegion.getAttributes().getDiskStoreName());
	}

	@Test
	public void testPersistentOverflowRegion() {
		assertNotNull("The Persistent, Overflow Region was not properly configured and initialized!",
			persistentOverflowRegion);

		assertNotNull(persistentOverflowRegion.getAttributes());
		assertEquals(DataPolicy.PERSISTENT_PARTITION, persistentOverflowRegion.getAttributes().getDataPolicy());
		assertNotNull(persistentOverflowRegion.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, persistentOverflowRegion.getAttributes()
			.getEvictionAttributes().getAction());
		assertEquals("DiskStoreOne", persistentOverflowRegion.getAttributes().getDiskStoreName());
	}

}
