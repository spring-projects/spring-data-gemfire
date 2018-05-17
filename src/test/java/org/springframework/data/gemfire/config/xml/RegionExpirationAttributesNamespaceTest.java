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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;

import javax.annotation.Resource;

import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.Assert;

/**
 * The RegionExpirationAttributesNamespaceTest class is a test suite of test cases testing the configuration of
 * ExpirationAttribute settings on Region Entries.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionAttributes
 * @see org.apache.geode.cache.EvictionAttributes
 * @since 1.5.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class RegionExpirationAttributesNamespaceTest {

	@Resource(name = "ReplicateExample")
	private Region<?, ?> replicateExample;

	@Resource(name = "PreloadedExample")
	private Region<?, ?> preloadedExample;

	@Resource(name = "PartitionExample")
	private Region<?, ?> partitionExample;

	@Resource(name = "LocalExample")
	private Region<?, ?> localExample;

	private void assertRegionMetaData(final Region<?, ?> region, final String regionName, final DataPolicy dataPolicy) {
		assertRegionMetaData(region, regionName, Region.SEPARATOR + regionName, dataPolicy);
	}

	private void assertRegionMetaData(Region<?, ?> region, String regionName, String regionFullPath,
			DataPolicy dataPolicy) {

		assertNotNull(String.format("The '%1$s' Region was not properly configured and initialized!", regionName), region);
		assertEquals(regionName, region.getName());
		assertEquals(regionFullPath, region.getFullPath());
		assertNotNull(region.getAttributes());
		assertEquals(dataPolicy, region.getAttributes().getDataPolicy());
	}

	private void assertNoExpiration(final ExpirationAttributes expirationAttributes) {

		if (expirationAttributes != null) {
			//assertEquals(ExpirationAction.INVALIDATE, expirationAttributes.getAction());
			assertEquals(0, expirationAttributes.getTimeout());
		}
	}

	private void assertExpirationAttributes(ExpirationAttributes expirationAttributes,
			final int timeout, final ExpirationAction action) {
		assertNotNull(expirationAttributes);
		assertEquals(timeout, expirationAttributes.getTimeout());
		assertEquals(action, expirationAttributes.getAction());
	}

	@SuppressWarnings("unchecked")
	private void assertCustomExpiry(CustomExpiry<?, ?> customExpiry, String name, int timeout,
			ExpirationAction action) {

		assertNotNull(customExpiry);
		assertEquals(name, customExpiry.toString());
		assertExpirationAttributes(customExpiry.getExpiry(mock(Region.Entry.class)), timeout, action);
	}

	@Test
	public void testReplicateExampleExpirationAttributes() {

		assertRegionMetaData(replicateExample, "ReplicateExample", DataPolicy.REPLICATE);
		assertExpirationAttributes(replicateExample.getAttributes().getEntryTimeToLive(),
			600, ExpirationAction.DESTROY);
		assertExpirationAttributes(replicateExample.getAttributes().getEntryIdleTimeout(),
			300, ExpirationAction.INVALIDATE);
		assertNull(replicateExample.getAttributes().getCustomEntryTimeToLive());
		assertNull(replicateExample.getAttributes().getCustomEntryIdleTimeout());
	}

	@Test
	public void testPreloadedExampleExpirationAttributes() {

		assertRegionMetaData(preloadedExample, "PreloadedExample", DataPolicy.PRELOADED);
		assertExpirationAttributes(preloadedExample.getAttributes().getEntryTimeToLive(),
			120, ExpirationAction.LOCAL_DESTROY);
		assertNoExpiration(preloadedExample.getAttributes().getEntryIdleTimeout());
		assertNull(preloadedExample.getAttributes().getCustomEntryTimeToLive());
		assertNull(preloadedExample.getAttributes().getCustomEntryIdleTimeout());
	}

	@Test
	public void testPartitionExampleExpirationAttributes() {

		assertRegionMetaData(partitionExample, "PartitionExample", DataPolicy.PARTITION);
		assertExpirationAttributes(partitionExample.getAttributes().getEntryTimeToLive(),
			300, ExpirationAction.DESTROY);
		assertNoExpiration(partitionExample.getAttributes().getEntryIdleTimeout());
		assertNull(partitionExample.getAttributes().getCustomEntryTimeToLive());
		assertCustomExpiry(partitionExample.getAttributes().getCustomEntryIdleTimeout(), "PartitionCustomExpiry",
			120, ExpirationAction.INVALIDATE);
	}

	@Test
	public void testLocalExampleExpirationAttributes() {

		assertRegionMetaData(localExample, "LocalExample", DataPolicy.NORMAL);
		assertNoExpiration(localExample.getAttributes().getEntryTimeToLive());
		assertNoExpiration(localExample.getAttributes().getEntryIdleTimeout());
		assertCustomExpiry(localExample.getAttributes().getCustomEntryTimeToLive(), "LocalTtlCustomExpiry",
			180, ExpirationAction.LOCAL_DESTROY);
		assertCustomExpiry(localExample.getAttributes().getCustomEntryIdleTimeout(), "LocalTtiCustomExpiry",
			60, ExpirationAction.LOCAL_INVALIDATE);
	}

	public static class TestCustomExpiry<K, V> implements CustomExpiry<K, V> {

		private ExpirationAction action;

		private Integer timeout;

		private String name;

		@Override
		public ExpirationAttributes getExpiry(final Region.Entry<K, V> kvEntry) {
			Assert.state(timeout != null, "The expiration 'timeout' must be specified!");
			Assert.state(action != null, "The expiration 'action' must be specified!");
			return new ExpirationAttributes(timeout, action);
		}

		public void setAction(final ExpirationAction action) {
			this.action = action;
		}

		public void setName(final String name) {
			this.name = name;
		}

		public void setTimeout(final Integer timeout) {
			this.timeout = timeout;
		}

		@Override
		public void close() {
		}

		@Override
		public String toString() {
			return this.name;
		}
	}
}
