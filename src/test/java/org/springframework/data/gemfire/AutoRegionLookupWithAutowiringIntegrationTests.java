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
import static org.junit.Assert.assertNotNull;

import javax.annotation.Resource;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The AutoRegionLookupWithAutowiringIntegrationTests class is a test suite class testing the behavior of
 * Spring Data GemFire's auto Region lookup functionality when combined with Spring's component auto-wiring capabilities.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.5.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class AutoRegionLookupWithAutowiringIntegrationTests {

	@Autowired
	private TestComponent testComponent;

	protected static void assertRegionMetaData(final Region<?, ?> region,
			final String expectedName, final DataPolicy expectedDataPolicy) {
		assertRegionMetaData(region, expectedName, Region.SEPARATOR + expectedName, expectedDataPolicy);
	}

	protected static void assertRegionMetaData(final Region<?, ?> region, final String expectedName,
			final String expectedFullPath, final DataPolicy expectedDataPolicy) {
		assertNotNull(String.format("Region (%1$s) was not properly configured and initialized!", expectedName), region);
		assertEquals(expectedName, region.getName());
		assertEquals(expectedFullPath, region.getFullPath());
		assertNotNull(String.format("Region (%1$s) must have RegionAttributes defined!", expectedName),
			region.getAttributes());
		assertEquals(expectedDataPolicy, region.getAttributes().getDataPolicy());
		assertFalse(region.getAttributes().getDataPolicy().withPersistence());
	}

	@Test
	public void testAutowiredNativeRegions() {
		assertRegionMetaData(testComponent.nativePartitionedRegion, "NativePartitionedRegion", DataPolicy.PARTITION);
		assertRegionMetaData(testComponent.nativeReplicateParent, "NativeReplicateParent", DataPolicy.REPLICATE);
		assertRegionMetaData(testComponent.nativeReplicateChild, "NativeReplicateChild",
			"/NativeReplicateParent/NativeReplicateChild", DataPolicy.REPLICATE);
		assertRegionMetaData(testComponent.nativeReplicateGrandchild, "NativeReplicateGrandchild",
			"/NativeReplicateParent/NativeReplicateChild/NativeReplicateGrandchild", DataPolicy.REPLICATE);
	}

	@Component
	public static final class TestComponent {

		@Resource(name = "NativePartitionedRegion")
		Region<?, ?> nativePartitionedRegion;

		@Resource(name = "NativeReplicateParent")
		Region<?, ?> nativeReplicateParent;

		@Resource(name = "/NativeReplicateParent/NativeReplicateChild")
		Region<?, ?> nativeReplicateChild;

		@Resource(name = "/NativeReplicateParent/NativeReplicateChild/NativeReplicateGrandchild")
		Region<?, ?> nativeReplicateGrandchild;

	}

}
