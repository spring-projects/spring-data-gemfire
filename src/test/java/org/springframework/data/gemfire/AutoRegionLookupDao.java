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
import org.springframework.context.annotation.DependsOn;
import org.springframework.dao.support.DaoSupport;
import org.springframework.stereotype.Repository;

/**
 * The AutoRegionLookupDao class is a Data Access Object (DAO) encapsulating references to several GemFire Cache Regions
 * defined in native GemFire cache.xml and registered as beans in the Spring context using Spring Data GemFire's
 * auto Region lookup functionality.  This class is used in the AutoRegionLookupWithComponentScanningIntegrationTests
 * class to ensure this @Repository component is auto-wired properly.
 *
 * @author John Blum
 * @see org.springframework.dao.support.DaoSupport
 * @see org.springframework.stereotype.Repository
 * @see org.apache.geode.cache.Region
 * @since 1.5.0
 */
/*
*/
@DependsOn("gemfireCache")
//@Lazy
@Repository("autoRegionLookupDao")
@SuppressWarnings("unused")
public class AutoRegionLookupDao extends DaoSupport {

	@Resource(name = "NativePartitionedRegion")
	private Region<?, ?> nativePartitionedRegion;

	@Resource(name = "NativeReplicateParent")
	private Region<?, ?> nativeReplicateParent;

	@Resource(name = "/NativeReplicateParent/NativeReplicateChild")
	private Region<?, ?> nativeReplicateChild;

	@Resource(name = "/NativeReplicateParent/NativeReplicateChild/NativeReplicateGrandchild")
	private Region<?, ?> nativeReplicateGrandchild;

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

	@Override
	protected void checkDaoConfig() throws IllegalArgumentException {
		assertRegionMetaData(nativePartitionedRegion, "NativePartitionedRegion", DataPolicy.PARTITION);
		assertRegionMetaData(nativeReplicateParent, "NativeReplicateParent", DataPolicy.REPLICATE);
		assertRegionMetaData(nativeReplicateChild, "NativeReplicateChild",
			"/NativeReplicateParent/NativeReplicateChild", DataPolicy.REPLICATE);
		assertRegionMetaData(nativeReplicateGrandchild, "NativeReplicateGrandchild",
			"/NativeReplicateParent/NativeReplicateChild/NativeReplicateGrandchild", DataPolicy.REPLICATE);
	}

}
