/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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

import javax.annotation.Resource;

import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The ColocatedRegionIntegrationTest class is a test suite class containing test cases for JIRA issue SGF-195,
 * concerning colocated Regions in GemFire.
 *
 * @author John Blum
 * @link https://jira.springsource.org/browse/SGF-195
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.3.3
 */
@ContextConfiguration("colocated-region.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@SuppressWarnings("unused")
public class ColocatedRegionIntegrationTest {

	@Resource(name = "colocatedRegion")
	private Region colocatedRegion;

	@Resource(name = "sourceRegion")
	private Region sourceRegion;

	protected static void assertRegionExists(final String expectedRegionName, final Region region) {
		assertNotNull(region);
		assertEquals(String.format("Expected Region with name %1$s; but was %2$s!",
			expectedRegionName, region.getName()), expectedRegionName, region.getName());
	}

	@Test
	public void testRegionsColocated() {
		assertRegionExists("Source", sourceRegion);
		assertRegionExists("Colocated", colocatedRegion);
		assertNotNull(colocatedRegion.getAttributes());
		assertNotNull(colocatedRegion.getAttributes().getPartitionAttributes());
		assertEquals(sourceRegion.getName(), colocatedRegion.getAttributes().getPartitionAttributes().getColocatedWith());
	}

}
