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

import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * The LookupSubRegionTest class is a test suite of test cases testing the contract and functionality of Region lookups
 * using Spring Data GemFire configuration and GemFire native cache.xml.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.Region
 * @since 1.3.3
 * @since 7.0.1 (GemFire)
 */
@RunWith(SpringRunner.class)
@ContextConfiguration("lookupSubRegion.xml")
@SuppressWarnings("unused")
public class LookupSubRegionTest {

	@Autowired
	private ApplicationContext applicationContext;

	private void assertRegionExists(final String expectedRegionName, final String expectedRegionPath, final Region region) {

		assertNotNull(String.format("The Region with name (%1$s) at path (%2$s) was null!",
			expectedRegionName, expectedRegionPath), region);
		assertEquals(String.format("Expected Region name of %1$s; but was %2$s!", expectedRegionName, region.getName()),
			expectedRegionName, region.getName());
		assertEquals(String.format("Expected Region path of %1$s; but was %2$s!", expectedRegionPath, region.getFullPath()),
			expectedRegionPath, region.getFullPath());
	}

	@Test
	public void testDirectLookup() {

		Region accounts = applicationContext.getBean("/Customers/Accounts", Region.class);

		assertRegionExists("Accounts", "/Customers/Accounts", accounts);
		assertFalse(applicationContext.containsBean("Customers/Accounts"));
		assertFalse(applicationContext.containsBean("/Customers"));
		assertFalse(applicationContext.containsBean("Customers"));

		Region items = applicationContext.getBean("Customers/Accounts/Orders/Items", Region.class);

		assertRegionExists("Items", "/Customers/Accounts/Orders/Items", items);
		assertFalse(applicationContext.containsBean("/Customers/Accounts/Orders/Items"));
		assertFalse(applicationContext.containsBean("/Customers/Accounts/Orders"));
		assertFalse(applicationContext.containsBean("Customers/Accounts/Orders"));
	}

	@Test
	public void testNestedLookup() {

		Region parent = applicationContext.getBean("Parent", Region.class);

		assertRegionExists("Parent", "/Parent", parent);
		assertFalse(applicationContext.containsBean("/Parent"));

		Region child = applicationContext.getBean("/Parent/Child", Region.class);

		assertRegionExists("Child", "/Parent/Child", child);
		assertFalse(applicationContext.containsBean("Parent/Child"));

		Region grandchild = applicationContext.getBean("/Parent/Child/Grandchild", Region.class);

		assertRegionExists("Grandchild", "/Parent/Child/Grandchild", grandchild);
		assertFalse(applicationContext.containsBean("Parent/Child/Grandchild"));
	}
}
