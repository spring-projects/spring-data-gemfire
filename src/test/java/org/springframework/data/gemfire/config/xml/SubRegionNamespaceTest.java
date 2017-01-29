/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The SubRegionNamespaceTest class is a test suite of test cases testing the contract and functionality of
 * Region/SubRegion creation in a GemFire Cache.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "subregion-ns.xml", initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings({ "deprecation", "rawtypes", "unused" })
public class SubRegionNamespaceTest {

	@Autowired
	private ApplicationContext context;

    @Test
    public void testNestedRegionsCreated() {
        Cache cache = context.getBean(Cache.class);

        assertNotNull(cache.getRegion("parent"));
        assertNotNull(cache.getRegion("/parent/child"));
        assertNotNull(cache.getRegion("/parent/child/grandchild"));
    }

	@Test
	public void testNestedReplicatedRegions() {
        Region parent = context.getBean("parent", Region.class);
		Region child = context.getBean("/parent/child", Region.class);
		Region grandchild = context.getBean("/parent/child/grandchild", Region.class);

		assertNotNull(child);
		assertEquals("child", child.getName());
		assertEquals("/parent/child", child.getFullPath());
		assertSame(child, parent.getSubregion("child"));
		assertNotNull(grandchild);
		assertEquals("grandchild", grandchild.getName());
		assertEquals("/parent/child/grandchild", grandchild.getFullPath());
		assertSame(grandchild, child.getSubregion("grandchild"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testMixedNestedRegions() {
		Region parent = context.getBean("replicatedParent", Region.class);
		Region child = context.getBean("/replicatedParent/replicatedChild", Region.class);
		Region grandchild = context.getBean("/replicatedParent/replicatedChild/partitionedGrandchild", Region.class);

		assertNotNull(child);
		assertEquals("/replicatedParent/replicatedChild", child.getFullPath());
		assertEquals(child, parent.getSubregion("replicatedChild"));
		assertNotNull(grandchild);
		assertEquals("/replicatedParent/replicatedChild/partitionedGrandchild", grandchild.getFullPath());
		assertSame(grandchild, child.getSubregion("partitionedGrandchild"));
	}

	@Test
	public void testNestedRegionsWithSiblings() {
		Region parent = context.getBean("parentWithSiblings", Region.class);
		Region child1 = context.getBean("/parentWithSiblings/child1", Region.class);
		assertEquals("/parentWithSiblings/child1", child1.getFullPath());
		Region child2 = context.getBean("/parentWithSiblings/child2", Region.class);
		assertEquals("/parentWithSiblings/child2", child2.getFullPath());
		assertSame(child1, parent.getSubregion("child1"));
		assertSame(child2, parent.getSubregion("child2"));
		Region grandchild1 = context.getBean("/parentWithSiblings/child1/grandChild11", Region.class);
		assertEquals("/parentWithSiblings/child1/grandChild11", grandchild1.getFullPath());
	}

	@Test
	@SuppressWarnings("unused" )
	public void testComplexNestedRegions() throws Exception {
		Region parent = context.getBean("complexNested", Region.class);
		Region child1 = context.getBean("/complexNested/child1", Region.class);
		Region child2 = context.getBean("/complexNested/child2", Region.class);
		Region grandchild11 = context.getBean("/complexNested/child1/grandChild11", Region.class);

		ReplicatedRegionFactoryBean grandchild11FactoryBean = context.getBean("&/complexNested/child1/grandChild11",
			ReplicatedRegionFactoryBean.class);

		assertNotNull(grandchild11FactoryBean);

		CacheLoader expectedCacheLoader = TestUtils.readField("cacheLoader", grandchild11FactoryBean);

		assertNotNull(expectedCacheLoader);

		CacheLoader actualCacheLoader = grandchild11.getAttributes().getCacheLoader();

		assertSame(expectedCacheLoader, actualCacheLoader);
	}
}
