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

package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import javax.annotation.Resource;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.fork.SpringCacheServerProcess;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The ClientSubRegionTest class is a test suite of test cases testing SubRegion functionality from a client
 * GemFire Cache.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.4.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("clientcache-with-subregion-config.xml")
@SuppressWarnings("unused")
public class ClientSubRegionTest {

	@Autowired
	private ClientCache clientCache;

	@Resource(name = "parentTemplate")
	private GemfireTemplate parentTemplate;

	@Resource(name = "childTemplate")
	private GemfireTemplate childTemplate;

	@Resource(name = "Parent")
	private Region parent;

	@Resource(name = "/Parent/Child")
	private Region child;

	@BeforeClass
	public static void startCacheServer() throws Exception {
		ForkUtil.startCacheServer(SpringCacheServerProcess.class.getName() + " "
			+ "/org/springframework/data/gemfire/client/servercache-with-region-for-client.xml");
	}

	@Test
	public void testGemFireSubRegionCreationConfiguration() {
		assertNotNull("The Client Cache was not properly initialized!", clientCache);

		Region parent = clientCache.getRegion("Parent");

		assertNotNull(parent);
		assertEquals("Parent", parent.getName());
		assertEquals("/Parent", parent.getFullPath());

		Region child = parent.getSubregion("Child");

		assertNotNull(child);
		assertEquals("Child", child.getName());
		assertEquals("/Parent/Child", child.getFullPath());

		Region clientCacheChild = clientCache.getRegion("/Parent/Child");

		assertSame(child, clientCacheChild);
	}

	@Test
	public void testSpringSubRegionCreationConfiguration() {
		assertNotNull(parent);
		assertEquals("Parent", parent.getName());
		assertEquals("/Parent", parent.getFullPath());
		assertNotNull(child);
		assertEquals("Child", child.getName());
		assertEquals("/Parent/Child", child.getFullPath());
	}

	@Test
	public void testTemplateCreationConfiguration() {
		assertNotNull(parentTemplate);
		assertSame(parent, parentTemplate.getRegion());
		assertNotNull(childTemplate);
		assertSame(child, childTemplate.getRegion());
	}

}
