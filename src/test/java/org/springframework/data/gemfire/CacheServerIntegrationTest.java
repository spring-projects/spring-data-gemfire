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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import javax.annotation.Resource;

import org.apache.geode.cache.server.CacheServer;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The CacheServerIntegrationTest class is a test suite of test cases testing the functionality of GemFire Cache Servers
 * configured using the Spring Data GemFire XML namespace.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.server.CacheServerFactoryBean
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.3.3
 */
@ContextConfiguration("cache-server-with-subscription-disk-store.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class CacheServerIntegrationTest {

	@Resource(name = "testCacheServer")
	private CacheServer cacheServer;

	protected static boolean createDirectory(final File path) {
		return (path != null && (path.isDirectory() || path.mkdirs()));
	}

	protected static File createFile(final String pathname) {
		return new File(pathname);
	}

	protected static void deleteRecursive(final File path) {
		if (path.isDirectory()) {
			for (File file : path.listFiles()) {
				deleteRecursive(file);
			}
		}

		path.delete();
	}

	@BeforeClass
	public static void setupBeforeClass() {
		assertTrue(createDirectory(createFile("./gemfire/subscription-disk-store")));
	}

	@AfterClass
	public static void tearDownAfterClass() {
		deleteRecursive(createFile("./gemfire"));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void testCacheServerRunningWithSubscription() {
		assertNotNull(cacheServer);
		assertTrue(cacheServer.isRunning());
		assertEquals("localhost", cacheServer.getBindAddress());
		assertNotNull(cacheServer.getGroups());
		assertEquals(1, cacheServer.getGroups().length);
		assertEquals("test-server", cacheServer.getGroups()[0]);
		assertEquals(1, cacheServer.getMaxConnections());
		assertNotNull(cacheServer.getClientSubscriptionConfig());
		assertEquals(512, cacheServer.getClientSubscriptionConfig().getCapacity());
		assertEquals("testSubscriptionDiskStore", cacheServer.getClientSubscriptionConfig().getDiskStoreName());
		assertTrue("ENTRY".equalsIgnoreCase(cacheServer.getClientSubscriptionConfig().getEvictionPolicy()));
	}

}
