/*
 * Copyright 2011-2013 the original author or authors.
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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.server.CacheServer;

/**
 * 
 * @author Costin Leau
 * @author David Turanski
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="server-ns.xml",
	initializers=GemfireTestApplicationContextInitializer.class)
public class CacheServerNamespaceTest {

	@Autowired ApplicationContext ctx;

	@Test
	public void testBasicCacheServer() throws Exception {
		CacheServer cacheServer = ctx.getBean("advanced-config", CacheServer.class);
		assertTrue(cacheServer.isRunning());
		assertEquals(1, cacheServer.getGroups().length);
		assertEquals("test-server", cacheServer.getGroups()[0]);
		assertEquals(22, cacheServer.getMaxConnections());

	}
}
