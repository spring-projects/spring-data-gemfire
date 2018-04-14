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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;

import org.apache.geode.cache.Cache;
import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration test trying various basic configurations of GemFire through
 * Spring.
 *
 * Made abstract to avoid multiple caches running at the same time.
 *
 * @author Costin Leau
 * @author John Blum
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(locations = "basic-cache.xml")
public class CacheIntegrationTest {

	@Autowired
	ApplicationContext applicationContext;

	Cache cache;

	@After
	public void tearDown() {
		GemfireUtils.close(this.cache);
	}

	@Test
	public void testBasicCache() throws Exception {
		cache = applicationContext.getBean("default-cache",Cache.class);
	}

	@Test
	public void testCacheWithProps() throws Exception {
		cache = applicationContext.getBean("cache-with-props", Cache.class);

		// the name property seems to be ignored
		assertEquals("cache-with-props", cache.getDistributedSystem().getName());
		assertEquals("cache-with-props", cache.getName());
	}

	@Test
	public void testNamedCache() throws Exception {

		cache = applicationContext.getBean("named-cache", Cache.class);

		assertEquals("named-cache", cache.getDistributedSystem().getName());
		assertEquals("named-cache", cache.getName());
	}

	@Test
	public void testCacheWithXml() throws Exception {
		applicationContext.getBean("cache-with-xml", Cache.class);
	}
}
