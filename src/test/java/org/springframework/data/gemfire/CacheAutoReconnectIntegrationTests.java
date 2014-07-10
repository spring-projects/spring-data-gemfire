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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeNotNull;

import java.io.File;

import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.data.gemfire.config.GemfireConstants;

import com.gemstone.gemfire.cache.Cache;

/**
 * The CacheAutoReconnectIntegrationTests class is a tests suite of test cases testing Spring Data GemFire's support
 * of GemFire's Auto-Reconnect functionality release in 8.0.
 *
 * @author John Blum
 * @see org.junit.Test
 * @since 1.5.0
 */
public class CacheAutoReconnectIntegrationTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		assumeNotNull(applicationContext);
		applicationContext.close();
	}

	protected Cache getCache(String configLocation) {
		String baseConfigLocation = File.separator.concat(
			getClass().getPackage().getName().replace('.', File.separatorChar));
		applicationContext = new ClassPathXmlApplicationContext(baseConfigLocation.concat(File.separator).concat(configLocation));
		return applicationContext.getBean(GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME, Cache.class);
	}

	@Test
	public void testAutoReconnectDisabled() {
		Cache cache = getCache("cacheAutoReconnectDisabledIntegrationTests.xml");
		assertNotNull(cache.getDistributedSystem());
		assertNotNull(cache.getDistributedSystem().getProperties());
		assertTrue(Boolean.valueOf(cache.getDistributedSystem().getProperties().getProperty("disable-auto-reconnect")));
	}

	@Test
	public void testAutoReconnectEnabled() {
		Cache cache = getCache("cacheAutoReconnectEnabledIntegrationTests.xml");
		assertNotNull(cache.getDistributedSystem());
		assertNotNull(cache.getDistributedSystem().getProperties());
		assertFalse(Boolean.valueOf(cache.getDistributedSystem().getProperties().getProperty("disable-auto-reconnect")));
	}

}
