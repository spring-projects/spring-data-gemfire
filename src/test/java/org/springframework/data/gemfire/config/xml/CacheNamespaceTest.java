/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.springframework.data.gemfire.support.GemfireBeanFactoryLocator.newBeanFactoryLocator;

import java.util.Properties;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.util.GatewayConflictHelper;
import org.apache.geode.cache.util.GatewayConflictResolver;
import org.apache.geode.cache.util.TimestampedEntryEvent;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Unit tests for {@link CacheParser}.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.springframework.data.gemfire.config.xml.CacheParser
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(locations = "cache-ns.xml")
@SuppressWarnings("unused")
public class CacheNamespaceTest{

	@Autowired
	private ApplicationContext applicationContext;

	@Test
	public void testNoNamedCache() throws Exception {

		assertTrue(applicationContext.containsBean("gemfireCache"));
		assertTrue(applicationContext.containsBean("gemfire-cache"));

		CacheFactoryBean cacheFactoryBean = applicationContext.getBean("&gemfireCache", CacheFactoryBean.class);

		assertNull(cacheFactoryBean.getCacheXml());

		Properties gemfireProperties = cacheFactoryBean.getProperties();

		assertNotNull(gemfireProperties);
		assertFalse(cacheFactoryBean.getEnableAutoReconnect());
		assertTrue(gemfireProperties.containsKey("disable-auto-reconnect"));
		assertTrue(Boolean.parseBoolean(gemfireProperties.getProperty("disable-auto-reconnect")));
		assertFalse(cacheFactoryBean.getUseClusterConfiguration());
		assertTrue(gemfireProperties.containsKey("use-cluster-configuration"));
		assertFalse(Boolean.parseBoolean(gemfireProperties.getProperty("use-cluster-configuration")));

		Cache gemfireCache = applicationContext.getBean("gemfireCache", Cache.class);

		assertNotNull(gemfireCache);
		assertNotNull(gemfireCache.getDistributedSystem());
		assertNotNull(gemfireCache.getDistributedSystem().getProperties());
		assertNotNull(gemfireCache.getDistributedSystem().getProperties().containsKey("disable-auto-reconnect"));
		assertTrue(Boolean.parseBoolean(gemfireCache.getDistributedSystem().getProperties()
			.getProperty("disable-auto-reconnect")));
	}

	@Test
	public void testNamedCache() throws Exception {

		assertTrue(applicationContext.containsBean("cache-with-name"));

		CacheFactoryBean cacheFactoryBean =
			applicationContext.getBean("&cache-with-name", CacheFactoryBean.class);

		assertNull(cacheFactoryBean.getCacheXml());

		Properties gemfireProperties = cacheFactoryBean.getProperties();

		assertNotNull(gemfireProperties);
		assertFalse(cacheFactoryBean.getEnableAutoReconnect());
		assertTrue(gemfireProperties.containsKey("disable-auto-reconnect"));
		assertTrue(Boolean.parseBoolean(gemfireProperties.getProperty("disable-auto-reconnect")));
		assertFalse(cacheFactoryBean.getUseClusterConfiguration());
		assertTrue(gemfireProperties.containsKey("use-cluster-configuration"));
		assertFalse(Boolean.parseBoolean(gemfireProperties.getProperty("use-cluster-configuration")));

		Cache gemfireCache = applicationContext.getBean("gemfireCache", Cache.class);

		assertTrue(Boolean.parseBoolean(gemfireCache.getDistributedSystem().getProperties()
			.getProperty("disable-auto-reconnect")));

		assertFalse(Boolean.parseBoolean(gemfireCache.getDistributedSystem().getProperties()
			.getProperty("use-cluster-configuration")));
	}

	@Test
	public void testCacheWithAutoReconnectDisabled() throws Exception {

		assertTrue(applicationContext.containsBean("cache-with-auto-reconnect-disabled"));

		CacheFactoryBean cacheFactoryBean =
			applicationContext.getBean("&cache-with-auto-reconnect-disabled", CacheFactoryBean.class);

		assertFalse(cacheFactoryBean.getEnableAutoReconnect());

		Cache gemfireCache = applicationContext.getBean("cache-with-auto-reconnect-disabled", Cache.class);

		assertTrue(Boolean.parseBoolean(gemfireCache.getDistributedSystem().getProperties()
			.getProperty("disable-auto-reconnect")));
	}

	@Test
	public void testCacheWithAutoReconnectEnabled() throws Exception {

		assertTrue(applicationContext.containsBean("cache-with-auto-reconnect-enabled"));

		CacheFactoryBean cacheFactoryBean =
			applicationContext.getBean("&cache-with-auto-reconnect-enabled", CacheFactoryBean.class);

		assertTrue(cacheFactoryBean.getEnableAutoReconnect());

		Cache gemfireCache = applicationContext.getBean("cache-with-auto-reconnect-enabled", Cache.class);

		assertFalse(Boolean.parseBoolean(gemfireCache.getDistributedSystem().getProperties()
			.getProperty("disable-auto-reconnect")));
	}

	@Test
	public void testCacheWithGatewayConflictResolver() {

		Cache cache = applicationContext.getBean("cache-with-gateway-conflict-resolver", Cache.class);

		assertTrue(cache.getGatewayConflictResolver() instanceof TestGatewayConflictResolver);
	}

	@Test(expected = IllegalStateException.class)
	public void testCacheWithNoBeanFactoryLocator() throws Exception {

		assertTrue(applicationContext.containsBean("cache-with-no-bean-factory-locator"));

		CacheFactoryBean cacheFactoryBean =
			applicationContext.getBean("&cache-with-no-bean-factory-locator", CacheFactoryBean.class);

		assertNull(cacheFactoryBean.getBeanFactoryLocator());

		newBeanFactoryLocator().useBeanFactory("cache-with-no-bean-factory-locator");
	}

	@Test
	public void testCacheWithUseClusterConfigurationDisabled() {

		assertTrue(applicationContext.containsBean("cache-with-use-cluster-configuration-disabled"));

		CacheFactoryBean cacheFactoryBean =
			applicationContext.getBean("&cache-with-use-cluster-configuration-disabled", CacheFactoryBean.class);

		assertFalse(cacheFactoryBean.getEnableAutoReconnect());

		Cache gemfireCache =
			applicationContext.getBean("cache-with-use-cluster-configuration-disabled", Cache.class);

		assertFalse(Boolean.parseBoolean(gemfireCache.getDistributedSystem().getProperties()
			.getProperty("use-cluster-configuration")));
	}

	@Test
	public void testCacheWithUseClusterConfigurationEnabled() {

		assertTrue(applicationContext.containsBean("cache-with-use-cluster-configuration-enabled"));

		CacheFactoryBean cacheFactoryBean =
			applicationContext.getBean("&cache-with-use-cluster-configuration-enabled", CacheFactoryBean.class);

		assertTrue(cacheFactoryBean.getUseClusterConfiguration());

		Cache gemfireCache =
			applicationContext.getBean("cache-with-use-cluster-configuration-enabled", Cache.class);

		assertTrue(Boolean.parseBoolean(gemfireCache.getDistributedSystem().getProperties()
			.getProperty("use-cluster-configuration")));
	}

	@Test
	public void testCacheWithXmlAndProperties() throws Exception {

		assertTrue(applicationContext.containsBean("cache-with-xml-and-props"));

		CacheFactoryBean cacheFactoryBean = applicationContext.getBean("&cache-with-xml-and-props", CacheFactoryBean.class);

		Resource cacheXmlResource = cacheFactoryBean.getCacheXml();

		assertEquals("gemfire-cache.xml", cacheXmlResource.getFilename());
		assertTrue(applicationContext.containsBean("gemfireProperties"));
		assertEquals(applicationContext.getBean("gemfireProperties"), TestUtils.readField("properties", cacheFactoryBean));
		assertEquals(Boolean.TRUE, TestUtils.readField("pdxReadSerialized", cacheFactoryBean));
		assertEquals(Boolean.FALSE, TestUtils.readField("pdxIgnoreUnreadFields", cacheFactoryBean));
		assertEquals(Boolean.TRUE, TestUtils.readField("pdxPersistent", cacheFactoryBean));
	}

	@Test
	public void testHeapTunedCache() throws Exception {

		assertTrue(applicationContext.containsBean("heap-tuned-cache"));

		CacheFactoryBean cacheFactoryBean =
			applicationContext.getBean("&heap-tuned-cache", CacheFactoryBean.class);

		Float criticalHeapPercentage = cacheFactoryBean.getCriticalHeapPercentage();
		Float evictionHeapPercentage = cacheFactoryBean.getEvictionHeapPercentage();

		assertEquals(70.0f, criticalHeapPercentage, 0.0001);
		assertEquals(60.0f, evictionHeapPercentage, 0.0001);
	}

	@Test
	public void testOffHeapTunedCache() throws Exception {

		assertTrue(applicationContext.containsBean("off-heap-tuned-cache"));

		CacheFactoryBean cacheFactoryBean =
			applicationContext.getBean("&off-heap-tuned-cache", CacheFactoryBean.class);

		Float criticalOffHeapPercentage = cacheFactoryBean.getCriticalOffHeapPercentage();
		Float evictionOffHeapPercentage = cacheFactoryBean.getEvictionOffHeapPercentage();

		assertEquals(90.0f, criticalOffHeapPercentage, 0.0001);
		assertEquals(50.0f, evictionOffHeapPercentage, 0.0001);
	}

	@Test
	public void namedClientCacheWithNoPropertiesAndNoCacheXml() throws Exception {

		assertTrue(applicationContext.containsBean("client-cache-with-name"));

		ClientCacheFactoryBean clientCacheFactoryBean =
			applicationContext.getBean("&client-cache-with-name", ClientCacheFactoryBean.class);

		assertNull(clientCacheFactoryBean.getCacheXml());
		assertNull(clientCacheFactoryBean.getProperties());
	}

	@Test
	public void clientCacheWithXmlNoProperties() throws Exception {

		assertTrue(applicationContext.containsBean("client-cache-with-xml"));

		ClientCacheFactoryBean clientCacheFactoryBean =
			applicationContext.getBean("&client-cache-with-xml", ClientCacheFactoryBean.class);

		Resource cacheXmlResource = clientCacheFactoryBean.getCacheXml();

		assertEquals("gemfire-client-cache.xml", cacheXmlResource.getFilename());

		assertNull(clientCacheFactoryBean.getProperties());
	}

	public static class TestGatewayConflictResolver implements GatewayConflictResolver {

		@Override
		public void onEvent(TimestampedEntryEvent arg0, GatewayConflictHelper arg1) {
			throw new UnsupportedOperationException("Not Implemented!");
		}
	}
}
