/*
 * Copyright 2010-2013 the original author or authors.
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

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Properties;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.GemfireBeanFactoryLocator;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.util.ReflectionTestUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.util.GatewayConflictHelper;
import com.gemstone.gemfire.cache.util.GatewayConflictResolver;
import com.gemstone.gemfire.cache.util.TimestampedEntryEvent;

/**
 * @author Costin Leau
 * @author John Blum
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "cache-ns.xml", initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class CacheNamespaceTest{

	@Autowired
	private ApplicationContext context;

	@Test
	public void testNoNamedCache() throws Exception {
		assertTrue(context.containsBean("gemfireCache"));
		assertTrue(context.containsBean("gemfire-cache")); // assert alias is registered

		Cache gemfireCache = context.getBean("gemfireCache", Cache.class);

		assertNotNull(gemfireCache);
		assertNotNull(gemfireCache.getDistributedSystem());
		assertNotNull(gemfireCache.getDistributedSystem().getProperties());
		assertTrue(Boolean.parseBoolean(gemfireCache.getDistributedSystem().getProperties()
			.getProperty("disable-auto-reconnect")));

		CacheFactoryBean cacheFactoryBean = context.getBean("&gemfireCache", CacheFactoryBean.class);

		assertNull(TestUtils.readField("cacheXml", cacheFactoryBean));

		Properties gemfireProperties = cacheFactoryBean.getProperties();

		assertNotNull(gemfireProperties);
		assertTrue(gemfireProperties.containsKey("disable-auto-reconnect"));
		assertTrue(Boolean.parseBoolean(gemfireProperties.getProperty("disable-auto-reconnect")));
		assertFalse(cacheFactoryBean.getEnableAutoReconnect());
	}

	@Test
	public void testNamedCache() throws Exception {
		assertTrue(context.containsBean("cache-with-name"));

		Cache gemfireCache = context.getBean("gemfireCache", Cache.class);

		assertTrue(Boolean.parseBoolean(gemfireCache.getDistributedSystem().getProperties()
			.getProperty("disable-auto-reconnect")));

		CacheFactoryBean cacheFactoryBean = context.getBean("&cache-with-name", CacheFactoryBean.class);

		assertNull(TestUtils.readField("cacheXml", cacheFactoryBean));

		Properties gemfireProperties = cacheFactoryBean.getProperties();

		assertNotNull(gemfireProperties);
		assertTrue(gemfireProperties.containsKey("disable-auto-reconnect"));
		assertTrue(Boolean.parseBoolean(gemfireProperties.getProperty("disable-auto-reconnect")));
		assertFalse(cacheFactoryBean.getEnableAutoReconnect());
	}

	@Test
	public void testCacheWithXmlAndProperties() throws Exception {
		assertTrue(context.containsBean("cache-with-xml-and-props"));

		CacheFactoryBean cacheFactoryBean = context.getBean("&cache-with-xml-and-props", CacheFactoryBean.class);
		Resource cacheXmlResource = TestUtils.readField("cacheXml", cacheFactoryBean);

		assertEquals("gemfire-cache.xml", cacheXmlResource.getFilename());
		assertTrue(context.containsBean("gemfireProperties"));
		assertEquals(context.getBean("gemfireProperties"), TestUtils.readField("properties", cacheFactoryBean));
		assertEquals(Boolean.TRUE, TestUtils.readField("pdxReadSerialized", cacheFactoryBean));
		assertEquals(Boolean.FALSE, TestUtils.readField("pdxIgnoreUnreadFields", cacheFactoryBean));
		assertEquals(Boolean.TRUE, TestUtils.readField("pdxPersistent", cacheFactoryBean));

	}

	@Test
	public void testCacheWithGatewayConflictResolver() {
		Cache cache = context.getBean("cache-with-gateway-conflict-resolver", Cache.class);

		assertTrue(cache.getGatewayConflictResolver() instanceof TestGatewayConflictResolver);
	}

	@Test
	public void testCacheWithAutoReconnectDisabled() {
		assertTrue(context.containsBean("cache-with-auto-reconnect-disabled"));

		Cache gemfireCache = context.getBean("cache-with-auto-reconnect-disabled", Cache.class);

		assertTrue(Boolean.parseBoolean(gemfireCache.getDistributedSystem().getProperties()
			.getProperty("disable-auto-reconnect")));

		CacheFactoryBean cacheFactoryBean = context.getBean("&cache-with-auto-reconnect-disabled", CacheFactoryBean.class);

		assertFalse(cacheFactoryBean.getEnableAutoReconnect());
	}

	@Test
	public void testCacheWithAutoReconnectEnabled() {
		assertTrue(context.containsBean("cache-with-auto-reconnect-enabled"));

		Cache gemfireCache = context.getBean("cache-with-auto-reconnect-enabled", Cache.class);

		assertFalse(Boolean.parseBoolean(gemfireCache.getDistributedSystem().getProperties()
			.getProperty("disable-auto-reconnect")));

		CacheFactoryBean cacheFactoryBean = context.getBean("&cache-with-auto-reconnect-enabled", CacheFactoryBean.class);

		assertTrue(cacheFactoryBean.getEnableAutoReconnect());
	}

	@Test
	public void testHeapTunedCache() throws Exception {
		assertTrue(context.containsBean("heap-tuned-cache"));

		CacheFactoryBean cacheFactoryBean = context.getBean("&heap-tuned-cache", CacheFactoryBean.class);

		Float criticalHeapPercentage = (Float) TestUtils.readField("criticalHeapPercentage", cacheFactoryBean);
		Float evictionHeapPercentage = (Float) TestUtils.readField("evictionHeapPercentage", cacheFactoryBean);

		assertEquals(70.0f, criticalHeapPercentage, 0.0001);
		assertEquals(60.0f, evictionHeapPercentage, 0.0001);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testNoBeanFactoryLocator() throws Exception {
		assertTrue(context.containsBean("no-bean-factory-locator"));

		CacheFactoryBean cacheFactoryBean = context.getBean("&no-bean-factory-locator", CacheFactoryBean.class);

		assertThat(ReflectionTestUtils.getField(cacheFactoryBean, "factoryLocator"), is(nullValue()));

		GemfireBeanFactoryLocator beanFactoryLocator = new GemfireBeanFactoryLocator();

		try {
			assertNotNull(beanFactoryLocator.useBeanFactory("cache-with-name"));
			beanFactoryLocator.useBeanFactory("no-bean-factory-locator");
		}
		finally {
			beanFactoryLocator.destroy();
		}
	}

	@Test
	public void testNamedClientCache() throws Exception {
		assertTrue(context.containsBean("client-cache-with-name"));

		ClientCacheFactoryBean clientCacheFactoryBean = context.getBean("&client-cache-with-name", ClientCacheFactoryBean.class);

		assertNull(TestUtils.readField("cacheXml", clientCacheFactoryBean));

		Properties gemfireProperties = clientCacheFactoryBean.getProperties();

		assertNotNull(gemfireProperties);
		assertTrue(gemfireProperties.isEmpty());
	}

	@Test
	public void testClientCacheWithXml() throws Exception {
		assertTrue(context.containsBean("client-cache-with-xml"));

		ClientCacheFactoryBean clientCacheFactoryBean = context.getBean("&client-cache-with-xml", ClientCacheFactoryBean.class);
		Resource cacheXmlResource = TestUtils.readField("cacheXml", clientCacheFactoryBean);

		assertEquals("gemfire-client-cache.xml", cacheXmlResource.getFilename());

		Properties gemfireProperties = clientCacheFactoryBean.getProperties();

		assertNotNull(gemfireProperties);
		assertTrue(gemfireProperties.isEmpty());
	}

	public static class TestGatewayConflictResolver implements GatewayConflictResolver {
		@Override
		public void onEvent(TimestampedEntryEvent arg0, GatewayConflictHelper arg1) {
			throw new UnsupportedOperationException("Not Implemented!");
		}
	}

}
