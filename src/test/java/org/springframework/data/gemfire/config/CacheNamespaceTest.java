/*
 * Copyright 2010-2012 the original author or authors.
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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.GemfireBeanFactoryLocator;
import org.springframework.data.gemfire.RecreatingContextTest;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * @author Costin Leau
 */
public class CacheNamespaceTest extends RecreatingContextTest {

	@Override
	protected String location() {
		return "org/springframework/data/gemfire/config/cache-ns.xml";
	}

	@Test
	public void testAll() throws Exception {
		testBasicCache();
		testNamedCache();
		testCacheWithXml();
		// testBasicClientCache();
		// testBasicClientCacheWithXml();
		testHeapTunedCache();
	}

	private void testBasicCache() throws Exception {
		assertTrue(ctx.containsBean("gemfire-cache"));
		CacheFactoryBean cfb = (CacheFactoryBean) ctx.getBean("&gemfire-cache");
		assertNull(TestUtils.readField("cacheXml", cfb));
		assertNull(TestUtils.readField("properties", cfb));
	}

	private void testNamedCache() throws Exception {
		assertTrue(ctx.containsBean("cache-with-name"));
		CacheFactoryBean cfb = (CacheFactoryBean) ctx.getBean("&cache-with-name");
		assertNull(TestUtils.readField("cacheXml", cfb));
		assertNull(TestUtils.readField("properties", cfb));
	}

	private void testCacheWithXml() throws Exception {
		assertTrue(ctx.containsBean("cache-with-xml"));
		CacheFactoryBean cfb = (CacheFactoryBean) ctx.getBean("&cache-with-xml");
		Resource res = TestUtils.readField("cacheXml", cfb);
		assertEquals("gemfire-cache.xml", res.getFilename());
		assertEquals(ctx.getBean("props"), TestUtils.readField("properties", cfb));

		assertEquals(Boolean.FALSE, TestUtils.readField("pdxIgnoreUnreadFields", cfb));
		assertEquals(Boolean.TRUE, TestUtils.readField("pdxPersistent", cfb));

	}

	@Test(expected = IllegalArgumentException.class)
	public void testNoBeanFactory() throws Exception {
		assertTrue(ctx.containsBean("no-bl"));
		CacheFactoryBean cfb = (CacheFactoryBean) ctx.getBean("&no-bl");

		assertThat((Boolean) ReflectionTestUtils.getField(cfb, "useBeanFactoryLocator"), is(false));
		assertThat(ReflectionTestUtils.getField(cfb, "factoryLocator"), is(nullValue()));

		GemfireBeanFactoryLocator locator = new GemfireBeanFactoryLocator();
		try {
			assertNotNull(locator.useBeanFactory("cache-with-name"));
			locator.useBeanFactory("no-bl");
		}
		finally {
			locator.destroy();
		}
	}

	@Test
	public void testBasicClientCache() throws Exception {
		assertTrue(ctx.containsBean("client-cache"));
		ClientCacheFactoryBean cfb = (ClientCacheFactoryBean) ctx.getBean("&client-cache");
		assertNull(TestUtils.readField("cacheXml", cfb));
		assertNull(TestUtils.readField("properties", cfb));
	}

	@Test
	public void testBasicClientCacheWithXml() throws Exception {
		assertTrue(ctx.containsBean("client-cache-with-xml"));
		ClientCacheFactoryBean cfb = (ClientCacheFactoryBean) ctx.getBean("&client-cache-with-xml");
		Resource res = TestUtils.readField("cacheXml", cfb);
		assertEquals("gemfire-client-cache.xml", res.getFilename());
	}

	private void testHeapTunedCache() throws Exception {
		assertTrue(ctx.containsBean("heap-tuned-cache"));
		CacheFactoryBean cfb = (CacheFactoryBean) ctx.getBean("&heap-tuned-cache");
		Float chp = (Float) TestUtils.readField("criticalHeapPercentage", cfb);
		Float ehp = (Float) TestUtils.readField("evictionHeapPercentage", cfb);
		assertEquals(70, chp, 0.0001);
		assertEquals(60, ehp, 0.0001);
	}
}