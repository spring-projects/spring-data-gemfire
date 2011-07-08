/*
 * Copyright 2010-2011 the original author or authors.
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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.GemfireBeanFactoryLocator;
import org.springframework.data.gemfire.RecreatingContextTest;
import org.springframework.data.gemfire.TestUtils;

/**
 * @author Costin Leau
 */
public class CacheNamespaceTest extends RecreatingContextTest {

	@Override
	protected String location() {
		return "org/springframework/data/gemfire/config/cache-ns.xml";
	}

	@Test
	public void testBasicCache() throws Exception {
		assertTrue(ctx.containsBean("gemfire-cache"));
		CacheFactoryBean cfb = (CacheFactoryBean) ctx.getBean("&gemfire-cache");
		assertNull(TestUtils.readField("cacheXml", cfb));
		assertNull(TestUtils.readField("properties", cfb));
	}

	@Test
	public void testNamedCache() throws Exception {
		assertTrue(ctx.containsBean("cache-with-name"));
		CacheFactoryBean cfb = (CacheFactoryBean) ctx.getBean("&cache-with-name");
		assertNull(TestUtils.readField("cacheXml", cfb));
		assertNull(TestUtils.readField("properties", cfb));
	}

	@Test
	public void testCacheWithXml() throws Exception {
		assertTrue(ctx.containsBean("cache-with-xml"));
		CacheFactoryBean cfb = (CacheFactoryBean) ctx.getBean("&cache-with-xml");
		Resource res = TestUtils.readField("cacheXml", cfb);
		assertEquals("cache.xml", res.getFilename());
		assertEquals(ctx.getBean("props"), TestUtils.readField("properties", cfb));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testNoBeanFactory() throws Exception {
		assertTrue(ctx.containsBean("no-bl"));
		CacheFactoryBean cfb = (CacheFactoryBean) ctx.getBean("&no-bl");
		GemfireBeanFactoryLocator locator = new GemfireBeanFactoryLocator();
		try {
			assertNotNull(locator.useBeanFactory("cache-with-name"));
			locator.useBeanFactory("no-bl");
		} finally {
			locator.destroy();
		}
	}

}