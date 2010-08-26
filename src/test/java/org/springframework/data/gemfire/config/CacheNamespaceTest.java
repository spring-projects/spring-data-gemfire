/*
 * Copyright 2010 the original author or authors.
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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("cache-ns.xml")
@DirtiesContext
public class CacheNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@Test
	public void testBasicCache() throws Exception {
		assertTrue(context.containsBean("cache"));
		CacheFactoryBean cfb = (CacheFactoryBean) context.getBean("&cache");
		assertNull(TestUtils.readField("cacheXml", cfb));
		assertNull(TestUtils.readField("properties", cfb));
	}

	@Test
	public void testNamedCache() throws Exception {
		assertTrue(context.containsBean("cache-with-name"));
		CacheFactoryBean cfb = (CacheFactoryBean) context.getBean("&cache-with-name");
		assertNull(TestUtils.readField("cacheXml", cfb));
		assertNull(TestUtils.readField("properties", cfb));
	}

	@Test
	public void testCacheWithXml() throws Exception {
		assertTrue(context.containsBean("cache-with-xml"));
		CacheFactoryBean cfb = (CacheFactoryBean) context.getBean("&cache-with-xml");
		Resource res = TestUtils.readField("cacheXml", cfb);
		assertEquals("cache.xml", res.getFilename());
		assertEquals(context.getBean("props"), TestUtils.readField("properties", cfb));
	}
}