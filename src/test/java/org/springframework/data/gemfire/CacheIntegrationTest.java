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

package org.springframework.data.gemfire;


import junit.framework.Assert;

import org.junit.Test;

import com.gemstone.gemfire.cache.Cache;

/**
 * Integration test trying various basic configurations of GemFire through Spring.
 * 
 * Made abstract to avoid multiple caches running at the same time. 
 * 
 * @author Costin Leau
 */
public class CacheIntegrationTest extends RecreatingContextTest{

	@Override
	protected String location() {
		return "org/springframework/data/gemfire/basic-cache.xml";
	}

	@Test
	public void testBasicCache() throws Exception {
		ctx.getBean("default-cache");
	}

	@Test
	public void testCacheWithProps() throws Exception {
		Cache cache = ctx.getBean("cache-with-props", Cache.class);
		// the name property seems to be ignored
		Assert.assertEquals("cache-with-props", cache.getDistributedSystem().getName());
		Assert.assertEquals("cache-with-props", cache.getName());
	}

	@Test
	public void testNamedCache() throws Exception {
		Cache cache = ctx.getBean("named-cache", Cache.class);
		Assert.assertEquals("cache-with-props", cache.getDistributedSystem().getName());
		Assert.assertEquals("cache-with-props", cache.getName());
	}

	@Test
	public void testCacheWithXml() throws Exception {
		Cache cache = ctx.getBean("cache-with-xml", Cache.class);
	}
}
