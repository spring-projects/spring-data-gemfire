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
package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import org.junit.Test;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;

/**
 * 
 * @author David Turanski
 * 
 */
public class SubRegionTest extends RecreatingContextTest {
	@Override
	protected String location() {
		return "org/springframework/data/gemfire/basic-subregion.xml";
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Test
	public void testBasic() throws Exception {
		CacheFactoryBean cfb = new CacheFactoryBean();
		cfb.setUseBeanFactoryLocator(false);
		cfb.afterPropertiesSet();
		GemFireCache cache = cfb.getObject();
		RegionFactoryBean rfb = new ReplicatedRegionFactoryBean();
		rfb.setCache(cache);
		rfb.setName("parent");
		rfb.afterPropertiesSet();
		Region parent = rfb.getObject();

		SubRegionFactoryBean srfb = new SubRegionFactoryBean();
		srfb.setParent(parent);
		srfb.setName("/parent/child");
		srfb.setRegionName("child");
		srfb.afterPropertiesSet();
		Region child = srfb.getObject();

		assertNotNull(parent.getSubregion("child"));
		assertSame(child, parent.getSubregion("child"));

		cache.close();
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testContext() throws Exception {
		Region parent = ctx.getBean("parent", Region.class);
		Region child = ctx.getBean("/parent/child", Region.class);
		assertNotNull(parent.getSubregion("child"));
		assertSame(child, parent.getSubregion("child"));
		assertEquals("/parent/child", child.getFullPath());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testChildOnly() throws Exception {
		Region child = ctx.getBean("/parent/child", Region.class);
		assertEquals("/parent/child", child.getFullPath());
	}
}
