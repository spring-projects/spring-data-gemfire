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
package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import org.junit.Test;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.Region;

/**
 * 
 * @author David Turanski
 * @author John Blum
 */
public class SubRegionTest extends RecreatingContextTest {
	@Override
	protected String location() {
		return "org/springframework/data/gemfire/basic-subregion.xml";
	}

	@Test
	public void testAll() throws Exception {
		testBasic();
		testContext();
		testChildOnly();
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	private void testBasic() throws Exception {
		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setBeanName("gemfireCache");
		cacheFactoryBean.setUseBeanFactoryLocator(false);

		Cache cache = cacheFactoryBean.getObject();

		assertNotNull(cache);

		RegionFactoryBean regionFactory = new ReplicatedRegionFactoryBean();

		regionFactory.setCache(cache);
		regionFactory.setName("Outer");
		regionFactory.afterPropertiesSet();

		Region outer = regionFactory.getObject();

		assertNotNull(outer);
		assertEquals("Outer", outer.getName());
		assertEquals("/Outer", outer.getFullPath());
		assertSame(outer, cache.getRegion("/Outer"));

		RegionFactoryBean subRegionFactory = new RegionFactoryBean() { };

		subRegionFactory.setCache(cache);
		subRegionFactory.setParent(outer);
		subRegionFactory.setName("/Outer/Inner");
		subRegionFactory.setRegionName("Inner");
		subRegionFactory.afterPropertiesSet();

		Region inner = subRegionFactory.getObject();

		assertNotNull(inner);
		assertSame(inner, outer.getSubregion("Inner"));
		assertSame(inner, cache.getRegion("/Outer/Inner"));
	}

	@SuppressWarnings("rawtypes")
	private void testContext() throws Exception {
		Region parent = ctx.getBean("parent", Region.class);
		Region child = ctx.getBean("/parent/child", Region.class);
		assertNotNull(parent.getSubregion("child"));
		assertSame(child, parent.getSubregion("child"));
		assertEquals("/parent/child", child.getFullPath());
	}

	@SuppressWarnings("rawtypes")
	public void testChildOnly() throws Exception {
		Region child = ctx.getBean("/parent/child", Region.class);
		assertEquals("/parent/child", child.getFullPath());
	}
}
