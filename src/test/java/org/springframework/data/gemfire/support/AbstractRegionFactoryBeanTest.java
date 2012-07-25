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
package org.springframework.data.gemfire.support;

import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.RegionFactoryBean;

import com.gemstone.gemfire.cache.GemFireCache;

/**
 * @author David Turanski
 * 
 */
public abstract class AbstractRegionFactoryBeanTest {
	protected GemFireCache cache;

	Map<String, RegionFactoryBeanConfig> regionFactoryBeanConfigs = new HashMap<String, RegionFactoryBeanConfig>();

	@Before
	public void setUp() throws Exception {
		CacheFactoryBean cfb = new CacheFactoryBean();
		cfb.setBeanName("gemfire-cache");
		cfb.setUseBeanFactoryLocator(false);
		cfb.afterPropertiesSet();
		cache = cfb.getObject();
	}

	@Test
	public void testAll() throws Exception {
		createRegionFactoryBeanConfigs();
		for (RegionFactoryBeanConfig rfbc : regionFactoryBeanConfigs.values()) {
			rfbc.test();
		}
	}

	protected void addRFBConfig(RegionFactoryBeanConfig rfbc) {
		if (regionFactoryBeanConfigs.containsKey(rfbc.regionName)) {
			throw new RuntimeException("duplicate region name " + rfbc.regionName);
		}
		regionFactoryBeanConfigs.put(rfbc.regionName, rfbc);
	}

	@After
	public void tearDown() {
		cache.close();
	}

	public abstract class RegionFactoryBeanConfig {
		@SuppressWarnings("rawtypes")
		public final RegionFactoryBean regionFactoryBean;

		public final String regionName;

		public Exception exception;

		@SuppressWarnings("rawtypes")
		public RegionFactoryBeanConfig(RegionFactoryBean rfb, String regionName) {
			this.regionFactoryBean = rfb;
			this.regionName = regionName;
			rfb.setBeanName(regionName);
			rfb.setCache(cache);
		}

		public void test() {
			configureRegionFactoryBean();
			try {
				regionFactoryBean.afterPropertiesSet();
			}
			catch (Exception e) {
				this.exception = e;
			}
			verify();
		}

		public abstract void configureRegionFactoryBean();

		public abstract void verify();
	}

	protected abstract void createRegionFactoryBeanConfigs();
}
