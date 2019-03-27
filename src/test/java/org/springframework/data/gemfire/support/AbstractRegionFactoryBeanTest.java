/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire.support;

import java.io.File;
import java.io.FilenameFilter;
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.test.StubCache;

import com.gemstone.gemfire.cache.GemFireCache;

/**
 * @author David Turanski
 * @author John Blum
 */
public abstract class AbstractRegionFactoryBeanTest {

	private GemFireCache cache;

	private Map<String, RegionFactoryBeanConfig> regionFactoryBeanConfigs = new HashMap<String, RegionFactoryBeanConfig>();

	@AfterClass
	public static void cleanUp() {
		for (String name : new File(".").list(new FilenameFilter() {
			@Override public boolean accept(File dir, String name) {
				return name.startsWith("BACKUP");
			}
		})) {
			new File(name).delete();
		}
	}

	@Before
	public void setUp() throws Exception {
		cache = new StubCache();
	}

	@After
	public void tearDown() {
		cache.close();
		cache = null;
	}

	@Test
	public void testAll() throws Exception {
		createRegionFactoryBeanConfigs();

		for (RegionFactoryBeanConfig regionFactoryBeanConfig : regionFactoryBeanConfigs.values()) {
			regionFactoryBeanConfig.test();
		}
	}

	protected void add(RegionFactoryBeanConfig regionFactoryBeanConfig) {
		if (regionFactoryBeanConfigs.containsKey(regionFactoryBeanConfig.regionName)) {
			throw new RuntimeException("duplicate region name " + regionFactoryBeanConfig.regionName);
		}

		regionFactoryBeanConfigs.put(regionFactoryBeanConfig.regionName, regionFactoryBeanConfig);
	}

	protected abstract void createRegionFactoryBeanConfigs();

	public abstract class RegionFactoryBeanConfig {

		public Exception exception;

		@SuppressWarnings("rawtypes")
		public final RegionFactoryBean regionFactoryBean;

		public final String regionName;

		@SuppressWarnings("rawtypes")
		public RegionFactoryBeanConfig(RegionFactoryBean regionFactoryBean, String regionName) {
			this.regionFactoryBean = regionFactoryBean;
			this.regionName = regionName;
			regionFactoryBean.setBeanName(regionName);
			regionFactoryBean.setCache(cache);
		}

		public abstract void configureRegionFactoryBean();

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

		public abstract void verify();
	}

}
