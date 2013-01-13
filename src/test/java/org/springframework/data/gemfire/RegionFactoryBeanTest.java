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

import org.springframework.data.gemfire.support.AbstractRegionFactoryBeanTest;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;

/**
 * @author David Turanski
 * 
 */
public class RegionFactoryBeanTest extends AbstractRegionFactoryBeanTest {

	@SuppressWarnings("rawtypes")
	private RegionFactoryBeanConfig defaultConfig() {
		return new RegionFactoryBeanConfig(new RegionFactoryBean(), "default") {

			@Override
			public void verify() {
				Region region = regionFactoryBean.getRegion();
				assertEquals(DataPolicy.DEFAULT, region.getAttributes().getDataPolicy());
			}

			@Override
			public void configureRegionFactoryBean() {
				// TODO Auto-generated method stub
			}
		};

	}

	@SuppressWarnings("rawtypes")
	private RegionFactoryBeanConfig persistent() {
		return new RegionFactoryBeanConfig(new RegionFactoryBean(), "persistent") {

			@Override
			public void verify() {
				Region region = regionFactoryBean.getRegion();
				assertEquals(DataPolicy.PERSISTENT_REPLICATE, region.getAttributes().getDataPolicy());
			}

			@Override
			public void configureRegionFactoryBean() {
				regionFactoryBean.setPersistent(true);
			}
		};
	}

	@SuppressWarnings("rawtypes")
	private RegionFactoryBeanConfig invalidPersistence() {
		return new RegionFactoryBeanConfig(new RegionFactoryBean(), "invalid-persistence") {

			@Override
			public void configureRegionFactoryBean() {
				regionFactoryBean.setDataPolicy("persistent_replicate");
				regionFactoryBean.setPersistent(false);
			}

			@Override
			public void verify() {
				assertNotNull(this.exception);
				assertEquals("Data policy persistent_replicate is invalid when persistent is false",
						exception.getMessage());
			}
		};
	}

	@Override
	protected void createRegionFactoryBeanConfigs() {
		addRFBConfig(defaultConfig());
		addRFBConfig(persistent());
		addRFBConfig(invalidPersistence());
	}
}
