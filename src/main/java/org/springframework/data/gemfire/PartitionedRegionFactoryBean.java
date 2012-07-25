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

import java.util.concurrent.ConcurrentMap;

import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionFactory;

/**
 * @author David Turanski
 * 
 */
public class PartitionedRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> {

	@Override
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, String dataPolicy) {
		if (dataPolicy != null) {
			DataPolicy dp = new DataPolicyConverter().convert(dataPolicy);
			Assert.notNull(dp, "Data policy " + dataPolicy + " is invalid");
			Assert.isTrue(dp.withPartitioning(), "Data Policy " + dp.toString()
					+ " is not supported in partitioned regions");
			if (!isPersistent()) {
				regionFactory.setDataPolicy(dp);
			}
			else {
				Assert.isTrue(dp.withPersistence(), "Data Policy " + dp.toString()
						+ "is invalid when persistent is false");
				regionFactory.setDataPolicy(dp);
			}
			return;
		}
		if (isPersistent()) {
			// check first for GemFire 6.5
			if (ConcurrentMap.class.isAssignableFrom(Region.class)) {
				regionFactory.setDataPolicy(DataPolicy.PERSISTENT_PARTITION);
			}
			else {
				throw new IllegalArgumentException(
						"Can define persistent partitions only from GemFire 6.5 onwards - current version is ["
								+ CacheFactory.getVersion() + "]");
			}
		}
		else {
			regionFactory.setDataPolicy(DataPolicy.PARTITION);
		}
	}

}
